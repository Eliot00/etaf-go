;;; etaf-go-rules.el --- Captures, ko and suicide for Go -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; A small, self-contained rule engine used to replay SGF records and to let
;; a reader try alternative moves.  It owns move legality only; scoring,
;; komi, life and death judgement stay with the SGF author.
;;
;; A board is a square vector of 0 (empty), 1 (black) and 2 (white).
;; A point is a zero-based index into that vector.

;;; Code:
(require 'cl-lib)

(defconst etaf-go--black 1)
(defconst etaf-go--white 2)

(defun etaf-go--color-name (color)
  "Return the Chinese name of stone COLOR."
  (pcase color
    (1 "黑")
    (2 "白")
    (_ "空")))

(defun etaf-go--opponent (color)
  "Return the other color than COLOR."
  (if (= color etaf-go--black) etaf-go--white etaf-go--black))

(defun etaf-go--neighbors (size point)
  "Return the orthogonal neighbors of POINT on a SIZE square board."
  (let ((row (/ point size))
        (column (% point size))
        neighbors)
    (when (> row 0) (push (- point size) neighbors))
    (when (< row (1- size)) (push (+ point size) neighbors))
    (when (> column 0) (push (1- point) neighbors))
    (when (< column (1- size)) (push (1+ point) neighbors))
    (nreverse neighbors)))

(defun etaf-go--group (board size start)
  "Return the group at START on BOARD as (:stones LIST :liberties LIST)."
  (let ((color (aref board start))
        (seen (make-vector (length board) nil))
        (liberty-seen (make-vector (length board) nil))
        (stack (list start))
        stones liberties)
    (aset seen start t)
    (while stack
      (let ((point (pop stack)))
        (push point stones)
        (dolist (next (etaf-go--neighbors size point))
          (let ((value (aref board next)))
            (cond
             ((= value 0)
              (unless (aref liberty-seen next)
                (aset liberty-seen next t)
                (push next liberties)))
             ((= value color)
              (unless (aref seen next)
                (aset seen next t)
                (push next stack))))))))
    (list :stones (nreverse stones) :liberties (nreverse liberties))))

(defun etaf-go--play (board size color point ko)
  "Play COLOR at POINT on BOARD with KO forbidden.
Return a plist.  On success it holds :ok t, :board, :captured, :ko and
:liberties.  On a refused move it holds :ok nil and :reason."
  (cond
   ((not (integerp point))
    (list :ok nil :reason "弃着不是棋盘上的落子"))
   ((or (< point 0) (>= point (length board)))
    (list :ok nil :reason "落子超出棋盘"))
   ((/= (aref board point) 0)
    (list :ok nil :reason "这里已经有子了"))
   ((and (integerp ko) (= point ko))
    (list :ok nil :reason "打劫：不能立即提回"))
   (t
    (let ((next (copy-sequence board))
          (enemy (etaf-go--opponent color))
          (checked (make-vector (length board) nil))
          captured)
      (aset next point color)
      (dolist (neighbor (etaf-go--neighbors size point))
        (when (and (= (aref next neighbor) enemy)
                   (not (aref checked neighbor)))
          (let* ((group (etaf-go--group next size neighbor))
                 (stones (plist-get group :stones)))
            (dolist (stone stones)
              (aset checked stone t))
            (unless (plist-get group :liberties)
              (dolist (stone stones)
                (aset next stone 0)
                (push stone captured))))))
      (let* ((own (etaf-go--group next size point))
             (liberties (plist-get own :liberties))
             (ko-point (when (and (= (length captured) 1)
                                  (= 1 (length (plist-get own :stones)))
                                  (= 1 (length liberties)))
                         (car captured))))
        (if (null liberties)
            (list :ok nil :reason "禁着点：不能自杀")
          (list :ok t :board next :captured (nreverse captured)
                :ko ko-point :liberties (length liberties))))))))

(defun etaf-go--territory (board size)
  "Return a vector marking the owner of every empty point on BOARD.
Entries are 0 for neutral or occupied points, 1 for black and 2 for
white.  An empty region belongs to a color only when every stone
touching it has that color."
  (let* ((length (length board))
         (owner (make-vector length 0))
         (seen (make-vector length nil)))
    (dotimes (start length)
      (when (and (= (aref board start) 0) (not (aref seen start)))
        (let ((region nil)
              (black nil)
              (white nil)
              (stack (list start)))
          (aset seen start t)
          (while stack
            (let ((point (pop stack)))
              (push point region)
              (dolist (next (etaf-go--neighbors size point))
                (pcase (aref board next)
                  (0 (unless (aref seen next)
                       (aset seen next t)
                       (push next stack)))
                  (1 (setq black t))
                  (2 (setq white t))))))
          (when (if black (not white) white)
            (let ((color (if black etaf-go--black etaf-go--white)))
              (dolist (point region)
                (aset owner point color)))))))
    owner))

(defun etaf-go--score (board size &optional komi)
  "Return a Chinese area score for BOARD of SIZE with KOMI, default 0.
The plist holds :owner (from `etaf-go--territory'), :black, :white,
:neutral, :komi and :margin, where :margin is black minus white minus
komi."
  (let* ((komi (or komi 0))
         (owner (etaf-go--territory board size))
         (black 0)
         (white 0)
         (neutral 0))
    (dotimes (point (length board))
      (let ((value (aref board point)))
        (cond
         ((= value etaf-go--black) (cl-incf black))
         ((= value etaf-go--white) (cl-incf white))
         ((= (aref owner point) etaf-go--black) (cl-incf black))
         ((= (aref owner point) etaf-go--white) (cl-incf white))
         (t (cl-incf neutral)))))
    (list :owner owner :black black :white white :neutral neutral
          :komi komi :margin (- black white komi))))

(provide 'etaf-go-rules)
;;; etaf-go-rules.el ends here
