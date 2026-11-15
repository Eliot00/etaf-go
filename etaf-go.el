;;; etaf-go.el --- Go records and life-and-death practice on ETAF -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (etaf "0.3.1") (etaf-ui "0.3.0"))
;; Keywords: games
;; URL: https://github.com/YOUR-NAME/etaf-go
;; Author: Elliot <hack00mind@gmail.com>
;; Maintainer: Elliot <hack00mind@gmail.com>

;;; Commentary:
;; A small Go (weiqi) application built on ETAF.  It reads SGF records and
;; renders the board with vector stones, coordinate margins and SGF marks.
;;
;; There is no computer player: Go needs a real engine such as KataGo, and
;; this application deliberately covers the everyday study workflow first.
;;
;; Four modes:
;;   replay   - read a record and follow its variations;
;;   practice - answer a life-and-death problem; moves are checked against
;;              the SGF's next solution move and replies are played for you;
;;   study    - play free moves from the current position with captures,
;;              simple ko and suicide handled by the built-in rules;
;;   edit     - set up stones, marks, labels and the player, then save the
;;              node as a new problem record.
;;
;; The score estimate uses Chinese area scoring: stones plus empty regions
;; bordered by one color only.  Seki and life-and-death are left neutral,
;; so treat it as a study aid rather than a finished score.
;;
;; Function: etaf-go-open (&optional file practice)
;; Open FILE, or read one from `etaf-go-directory'.  With PRACTICE non-nil
;; the session starts in practice mode.  Interactively, a prefix argument
;; selects practice mode.
;;
;; Function: etaf-go-new (&optional size)
;; Open an empty record in edit mode, ready to be set up and saved.
;;
;; Function: etaf-go-problems ()
;; Choose a life-and-death problem from `etaf-go-problem-directory'.
;;
;; Function: etaf-go-demo ()
;; Open a built-in 9x9 capture problem, useful for a first look.
;;
;; Function: etaf-go-close (&optional buffer)
;; Close the Go application in BUFFER.
;;
;; Keys in the board buffer: n/p next/previous, g start, G end, m cycle
;; replay/practice/study, e edit, u undo, r reset, v variations, a hint,
;; t move numbers, S area score, P player in edit, T theme, s pass in
;; study, c comment, x delete branch, w/W save, N new, o open, F1 help
;; and q close.

;;; Code:
(require 'etaf)
(require 'etaf-ui)

(defconst etaf-go--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(require 'etaf-go-view)

(defgroup etaf-go nil
  "Go board, SGF replay and life-and-death practice."
  :group 'games)

(defcustom etaf-go-directory (expand-file-name "go" user-emacs-directory)
  "Directory offered when opening an SGF record."
  :type 'directory)

(defcustom etaf-go-problem-directory
  (expand-file-name "problems" etaf-go-directory)
  "Directory searched by `etaf-go-problems'."
  :type 'directory)

(defcustom etaf-go-appearance 'auto
  "Initial appearance of new Go sessions.
`auto' follows the opening frame's background mode."
  :type '(choice (const auto) (const light) (const dark)))

(defcustom etaf-go-cell-lines 'auto
  "Height of one graphical board cell in text lines.
`auto' chooses the smallest near-square cell that fits the window.
An integer 1-6 forces that many lines: larger cells make the board
squarer but taller and may require scrolling."
  :type '(choice (const :tag "Auto" auto)
                 (integer :tag "Lines")))

(defconst etaf-go--demo-sgf
  (concat "(;GM[1]FF[4]SZ[9]CA[UTF-8]GN[提一子]"
          "C[黑先，提掉白▲一子。]AB[de][fe][ed]AW[ee]TR[ee]"
          ";B[ef]C[提子成功，白棋被提。])")
  "A small capture problem used by `etaf-go-demo'.")

(defun etaf-go--read-sgf (file)
  "Return the SGF text of FILE."
  (unless (file-regular-p file)
    (user-error "无法读取棋谱：%s" file))
  (with-temp-buffer
    (set-buffer-multibyte t)
    (insert-file-contents file)
    (let ((text (buffer-string)))
      (when (and (> (length text) 0) (eq (aref text 0) #xfeff))
        (setq text (substring text 1)))
      text)))

(defun etaf-go--display (session name)
  "Display SESSION as the application buffer NAME and return the buffer."
  (etaf-app-open name
                 (etaf-view (etaf-go--app :session session))
                 :state session
                 :cursor nil
                 ;; ETAF lays the canvas out at the font's line height.  A
                 ;; global `line-spacing' adds a pixel gap between the board's
                 ;; image rows, so keep the application buffer at nil.
                 :on-fit (lambda (_window)
                           (setq-local line-spacing nil))
                 :on-close (lambda () nil))
  (get-buffer name))

(defun etaf-go--open-file (file &optional practice)
  "Open the SGF record FILE in its own buffer."
  (let* ((file (expand-file-name file))
         (text (etaf-go--read-sgf file))
         (root (etaf-go--sgf-parse text))
         (session (etaf-go--session-create
                   file root (if practice 'practice 'replay)))
         (name (format "*Go: %s*" (file-name-nondirectory file))))
    (etaf-go--display session name)))

;;;###autoload
(defun etaf-go-open (&optional file practice)
  "Open an SGF record, starting in practice mode with PRACTICE non-nil.
Interactively read FILE from `etaf-go-directory'; a prefix argument
starts a life-and-death session."
  (interactive
   (let ((directory (if (file-directory-p etaf-go-directory)
                        etaf-go-directory
                      default-directory)))
     (list (read-file-name "SGF 棋谱: " directory nil t)
           current-prefix-arg)))
  (unless file
    (setq file (read-file-name "SGF 棋谱: " nil nil t)))
  (etaf-go--open-file file practice))

;;;###autoload
(defun etaf-go-new (&optional size)
  "Open a new empty record on a SIZE board, defaulting to 19.
New records start in edit mode so a position can be set up and saved
as a life-and-death problem."
  (interactive (list (read-number "棋盘大小: " 19)))
  (let ((size (or size 19)))
    (unless (and (integerp size) (<= 2 size 25))
      (user-error "棋盘大小需在 2 到 25 之间"))
    (let* ((text (format
                  "(;GM[1]FF[4]SZ[%d]CA[UTF-8]GN[新棋谱]KM[6.5]PL[B])"
                  size))
           (directory (if (file-directory-p etaf-go-directory)
                          etaf-go-directory
                        default-directory))
           (file (expand-file-name "新棋谱.sgf" directory))
           (root (etaf-go--sgf-parse text))
           (session (etaf-go--session-create file root 'edit)))
      (etaf-go--display session "*Go: 新棋谱*"))))

;;;###autoload
(defun etaf-go-problems ()
  "Choose a life-and-death problem from `etaf-go-problem-directory'."
  (interactive)
  (let* ((directory (expand-file-name etaf-go-problem-directory))
         (files (and (file-directory-p directory)
                     (directory-files-recursively directory "\\.sgf\\'")))
         (choices (mapcar (lambda (file)
                            (cons (file-relative-name file directory) file))
                          files)))
    (unless choices
      (user-error "没有找到死活题；把 .sgf 文件放进 %s" directory))
    (let ((choice (completing-read "死活题: " choices nil t)))
      (etaf-go--open-file (cdr (assoc choice choices)) t))))

;;;###autoload
(defun etaf-go-demo ()
  "Open a built-in 9x9 capture problem."
  (interactive)
  (let* ((root (etaf-go--sgf-parse etaf-go--demo-sgf))
         (session (etaf-go--session-create
                   "示例死活题" root 'practice)))
    (etaf-go--display session "*Go: 示例死活题*")))

;;;###autoload
(defun etaf-go-close (&optional buffer)
  "Close the Go application in BUFFER, or the current buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (unless (etaf-go--session-p (etaf-app-state buffer))
      (user-error "当前缓冲区不是围棋应用"))
    (etaf-app-close buffer)))

(provide 'etaf-go)
;;; etaf-go.el ends here
