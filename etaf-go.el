;;; etaf-go.el --- Go play, records and life-and-death on ETAF -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.6.0
;; Package-Requires: ((emacs "29.1") (etaf "0.3.4") (etaf-ui "0.3.3"))
;; Keywords: games
;; URL: https://github.com/Eliot00/etaf-go
;; Author: Elliot <hack00mind@gmail.com>
;; Maintainer: Elliot <hack00mind@gmail.com>

;;; Commentary:
;; A small Go (weiqi) application built on ETAF.  It reads SGF records,
;; plays local two-player games and renders the board with vector stones,
;; coordinate margins and SGF marks.
;;
;; There is no built-in computer player: strength comes from a GTP engine
;; you point the package at.  `play' is hot-seat when no engine is set and a
;; game against that engine when `etaf-go-engine-program' is configured.
;;
;; Five modes:
;;   play     - local two-player game with captures, ko, pass, resign,
;;              undo and an area-score result after two passes;
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
;; Function: etaf-go ()
;; Open the main menu: new local game, open a record, practice or author.
;;
;; Function: etaf-go-open ()
;; Alias for `etaf-go'; opens the main menu.
;;
;; Function: etaf-go-open-file (&optional file practice)
;; Open FILE directly, or read one from `etaf-go-directory'.  With
;; PRACTICE non-nil the session starts in practice mode.
;;
;; Function: etaf-go-play (&optional size)
;; Start a game on a SIZE board.  With `etaf-go-engine-program' set one side
;; is a GTP engine, otherwise both sides are local players.
;;
;; Function: etaf-go-play-local (&optional size)
;; Start a local two-player game regardless of any engine setting.
;;
;; Function: etaf-go-new (&optional size)
;; Open an empty record in edit mode, ready to be set up and saved.
;;
;; Function: etaf-go-problems ()
;; Choose a life-and-death problem from `etaf-go-problem-directory'.
;;
;; Function: etaf-go-close (&optional buffer)
;; Close the Go application or main menu in BUFFER.
;;
;; The package installs no global keys.  Use these commands from your own
;; configuration if you want bindings.
;;
;; Keys in the board buffer: n/p next/previous, g start, G end, m cycle
;; replay/practice/study, e edit, u undo, r reset, v variations, a hint,
;; t move numbers, S area score, P player in edit, T theme, s pass in
;; play/study, R resign in play, c comment, x delete branch, w/W save,
;; N new, o open, F1 help and q close.

;;; Code:
(require 'face-remap)
(require 'etaf)
(require 'etaf-ui)

(defconst etaf-go--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(require 'etaf-go-view)
(require 'etaf-go-progress)

(declare-function etaf-go--engine-close "etaf-go-view" (session))
(declare-function etaf-go--home-open "etaf-go-view" ())
(declare-function ebox-viewport-window-width "ebox-buffer" (window))
(declare-function ebox-buffer-mount "ebox-buffer" (buffer-or-name))
(declare-function etaf-go-gtp-start "etaf-go-gtp"
                  (program &optional arguments))
(declare-function etaf-go-gtp-stop "etaf-go-gtp" (session))
(declare-function etaf-go-gtp-cleanup "etaf-go-gtp" ())

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

(defcustom etaf-go-progress-file
  (expand-file-name "progress.txt" etaf-go-directory)
  "File that remembers which problems have been solved.
It is plain text, one line per attempt, so it can be edited freely."
  :type 'file)

(defcustom etaf-go-appearance 'auto
  "Initial appearance of new Go sessions.
`auto' follows the opening frame's background mode."
  :type '(choice (const auto) (const light) (const dark)))

(defcustom etaf-go-engine-program nil
  "GTP engine command used by `etaf-go-play'.
A string names one executable; a list is a full command with arguments,
for example (\"katago\" \"gtp\" \"-model\" \"...\" \"-config\" \"...\").
When nil, engine play is unavailable and only local two-player play runs."
  :type '(choice (const :tag "None" nil) string (repeat string)))

(defcustom etaf-go-engine-arguments nil
  "Extra arguments passed to `etaf-go-engine-program'.
Useful when the program is a string and only a few flags are needed."
  :type '(repeat string))

(defcustom etaf-go-engine-size 19
  "Board size offered when a new engine game starts."
  :type 'integer)

(defcustom etaf-go-engine-handicap 'black
  "Which side the engine plays in a new game.
`black' puts the human on white, `white' puts the human on black, and
`none' leaves both sides to the local players."
  :type '(choice (const black) (const white) (const none)))

(defcustom etaf-go-engine-timeout 10
  "Seconds to wait for one GTP response before reporting a timeout."
  :type 'number)

(defcustom etaf-go-engine-komi 6.5
  "Komi reported to the engine in a new game."
  :type 'number)

(defcustom etaf-go-cell-lines 'auto
  "Height of one graphical board cell in text lines.
`auto' chooses the largest near-square cell that leaves the title and
padding visible; the controls below the board may scroll on short
frames.  An integer 1-8 forces that many lines: larger cells make the
board bigger but may require scrolling."
  :type '(choice (const :tag "Auto" auto)
                 (integer :tag "Lines")))

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

(defvar-local etaf-go--applied-scale 1.0
  "Face scale currently applied to the board buffer, or 1.0.")

(defvar-local etaf-go--scale-cookie nil
  "Cookie of the board's relative default-face remap, or nil.")

(defun etaf-go--notify-scale-change ()
  "Ask Ebox to re-measure after the board's face scale changed.
Ebox treats `text-scale-mode-hook' as the signal that a mounted buffer's
glyph metrics changed: it clears its pixel caches, lays the tree out again
and refits the application.  An unmounted buffer needs no notification
because it measures the new face when it mounts."
  (when (and (fboundp 'ebox-buffer-mount)
             (ebox-buffer-mount (current-buffer)))
    (run-hooks 'text-scale-mode-hook)))

(defun etaf-go--apply-scale (scale)
  "Apply SCALE to the board buffer's default face.
Ebox only allows whole-line cell heights, so a 21-cell board in a short
window needs a shorter line before it can use two lines per cell.  The
remap is relative, so it composes with the application's canvas colours
and the user's own text scale.  A nil or 1.0 SCALE removes it."
  (let* ((scale (or scale 1.0))
         (current (if etaf-go--scale-cookie etaf-go--applied-scale 1.0)))
    (unless (= scale current)
      (when etaf-go--scale-cookie
        (face-remap-remove-relative etaf-go--scale-cookie)
        (setq etaf-go--scale-cookie nil))
      (setq etaf-go--applied-scale scale)
      (unless (= scale 1.0)
        (setq etaf-go--scale-cookie
              (face-remap-add-relative 'default :height scale)))
      (etaf-go--notify-scale-change))))

(defun etaf-go--board-scale-for (size &optional window cell-lines view)
  "Choose the board face scale for SIZE in WINDOW.
CELL-LINES is the session's `etaf-go-cell-lines' value; an integer pins
the cell height and leaves the face unscaled, because a forced cell may
scroll.  VIEW is the region the board shows; a local problem has a smaller
grid and can afford a larger face.  Return 1.0 on a text frame or when no
measured scale fits."
  (if (and (display-graphic-p) (image-type-available-p 'svg))
      (if (integerp cell-lines)
          1.0
        (let* ((window (or window (get-buffer-window (current-buffer) t)
                           (selected-window)))
               (width (if (window-live-p window)
                          (ebox-viewport-window-width window)
                        1080))
               (height (if (window-live-p window)
                           (window-body-height window t)
                         760))
               (view (or view (etaf-go--region-full size)))
               (options (etaf-go--scale-options))
               (choice (etaf-go--choose-scale
                        view options 8 width height
                        (if (etaf-go--row-layout-p width)
                            etaf-go--sidebar-columns
                          0))))
          (or (car choice) 1.0)))
    1.0))

(defun etaf-go--display (session name)
  "Display SESSION as the application buffer NAME and return the buffer.
The board buffer's default face is scaled before the view measures it, so
Ebox sees the chosen line height; the fit callback re-scales after each
window change, which keeps a resized frame fitting."
  (unless (etaf-go--session-cell-lines session)
    (setf (etaf-go--session-cell-lines session) etaf-go-cell-lines))
  (etaf-app-open name
                 (etaf-view (etaf-go--app :session session))
                 :state session
                 :cursor nil
                 ;; ETAF lays the canvas out at the font's line height.  A
                 ;; global `line-spacing' adds a pixel gap between the
                 ;; board's image rows, so keep it at nil.
                 :on-fit (lambda (window)
                           (setq-local line-spacing nil)
                           (let ((size (etaf-go--session-size session)))
                             (etaf-go--apply-scale
                              (etaf-go--board-scale-for
                               size window
                               (etaf-go--session-cell-lines session)
                               ;; Before the first render the region is
                               ;; not known; a session opened directly in
                               ;; practice starts at its root.
                               (or etaf-go--region
                                   (if (eq (etaf-go--session-mode session)
                                           'practice)
                                       (etaf-go--problem-region
                                        (list (etaf-go--session-root session))
                                        size)
                                     (etaf-go--region-full size)))))))
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
(defun etaf-go ()
  "Open the Go main menu."
  (interactive)
  (etaf-go--home-open))

;;;###autoload
(defun etaf-go-open ()
  "Open the Go main menu."
  (interactive)
  (etaf-go--home-open))

;;;###autoload
(defun etaf-go-open-file (&optional file practice)
  "Open the SGF record FILE, starting in practice mode with PRACTICE.
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

(defun etaf-go--empty-session (size mode)
  "Return an empty SGF session of SIZE for MODE.
MODE is `play' or `edit' and selects the title, file name and buffer."
  (let ((size (or size 19)))
    (unless (and (integerp size) (<= 2 size 25))
      (user-error "棋盘大小需在 2 到 25 之间"))
    (let* ((title (if (eq mode 'play) "新对局" "新棋谱"))
           (text (format
                  "(;GM[1]FF[4]SZ[%d]CA[UTF-8]GN[%s]KM[6.5]PL[B])"
                  size title))
           (directory (if (file-directory-p etaf-go-directory)
                          etaf-go-directory
                        default-directory))
           (file (expand-file-name (concat title ".sgf") directory))
           (root (etaf-go--sgf-parse text)))
      (etaf-go--session-create file root mode))))

(defun etaf-go--engine-session (size color)
  "Return a play session of SIZE with an engine playing COLOR.
Signal `user-error' when no engine is configured or it does not start."
  (unless etaf-go-engine-program
    (user-error "没有配置 GTP 引擎；设置 etaf-go-engine-program 后再试"))
  (let* ((session (etaf-go--empty-session size 'play))
         (engine (condition-case error
                     (etaf-go-gtp-start etaf-go-engine-program
                                        etaf-go-engine-arguments)
                   (etaf-go-gtp-error
                    (user-error "无法启动引擎：%s"
                                (or (cadr error)
                                    (error-message-string error)))))))
    (setf (etaf-go--session-engine session) engine
          (etaf-go--session-engine-color session)
          (if (eq color 'black) etaf-go--black etaf-go--white))
    session))

;;;###autoload
(defun etaf-go-play (&optional size)
  "Start a game on a SIZE board, defaulting to `etaf-go-engine-size'.
With `etaf-go-engine-program' set, one side is a GTP engine and the other
is you, following `etaf-go-engine-handicap'.  Without an engine both
sides are local players.  `s' passes and `R' resigns; two passes end the
game and show the area score."
  (interactive (list (read-number "棋盘大小: " etaf-go-engine-size)))
  (let* ((size (or size etaf-go-engine-size 19))
         (has-engine (and etaf-go-engine-program
                          (not (eq etaf-go-engine-handicap 'none))))
         (session (if has-engine
                      (etaf-go--engine-session size etaf-go-engine-handicap)
                    (etaf-go--empty-session size 'play)))
         (size (etaf-go--session-size session))
         (name (if has-engine
                   (format "*Go: 人机 %d 路*" size)
                 (format "*Go: 对弈 %d 路*" size))))
    (etaf-go--display session name)))

;;;###autoload
(defun etaf-go-play-local (&optional size)
  "Start a local two-player game on a SIZE board, defaulting to 19."
  (interactive (list (read-number "棋盘大小: " 19)))
  (let* ((session (etaf-go--empty-session size 'play))
         (size (etaf-go--session-size session)))
    (etaf-go--display session (format "*Go: 对弈 %d 路*" size))))

;;;###autoload
(defun etaf-go-new (&optional size)
  "Open a new empty record on a SIZE board, defaulting to 19.
New records start in edit mode so a position can be set up and saved
as a life-and-death problem."
  (interactive (list (read-number "棋盘大小: " 19)))
  (let ((session (etaf-go--empty-session size 'edit)))
    (etaf-go--display session "*Go: 新棋谱*")))

;;;###autoload
(defun etaf-go-problems ()
  "Choose a life-and-death problem from `etaf-go-problem-directory'.
Solved problems are marked and sorted after unsolved ones; with a prefix
argument every problem is offered in one list."
  (interactive "P")
  (let* ((directory (expand-file-name etaf-go-problem-directory))
         (files (and (file-directory-p directory)
                     (directory-files-recursively directory "\\.sgf\\'")))
         (summary (etaf-go-progress-summary files)))
    (unless files
      (user-error "没有找到死活题；把 .sgf 文件放进 %s" directory))
    (let* ((unsolved (cl-remove-if #'etaf-go-progress-solved-p files))
           (pool (if (or current-prefix-arg (null unsolved)) files unsolved))
           (choices
            (mapcar (lambda (file)
                      (cons (format "%s%s"
                                    (if (etaf-go-progress-solved-p file) "✓ " "  ")
                                    (file-relative-name file directory))
                            file))
                    pool))
           (prompt (format "死活题 %d/%d: " (car summary) (cdr summary)))
           (choice (completing-read prompt choices nil t)))
      (etaf-go--open-file (cdr (assoc choice choices)) t))))

;;;###autoload
(defun etaf-go-problems-reset ()
  "Forget which problems have been solved.
Interactively clear every record; with a prefix argument clear only the
problems in `etaf-go-problem-directory'."
  (interactive "P")
  (let* ((directory (expand-file-name etaf-go-problem-directory))
         (files (and current-prefix-arg (file-directory-p directory)
                     (directory-files-recursively directory "\\.sgf\\'"))))
    (when (or (null files)
              (y-or-n-p (format "清除 %d 道题的进度？" (length files))))
      (etaf-go-progress-reset files)
      (message "进度已清除"))))

;;;###autoload
(defun etaf-go-close (&optional buffer)
  "Close the Go application or main menu in BUFFER.
An engine game also stops its GTP process."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (state (etaf-app-state buffer)))
    (unless (or (etaf-go--session-p state) (eq state 'home))
      (user-error "当前缓冲区不是围棋应用"))
    (when (etaf-go--session-p state)
      (when (etaf-go--session-engine state)
        (ignore-errors (etaf-go--engine-close state))))
    (etaf-app-close buffer)))

(provide 'etaf-go)
;;; etaf-go.el ends here
