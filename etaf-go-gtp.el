;;; etaf-go-gtp.el --- GTP engine for etaf-go -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; A small Go Text Protocol client.  GTP is what KataGo, GNU Go, Leela Zero
;; and most other engines speak, so one client covers all of them: start an
;; engine, set up the board, and ask it for a move.
;;
;; The client is deliberately synchronous.  A generated move takes a fraction
;; of a second with GNU Go and a second or two with a small KataGo visit
;; count, which is acceptable inside a board press.  Commands time out so a
;; wedged engine cannot freeze the board forever.
;;
;; A session is one engine process.  `etaf-go-gtp-command' writes one command
;; and returns its response text, or signals `etaf-go-gtp-error' with the
;; engine's own error line.

;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defvar etaf-go--column-letters)

(define-error 'etaf-go-gtp-error "GTP engine error")

(cl-defstruct (etaf-go-gtp (:constructor etaf-go-gtp--make))
  "One running GTP engine."
  process buffer name)

(defvar etaf-go-gtp--sessions nil
  "Live GTP sessions, so a stray process can be cleaned up.")

(defun etaf-go-gtp-program-arguments (program arguments)
  "Return a command list from PROGRAM and ARGUMENTS.
PROGRAM is a string (one executable) or a list of strings.  ARGUMENTS is a
list of strings appended to it."
  (let ((command
         (cond
          ((null program) nil)
          ((stringp program) (list program))
          ((listp program) (mapcar #'identity program))
          (t (signal 'etaf-go-gtp-error (list "Engine program must be a string or list"))))))
    (when (and arguments (not (listp arguments)))
      (signal 'etaf-go-gtp-error (list "Engine arguments must be a list")))
    (append command arguments)))

(defun etaf-go-gtp-start (program &optional arguments)
  "Start a GTP engine for PROGRAM with ARGUMENTS and return its session.
PROGRAM is one executable or a command list; ARGUMENTS appends more.  The
engine is sent `name' to confirm the connection."
  (let* ((command (etaf-go-gtp-program-arguments program arguments))
         (buffer (generate-new-buffer " *etaf-go-gtp*")))
    (unless command
      (kill-buffer buffer)
      (signal 'etaf-go-gtp-error (list "No engine program configured")))
    (let ((process (make-process
                    :name "etaf-go-gtp"
                    :buffer buffer
                    :command command
                    :connection-type 'pipe
                    :coding 'utf-8-unix
                    :noquery t)))
      (with-current-buffer buffer (erase-buffer))
      (let ((session (etaf-go-gtp--make :process process :buffer buffer)))
        (push session etaf-go-gtp--sessions)
        (condition-case nil
            (let ((name (string-trim (or (etaf-go-gtp-command session "name") ""))))
              (setf (etaf-go-gtp-name session)
                    (if (string-empty-p name) "GTP 引擎" name)))
          (error
           (etaf-go-gtp-stop session)
           (signal 'etaf-go-gtp-error
                   (list (format "引擎没有响应：%s"
                                 (mapconcat #'identity command " "))))))
        session))))

(defun etaf-go-gtp-live-p (session)
  "Return non-nil when SESSION's process is still running."
  (and session
       (process-live-p (etaf-go-gtp-process session))
       t))

(defun etaf-go-gtp-stop (session)
  "Stop SESSION's engine and release its process buffer."
  (when session
    (setq etaf-go-gtp--sessions (delq session etaf-go-gtp--sessions))
    (let ((process (etaf-go-gtp-process session))
          (buffer (etaf-go-gtp-buffer session)))
      (when (process-live-p process)
        (ignore-errors (process-send-string process "quit\n"))
        (accept-process-output process 0.2)
        (when (process-live-p process)
          (delete-process process)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))
  nil)

(defun etaf-go-gtp-cleanup ()
  "Stop every live GTP session."
  (interactive)
  (mapc #'etaf-go-gtp-stop (copy-sequence etaf-go-gtp--sessions)))

(defun etaf-go-gtp--read-response (session timeout)
  "Return SESSION's next complete response, waiting up to TIMEOUT seconds."
  (let ((buffer (etaf-go-gtp-buffer session))
        (process (etaf-go-gtp-process session))
        (deadline (+ (float-time) timeout))
        response)
    (while (and (not response) (< (float-time) deadline))
      (with-current-buffer buffer
        (goto-char (point-min))
        (when (re-search-forward "\n\n" nil t)
          (setq response (buffer-substring-no-properties (point-min) (match-end 0)))
          (delete-region (point-min) (match-end 0))))
      (unless response
        (accept-process-output process
                               (max 0.01 (min 0.1 (- deadline (float-time)))))))
    (unless response
      (signal 'etaf-go-gtp-error
              (list "引擎在时限内没有回应（genmove 可能太慢，调大 etaf-go-engine-timeout）")))
    response))

(defun etaf-go-gtp-command (session command &optional timeout)
  "Send SESSION one GTP COMMAND and return its response text.
TIMEOUT defaults to `etaf-go-engine-timeout'.  A `?' response signals
`etaf-go-gtp-error' with the engine's message; a `=' prefix is removed."
  (unless (etaf-go-gtp-live-p session)
    (signal 'etaf-go-gtp-error (list "引擎没有在运行")))
  (when (string-match-p "[\r\n]" command)
    (signal 'etaf-go-gtp-error (list "GTP command must be one line")))
  (with-current-buffer (etaf-go-gtp-buffer session) (erase-buffer))
  (process-send-string (etaf-go-gtp-process session)
                       (concat command "\n"))
  (let* ((raw (etaf-go-gtp--read-response
               session (or timeout (bound-and-true-p etaf-go-engine-timeout) 10)))
         (text (string-trim raw)))
    (cond
     ((string-prefix-p "=" text)
      (string-trim (substring text 1)))
     ((string-prefix-p "?" text)
      (signal 'etaf-go-gtp-error
              (list (string-trim (substring text 1)))))
     (t
      (signal 'etaf-go-gtp-error
              (list (format "无法解析的 GTP 回应：%s" text)))))))

(defun etaf-go-gtp-sync-position (session size komi moves)
  "Tell SESSION the board is a SIZE square with KOMI and MOVES.
MOVES is a list of (COLOR . VERTEX) strings, oldest first."
  (etaf-go-gtp-command session (format "boardsize %d" size))
  (etaf-go-gtp-command session (format "komi %s" komi))
  (etaf-go-gtp-command session "clear_board")
  (dolist (move moves)
    (etaf-go-gtp-command session
                         (format "play %s %s" (car move) (cdr move)))))

(defun etaf-go-gtp-genmove (session color &optional timeout)
  "Ask SESSION to generate a move for COLOR, `b' or `w'.
Return the vertex string, or nil when the engine passes or resigns."
  (let ((vertex (etaf-go-gtp-command
                 session (format "genmove %s" color) timeout)))
    (cond
     ((string-empty-p (or vertex "")) nil)
     ((member (downcase vertex) '("pass" "resign")) nil)
     (t vertex))))

(defun etaf-go-gtp-vertex-index (size vertex)
  "Return VERTEX's board index on a SIZE board, `pass', `resign' or nil."
  (when (stringp vertex)
    (let ((value (string-trim (downcase vertex))))
      (cond
       ((string-empty-p value) 'pass)
       ((member value '("pass" "tt")) 'pass)
       ((string= value "resign") 'resign)
       ((string-match "\\`\\([a-hj-z]\\)\\([0-9]+\\)\\'" value)
        (let* ((letter (upcase (match-string 1 value)))
               (row (string-to-number (match-string 2 value)))
               (column (cl-position (aref letter 0)
                                    etaf-go--column-letters)))
          (when (and column (< column size) (>= row 1) (<= row size))
            (+ (* (- size row) size) column))))
       (t nil)))))

(provide 'etaf-go-gtp)
;;; etaf-go-gtp.el ends here
