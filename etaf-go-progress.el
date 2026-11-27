;;; etaf-go-progress.el --- Solved-problem bookkeeping -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; A tiny record of which life-and-death problems have been solved, so
;; `etaf-go-problems' can show progress and prefer fresh problems.
;;
;; The store is one plain-text file, one problem per line:
;;
;;   solved<TAB>/path/to/problem.sgf
;;   attempts<TAB>/path/to/problem.sgf
;;
;; Keeping it text means it is easy to read, diff, edit and delete; there is
;; no database and no locking beyond the usual single-Emacs assumption.

;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defvar etaf-go-progress-file)
(defvar etaf-go-directory)

(defconst etaf-go-progress--heading
  ";; etaf-go progress: solved/attempts per problem file\n"
  "First line of the progress file, so a reader knows what it is.")

(defun etaf-go-progress--path ()
  "Return the absolute path of the progress file."
  (let ((configured (and (boundp 'etaf-go-progress-file)
                         etaf-go-progress-file)))
    (expand-file-name
     (or configured
         (expand-file-name "progress.txt"
                           (or (and (boundp 'etaf-go-directory)
                                    etaf-go-directory)
                               user-emacs-directory))))))

(defun etaf-go-progress--read ()
  "Return the progress records as an alist of (FILE . PLIST).
PLIST holds :solved and :attempts integers."
  (let ((file (etaf-go-progress--path))
        (records nil))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (or (string-empty-p line)
                        (string-prefix-p ";;" line))
              (let ((parts (split-string line "\t")))
                (when (>= (length parts) 2)
                  (let* ((kind (intern (concat ":" (car parts))))
                         (path (mapconcat #'identity (cdr parts) "\t"))
                         (entry (assoc path records)))
                    (unless entry
                      (setq entry (cons path nil))
                      (push entry records))
                    (setcdr entry
                            (plist-put (cdr entry) kind
                                       (1+ (or (plist-get (cdr entry) kind) 0)))))))))
          (forward-line 1))))
    (nreverse records)))

(defun etaf-go-progress--write (records)
  "Write RECORDS to the progress file and return its path."
  (let* ((file (etaf-go-progress--path))
         (parent (file-name-directory file))
         (lines nil))
    (when (and parent (not (file-directory-p parent)))
      (make-directory parent t))
    (dolist (entry records)
      (dolist (kind '(:solved :attempts))
        (dotimes (_ (or (plist-get (cdr entry) kind) 0))
          (push (concat (substring (symbol-name kind) 1) "\t" (car entry))
                lines))))
    (with-temp-buffer
      (insert etaf-go-progress--heading)
      (dolist (line (nreverse lines))
        (insert line "\n"))
      (write-region (point-min) (point-max) file nil 'silent))
    file))

(defun etaf-go-progress-record (file kind)
  "Record that FILE was solved or attempted, according to KIND.
KIND is `solved' or `attempts'; both counters only ever grow, so a problem
solved twice still reads as solved."
  (let* ((kind (intern (concat ":" (substring (symbol-name kind) 0))))
         (key (expand-file-name file))
         (records (etaf-go-progress--read))
         (entry (assoc key records)))
    (unless entry
      (setq entry (cons key nil))
      (setq records (append records (list entry))))
    (setcdr entry (plist-put (cdr entry) kind
                             (1+ (or (plist-get (cdr entry) kind) 0))))
    (etaf-go-progress--write records)))

(defun etaf-go-progress-entry (file)
  "Return FILE's progress plist, or nil."
  (cdr (assoc (expand-file-name file) (etaf-go-progress--read))))

(defun etaf-go-progress-solved-p (file)
  "Return non-nil when FILE has been solved at least once."
  (> (or (plist-get (etaf-go-progress-entry file) :solved) 0) 0))

(defun etaf-go-progress-summary (files)
  "Return (SOLVED . TOTAL) for FILES."
  (let ((solved 0))
    (dolist (file files)
      (when (etaf-go-progress-solved-p file)
        (cl-incf solved)))
    (cons solved (length files))))

(defun etaf-go-progress-reset (files)
  "Clear progress for FILES, or for every record when FILES is nil."
  (let ((records (etaf-go-progress--read)))
    (if files
        (let ((keys (mapcar #'expand-file-name files)))
          (setq records (cl-remove-if (lambda (entry)
                                        (member (car entry) keys))
                                      records)))
      (setq records nil))
    (etaf-go-progress--write records)))

(provide 'etaf-go-progress)
;;; etaf-go-progress.el ends here
