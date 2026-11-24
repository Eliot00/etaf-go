;;; init-dev.el --- Local development entry for etaf-go -*- lexical-binding: t -*-

;;; Commentary:
;; Load etaf-go straight from this checkout and provide a reload command.
;; Use it without changing your main configuration:
;;
;;   emacs -Q -l dev/init-dev.el
;;
;; or from a running Emacs:
;;
;;   M-x load-file RET ~/Code/Libre/etaf-go/dev/init-dev.el RET
;;
;; Editing an ETAF Component requires a redefine boundary, so reload with
;; `etaf-go-dev-reload' instead of evaluating the view file directly.

;;; Code:
(require 'package)
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(let* ((dev-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root (file-name-directory (directory-file-name dev-dir))))
  (add-to-list 'load-path root)
  (setq etaf-go-directory (expand-file-name "examples" root)
        etaf-go-problem-directory (expand-file-name "examples" root)))

(setq load-prefer-newer t)
(setq debug-on-error t)

(require 'etaf-go)

(defun etaf-go-dev-reload ()
  "Reload the checkout and open the demo.
Close every running Go application first so its Component instances are
disposed before their declarations are replaced."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((state (etaf-app-state buffer)))
      (when (or (etaf-go--session-p state) (eq state 'home))
        (etaf-go-close buffer))))
  (etaf-component-redefine-run
   (lambda ()
     (load (expand-file-name "etaf-go-rules.el" etaf-go--directory) nil t)
     (load (expand-file-name "etaf-go-sgf.el" etaf-go--directory) nil t)
     (load (expand-file-name "etaf-go-gtp.el" etaf-go--directory) nil t)
     (load (expand-file-name "etaf-go-view.el" etaf-go--directory) nil t)
     (load (expand-file-name "etaf-go.el" etaf-go--directory) nil t)))
  (etaf-go))

(message "etaf-go development loaded from %s; use M-x etaf-go or M-x etaf-go-dev-reload"
         etaf-go--directory)

(provide 'init-dev)
;;; init-dev.el ends here
