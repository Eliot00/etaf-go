;;; etaf-go-test.el --- ERT entry for etaf-go -*- lexical-binding: t -*-

;;; Commentary:
;; Run with:
;;
;;   make test
;;
;; or:
;;
;;   emacs -Q --batch -L . -L lisp -L test \
;;     -l test/etaf-go-test.el -f ert-run-tests-batch-and-exit

;;; Code:
(require 'ert)
(require 'package)
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root (file-name-directory (directory-file-name test-dir))))
  (add-to-list 'load-path root)
  (add-to-list 'load-path test-dir))

(setq load-prefer-newer t)
(require 'etaf-go)

(defconst etaf-go-test--demo-sgf
  (concat "(;GM[1]FF[4]SZ[9]CA[UTF-8]GN[提一子]"
          "C[黑先，提掉白▲一子。]AB[de][fe][ed]AW[ee]TR[ee]"
          ";B[ef]C[提子成功，白棋被提。])")
  "A 9x9 capture problem used by the tests.")

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "etaf-go-rules-test.el" test-dir) nil t)
  (load (expand-file-name "etaf-go-sgf-test.el" test-dir) nil t)
  (load (expand-file-name "etaf-go-gtp-test.el" test-dir) nil t)
  (load (expand-file-name "etaf-go-view-test.el" test-dir) nil t))

(provide 'etaf-go-test)
;;; etaf-go-test.el ends here
