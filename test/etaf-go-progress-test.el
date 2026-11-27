;;; etaf-go-progress-test.el --- Progress bookkeeping tests -*- lexical-binding: t -*-

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'etaf-go)

(defmacro etaf-go-progress-test--with-file (&rest body)
  "Run BODY with a temporary progress file."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "etaf-go-progress" t))
          (etaf-go-progress-file (expand-file-name "progress.txt" directory)))
     (unwind-protect
         (progn ,@body)
       (delete-directory directory t))))

(ert-deftest etaf-go-progress-empty ()
  "An unused progress file reports nothing solved."
  (etaf-go-progress-test--with-file
    (should-not (etaf-go-progress-entry "/tmp/a.sgf"))
    (should-not (etaf-go-progress-solved-p "/tmp/a.sgf"))
    (should (equal (etaf-go-progress-summary '("/tmp/a.sgf" "/tmp/b.sgf"))
                   '(0 . 2)))))

(ert-deftest etaf-go-progress-records-kinds ()
  "Solved and attempted counts are tracked per file."
  (etaf-go-progress-test--with-file
    (etaf-go-progress-record "/tmp/a.sgf" 'attempts)
    (etaf-go-progress-record "/tmp/a.sgf" 'attempts)
    (etaf-go-progress-record "/tmp/a.sgf" 'solved)
    (etaf-go-progress-record "/tmp/b.sgf" 'attempts)
    (let ((a (etaf-go-progress-entry "/tmp/a.sgf"))
          (b (etaf-go-progress-entry "/tmp/b.sgf")))
      (should (= (plist-get a :attempts) 2))
      (should (= (plist-get a :solved) 1))
      (should (= (plist-get b :attempts) 1))
      (should-not (plist-get b :solved)))
    (should (etaf-go-progress-solved-p "/tmp/a.sgf"))
    (should-not (etaf-go-progress-solved-p "/tmp/b.sgf"))
    (should (equal (etaf-go-progress-summary
                    '("/tmp/a.sgf" "/tmp/b.sgf" "/tmp/c.sgf"))
                   '(1 . 3)))))

(ert-deftest etaf-go-progress-file-is-text ()
  "The store is a readable text file with a heading."
  (etaf-go-progress-test--with-file
    (etaf-go-progress-record "/tmp/a.sgf" 'solved)
    (etaf-go-progress-record "/tmp/a.sgf" 'attempts)
    (let ((text (with-temp-buffer
                  (insert-file-contents etaf-go-progress-file)
                  (buffer-string))))
      (should (string-prefix-p ";; etaf-go progress" text))
      (should (string-match-p "solved\t/tmp/a.sgf" text))
      (should (string-match-p "attempts\t/tmp/a.sgf" text)))))

(ert-deftest etaf-go-progress-reset ()
  "Resetting clears chosen files, or everything."
  (etaf-go-progress-test--with-file
    (etaf-go-progress-record "/tmp/a.sgf" 'solved)
    (etaf-go-progress-record "/tmp/b.sgf" 'solved)
    (etaf-go-progress-reset '("/tmp/a.sgf"))
    (should-not (etaf-go-progress-solved-p "/tmp/a.sgf"))
    (should (etaf-go-progress-solved-p "/tmp/b.sgf"))
    (etaf-go-progress-reset nil)
    (should-not (etaf-go-progress-solved-p "/tmp/b.sgf"))))

(ert-deftest etaf-go-progress-practice-solves-are-recorded ()
  "Solving a problem writes it to the progress file."
  (etaf-go-progress-test--with-file
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (let* ((sgf (expand-file-name "problem.sgf" (file-name-directory
                                                   etaf-go-progress-file)))
             (buffer nil))
        (write-region etaf-go-test--demo-sgf nil sgf nil 'silent)
        (setq buffer (etaf-go-open-file sgf t))
        (unwind-protect
            (let ((runtime (etaf-runtime-for-buffer buffer))
                  (session (etaf-app-state buffer)))
              (etaf-go-test--press runtime 'go-cell-2-2)
              (should (= (or (plist-get (etaf-go-progress-entry sgf)
                                        :attempts)
                             0)
                         1))
              (should-not (etaf-go-progress-solved-p sgf))
              (etaf-go-test--press runtime 'go-cell-6-5)
              (should (etaf-go-progress-solved-p sgf))
              (should (null (etaf-go--session-engine session))))
          (etaf-go-close buffer))))))

(provide 'etaf-go-progress-test)
;;; etaf-go-progress-test.el ends here
