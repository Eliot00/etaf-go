;;; etaf-go-gtp-test.el --- GTP client tests -*- lexical-binding: t -*-

;;; Commentary:
;; The protocol is tested against test/fake-gtp-engine.py so the suite needs
;; no real engine.  The fake answers `name' and plays the first empty point.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'etaf-go)

(defconst etaf-go-gtp-test--engine
  (expand-file-name "fake-gtp-engine.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the fake GTP engine used by these tests.")

(defun etaf-go-gtp-test--start ()
  "Start the fake engine, or skip when Python 3 is unavailable."
  (unless (executable-find "python3")
    (ert-skip "python3 is not available"))
  (etaf-go-gtp-start (list "python3" etaf-go-gtp-test--engine)))

(ert-deftest etaf-go-gtp-starts-and-names-engine ()
  "Starting the engine reports its name."
  (let ((session (etaf-go-gtp-test--start)))
    (unwind-protect
        (progn
          (should (etaf-go-gtp-live-p session))
          (should (equal (etaf-go-gtp-name session) "test-engine")))
      (etaf-go-gtp-stop session))))

(ert-deftest etaf-go-gtp-stops-process ()
  "Stopping an engine ends its process."
  (let ((session (etaf-go-gtp-test--start)))
    (should (etaf-go-gtp-live-p session))
    (etaf-go-gtp-stop session)
    (should-not (etaf-go-gtp-live-p session))))

(ert-deftest etaf-go-gtp-command-round-trip ()
  "A command returns the engine's response text."
  (let ((session (etaf-go-gtp-test--start)))
    (unwind-protect
        (progn
          (should (equal (etaf-go-gtp-command session "version") "0.1"))
          (should (equal (etaf-go-gtp-command session "boardsize 9") "")))
      (etaf-go-gtp-stop session))))

(ert-deftest etaf-go-gtp-engine-error-is-signalled ()
  "An unknown command signals the engine's error text."
  (let ((session (etaf-go-gtp-test--start)))
    (unwind-protect
        (let ((error (should-error (etaf-go-gtp-command session "nonsense")
                                   :type 'etaf-go-gtp-error)))
          (should (string-match-p "unknown command" (cadr error))))
      (etaf-go-gtp-stop session))))

(ert-deftest etaf-go-gtp-replays-position ()
  "Syncing a position replays the moves on the engine board."
  (let ((session (etaf-go-gtp-test--start)))
    (unwind-protect
        (progn
          (etaf-go-gtp-sync-position session 9 6.5 '(("b" . "E5") ("w" . "C3")))
          (should (equal (etaf-go-gtp-genmove session "b") "A1"))
          (should (equal (etaf-go-gtp-command session "play b E5") "")))
      (etaf-go-gtp-stop session))))

(ert-deftest etaf-go-gtp-genmove-and-vertices ()
  "A generated vertex maps to a board index, and pass maps to `pass'."
  (should (= (etaf-go-gtp-vertex-index 19 "D4") (+ (* 15 19) 3)))
  (should (= (etaf-go-gtp-vertex-index 9 "J1") (+ (* 8 9) 8)))
  (should (eq (etaf-go-gtp-vertex-index 9 "pass") 'pass))
  (should (eq (etaf-go-gtp-vertex-index 9 "resign") 'resign))
  (should-not (etaf-go-gtp-vertex-index 9 "Z9")))

(ert-deftest etaf-go-gtp-vertex-format ()
  "Board indices and SGF colours render as GTP vertices and colours."
  (should (equal (etaf-go--engine-gtp-vertex 19 (+ (* 15 19) 3)) "D4"))
  (should (equal (etaf-go--engine-gtp-vertex 9 'pass) "pass"))
  (should (equal (etaf-go--engine-gtp-color etaf-go--black) "b"))
  (should (equal (etaf-go--engine-gtp-color etaf-go--white) "w")))

(provide 'etaf-go-gtp-test)
;;; etaf-go-gtp-test.el ends here
