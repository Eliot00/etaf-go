;;; etaf-go-sgf-test.el --- SGF parser and position tests -*- lexical-binding: t -*-

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'etaf-go)

(defconst etaf-go-test--demo etaf-go--demo-sgf)

(ert-deftest etaf-go-sgf-demo-position ()
  "The built-in demo parses and captures its white stone."
  (let* ((root (etaf-go--sgf-parse etaf-go-test--demo))
         (size (etaf-go--board-size root))
         (session (etaf-go--session-create "demo.sgf" root 'practice))
         (position (etaf-go--position size root (list root))))
    (should (= size 9))
    (should (equal (etaf-go--session-title session) "提一子"))
    (should (= (etaf-go--session-main-count session) 1))
    (should (= (cl-count-if (lambda (stone) (= stone etaf-go--black))
                            (plist-get position :board))
               3))
    (should (= (cl-count-if (lambda (stone) (= stone etaf-go--white))
                            (plist-get position :board))
               1))
    (should (= (plist-get position :to-play) etaf-go--black))
    (should (= (length (etaf-go--node-marks root size)) 1))
    (let* ((move-node (car (etaf-go--node-children root)))
           (after (etaf-go--position size root (list root move-node))))
      (should (= (plist-get after :move-count) 1))
      (should (= (car (plist-get after :captures)) 1))
      (should (= (cdr (plist-get after :captures)) 0))
      (should (zerop (cl-count-if (lambda (stone) (= stone etaf-go--white))
                                  (plist-get after :board))))
      (should (= (plist-get after :last) (+ (* 5 9) 4)))
      (should-not (plist-get after :error)))))

(ert-deftest etaf-go-sgf-escapes-and-variations ()
  "Escaped brackets and sibling variations are read."
  (let* ((text "(;GM[1]SZ[19]C[a\\]b\\\\c];B[pd](;W[dp]C[left])(;W[dd]C[right]))")
         (root (etaf-go--sgf-parse text))
         (first (car (etaf-go--node-children root))))
    (should (equal (etaf-go--prop1 root "C") "a]b\\c"))
    (should (= (length (etaf-go--node-children first)) 2))))

(ert-deftest etaf-go-sgf-crlf-compressed-setup-and-pass ()
  "CRLF, setup ranges, multiple collections and pass moves work."
  (let* ((text (concat "(;GM[1]FF[4]SZ[19:19]AB[aa:bb]\r\n"
                       ";B[];W[tt])\r\n"
                       "(;GM[1]SZ[9];B[ee])"))
         (root (etaf-go--sgf-parse text))
         (size (etaf-go--board-size root)))
    (should (= size 19))
    (should (= (length (etaf-go--expand-points
                        size (etaf-go--prop root "AB")))
               4))
    (let* ((line (etaf-go--main-line root))
           (position (etaf-go--position size root (cons root line))))
      (should (= (plist-get position :move-count) 2))
      (should (eq (plist-get position :last) 'pass))
      (should (= (plist-get position :to-play) etaf-go--black))
      (should-not (plist-get position :error)))))

(ert-deftest etaf-go-sgf-infers-white-to-play ()
  "A record whose first move is White reports White to play."
  (let* ((root (etaf-go--sgf-parse "(;GM[1]SZ[9];W[ee])"))
         (position (etaf-go--position 9 root (list root))))
    (should (= (plist-get position :to-play) etaf-go--white))))

(ert-deftest etaf-go-sgf-rejects-malformed-values ()
  "An unterminated value signals a user error."
  (should-error (etaf-go--sgf-parse "(;GM[1]C[unterminated)")
                :type 'user-error))

(ert-deftest etaf-go-sgf-escaped-newline ()
  "A backslash-newline in a value is a line continuation."
  (let* ((root (etaf-go--sgf-parse "(;C[line one\\\nline two])")))
    (should (equal (etaf-go--prop1 root "C") "line oneline two"))))

(ert-deftest etaf-go-sgf-coordinates-and-info ()
  "SGF coordinates and game metadata are presented correctly."
  (should (equal (etaf-go--coord-name 19 3 15) "Q16"))
  (should (equal (etaf-go--point-name 9 'pass) "停一手"))
  (let* ((root (etaf-go--sgf-parse
                "(;GM[1]SZ[19]PB[Black]BR[9p]PW[White]WR[9p]KM[6.5]RE[B+R])")))
    (should (equal (etaf-go--game-info root)
                   "黑 Black 9p · 白 White 9p · B+R · 贴 6.5 目"))))

(ert-deftest etaf-go-sgf-serialize-round-trip ()
  "A serialized tree parses back into the same nodes and variations."
  (dolist (text (list etaf-go-test--demo
                      "(;GM[1]SZ[19]C[a\\]b\\\\c];B[pd](;W[dp]C[left])(;W[dd]C[right]))"
                      "(;GM[1]FF[4]SZ[19:19]AB[aa:bb];B[];W[tt])"))
    (let* ((one (etaf-go--sgf-parse text))
           (again (etaf-go--sgf-parse (etaf-go--sgf-serialize one))))
      (should (equal one again)))))

(ert-deftest etaf-go-sgf-escape-and-coord ()
  "Value escaping and SGF coordinates behave."
  (should (equal (etaf-go--sgf-escape "a]b\\c") "a\\]b\\\\c"))
  (should (equal (etaf-go--sgf-escape "") ""))
  (should (equal (etaf-go--sgf-coord 9 (+ (* 5 9) 4)) "ef"))
  (should (equal (etaf-go--sgf-coord 19 (+ (* 15 19) 15)) "pp"))
  (should (equal (etaf-go--sgf-coord 9 'pass) "")))

(ert-deftest etaf-go-sgf-set-prop ()
  "Setting a property replaces, appends and removes values."
  (let ((node (etaf-go--sgf-parse "(;GM[1]C[old]N[x])")))
    (etaf-go--set-prop node "C" (list "new"))
    (should (equal (etaf-go--prop1 node "C") "new"))
    (should (equal (etaf-go--prop1 node "N") "x"))
    (etaf-go--set-prop node "C" nil)
    (should (null (etaf-go--prop1 node "C")))
    (should (equal (etaf-go--prop1 node "N") "x"))))

(ert-deftest etaf-go-sgf-solution-children-skip-study-marks ()
  "Children carrying the EG study marker are not solution moves."
  (let ((root (etaf-go--sgf-parse "(;GM[1]SZ[9];B[cc]EG[1])")))
    (should (= (length (etaf-go--node-children root)) 1))
    (should (null (etaf-go--solution-children root)))
    ;; Without the marker the child is an ordinary solution line.
    (let ((plain (etaf-go--sgf-parse "(;GM[1]SZ[9];B[cc])")))
      (should (= (length (etaf-go--solution-children plain)) 1)))))

(ert-deftest etaf-go-sgf-merges-repeated-properties ()
  "A property written in several groups keeps every value."
  (let* ((root (etaf-go--sgf-parse "(;GM[1]SZ[9]AB[aa]AB[bb]AW[cc][dd])"))
         (size (etaf-go--board-size root)))
    (should (equal (etaf-go--prop root "AB") '("aa" "bb")))
    (should (equal (etaf-go--prop root "AW") '("cc" "dd")))
    (should (= (length (etaf-go--expand-points size (etaf-go--prop root "AB")))
               2))
    (let ((position (etaf-go--position size root (list root))))
      (should (= (cl-count-if (lambda (stone) (= stone etaf-go--black))
                              (plist-get position :board))
                 2))
      (should (= (cl-count-if (lambda (stone) (= stone etaf-go--white))
                              (plist-get position :board))
                 2)))))

(ert-deftest etaf-go-sgf-komi ()
  "Komi is read from KM and defaults to 6.5."
  (should (= (etaf-go--komi (etaf-go--sgf-parse "(;KM[7.5])")) 7.5))
  (should (= (etaf-go--komi (etaf-go--sgf-parse "(;KM[0])")) 0))
  (should (= (etaf-go--komi (etaf-go--sgf-parse "(;GM[1]SZ[9])")) 6.5)))

(provide 'etaf-go-sgf-test)
;;; etaf-go-sgf-test.el ends here
