;;; etaf-go-view-test.el --- Board artwork and UI event tests -*- lexical-binding: t -*-

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'etaf-go)

(defun etaf-go-test--open-sample (&optional mode)
  "Open the sample 9x9 record in MODE, default practice."
  (let* ((root (etaf-go--sgf-parse etaf-go-test--demo-sgf))
         (session (etaf-go--session-create "test.sgf" root
                                           (or mode 'practice))))
    (etaf-go--display session "*Go: test*")))

(defun etaf-go-test--press (runtime reference)
  "Invoke REFERENCE's committed press handler in RUNTIME."
  (let ((handler (plist-get (etaf-runtime-host-props-for runtime reference)
                            :on-press)))
    (should handler)
    (etaf-call-with-batch (lambda () (funcall handler)))
    (etaf-runtime-flush runtime)))

(defun etaf-go-test--text (buffer)
  "Return BUFFER's visible text."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun etaf-go-test--count (buffer character)
  "Count CHARACTER in BUFFER."
  (with-current-buffer buffer
    (cl-count character (buffer-string))))

(ert-deftest etaf-go-view-svg-cells ()
  "Cell artwork builds valid SVG descriptors."
  (let* ((root (etaf-go--sgf-parse etaf-go-test--demo-sgf))
         (size 9)
         (position (etaf-go--position size root (list root)))
         (marks (etaf-go--node-marks root size))
         (metrics (list :columns 2 :lines 1 :width 16 :height 17 :graphic t))
         (cache (cons (make-hash-table :test #'equal)
                      (make-hash-table :test #'equal)))
         (art (etaf-go--intersection-art cache etaf-go--light-theme metrics
                                         size 5 5 position marks t nil nil))
         (margin (etaf-go--margin-art cache etaf-go--light-theme metrics
                                      size 0 3 nil)))
    (should (eq (car art) 'image))
    (should (eq (plist-get (cdr art) :type) 'svg))
    (should (stringp (plist-get (cdr art) :data)))
    (should (eq (car margin) 'image))
    (should (= (hash-table-count (car cache)) 2))))

(ert-deftest etaf-go-view-board-metrics-uses-display-metrics ()
  "Board metrics use the runtime's font metrics and viewport."
  (let ((etaf-go-cell-lines 'auto))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
              ((symbol-function 'etaf-display-metrics)
               (lambda (&optional _)
                 '(:char-width 12 :line-height 27 :ascent 80)))
              ((symbol-function 'etaf-viewport)
               (lambda (&optional _) (cons 1440 50))))
      (let ((cell (etaf-go--board-metrics 9)))
        (should (= (plist-get cell :lines) 4))
        (should (= (plist-get cell :columns) 9))
        (should (= (plist-get cell :width) 108))
        (should (= (plist-get cell :height) 108))
        (should (= (plist-get cell :char-width) 12))
        (should (plist-get cell :graphic))
        (should (plist-get cell :compact))))))

(ert-deftest etaf-go-view-cycle-cell-lines ()
  "The cell-line control cycles auto, 1, 2, 3, 4 and back."
  (let ((ref (etaf-ref 'auto))
        (hint (etaf-ref nil)))
    (dolist (expected '(1 2 3 4 auto))
      (etaf-go--cycle-cell-lines ref hint)
      (should (equal (etaf-value ref) expected)))))

(ert-deftest etaf-go-view-forced-lines-are-square ()
  "Forcing two lines gives a 35x34 cell for a 7x17 font."
  (let ((etaf-go-cell-lines 2))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
              ((symbol-function 'etaf-display-metrics)
               (lambda (&optional _)
                 '(:char-width 7 :line-height 17 :ascent 80)))
              ((symbol-function 'etaf-viewport)
               (lambda (&optional _) (cons 1085 34))))
      (let ((cell (etaf-go--board-metrics 19)))
        (should (= (plist-get cell :lines) 2))
        (should (= (plist-get cell :columns) 5))
        (should (= (plist-get cell :width) 35))
        (should (= (plist-get cell :height) 34))))))

(ert-deftest etaf-go-view-cell-aspect-selection ()
  "Cell selection takes the largest near-square cell that fits."
  (let ((cell (etaf-go--best-cell 12 27 4 10)))
    (should (= (plist-get cell :lines) 4))
    (should (= (plist-get cell :columns) 9))
    (should (= (plist-get cell :width) 108))
    (should (= (plist-get cell :height) 108)))
  (let ((cell (etaf-go--best-cell 12 30 4 10)))
    (should (= (plist-get cell :lines) 4))
    (should (= (plist-get cell :columns) 10)))
  (let ((cell (etaf-go--best-cell 10 20 4 10)))
    (should (= (plist-get cell :lines) 4))
    (should (= (plist-get cell :columns) 8)))
  (let ((cell (etaf-go--best-cell 12 27 1 10)))
    (should (= (plist-get cell :lines) 1))
    (should (= (plist-get cell :columns) 2)))
  (let ((cell (etaf-go--best-cell 12 27 4 3)))
    (should (= (plist-get cell :lines) 1))
    (should (= (plist-get cell :columns) 2))))

(ert-deftest etaf-go-view-19x19-metrics-use-height ()
  "A 19x19 board takes multiple text lines per cell in a normal window."
  (let ((etaf-go-cell-lines 'auto))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
              ((symbol-function 'etaf-display-metrics)
               (lambda (&optional _)
                 '(:char-width 9 :line-height 20 :ascent 80)))
              ((symbol-function 'etaf-viewport)
               (lambda (&optional _) (cons 1600 50))))
      (let ((cell (etaf-go--board-metrics 19)))
        (should (= (plist-get cell :lines) 2))
        (should (= (plist-get cell :columns) 4))
        (should (= (plist-get cell :height) 40))
        (should-not (plist-get cell :compact))))))

(ert-deftest etaf-go-view-short-window-uses-two-lines ()
  "A 45-line window still gives the board two text lines per cell."
  (let ((etaf-go-cell-lines 'auto))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
              ((symbol-function 'etaf-display-metrics)
               (lambda (&optional _)
                 '(:char-width 7 :line-height 14 :ascent 80)))
              ((symbol-function 'etaf-viewport)
               (lambda (&optional _) (cons 1085 45))))
      (let ((cell (etaf-go--board-metrics 19)))
        (should (= (plist-get cell :lines) 2))
        (should (= (plist-get cell :columns) 4))
        (should (= (plist-get cell :width) 28))
        (should (= (plist-get cell :height) 28))))))

(ert-deftest etaf-go-view-narrow-window-stacks-panel ()
  "When the side panel would shrink the board, the board keeps its size."
  (let ((etaf-go-cell-lines 'auto))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
              ((symbol-function 'etaf-display-metrics)
               (lambda (&optional _)
                 '(:char-width 9 :line-height 20 :ascent 80)))
              ((symbol-function 'etaf-viewport)
               (lambda (&optional _) (cons 1000 50))))
      (let ((cell (etaf-go--board-metrics 19)))
        (should (= (plist-get cell :lines) 2))
        (should (= (plist-get cell :columns) 4))
        (should (plist-get cell :compact))))))

(ert-deftest etaf-go-view-svg-resolves-palette-handle ()
  "A reactive palette handle still paints the board instead of black."
  (let* ((root (etaf-go--sgf-parse etaf-go-test--demo-sgf))
         (size 9)
         (position (etaf-go--position size root (list root)))
         (marks (etaf-go--node-marks root size))
         (metrics (list :columns 2 :lines 1 :width 16 :height 17 :graphic t))
         (cache (cons (make-hash-table :test #'equal)
                      (make-hash-table :test #'equal))))
    (dolist (palette (list (etaf-ref etaf-go--light-theme)
                           (etaf-computed (lambda () etaf-go--light-theme))))
      (let* ((art (etaf-go--intersection-art cache palette metrics
                                             size 5 5 position marks t nil nil))
             (margin (etaf-go--margin-art cache palette metrics size 0 3 nil))
             (data (plist-get (cdr art) :data))
             (margin-data (plist-get (cdr margin) :data)))
        (should (string-match-p "#E6C58B" data))
        (should (string-match-p "#6A4A24" data))
        (should (string-match-p "#DDB87A" margin-data))))))

(ert-deftest etaf-go-view-all-19x19-cells ()
  "Every cell of a 19x19 board produces artwork without an error."
  (let* ((root (etaf-go--sgf-parse "(;GM[1]SZ[19]AB[dd][pp]TR[dd])"))
         (size 19)
         (position (etaf-go--position size root (list root)))
         (marks (etaf-go--node-marks root size))
         (metrics (list :columns 2 :lines 1 :width 16 :height 17 :graphic t))
         (cache (cons (make-hash-table :test #'equal)
                      (make-hash-table :test #'equal)))
         (count 0))
    (dotimes (row (+ size 2))
      (dotimes (column (+ size 2))
        (if (etaf-go--grid-point row column size)
            (etaf-go--intersection-art cache etaf-go--dark-theme metrics
                                       size row column position marks t nil nil)
          (etaf-go--margin-art cache etaf-go--dark-theme metrics
                               size row column nil))
        (cl-incf count)))
    (should (= count (* (+ size 2) (+ size 2))))
    (should (= (hash-table-count (car cache)) (* (+ size 2) (+ size 2))))))

(ert-deftest etaf-go-view-app-clears-global-line-spacing ()
  "The application buffer ignores a global `line-spacing'."
  (let ((previous (default-value 'line-spacing)))
    (unwind-protect
        (progn
          (setq-default line-spacing 0.1)
          (cl-letf (((symbol-function 'display-graphic-p)
                     (lambda (&rest _) nil)))
            (let ((buffer (etaf-go-test--open-sample)))
              (unwind-protect
                  (should (null (buffer-local-value 'line-spacing buffer)))
                (when (and (buffer-live-p buffer)
                           (etaf-go--session-p (etaf-app-state buffer)))
                  (etaf-go-close buffer))))))
      (setq-default line-spacing previous))))

(ert-deftest etaf-go-view-practice-and-study ()
  "Practice, hint, undo, mode switching, study moves and passing work."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (let* ((buffer (etaf-go-test--open-sample))
           (runtime (etaf-runtime-for-buffer buffer)))
      (unwind-protect
          (progn
            ;; A wrong practice move is refused without changing the board.
            (etaf-go-test--press runtime 'go-cell-2-2)
            (should (= (etaf-go-test--count buffer ?●) 3))
            (should (= (etaf-go-test--count buffer ?○) 1))
            (should (string-match-p "不是正解" (etaf-go-test--text buffer)))
            ;; The solution captures the white stone.
            (etaf-go-test--press runtime 'go-cell-6-5)
            (should (= (etaf-go-test--count buffer ?●) 4))
            (should (= (etaf-go-test--count buffer ?○) 0))
            (should (string-match-p "正解完成" (etaf-go-test--text buffer)))
            (should (string-match-p "提子成功" (etaf-go-test--text buffer)))
            ;; Undo, then let the hint play the solution.
            (etaf-go-test--press runtime 'go-prev)
            (should (= (etaf-go-test--count buffer ?●) 3))
            (should (= (etaf-go-test--count buffer ?○) 1))
            (etaf-go-test--press runtime 'go-hint)
            (should (= (etaf-go-test--count buffer ?●) 4))
            (should (= (etaf-go-test--count buffer ?○) 0))
            (etaf-go-test--press runtime 'go-start)
            (should (= (etaf-go-test--count buffer ?●) 3))
            (should (= (etaf-go-test--count buffer ?○) 1))
            ;; Practice -> study; play a free move that joins the record.
            (etaf-go-test--press runtime 'go-mode)
            (should (string-match-p "模式·研究" (etaf-go-test--text buffer)))
            (etaf-go-test--press runtime 'go-cell-2-2)
            (should (= (etaf-go-test--count buffer ?●) 4))
            (should (string-match-p "已写入棋谱" (etaf-go-test--text buffer)))
            (etaf-go-test--press runtime 'go-prev)
            (should (= (etaf-go-test--count buffer ?●) 3))
            ;; Pass in study records a pass node, then undo removes it.
            (etaf-go-test--press runtime 'go-pass)
            (should (string-match-p "黑 停一手（已写入棋谱"
                                    (etaf-go-test--text buffer)))
            (etaf-go-test--press runtime 'go-prev)
            (should-not (string-match-p "黑 停一手（已写入棋谱"
                                        (etaf-go-test--text buffer)))
            ;; Display toggles keep the position.
            (etaf-go-test--press runtime 'go-theme)
            (etaf-go-test--press runtime 'go-numbers)
            (should (= (etaf-go-test--count buffer ?●) 3)))
        (when (and (buffer-live-p buffer)
                   (etaf-go--session-p (etaf-app-state buffer)))
          (etaf-go-close buffer))))))

(ert-deftest etaf-go-view-study-branch-persists-and-saves ()
  "Study moves stay in the tree, survive saving and reopen identically."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (let* ((directory (make-temp-file "etaf-go-test" t))
           (file (expand-file-name "study.sgf" directory))
           (buffer nil))
      (unwind-protect
          (progn
            (write-region etaf-go-test--demo-sgf nil file nil 'silent)
            (setq buffer (etaf-go-open-file file))
            (let ((runtime (etaf-runtime-for-buffer buffer)))
              ;; Replay -> practice -> study, then play a recorded branch move.
              (etaf-go-test--press runtime 'go-mode)
              (etaf-go-test--press runtime 'go-mode)
              (etaf-go-test--press runtime 'go-cell-2-2)
              (should (string-match-p "已写入棋谱"
                                      (etaf-go-test--text buffer)))
              ;; The tree keeps the new variation next to the record line.
              (let* ((session (etaf-app-state buffer))
                     (root (etaf-go--session-root session)))
                (should (= (length (etaf-go--node-children root)) 2))
                (should (equal (etaf-go--prop1
                                (nth 1 (etaf-go--node-children root)) "B")
                               "bb")))
              ;; The board still shows the study stone after navigating away.
              (etaf-go-test--press runtime 'go-start)
              (etaf-go-test--press runtime 'go-next)
              (should (= (etaf-go-test--count buffer ?●) 4))
              ;; Saving writes the branch; the marker round-trips.
              (etaf-go-test--press runtime 'go-save)
              (let ((text (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string))))
                (should (string-match-p "EG\\[1\\]" text))
                (should (string-match-p "B\\[bb\\]" text)))
              ;; A fresh session of the saved file replays the branch.
              (etaf-go-close buffer)
              (setq buffer (etaf-go-open-file file)
                    runtime (etaf-runtime-for-buffer buffer))
              (etaf-go-test--press runtime 'go-mode)
              (etaf-go-test--press runtime 'go-mode)
              (should (= (length (etaf-go--node-children
                                  (etaf-go--session-root
                                   (etaf-app-state buffer))))
                         2))))
        (when (and (buffer-live-p buffer)
                   (etaf-go--session-p (etaf-app-state buffer)))
          (etaf-go-close buffer))
        (delete-directory directory t)))))

(ert-deftest etaf-go-view-study-does-not-become-a-solution ()
  "Practice ignores branches added by study mode."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (let* ((buffer (etaf-go-test--open-sample))
           (runtime (etaf-runtime-for-buffer buffer)))
      (unwind-protect
          (progn
            ;; Study a move, return to the start, then back to practice.
            (etaf-go-test--press runtime 'go-mode)
            (etaf-go-test--press runtime 'go-cell-2-2)
            (etaf-go-test--press runtime 'go-start)
            (etaf-go-test--press runtime 'go-mode)
            (etaf-go-test--press runtime 'go-mode)
            (should (string-match-p "模式·死活" (etaf-go-test--text buffer)))
            ;; The studied point is still not the answer.
            (etaf-go-test--press runtime 'go-cell-2-2)
            (should (string-match-p "不是正解" (etaf-go-test--text buffer)))
            ;; The recorded solution still solves the problem.
            (etaf-go-test--press runtime 'go-cell-6-5)
            (should (string-match-p "正解" (etaf-go-test--text buffer))))
        (when (and (buffer-live-p buffer)
                   (etaf-go--session-p (etaf-app-state buffer)))
          (etaf-go-close buffer))))))

(ert-deftest etaf-go-view-comment-editing ()
  "The comment dialog opens, submits and closes from the buttons."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (let* ((buffer (etaf-go-test--open-sample))
           (runtime (etaf-runtime-for-buffer buffer)))
      (unwind-protect
          (progn
            (etaf-go-test--press runtime 'go-comment)
            (should (string-match-p "编辑注释" (etaf-go-test--text buffer)))
            (etaf-go-test--press runtime 'go-comment-save)
            (should (string-match-p "注释已保存到当前节点"
                                    (etaf-go-test--text buffer)))
            (should-not (string-match-p "编辑注释"
                                        (etaf-go-test--text buffer))))
        (when (and (buffer-live-p buffer)
                   (etaf-go--session-p (etaf-app-state buffer)))
          (etaf-go-close buffer))))))

(ert-deftest etaf-go-view-comment-submit-writes-node ()
  "Submitting the editor writes or removes the current node's comment."
  (let* ((root (etaf-go--sgf-parse etaf-go-test--demo-sgf))
         (path (etaf-ref (list root)))
         (open (etaf-ref t))
         (draft (etaf-ref "试一下注释"))
         (dirty (etaf-ref nil))
         (hint (etaf-ref nil)))
    (etaf-go--submit-comment path open draft dirty hint root)
    (should (equal (etaf-go--prop1 root "C") "试一下注释"))
    (should (etaf-value dirty))
    (should-not (etaf-value open))
    ;; An empty draft removes the comment again.
    (setf (etaf-value draft) "")
    (etaf-go--submit-comment path open draft dirty hint root)
    (should (null (etaf-go--prop1 root "C")))))

(ert-deftest etaf-go-view-delete-branch ()
  "Deleting a branch removes its node and marks the session dirty."
  (let* ((root (etaf-go--sgf-parse etaf-go-test--demo-sgf))
         (mode (etaf-ref 'study))
         (path (etaf-ref (list root)))
         (dirty (etaf-ref nil))
         (hint (etaf-ref nil))
         (position (etaf-ref (etaf-go--position 9 root (list root)))))
    (cl-letf (((symbol-function 'etaf-confirm-dialog) (lambda (_) t)))
      (etaf-go--study-play path dirty hint position 9 (+ (* 1 9) 1))
      (should (= (length (etaf-go--node-children root)) 2))
      (should (etaf-value dirty))
      (etaf-go--delete-branch mode path dirty hint)
      (should (= (length (etaf-go--node-children root)) 1))
      (should (equal (etaf-value path) (list root)))
      (should (string-match-p "分支已删除" (etaf-value hint))))))

(ert-deftest etaf-go-view-maybe-close-confirms-dirty ()
  "Closing a dirty session asks first and keeps the buffer on decline."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
            ((symbol-function 'etaf-confirm-dialog) (lambda (_) nil)))
    (let ((buffer (etaf-go-test--open-sample)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (etaf-go--maybe-close (etaf-ref t)))
            (should (buffer-live-p buffer))
            (cl-letf (((symbol-function 'etaf-confirm-dialog)
                       (lambda (_) t)))
              (with-current-buffer buffer
                (etaf-go--maybe-close (etaf-ref t))))
            (should-not (buffer-live-p buffer)))
        (when (and (buffer-live-p buffer)
                   (etaf-go--session-p (etaf-app-state buffer)))
          (etaf-go-close buffer))))))

(ert-deftest etaf-go-view-territory-art ()
  "An empty point owned by black paints territory in the SVG cell."
  (let* ((root (etaf-go--sgf-parse "(;GM[1]SZ[3]AB[aa][ab])"))
         (position (etaf-go--position 3 root (list root)))
         (territory (etaf-go--territory (plist-get position :board) 3))
         (metrics (list :columns 2 :lines 1 :width 16 :height 17 :graphic t))
         (cache (cons (make-hash-table :test #'equal)
                      (make-hash-table :test #'equal)))
         (art (etaf-go--intersection-art cache etaf-go--dark-theme metrics
                                         3 3 3 position nil nil territory nil))
         (data (plist-get (cdr art) :data)))
    (should (eq (car art) 'image))
    (should (string-match-p "#14161A" data))))

(ert-deftest etaf-go-view-edit-mode-authoring ()
  "Edit mode writes setup stones, marks and the player into the node."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (let* ((buffer (etaf-go-test--open-sample))
           (runtime (etaf-runtime-for-buffer buffer))
           (root (etaf-go--session-root (etaf-app-state buffer))))
      (unwind-protect
          (progn
            (etaf-go-test--press runtime 'go-edit)
            (should (string-match-p "模式·编辑" (etaf-go-test--text buffer)))
            ;; A black setup stone appears and is written as AB.
            (etaf-go-test--press runtime 'go-tool-black)
            (etaf-go-test--press runtime 'go-cell-1-1)
            (should (= (etaf-go-test--count buffer ?●) 4))
            (should (member "aa" (etaf-go--prop root "AB")))
            ;; White replaces the same point instead of stacking.
            (etaf-go-test--press runtime 'go-tool-white)
            (etaf-go-test--press runtime 'go-cell-1-1)
            (should (= (etaf-go-test--count buffer ?●) 3))
            (should (= (etaf-go-test--count buffer ?○) 2))
            (should-not (member "aa" (etaf-go--prop root "AB")))
            (should (member "aa" (etaf-go--prop root "AW")))
            ;; Erase records AE on the current node.
            (etaf-go-test--press runtime 'go-tool-erase)
            (etaf-go-test--press runtime 'go-cell-1-1)
            (should (= (etaf-go-test--count buffer ?○) 1))
            (should (member "aa" (etaf-go--prop root "AE")))
            ;; Marks and the player are written too.
            (etaf-go-test--press runtime 'go-tool-triangle)
            (etaf-go-test--press runtime 'go-cell-2-2)
            (should (member "bb" (etaf-go--prop root "TR")))
            (etaf-go-test--press runtime 'go-player)
            (should (equal (etaf-go--prop1 root "PL") "B"))
            (etaf-go-test--press runtime 'go-player)
            (should (equal (etaf-go--prop1 root "PL") "W"))
            ;; The score overlay reports its area summary.
            (etaf-go-test--press runtime 'go-score)
            (should (string-match-p "数子 黑3 白1" (etaf-go-test--text buffer))))
        (when (and (buffer-live-p buffer)
                   (etaf-go--session-p (etaf-app-state buffer)))
          (etaf-go-close buffer))))))

(ert-deftest etaf-go-view-edit-skips-move-nodes ()
  "Editing a node that holds a move explains where to edit instead."
  (let* ((root (etaf-go--sgf-parse etaf-go-test--demo-sgf))
         (move (car (etaf-go--node-children root)))
         (path (etaf-ref (list root move)))
         (dirty (etaf-ref nil))
         (hint (etaf-ref nil))
         (tool (etaf-ref 'black)))
    (etaf-go--edit-point tool path dirty hint 9 move (+ (* 1 9) 1))
    (should-not (etaf-value dirty))
    (should (string-match-p "回到开局" (etaf-value hint)))))

(ert-deftest etaf-go-view-edit-label ()
  "Labels are set, replaced and removed on the current node."
  (let ((node (etaf-go--sgf-parse "(;GM[1]SZ[9])")))
    (etaf-go--edit-label node 9 0 "A")
    (should (equal (etaf-go--prop node "LB") '("aa:A")))
    (etaf-go--edit-label node 9 0 "B")
    (should (equal (etaf-go--prop node "LB") '("aa:B")))
    (etaf-go--edit-label node 9 0 "")
    (should-not (etaf-go--prop node "LB"))))

(ert-deftest etaf-go-view-new-record-starts-in-edit-mode ()
  "A new record opens an empty board ready for setup."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (let ((buffer (etaf-go-new 9)))
      (unwind-protect
          (progn
            (let ((session (etaf-app-state buffer)))
              (should (= (etaf-go--session-size session) 9))
              (should (eq (etaf-go--session-mode session) 'edit))
              (should (equal (etaf-go--prop1
                              (etaf-go--session-root session) "SZ")
                             "9")))
            (should (string-match-p "模式·编辑" (etaf-go-test--text buffer)))
            (should (= (etaf-go-test--count buffer ?●) 0)))
        (when (and (buffer-live-p buffer)
                   (etaf-go--session-p (etaf-app-state buffer)))
          (etaf-go-close buffer))))))

(ert-deftest etaf-go-view-edit-saves-and-reopens ()
  "A problem authored in edit mode saves and reads back."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (let* ((directory (make-temp-file "etaf-go-edit" t))
           (file (expand-file-name "problem.sgf" directory))
           (buffer nil))
      (unwind-protect
          (progn
            (setq buffer (etaf-go-new 9))
            (let ((runtime (etaf-runtime-for-buffer buffer)))
              (etaf-go-test--press runtime 'go-tool-black)
              (etaf-go-test--press runtime 'go-cell-3-3)
              (etaf-go-test--press runtime 'go-tool-triangle)
              (etaf-go-test--press runtime 'go-cell-3-4)
              (etaf-go-test--press runtime 'go-player)
              (cl-letf (((symbol-function 'etaf-call-with-input)
                         (lambda (reader) (funcall reader)))
                        ((symbol-function 'read-file-name)
                         (lambda (&rest _) file)))
                (etaf-go-test--press runtime 'go-save)))
            (should (file-exists-p file))
            (let ((text (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))
              (should (string-match-p "SZ\\[9\\]" text))
              (should (string-match-p "AB\\[cc\\]" text))
              (should (string-match-p "TR\\[dc\\]" text))
              (should (string-match-p "PL\\[W\\]" text)))
            (etaf-go-close buffer)
            (setq buffer (etaf-go-open-file file))
            (let* ((session (etaf-app-state buffer))
                   (root (etaf-go--session-root session))
                   (board (plist-get (etaf-go--position 9 root (list root))
                                     :board)))
              (should (= (etaf-go--session-size session) 9))
              (should (= (aref board (+ (* 2 9) 2)) etaf-go--black))
              (should (= (aref board (+ (* 2 9) 3)) 0))))
        (when (and (buffer-live-p buffer)
                   (etaf-go--session-p (etaf-app-state buffer)))
          (etaf-go-close buffer))
        (delete-directory directory t)))))

(ert-deftest etaf-go-view-home ()
  "The main menu opens with the local-play choices."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (let ((buffer (etaf-go)))
      (unwind-protect
          (progn
            (should (eq (etaf-app-state buffer) 'home))
            (should (string-match-p "本地双人" (etaf-go-test--text buffer)))
            (should (string-match-p "19 路" (etaf-go-test--text buffer)))
            (should (string-match-p "打开棋谱" (etaf-go-test--text buffer))))
        (when (buffer-live-p buffer)
          (etaf-go-close buffer))))))

(ert-deftest etaf-go-view-home-launches-game ()
  "A menu choice opens the game and closes the menu without hanging."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (let* ((home (etaf-go))
           (runtime (etaf-runtime-for-buffer home)))
      (unwind-protect
          (progn
            (etaf-go-test--press runtime 'go-home-play-9)
            (sit-for 0.3)
            (should-not (buffer-live-p home))
            (should (cl-find-if
                     (lambda (buffer)
                       (string-prefix-p "*Go: 对弈 9 路*"
                                        (buffer-name buffer)))
                     (buffer-list))))
        (dolist (buffer (buffer-list))
          (when (and (string-prefix-p "*Go" (buffer-name buffer))
                     (etaf-app-state buffer))
            (etaf-go-close buffer)))))))

(ert-deftest etaf-go-view-play-hot-seat ()
  "Local play alternates turns, ends after two passes and can undo."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (let* ((buffer (etaf-go-play 9))
           (runtime (etaf-runtime-for-buffer buffer)))
      (unwind-protect
          (progn
            (should (string-match-p "模式·对弈" (etaf-go-test--text buffer)))
            (should (string-match-p "黑棋行棋" (etaf-go-test--text buffer)))
            ;; Black and White alternate on the same board.
            (etaf-go-test--press runtime 'go-cell-3-3)
            (should (string-match-p "白棋行棋" (etaf-go-test--text buffer)))
            (etaf-go-test--press runtime 'go-cell-4-4)
            (should (string-match-p "黑棋行棋" (etaf-go-test--text buffer)))
            ;; Undo removes the played node, not just the cursor.
            (etaf-go-test--press runtime 'go-prev)
            (should (string-match-p "白棋行棋" (etaf-go-test--text buffer)))
            (let* ((session (etaf-app-state buffer))
                   (root (etaf-go--session-root session)))
              (should (= (length (etaf-go--node-children root)) 1)))
            ;; Two passes finish the game and show the area score.
            (etaf-go-test--press runtime 'go-pass)
            (etaf-go-test--press runtime 'go-pass)
            (should (string-match-p "对局结束" (etaf-go-test--text buffer)))
            (should (string-match-p "数子" (etaf-go-test--text buffer)))
            ;; Undo reopens the game.
            (etaf-go-test--press runtime 'go-prev)
            (should-not (string-match-p "对局结束"
                                        (etaf-go-test--text buffer))))
        (when (and (buffer-live-p buffer)
                   (etaf-go--session-p (etaf-app-state buffer)))
          (etaf-go-close buffer))))))

(ert-deftest etaf-go-view-play-resign ()
  "Resigning ends the game for the side to move."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
            ((symbol-function 'etaf-confirm-dialog) (lambda (_) t)))
    (let* ((buffer (etaf-go-play 9))
           (runtime (etaf-runtime-for-buffer buffer)))
      (unwind-protect
          (progn
            (etaf-go-test--press runtime 'go-resign)
            (should (string-match-p "白中盘胜" (etaf-go-test--text buffer))))
        (when (and (buffer-live-p buffer)
                   (etaf-go--session-p (etaf-app-state buffer)))
          (etaf-go-close buffer))))))

(ert-deftest etaf-go-view-play-new-game ()
  "The new button in play mode replaces the game with a fresh one."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
    (let* ((buffer (etaf-go-play 9))
           (runtime (etaf-runtime-for-buffer buffer)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'etaf-call-with-input)
                       (lambda (reader) (funcall reader)))
                      ((symbol-function 'read-number) (lambda (&rest _) 13))
                      ((symbol-function 'etaf-confirm-dialog)
                       (lambda (&rest _) t)))
              (etaf-go-test--press runtime 'go-new)
              (sit-for 0.3))
            (should-not (buffer-live-p buffer))
            (should (cl-find-if
                     (lambda (candidate)
                       (string-prefix-p "*Go: 对弈 13 路*"
                                        (buffer-name candidate)))
                     (buffer-list))))
        (dolist (candidate (buffer-list))
          (when (and (string-prefix-p "*Go" (buffer-name candidate))
                     (etaf-app-state candidate))
            (etaf-go-close candidate)))))))

(ert-deftest etaf-go-view-practice-accepts-every-answer ()
  "Any sibling solution move is accepted as a correct answer."
  (let* ((node (etaf-go--sgf-parse
                "(;GM[1]SZ[9]AB[de][fe][ed]AW[ee](;B[ef])(;B[dd]))"))
         (path (etaf-ref (list node)))
         (hint (etaf-ref nil)))
    (should (= (length (etaf-go--practice-children node)) 2))
    (etaf-go--practice-press path hint etaf-go--black 9 (+ (* 5 9) 4))
    (should (string-match-p "正解" (etaf-value hint)))
    (should (= (length (etaf-value path)) 2))
    (setf (etaf-value path) (list node))
    (etaf-go--practice-press path hint etaf-go--black 9 (+ (* 3 9) 3))
    (should (string-match-p "正解" (etaf-value hint)))
    (should (equal (etaf-go--node-move (car (last (etaf-value path))) 9)
                   (list :color etaf-go--black :point (+ (* 3 9) 3))))))

(ert-deftest etaf-go-view-practice-rejects-failure-lines ()
  "A branch commented as a failure is not accepted and is not followed."
  (let* ((text (concat "(;GM[1]SZ[9]AB[de][fe][ed]AW[ee]"
                       "(;B[ef]C[正解])(;B[dd]C[失败，白棋做活])"
                       ")"))
         (node (etaf-go--sgf-parse text))
         (path (etaf-ref (list node)))
         (hint (etaf-ref nil)))
    (should (= (length (etaf-go--practice-children node)) 1))
    (etaf-go--practice-press path hint etaf-go--black 9 (+ (* 3 9) 3))
    (should (string-match-p "不是正解" (etaf-value hint)))
    (should (= (length (etaf-value path)) 1))
    (etaf-go--practice-press path hint etaf-go--black 9 (+ (* 5 9) 5))
    (should (string-match-p "正解" (etaf-value hint)))))

(ert-deftest etaf-go-view-practice-comment-marks ()
  "Comment words classify a branch as right, wrong or unmarked."
  (let ((node (etaf-go--sgf-parse "(;GM[1]SZ[9])")))
    (etaf-go--set-prop node "C" (list "正解，黑棋提子"))
    (should (eq (etaf-go--comment-mark node) 'right))
    (etaf-go--set-prop node "C" (list "黑棋正解"))
    (should (eq (etaf-go--comment-mark node) 'right))
    (etaf-go--set-prop node "C" (list "失败：白棋活出"))
    (should (eq (etaf-go--comment-mark node) 'wrong))
    (etaf-go--set-prop node "C" (list "黑失败，白棋做活"))
    (should (eq (etaf-go--comment-mark node) 'wrong))
    (etaf-go--set-prop node "C" (list "不正确，这里不入气"))
    (should (eq (etaf-go--comment-mark node) 'wrong))
    (etaf-go--set-prop node "C" (list "Correct answer"))
    (should (eq (etaf-go--comment-mark node) 'right))
    (etaf-go--set-prop node "C" (list "正解；否则失败"))
    (should (eq (etaf-go--comment-mark node) 'right))
    (etaf-go--set-prop node "C" (list "这里只是说明"))
    (should-not (etaf-go--comment-mark node))
    (etaf-go--set-prop node "C" nil)
    (should-not (etaf-go--comment-mark node))))

(ert-deftest etaf-go-view-practice-hint-names-multiple-answers ()
  "The hint notes that more answers exist when the line finishes."
  (let* ((node (etaf-go--sgf-parse "(;GM[1]SZ[9](;B[ef])(;B[dd]))"))
         (path (etaf-ref (list node)))
         (hint (etaf-ref nil)))
    (etaf-go--practice-next path hint etaf-go--black 9)
    (should (= (length (etaf-value path)) 2))
    (should (string-match-p "还有别的正解" (etaf-value hint)))))

(ert-deftest etaf-go-view-problem-region-local ()
  "A small problem becomes a local view with a two-point margin."
  (let ((root (etaf-go--sgf-parse
               "(;GM[1]SZ[19]AB[dd][ed]AW[de];B[ee];W[fe])")))
    (should (equal (etaf-go--problem-region (list root) 19)
                   '(1 1 6 7)))))

(ert-deftest etaf-go-view-problem-region-full ()
  "Wide problems and empty boards keep the full view."
  (let ((wide (etaf-go--sgf-parse
               "(;GM[1]SZ[19]AB[aa][ss]AW[as][sa];B[jj])"))
        (empty (etaf-go--sgf-parse "(;GM[1]SZ[19])")))
    (should (equal (etaf-go--problem-region (list wide) 19) '(0 0 19 19)))
    (should (equal (etaf-go--problem-region (list empty) 19) '(0 0 19 19)))))

(ert-deftest etaf-go-view-problem-region-minimum ()
  "A single point still gets a readable five-intersection view."
  (let ((root (etaf-go--sgf-parse "(;GM[1]SZ[19]AB[jj])")))
    (should (equal (etaf-go--problem-region (list root) 19) '(7 7 5 5)))))

(ert-deftest etaf-go-view-region-grid-points ()
  "Grid cells map to the visible board points."
  (should (equal (etaf-go--grid-point 1 1 19 '(2 3 5 4)) '(2 . 3)))
  (should (equal (etaf-go--grid-point 5 4 19 '(2 3 5 4)) '(6 . 6)))
  (should-not (etaf-go--grid-point 6 1 19 '(2 3 5 4)))
  (should-not (etaf-go--grid-point 1 5 19 '(2 3 5 4)))
  (should (equal (etaf-go--grid-point 1 1 19) '(0 . 0))))

(ert-deftest etaf-go-view-metrics-magnify-local-view ()
  "A local view gets larger cells than the full board."
  (let ((etaf-go-cell-lines 'auto))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
              ((symbol-function 'etaf-display-metrics)
               (lambda (&optional _)
                 '(:char-width 7 :line-height 17 :ascent 80)))
              ((symbol-function 'etaf-viewport)
               (lambda (&optional _) (cons 1085 34))))
      (let ((local (etaf-go--board-metrics 19 '(2 3 7 6)))
            (full (etaf-go--board-metrics 19)))
        (should (= (plist-get local :lines) 3))
        (should (= (plist-get local :columns) 7))
        (should (= (plist-get local :width) 49))
        (should (= (plist-get local :height) 51))
        (should (= (plist-get full :lines) 1))
        (should (> (plist-get local :width) (plist-get full :width)))))))

(ert-deftest etaf-go-view-toggle-zoom ()
  "Zoom toggles only in practice on a graphical display."
  (let* ((mode (etaf-ref 'practice))
         (zoom (etaf-ref nil))
         (hint (etaf-ref nil))
         (root (etaf-go--sgf-parse
                "(;GM[1]SZ[19]AB[dd][ed]AW[de];B[ee])"))
         (region (etaf-ref (etaf-go--problem-region (list root) 19))))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (etaf-go--toggle-zoom mode zoom hint region 19)
      (should (etaf-value zoom))
      (should (string-match-p "局部放大" (etaf-value hint)))
      (etaf-go--toggle-zoom mode zoom hint region 19)
      (should-not (etaf-value zoom))
      (should (string-match-p "全盘" (etaf-value hint))))
    (setf (etaf-value mode) 'replay)
    (etaf-go--toggle-zoom mode zoom hint region 19)
    (should (string-match-p "只用于死活" (etaf-value hint)))
    (setf (etaf-value mode) 'practice)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (etaf-go--toggle-zoom mode zoom hint region 19)
      (should (string-match-p "需要图形界面" (etaf-value hint))))))

(ert-deftest etaf-go-view-region-renders-local-board ()
  "A local view renders only its coordinates and intersections."
  (let* ((root (etaf-go--sgf-parse "(;GM[1]SZ[19]AB[dd][ed]AW[de])"))
         (position (etaf-go--position 19 root (list root)))
         (position-ref (etaf-ref position))
         (marks-ref (etaf-ref nil))
         (numbers-ref (etaf-ref nil))
         (territory-ref (etaf-ref nil))
         (metrics (list :columns 2 :lines 1 :width 40 :height 40
                        :char-width 1 :graphic nil))
         (buffer (etaf-app-open
                  "*Go: region test*"
                  (etaf-view
                   (etaf-go--board
                    :size 19 :position position-ref
                    :palette etaf-go--light-theme
                    :metrics metrics :marks marks-ref
                    :show-numbers numbers-ref :territory territory-ref
                    :view '(2 3 5 4)
                    :on-press (lambda (&rest _) nil)))
                  :state nil)))
    (unwind-protect
        (with-current-buffer buffer
          (let ((text (buffer-string)))
            (should (string-match-p "D" text))
            (should (string-match-p "G" text))
            (should (string-match-p "17" text))
            (should (string-match-p "13" text))
            (should-not (string-match-p "19" text))))
      (etaf-app-close buffer))))

(defun etaf-go-test--engine-program ()
  "Return the fake engine command, or nil when python3 is missing."
  (when (executable-find "python3")
    (list "python3"
          (expand-file-name "test/fake-gtp-engine.py" etaf-go--directory))))

(ert-deftest etaf-go-view-engine-plays-its-side ()
  "With an engine configured, it moves for its side and answers the human."
  (let ((program (etaf-go-test--engine-program)))
    (if (not program)
        (ert-skip "python3 is not available")
      (let ((etaf-go-engine-program program)
            (etaf-go-engine-handicap 'black))
        (cl-letf (((symbol-function 'display-graphic-p)
                   (lambda (&rest _) nil)))
          (let* ((buffer (etaf-go-play 9))
                 (runtime (etaf-runtime-for-buffer buffer))
                 (session (etaf-app-state buffer))
                 (root (etaf-go--session-root session)))
            (unwind-protect
                (progn
                  (etaf-go-test--wait 1.2)
                  (should (etaf-go--session-engine session))
                  (should (= (etaf-go--session-engine-color session)
                             etaf-go--black))
                  ;; The engine opened with a legal black move.
                  (should (= (length (etaf-go--node-children root)) 1))
                  (let* ((first (car (etaf-go--node-children root)))
                         (move (etaf-go--node-move first 9)))
                    (should (= (plist-get move :color) etaf-go--black))
                    (should (integerp (plist-get move :point)))
                    (should (< (plist-get move :point) (* 9 9))))
                  (should (string-match-p "轮到你"
                                          (etaf-go-test--text buffer)))
                  ;; The human answers as white; the engine then replies.
                  (etaf-go-test--press runtime 'go-cell-5-5)
                  (etaf-go-test--wait 1.4)
                  (should (string-match-p "第 [3-9][0-9]* /"
                                          (etaf-go-test--text buffer)
                                          )))
              (etaf-go-close buffer))))))))

(defun etaf-go-test--wait (seconds)
  "Wait up to SECONDS while letting timers and processes run.
`sit-for' does not drive timers in batch mode, so tests wait explicitly."
  (let ((deadline (+ (float-time) seconds)))
    (while (< (float-time) deadline)
      (accept-process-output nil 0.05))))

(provide 'etaf-go-view-test)
;;; etaf-go-view-test.el ends here
