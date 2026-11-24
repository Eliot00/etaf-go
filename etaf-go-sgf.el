;;; etaf-go-sgf.el --- SGF reading and Go positions -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; A small SGF (FF[4]) reader and the immutable description of one record.
;; Nodes keep properties and child variations; a path from the root is the
;; current line.  The replay position is derived from that path with
;; `etaf-go-rules', so navigating never mutates the source tree.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'etaf-go-rules)

(cl-defstruct (etaf-go--node (:constructor etaf-go--make-node))
  "One SGF node: property alist and child variations."
  props children)

(cl-defstruct (etaf-go--session (:constructor etaf-go--make-session))
  "One SGF file opened by the Go board.
ENGINE, when non-nil, is the GTP engine playing one side; ENGINE-COLOR is
1 or 2 and ENGINE-LEVEL is the requested strength keyword."
  file root size title info main-count mode
  engine engine-color engine-level)

(defconst etaf-go--column-letters "ABCDEFGHJKLMNOPQRSTUVWXYZ"
  "Column names on a Go board, without I as is customary.")

(defun etaf-go--column-name (column)
  "Return the display name of COLUMN."
  (if (< column (length etaf-go--column-letters))
      (string (aref etaf-go--column-letters column))
    (number-to-string (1+ column))))

(defun etaf-go--coord-name (size row column)
  "Return the human coordinate of ROW/COLUMN on a SIZE board."
  (concat (etaf-go--column-name column) (number-to-string (- size row))))

(defun etaf-go--point-name (size point)
  "Return the human coordinate of POINT on a SIZE board, or a pass label."
  (if (eq point 'pass)
      "停一手"
    (etaf-go--coord-name size (/ point size) (% point size))))

(defun etaf-go--prop (node key)
  "Return NODE's values for property KEY, or nil."
  (and node (cdr (assoc key (etaf-go--node-props node)))))

(defun etaf-go--prop1 (node key)
  "Return NODE's first value for property KEY, or nil."
  (car (etaf-go--prop node key)))

(defun etaf-go--point-index (size value)
  "Return the board index for SGF point VALUE, `pass', or nil."
  (when (stringp value)
    (let ((value (downcase value)))
      (cond
       ((string-empty-p value) 'pass)
       ((and (= (length value) 2)
             (<= ?a (aref value 0) ?z)
             (<= ?a (aref value 1) ?z))
        (let ((column (- (aref value 0) ?a))
              (row (- (aref value 1) ?a)))
          (if (and (< column size) (< row size))
              (+ (* row size) column)
            'pass)))
       (t nil)))))

(defun etaf-go--expand-points (size values)
  "Expand SGF point VALUES, including compressed ranges, on a SIZE board."
  (let (points)
    (dolist (value values)
      (when (stringp value)
        (let ((value (downcase value)))
          (if (string-match "\\`\\([a-z]\\)\\([a-z]\\):\\([a-z]\\)\\([a-z]\\)\\'" value)
              (let* ((column-a (- (aref value 0) ?a))
                     (row-a (- (aref value 1) ?a))
                     (column-b (- (aref value 3) ?a))
                     (row-b (- (aref value 4) ?a)))
                (cl-loop for row from (min row-a row-b) to (max row-a row-b) do
                         (cl-loop for column from (min column-a column-b)
                                  to (max column-a column-b) do
                                  (when (and (< row size) (< column size))
                                    (push (+ (* row size) column) points)))))
            (let ((point (etaf-go--point-index size value)))
              (when (integerp point)
                (push point points)))))))
    (nreverse points)))

(defun etaf-go--node-move (node size)
  "Return NODE's move as (:color 1|2 :point INDEX|pass), or nil."
  (cond
   ((etaf-go--prop node "B")
    (list :color etaf-go--black
          :point (etaf-go--point-index size (etaf-go--prop1 node "B"))))
   ((etaf-go--prop node "W")
    (list :color etaf-go--white
          :point (etaf-go--point-index size (etaf-go--prop1 node "W"))))
   (t nil)))

(defun etaf-go--node-marks (node size)
  "Return NODE's marks as an alist of (INDEX . MARKS).
Each mark is (:type triangle|square|circle|cross|label :label STRING)."
  (when node
    (let ((table (make-hash-table :test #'eql)))
      (dolist (entry '((TR . triangle) (SQ . square) (CR . circle) (MA . cross)))
        (dolist (point (etaf-go--expand-points
                        size (etaf-go--prop node (symbol-name (car entry)))))
          (push (list :type (cdr entry)) (gethash point table))))
      (dolist (value (etaf-go--prop node "LB"))
        (when (and (stringp value) (string-match ":" value))
          (let ((point (etaf-go--point-index size (substring value 0 (match-beginning 0))))
                (label (substring value (match-end 0))))
            (when (and (integerp point) (not (string-empty-p label)))
              (push (list :type 'label :label label) (gethash point table))))))
      (let (marks)
        (maphash (lambda (point values)
                   (push (cons point (nreverse values)) marks))
                 table)
        (nreverse marks)))))

(defun etaf-go--board-size (root)
  "Return ROOT's square board size, defaulting to 19."
  (let* ((value (etaf-go--prop1 root "SZ"))
         (size (and value (string-to-number (car (split-string value ":" t))))))
    (if (and (integerp size) (<= 2 size 25)) size 19)))

(defun etaf-go--main-line (root)
  "Return ROOT's main line as a list of nodes below ROOT."
  (let ((node root) line)
    (while (let ((child (car (etaf-go--node-children node))))
             (when child
               (setq node child)
               (push child line)
               t)))
    (nreverse line)))

(defun etaf-go--main-move-count (root size)
  "Count the moves on ROOT's main line for SIZE."
  (let ((count (if (etaf-go--node-move root size) 1 0)))
    (dolist (node (etaf-go--main-line root) count)
      (when (etaf-go--node-move node size)
        (cl-incf count)))))

(defun etaf-go--initial-to-play (root)
  "Return the color ROOT says moves first."
  (pcase (etaf-go--prop1 root "PL")
    ("B" etaf-go--black)
    ("W" etaf-go--white)
    (_ etaf-go--black)))

(defun etaf-go--solution-color (node size &optional fallback)
  "Return the color of the first recorded move below NODE, or FALLBACK.
Moves added in study mode do not count."
  (let ((cursor node) color)
    (while (and (not color) (car (etaf-go--solution-children cursor)))
      (setq cursor (car (etaf-go--solution-children cursor)))
      (let ((move (etaf-go--node-move cursor size)))
        (when move
          (setq color (plist-get move :color)))))
    (or color fallback)))

(defun etaf-go--position (size root path)
  "Build the position on a SIZE board from PATH, a root-first list of nodes."
  (let* ((count (* size size))
         (board (make-vector count 0))
         (numbers (make-vector count 0))
         (captures (cons 0 0))
         (ko nil) (moves 0) (last nil) (last-color nil) (last-captured 0)
         (to-play (etaf-go--initial-to-play root))
         (error nil))
    (cl-labels
        ((apply-setup
           (node)
           (dolist (entry '((1 . "AB") (2 . "AW") (0 . "AE")))
             (dolist (point (etaf-go--expand-points size (etaf-go--prop node (cdr entry))))
               (aset board point (car entry))
               (aset numbers point 0))))
         (place
           (color point)
           (if (eq point 'pass)
               (setq ko nil last 'pass last-color color last-captured 0
                     to-play (etaf-go--opponent color))
             (let ((result (etaf-go--play board size color point ko)))
               (if (not (plist-get result :ok))
                   (progn
                     (setq error (format "第 %d 手无法落子：%s"
                                         (1+ moves) (plist-get result :reason)))
                     nil)
                 (setq board (plist-get result :board)
                       ko (plist-get result :ko)
                       last point
                       last-color color
                       last-captured (length (plist-get result :captured))
                       to-play (etaf-go--opponent color))
                 (aset numbers point (1+ moves))
                 (dolist (stone (plist-get result :captured))
                   (aset numbers stone 0))
                 (if (= color etaf-go--black)
                     (setcar captures (+ (car captures) last-captured))
                   (setcdr captures (+ (cdr captures) last-captured)))
                 t)))))
      (catch 'stop
        (dolist (node path)
          (apply-setup node)
          (when-let* ((player (etaf-go--prop1 node "PL")))
            (setq to-play (pcase player
                            ("B" etaf-go--black)
                            ("W" etaf-go--white)
                            (_ to-play))))
          (when-let* ((move (etaf-go--node-move node size)))
            (cl-incf moves)
            (unless (place (plist-get move :color) (plist-get move :point))
              (cl-decf moves)
              (throw 'stop nil)))))
      (when path
        (let ((color (etaf-go--solution-color (car (last path)) size)))
          (when color
            (setq to-play color)))))
    (list :size size :board board :numbers numbers :to-play to-play
          :move-count moves :last last :last-color last-color
          :last-captured last-captured :ko ko :captures captures :error error)))

(defun etaf-go--clean-text (value)
  "Return VALUE as a single-line string, or nil."
  (when value
    (replace-regexp-in-string "[\n\r]+" " " value)))

(defun etaf-go--game-info (root)
  "Return a one-line description of ROOT's players and result."
  (let (parts)
    (when-let* ((name (etaf-go--clean-text (etaf-go--prop1 root "PB"))))
      (push (concat "黑 " name
                    (when-let* ((rank (etaf-go--clean-text
                                       (etaf-go--prop1 root "BR"))))
                      (concat " " rank)))
            parts))
    (when-let* ((name (etaf-go--clean-text (etaf-go--prop1 root "PW"))))
      (push (concat "白 " name
                    (when-let* ((rank (etaf-go--clean-text
                                       (etaf-go--prop1 root "WR"))))
                      (concat " " rank)))
            parts))
    (when-let* ((result (etaf-go--clean-text (etaf-go--prop1 root "RE"))))
      (push result parts))
    (when-let* ((komi (etaf-go--clean-text (etaf-go--prop1 root "KM"))))
      (push (concat "贴 " komi " 目") parts))
    (when-let* ((date (etaf-go--clean-text (etaf-go--prop1 root "DT"))))
      (push date parts))
    (string-join (nreverse parts) " · ")))

(defun etaf-go--komi (root)
  "Return ROOT's komi, defaulting to 6.5."
  (let ((value (etaf-go--prop1 root "KM")))
    (if (and (stringp value) (not (string-empty-p value)))
        (string-to-number value)
      6.5)))

(defun etaf-go--session-create (file root mode)
  "Return a session for FILE whose parsed tree is ROOT and initial MODE."
  (let* ((size (etaf-go--board-size root))
         (title (or (etaf-go--clean-text (etaf-go--prop1 root "GN"))
                    (file-name-base file)
                    "围棋")))
    (etaf-go--make-session :file file :root root :size size
                           :title title
                           :info (etaf-go--game-info root)
                           :main-count (etaf-go--main-move-count root size)
                           :mode mode)))

(defun etaf-go--sgf-coord (size point)
  "Return POINT's SGF coordinate on a SIZE board, or the empty pass value."
  (if (eq point 'pass)
      ""
    (string (+ ?a (% point size)) (+ ?a (/ point size)))))

(defun etaf-go--sgf-escape (text)
  "Return TEXT with the characters SGF values must escape."
  (let ((source (or text "")))
    (if (not (string-match-p "[]\\\\]" source))
        source
      (mapconcat (lambda (character)
                   (if (memq character '(?\\ ?\]))
                       (string ?\\ character)
                     (string character)))
                 source ""))))

(defun etaf-go--sgf-serialize-node (node)
  "Return NODE and its variations as SGF without the outer parentheses."
  (concat
   ";"
   (mapconcat
    (lambda (prop)
      (concat (car prop)
              (mapconcat (lambda (value)
                           (concat "[" (etaf-go--sgf-escape value) "]"))
                         (cdr prop)
                         "")))
    (etaf-go--node-props node)
    "")
   (mapconcat
    (lambda (child)
      (concat "(" (etaf-go--sgf-serialize-node child) ")"))
    (etaf-go--node-children node)
    "")))

(defun etaf-go--sgf-serialize (root)
  "Return ROOT's game tree as SGF text."
  (concat "(" (etaf-go--sgf-serialize-node root) ")\n"))

(defun etaf-go--set-prop (node key values)
  "Set NODE's property KEY to the list VALUES, or remove it when VALUES is nil."
  (let ((props (cl-remove-if (lambda (prop) (equal (car prop) key))
                             (etaf-go--node-props node))))
    (setf (etaf-go--node-props node)
          (if values (append props (list (cons key values))) props))))

(defun etaf-go--solution-children (node)
  "Return NODE's children without moves this app added in study mode."
  (cl-remove-if (lambda (child) (etaf-go--prop child "EG"))
                (and node (etaf-go--node-children node))))

(defun etaf-go--sgf-parse (text)
  "Parse SGF TEXT and return the first game tree.
Signal `user-error' when TEXT is not well-formed."
  (setq text (replace-regexp-in-string "\r\n?" "\n" text))
  (let ((length (length text))
        (index 0)
        (trees nil))
    (cl-labels
        ((peek () (and (< index length) (aref text index)))
         (space-p (character) (memq character '(?\s ?\t ?\n ?\r ?\f)))
         (letter-p (character)
           (and character
                (or (<= ?A character ?Z) (<= ?a character ?z))))
         (skip-space ()
           (while (and (< index length) (space-p (aref text index)))
             (cl-incf index)))
         (expect (character)
           (skip-space)
           (unless (eq (peek) character)
             (user-error "SGF 第 %d 个字符处缺少 %c" (1+ index) character))
           (cl-incf index))
         (parse-value ()
           (cl-incf index)
           (let (characters)
             (while (and (< index length) (not (eq (aref text index) ?\])))
               (let ((character (aref text index)))
                 (cl-incf index)
                 (if (eq character ?\\)
                     (cond
                      ((>= index length)
                       (user-error "SGF 值在转义符后意外结束"))
                      ((memq (aref text index) '(?\n ?\r))
                       (when (eq (aref text index) ?\r)
                         (cl-incf index))
                       (when (and (< index length)
                                  (eq (aref text index) ?\n))
                         (cl-incf index)))
                      (t
                       (push (aref text index) characters)
                       (cl-incf index)))
                   (push character characters))))
             (unless (eq (peek) ?\])
               (user-error "SGF 第 %d 个字符处的值没有结束" (1+ index)))
             (cl-incf index)
             (apply #'string (nreverse characters))))
         (parse-node ()
           (expect ?\;)
           (let (props)
             (skip-space)
             (while (letter-p (peek))
               (let ((start index))
                 (while (letter-p (peek))
                   (cl-incf index))
                 (let ((key (upcase (substring text start index)))
                       values)
                   (skip-space)
                   (while (eq (peek) ?\[)
                     (push (parse-value) values))
                   (let ((values (nreverse values))
                         (existing (assoc key props)))
                     (if existing
                         (setcdr existing (append (cdr existing) values))
                       (push (cons key values) props)))))
               (skip-space))
             (etaf-go--make-node :props (nreverse props))))
         (parse-tree ()
           (expect ?\()
           (let (first last)
             (catch 'done
               (while t
                 (skip-space)
                 (let ((character (peek)))
                   (cond
                    ((null character)
                     (user-error "SGF 棋谱树没有结束"))
                    ((eq character ?\))
                     (cl-incf index)
                     (throw 'done nil))
                    ((eq character ?\;)
                     (let ((node (parse-node)))
                       (if last
                           (setf (etaf-go--node-children last)
                                 (append (etaf-go--node-children last) (list node)))
                         (setq first node))
                       (setq last node)))
                    ((eq character ?\()
                     (unless last
                       (user-error "SGF 第 %d 个字符处的变化缺少父节点" (1+ index)))
                     (let ((subtree (parse-tree)))
                       (setf (etaf-go--node-children last)
                             (append (etaf-go--node-children last) (list subtree)))))
                    (t
                     (user-error "SGF 第 %d 个字符处无法解析" (1+ index)))))))
             first)))
      (when (and (> length 0) (eq (aref text 0) #xfeff))
        (cl-incf index))
      (skip-space)
      (while (< index length)
        (cond
         ((eq (peek) ?\() (push (parse-tree) trees))
         ((space-p (peek)) (cl-incf index))
         (t (user-error "SGF 第 %d 个字符处无法解析" (1+ index)))))
      (or (cl-find-if #'identity (nreverse trees))
          (user-error "SGF 文件中没有棋谱")))))

(provide 'etaf-go-sgf)
;;; etaf-go-sgf.el ends here
