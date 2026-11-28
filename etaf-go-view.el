;;; etaf-go-view.el --- ETAF board, replay and life-and-death practice -*- lexical-binding: t -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The presentation of one Go record: a vector board with wooden tones,
;; coordinate margins, move numbers and SGF marks.  Replay walks the record's
;; variations; practice checks a move against the children of the current SGF
;; node; study plays free moves on a trial branch with captures and ko.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'etaf)
(require 'etaf-ui)
(require 'etaf-go-sgf)
(require 'etaf-go-rules)
(require 'etaf-go-gtp)
(require 'etaf-go-progress)

(defvar etaf-go-appearance)
(defvar etaf-go-directory)
(defvar etaf-go-cell-lines)
(defvar etaf-go--applied-scale)
(defvar etaf-go--scale-cookie)
(defvar etaf-go-engine-program)
(defvar etaf-go-engine-arguments)
(defvar etaf-go-engine-handicap)
(defvar etaf-go-engine-size)
(defvar etaf-go-engine-timeout)
(declare-function etaf-go--open-file "etaf-go" (file &optional practice))
(declare-function etaf-go-new "etaf-go" (&optional size))
(declare-function etaf-go-play "etaf-go" (&optional size))
(declare-function etaf-go-play-local "etaf-go" (&optional size))
(declare-function etaf-go-open-file "etaf-go" (&optional file practice))
(declare-function etaf-go-problems "etaf-go" ())
(declare-function etaf-go-close "etaf-go" (&optional buffer))
(declare-function etaf-go--apply-scale "etaf-go" (scale))
(declare-function etaf-go--notify-scale-change "etaf-go" ())
(declare-function ebox-display-metrics "ebox-measure" (&optional buffer-or-name))
(declare-function ebox-buffer-mount "ebox-buffer" (buffer-or-name))

(defconst etaf-go--light-theme
  '(:ui-bg "#F2EEE5" :ui-fg "#2A2A27" :ui-muted-fg "#7A7468"
    :ui-panel-bg "#FFFDF7" :ui-panel-fg "#2A2A27" :ui-panel-border "#DDD5C7"
    :ui-border "#DDD5C7" :ui-accent "#8A5A2B" :ui-accent-fg "#FFFFFF"
    :ui-button-primary-bg "#8A5A2B" :ui-button-primary-fg "#FFFFFF"
    :ui-button-secondary-bg "#E9E0D1" :ui-button-secondary-fg "#5B4227"
    :ui-button-ghost-fg "#5E584E" :ui-disabled-bg "#ECE7DD" :ui-disabled-fg "#A49D91"
    :ui-disabled-border "#DDD5C7" :ui-divider-bg "#DDD5C7"
    :ui-info-bg "#E7EEE8" :ui-info-fg "#38604A" :ui-info-border "#CBDAD0"
    :ui-success-bg "#E5F0E4" :ui-success-fg "#35724B" :ui-success-border "#C8DDC8"
    :ui-warning-bg "#F7EFD8" :ui-warning-fg "#8B6E2E" :ui-warning-border "#E7D9B4"
    :ui-danger-bg "#F9E7E2" :ui-danger-fg "#A63B34" :ui-danger-border "#EACBC6"
    :ui-progress-track "#E1E7E2"
    :go-board "#E6C58B" :go-line "#6A4A24" :go-margin "#DDB87A"
    :go-black "#24262A" :go-white "#F8F3E7" :go-stone-edge "#B9A98C"
    :go-black-ink "#F3EEE2" :go-white-ink "#2B2C2E"
    :go-mark "#B3402F" :go-mark-light "#F2B9A6" :go-last "#C0392B"
    :go-shadow "#3A2A16" :go-focus "#2E7D6E" :go-accent "#2E7D6E")
  "Warm kaya board, ivory stones and ink marks.")

(defconst etaf-go--dark-theme
  '(:ui-bg "#1A1917" :ui-fg "#E8E3D8" :ui-muted-fg "#948C7F"
    :ui-panel-bg "#232019" :ui-panel-fg "#E8E3D8" :ui-panel-border "#38332A"
    :ui-border "#38332A" :ui-accent "#D9A05B" :ui-accent-fg "#241A10"
    :ui-button-primary-bg "#D9A05B" :ui-button-primary-fg "#241A10"
    :ui-button-secondary-bg "#3B352A" :ui-button-secondary-fg "#E3D3B6"
    :ui-button-ghost-fg "#C8BFB0" :ui-disabled-bg "#232019" :ui-disabled-fg "#6E675D"
    :ui-disabled-border "#38332A" :ui-divider-bg "#38332A"
    :ui-info-bg "#2A3A38" :ui-info-fg "#A9CEC5" :ui-info-border "#3F5854"
    :ui-success-bg "#26382C" :ui-success-fg "#A8D5B7" :ui-success-border "#3E5A48"
    :ui-warning-bg "#3E3628" :ui-warning-fg "#DEC58A" :ui-warning-border "#5C5138"
    :ui-danger-bg "#422A28" :ui-danger-fg "#EEA29A" :ui-danger-border "#67403A"
    :ui-progress-track "#38332A"
    :go-board "#3A2E20" :go-line "#A28A5E" :go-margin "#33281C"
    :go-black "#14161A" :go-white "#EDE5D3" :go-stone-edge "#7A6A4E"
    :go-black-ink "#F0E8D8" :go-white-ink "#2A2B2D"
    :go-mark "#E08A6E" :go-mark-light "#F2B9A6" :go-last "#E08A6E"
    :go-shadow "#050403" :go-focus "#7FCDB2" :go-accent "#7FCDB2")
  "Night slate board with warm stones and a jade focus ring.")

(defun etaf-go--color (token)
  "Read reactive TOKEN from this instance's scoped theme."
  (etaf-theme-token token (plist-get etaf-go--light-theme token)))

(defun etaf-go--best-cell (char-width line-height max-lines max-columns)
  "Choose the largest near-square cell within LINES and COLUMNS bounds.
CHAR-WIDTH and LINE-HEIGHT are pixels.  More lines make the board
larger, so return the most lines whose rounded columns still fit."
  (let (best)
    (dotimes (index (max 1 max-lines))
      (let* ((lines (1+ index))
             (columns (max 1 (round (/ (* lines line-height)
                                       (float char-width))))))
        (when (and (<= columns (max 1 max-columns))
                   (or (null best) (> lines (car best))))
          (setq best (list lines columns)))))
    (let* ((choice (or best (list 1 1)))
           (lines (nth 0 choice))
           (columns (nth 1 choice)))
      (list :columns columns :lines lines
            :width (* columns char-width)
            :height (* lines line-height)))))

(defconst etaf-go--row-layout-columns 86
  "Minimum window width in character columns for a side-by-side layout.")

(defconst etaf-go--font-scales '(1.0 0.9 0.8 0.75 0.7 0.65 0.6 0.55 0.5)
  "Face scales tried when fitting a board.
Ebox lays a board row out as whole text lines, so a 21-cell board in a
short window needs shorter lines before it can use two lines per cell.
Every scale is measured in the buffer; Emacs resolves a face height to a
font size, so its line height is not a plain multiple of the base one.")

(defconst etaf-go--sidebar-columns 36
  "Character columns kept for the side panel and its gap.
The panel itself is 34 columns wide; two more separate it from the board.
The application padding is counted separately.")

(defconst etaf-go--horizontal-padding 4
  "Character columns the application padding takes from the canvas.")

(defconst etaf-go--vertical-reserve 3
  "Text lines kept for the application padding and the title row.")

(defconst etaf-go--scale-slack 0.9
  "Smallest board, relative to the best fit, that keeps a larger face.
A smaller face shrinks every label in the application, so it is used only
when it draws a board at least a tenth larger than the best larger one.")

(defun etaf-go--board-dims (view metrics lines columns)
  "Return (WIDTH HEIGHT) pixels of a board drawn with LINES and COLUMNS.
VIEW fixes the board grid and METRICS the cell's character size."
  (let ((grid (+ 2 (max (etaf-go--region-height view)
                        (etaf-go--region-width view)))))
    (list (* grid columns (max 1 (plist-get metrics :char-width)))
          (* grid lines (max 1 (plist-get metrics :line-height))))))

(defun etaf-go--board-width-ch (metrics view)
  "Return the board width in character cells for METRICS and VIEW."
  (* (+ 2 (etaf-go--region-width view))
     (or (plist-get metrics :columns) 1)))

(defun etaf-go--cell-fit (view metrics max-lines width height)
  "Return (LINES COLUMNS) for the largest near-square cell that fits.
METRICS gives the character width and line height in pixels; VIEW fixes
the board grid.  WIDTH and HEIGHT are the pixels the board may use.
Ebox only allows whole-line cells, so the cell is COLUMNS character widths
wide and LINES line heights tall.  Take the roundest cell that fits and,
between cells of equal size, the one whose board is less elongated."
  (let* ((grid (+ 2 (max (etaf-go--region-height view)
                        (etaf-go--region-width view))))
         (char-width (max 1 (plist-get metrics :char-width)))
         (line-height (max 1 (plist-get metrics :line-height)))
         (limit (max 1 (min max-lines
                            (floor (/ height (float (* grid line-height)))))))
         (best nil))
    (dotimes (index limit)
      (let* ((lines (1+ index))
             (columns (max 1 (round (/ (* lines line-height)
                                       (float char-width)))))
             (board-width (* grid columns char-width))
             (board-height (* grid lines line-height))
             (short (min board-width board-height))
             (aspect (/ (float short)
                        (max board-width board-height))))
        (when (and (<= board-width width)
                   (<= board-height height)
                   (or (null best)
                       (> short (car best))
                       (and (= short (car best))
                            (> aspect (nth 1 best)))))
          (setq best (list short aspect lines columns)))))
    (and best (cddr best))))

(defun etaf-go--choose-scale (view options max-lines width height sidebar-columns)
  "Choose the largest font scale that still draws a near-best board.
OPTIONS is an alist of (SCALE . METRICS), one entry per face scale; VIEW
fixes the board grid and WIDTH and HEIGHT are the window's pixels.
SIDEBAR-COLUMNS is the width the side panel keeps in a row layout; the
application padding is added on top of it.

Return (SCALE LINES COLUMNS), or nil when no option fits.  A larger face
wins unless a smaller one draws a board at least
`etaf-go--scale-slack' as large as the best, because shrinking the face
also shrinks every other label in the buffer."
  (let ((best 0) candidates)
    (dolist (option options)
      (let* ((scale (car option))
             (metrics (cdr option))
             (char-width (max 1 (plist-get metrics :char-width)))
             (line-height (max 1 (plist-get metrics :line-height)))
             (shape (etaf-go--cell-fit
                     view metrics max-lines
                     (max 1 (- width
                               (* etaf-go--horizontal-padding char-width)
                               (* sidebar-columns char-width)))
                     (max 1 (- height
                               (* etaf-go--vertical-reserve line-height))))))
        (when shape
          (let* ((dims (etaf-go--board-dims view metrics
                                            (car shape) (cadr shape)))
                 (short (min (car dims) (cadr dims))))
            (push (list scale (car shape) (cadr shape) short) candidates)
            (setq best (max best short))))))
    (when candidates
      (let* ((limit (* etaf-go--scale-slack best))
             (kept (cl-remove-if (lambda (candidate)
                                   (< (nth 3 candidate) limit))
                                 candidates))
             (pick (car (sort kept
                              (lambda (a b)
                                (if (= (car a) (car b))
                                    (> (nth 3 a) (nth 3 b))
                                  (> (car a) (car b))))))))
        (list (car pick) (nth 1 pick) (nth 2 pick))))))

(defun etaf-go--measure-metrics ()
  "Return the current buffer's Ebox metrics, or a frame fallback.
The fallback serves callers before the runtime mounts."
  (or (ignore-errors (ebox-display-metrics (current-buffer)))
      (list :char-width (max 1 (frame-char-width))
            :line-height (max 1 (frame-char-height))
            :ascent 100)))

(defun etaf-go--scale-options ()
  "Return an alist mapping each candidate font scale to measured metrics.
The board's own remap is removed for the measurement and restored
afterwards; the application's canvas and the user's text scale stay in
effect.  Every scale is measured in the buffer because Emacs resolves a
face height to a font size, so the line height is not a plain multiple of
the base one."
  (let ((cookie etaf-go--scale-cookie))
    (unwind-protect
        (progn
          (when cookie
            (face-remap-remove-relative cookie)
            (setq etaf-go--scale-cookie nil))
          (let ((base (etaf-go--measure-metrics)))
            (mapcar
             (lambda (scale)
               (if (= scale 1.0)
                   (cons scale base)
                 (let ((probe (face-remap-add-relative
                               'default :height scale)))
                   (unwind-protect
                       (cons scale (etaf-go--measure-metrics))
                     (face-remap-remove-relative probe)))))
             etaf-go--font-scales)))
      (when cookie
        (setq etaf-go--scale-cookie
              (face-remap-add-relative 'default :height etaf-go--applied-scale))))))

(defvar etaf-go--layout-char-width nil
  "Character width in pixels that decides the row layout, or nil.
The frame's own width is used unless a test binds this.")

(defvar-local etaf-go--region nil
  "Region the mounted board shows, or nil before its first render.
The scale fit reads it so a zoomed problem can use a larger face.")

(defun etaf-go--row-layout-p (width)
  "Return non-nil when WIDTH pixels leave room for the side panel.
The decision uses the frame's own character width, not the possibly
scaled board face, so resizing the face never flips the layout."
  (>= (floor width (max 1 (or etaf-go--layout-char-width
                              (frame-char-width))))
      etaf-go--row-layout-columns))

(defun etaf-go--board-metrics (size &optional view)
  "Choose the cell shape for a SIZE board that draws the largest square.
VIEW limits the board to a local region, so its cells can be larger.
A graphical frame measures the buffer's current face, so the previous fit
chose the scale; a text frame uses whole lines."
  (let ((view (or view (etaf-go--region-full size))))
    (cond
     ((and (display-graphic-p) (image-type-available-p 'svg))
      (let* ((metrics (etaf-display-metrics))
             (char-width (max 1 (plist-get metrics :char-width)))
             (line-height (max 1 (plist-get metrics :line-height)))
             (viewport (etaf-viewport))
             (width (or (car viewport) (* 80 char-width)))
             (height (* (or (cdr viewport) 40) line-height))
             (row-layout (etaf-go--row-layout-p width))
             (forced (bound-and-true-p etaf-go-cell-lines))
             (grid (+ 2 (max (etaf-go--region-height view)
                             (etaf-go--region-width view))))
             (padding (* etaf-go--horizontal-padding char-width))
             (sidebar (* etaf-go--sidebar-columns char-width)))
        (if (integerp forced)
            ;; A forced height does not shrink to the window; a board that
            ;; no longer fits beside the panel moves it below so the full
            ;; width is available.
            (let* ((lines (max 1 (min 8 forced)))
                   (columns (max 1 (round (/ (* lines line-height)
                                             (float char-width)))))
                   (board-width (* grid columns char-width)))
              (list :columns columns :lines lines
                    :width (* columns char-width)
                    :height (* lines line-height)
                    :char-width char-width :graphic t
                    :compact (and (> board-width (- width padding sidebar))
                                  (<= board-width (- width padding)))))
          (let ((shape (etaf-go--cell-fit
                        view metrics 8
                        (max 1 (- width padding
                                  (if row-layout sidebar 0)))
                        (max 1 (- height
                                  (* etaf-go--vertical-reserve
                                     line-height))))))
            (if shape
                (list :columns (cadr shape) :lines (car shape)
                      :width (* (cadr shape) char-width)
                      :height (* (car shape) line-height)
                      :char-width char-width :graphic t
                      :compact (not row-layout))
              ;; Not even one line fits: keep the smallest cell and let the
              ;; window scroll rather than divide the board away.
              (list :columns 1 :lines 1 :width char-width
                    :height line-height :char-width char-width
                    :graphic t :compact (not row-layout)))))))
     (t
      '(:columns 2 :lines 1 :width 40 :height 40 :char-width 1
        :graphic nil)))))

(defun etaf-go--star-points (size)
  "Return the star points of a SIZE board as (ROW . COLUMN) pairs."
  (pcase size
    (9 '((2 . 2) (2 . 6) (4 . 4) (6 . 2) (6 . 6)))
    (13 '((3 . 3) (3 . 9) (6 . 6) (9 . 3) (9 . 9)))
    (19 '((3 . 3) (3 . 9) (3 . 15) (9 . 3) (9 . 9) (9 . 15)
          (15 . 3) (15 . 9) (15 . 15)))
    (_ nil)))

(defun etaf-go--palette-value (palette)
  "Return PALETTE as a plist, resolving a reactive handle."
  (if (or (etaf-ref-p palette) (etaf-computed-p palette))
      (etaf-value palette)
    palette))

(defun etaf-go--territory-background (palette territory index)
  "Return the text-mode board colour for INDEX, tinted when TERRITORY owns it."
  (let ((owner (and territory index (aref territory index))))
    (if (and owner (> owner 0))
        (etaf-ui-color-mix (plist-get palette :go-board)
                           (if (= owner etaf-go--black) "#000000" "#FFFFFF")
                           0.30)
      (plist-get palette :go-board))))

(defun etaf-go--cache-get (cache key)
  "Return KEY's artwork from CACHE, keeping recently used entries."
  (or (gethash key (car cache))
      (when-let* ((value (gethash key (cdr cache))))
        (etaf-go--cache-put cache key value))))

(defun etaf-go--cache-put (cache key value)
  "Store VALUE under KEY in CACHE and return VALUE."
  (when (>= (hash-table-count (car cache)) 1024)
    (setcdr cache (car cache))
    (setcar cache (make-hash-table :test #'equal)))
  (puthash key value (car cache)))

(defun etaf-go--region-full (size)
  "Return the full-board view (TOP LEFT HEIGHT WIDTH) for SIZE."
  (list 0 0 size size))

(defun etaf-go--region-width (region)
  "Return REGION's width in intersections."
  (nth 3 region))

(defun etaf-go--region-height (region)
  "Return REGION's height in intersections."
  (nth 2 region))

(defun etaf-go--grid-point (row column size &optional region)
  "Return (BOARD-ROW . BOARD-COLUMN) for grid ROW/COLUMN, or nil.
REGION, a (TOP LEFT HEIGHT WIDTH) view, narrows the visible board."
  (let* ((region (or region (etaf-go--region-full size)))
         (top (nth 0 region))
         (left (nth 1 region))
         (height (nth 2 region))
         (width (nth 3 region)))
    (when (and (>= row 1) (<= row height)
               (>= column 1) (<= column width))
      (cons (+ top (1- row)) (+ left (1- column))))))

(defun etaf-go--margin-label (size region row column)
  "Return the coordinate label for grid ROW/COLUMN outside the board."
  (let ((top (nth 0 region))
        (left (nth 1 region))
        (height (nth 2 region))
        (width (nth 3 region)))
    (cond
     ((and (= row 0) (>= column 1) (<= column width))
      (etaf-go--column-name (+ left (1- column))))
     ((and (= column 0) (>= row 1) (<= row height))
      (number-to-string (- size (+ top (1- row)))))
     (t nil))))

(defconst etaf-go--zoom-margin 2
  "Intersections kept around a problem when zooming.")

(defconst etaf-go--zoom-minimum 5
  "Smallest local view side, in intersections.")

(defconst etaf-go--zoom-limit 4096
  "Largest solution subtree walked when finding a problem's region.")

(defconst etaf-go--zoom-full-fraction 0.8
  "Use the full board when a problem covers this fraction of its area.")

(defun etaf-go--problem-region (path size)
  "Return a local view (TOP LEFT HEIGHT WIDTH) around the problem at PATH.
The region spans the stones in the starting position plus every move,
setup point and mark in the solution tree below it, with a two-point
margin.  A view covering most of the board is replaced by the full board."
  (let* ((root (car path))
         (start (car (last path)))
         (state (etaf-go--position size root path))
         (points nil)
         (seen (make-hash-table :test #'eq))
         (count 0))
    (dotimes (index (length (plist-get state :board)))
      (when (> (aref (plist-get state :board) index) 0)
        (push (cons (/ index size) (% index size)) points)))
    (cl-labels
        ((add (index)
           (push (cons (/ index size) (% index size)) points))
         (visit (node)
           (unless (or (gethash node seen)
                       (>= count etaf-go--zoom-limit))
             (puthash node t seen)
             (cl-incf count)
             (when-let* ((move (etaf-go--node-move node size)))
               (when (integerp (plist-get move :point))
                 (add (plist-get move :point))))
             (dolist (key '("AB" "AW" "AE"))
               (dolist (index (etaf-go--expand-points
                               size (etaf-go--prop node key)))
                 (add index)))
             (dolist (entry (etaf-go--node-marks node size))
               (add (car entry)))
             (dolist (child (etaf-go--solution-children node))
               (visit child)))))
      (visit start))
    (if (null points)
        (etaf-go--region-full size)
      (let* ((rows (mapcar #'car points))
             (cols (mapcar #'cdr points))
             (top (max 0 (- (apply #'min rows) etaf-go--zoom-margin)))
             (left (max 0 (- (apply #'min cols) etaf-go--zoom-margin)))
             (bottom (min (1- size)
                          (+ (apply #'max rows) etaf-go--zoom-margin)))
             (right (min (1- size)
                         (+ (apply #'max cols) etaf-go--zoom-margin)))
             (height (1+ (- bottom top)))
             (width (1+ (- right left))))
        (if (>= (* height width) (* etaf-go--zoom-full-fraction size size))
            (etaf-go--region-full size)
          ;; Keep small problems readable without cutting the board.
          (unless (= height size)
            (let* ((needed (max 0 (- etaf-go--zoom-minimum height)))
                   (before (min top (ceiling needed 2)))
                   (after (min (- size (+ top height)) (- needed before))))
              (setq top (- top before)
                    height (+ height before after))))
          (unless (= width size)
            (let* ((needed (max 0 (- etaf-go--zoom-minimum width)))
                   (before (min left (ceiling needed 2)))
                   (after (min (- size (+ left width)) (- needed before))))
              (setq left (- left before)
                    width (+ width before after))))
          (list top left height width))))))

(defun etaf-go--text-glyph (size row column piece star)
  "Return the text fallback glyph for a board point."
  (cond
   ((= piece etaf-go--black) "●")
   ((= piece etaf-go--white) "○")
   (star "╋")
   (t
    (let ((top (= row 0))
          (bottom (= row (1- size)))
          (left (= column 0))
          (right (= column (1- size))))
      (cond
       ((and top left) "┌")
       ((and top right) "┐")
       ((and bottom left) "└")
       ((and bottom right) "┘")
       (top "┬")
       (bottom "┴")
       (left "├")
       (right "┤")
       (t "┼"))))))

(defconst etaf-go--edit-tools
  '((black "黑" "黑子") (white "白" "白子") (erase "擦" "擦除")
    (triangle "△" "三角") (square "□" "方块") (circle "圆" "圆圈")
    (cross "×" "叉") (label "标" "标签"))
  "Tools offered in edit mode: key, short label and spoken name.")

(defconst etaf-go--mark-props
  '((triangle . "TR") (square . "SQ") (circle . "CR") (cross . "MA"))
  "SGF property written by each mark tool.")

(defun etaf-go--tool-name (tool)
  "Return the display name of edit TOOL."
  (or (caddr (assq tool etaf-go--edit-tools)) "黑子"))

(defun etaf-go--setup-coords (node size key)
  "Return NODE's setup KEY points on a SIZE board as coordinates."
  (mapcar (lambda (point) (etaf-go--sgf-coord size point))
          (etaf-go--expand-points size (etaf-go--prop node key))))

(defun etaf-go--edit-stone (node size point tool)
  "Set POINT on NODE according to stone TOOL on a SIZE board."
  (let ((coord (etaf-go--sgf-coord size point)))
    (dolist (key '("AB" "AW" "AE"))
      (let ((values (delete coord (etaf-go--setup-coords node size key))))
        (etaf-go--set-prop node key values)))
    (let ((key (pcase tool
                 ('black "AB")
                 ('white "AW")
                 ('erase "AE"))))
      (etaf-go--set-prop node key
                         (append (etaf-go--prop node key) (list coord))))))

(defun etaf-go--edit-mark (node size point tool)
  "Toggle the mark TOOL at POINT on NODE for a SIZE board."
  (let* ((key (cdr (assq tool etaf-go--mark-props)))
         (coord (etaf-go--sgf-coord size point))
         (values (etaf-go--setup-coords node size key)))
    (etaf-go--set-prop node key
                       (if (member coord values)
                           (delete coord values)
                         (append values (list coord))))))

(defun etaf-go--edit-label (node size point text)
  "Set or clear POINT's label on NODE for a SIZE board."
  (let* ((coord (etaf-go--sgf-coord size point))
         (prefix (concat coord ":"))
         (values (cl-remove-if (lambda (value)
                                 (string-prefix-p prefix value))
                               (etaf-go--prop node "LB"))))
    (etaf-go--set-prop node "LB"
                       (if (string-empty-p text)
                           values
                         (append values (list (concat prefix text)))))))

(defun etaf-go--toggle-player (node)
  "Switch NODE's player between black and white."
  (let ((next (if (equal (etaf-go--prop1 node "PL") "B") "W" "B")))
    (etaf-go--set-prop node "PL" (list next))
    next))

(defun etaf-go--edit-point (edit-tool-ref path-ref dirty-ref hint-ref
                                          size node point)
  "Apply the selected edit tool at POINT in NODE."
  (let ((tool (etaf-value edit-tool-ref))
        (name (etaf-go--point-name size point)))
    (if (etaf-go--node-move node size)
        (setf (etaf-value hint-ref)
              "当前节点含落子；按 g 回到开局，再编辑摆子和标记")
      (cond
       ((memq tool '(black white erase))
        (etaf-go--edit-stone node size point tool)
        (setf (etaf-value hint-ref)
              (format "%s：%s" (etaf-go--tool-name tool) name)))
       ((eq tool 'label)
        (let ((text (etaf-call-with-input
                     (lambda ()
                       (read-string (format "%s 标签（留空删除）: " name))))))
          (etaf-go--edit-label node size point (or text ""))
          (setf (etaf-value hint-ref)
                (if (string-empty-p (or text ""))
                    (format "%s：标签已删除" name)
                  (format "%s：标签 %s" name text)))))
       (t
        (etaf-go--edit-mark node size point tool)
        (setf (etaf-value hint-ref)
              (format "%s：%s" (etaf-go--tool-name tool) name))))
      (setf (etaf-value dirty-ref) t
            (etaf-value path-ref) (copy-sequence (etaf-value path-ref))))))

(defun etaf-go--stone-svg (palette x y radius color)
  "Draw a stone of COLOR at X/Y with RADIUS in PALETTE."
  (let ((fill (if (= color etaf-go--black)
                  (plist-get palette :go-black)
                (plist-get palette :go-white))))
    (concat
     (etaf-ui-svg-circle x (+ y (* radius 0.11)) (+ radius 0.07)
                         :fill (plist-get palette :go-shadow) :opacity 0.20)
     (etaf-ui-svg-circle x y radius
                         :fill fill :stroke (plist-get palette :go-stone-edge)
                         :stroke-width 0.9)
     (etaf-ui-svg-circle (- x (* radius 0.28)) (- y (* radius 0.30))
                         (* radius 0.20) :fill "#FFFFFF" :opacity 0.12))))

(defun etaf-go--number-svg (palette x y radius color number)
  "Draw move NUMBER on a stone of COLOR at X/Y with RADIUS."
  (let* ((size (if (>= number 100) (* radius 0.78) (* radius 1.05)))
         (ink (if (= color etaf-go--black)
                  (plist-get palette :go-black-ink)
                (plist-get palette :go-white-ink))))
    (etaf-ui-svg-text x (+ y (* size 0.35)) (number-to-string number)
                      :font-family "Helvetica, Arial, sans-serif"
                      :font-weight 600 :font-size size
                      :text-anchor "middle" :fill ink)))

(defun etaf-go--mark-svg (palette x y radius color marks)
  "Draw MARKS around X/Y with RADIUS for a stone of COLOR in PALETTE."
  (let* ((ink (if (= color etaf-go--black)
                  (plist-get palette :go-mark-light)
                (plist-get palette :go-mark)))
         (mark-radius (* radius 0.42)))
    (mapconcat
     (lambda (mark)
       (pcase (plist-get mark :type)
         ('triangle
          (etaf-ui-svg-path
           (format "M %.2f %.2f L %.2f %.2f L %.2f %.2f Z"
                   x (- y mark-radius)
                   (+ x (* mark-radius 0.87)) (+ y (* mark-radius 0.5))
                   (- x (* mark-radius 0.87)) (+ y (* mark-radius 0.5)))
           :fill "none" :stroke ink :stroke-width 1.1))
         ('square
          (etaf-ui-svg-rect (- x mark-radius) (- y mark-radius)
                            (* 2 mark-radius) (* 2 mark-radius)
                            :fill "none" :stroke ink :stroke-width 1.1))
         ('circle
          (etaf-ui-svg-circle x y mark-radius
                              :fill "none" :stroke ink :stroke-width 1.1))
         ('cross
          (concat
           (etaf-ui-svg-line (- x mark-radius) (- y mark-radius)
                             (+ x mark-radius) (+ y mark-radius)
                             :stroke ink :stroke-width 1.1)
           (etaf-ui-svg-line (- x mark-radius) (+ y mark-radius)
                             (+ x mark-radius) (- y mark-radius)
                             :stroke ink :stroke-width 1.1)))
         ('label
          (etaf-ui-svg-text x (+ y (* radius 0.32))
                            (truncate-string-to-width
                             (replace-regexp-in-string
                              "[\n\r]+" " "
                              (or (plist-get mark :label) ""))
                             3 nil nil "")
                            :font-family "Songti SC,STSong,Noto Serif CJK SC,serif"
                            :font-weight 700 :font-size (* radius 0.9)
                            :text-anchor "middle" :fill ink))
         (_ "")))
     marks)))

(defun etaf-go--margin-art (cache palette metrics size row column focused
                                  &optional region)
  "Return cached artwork for a coordinate margin cell.
REGION narrows the visible board to a local view."
  (let* ((region (or region (etaf-go--region-full size)))
         (palette (etaf-go--palette-value palette))
         (width (plist-get metrics :width))
         (height (plist-get metrics :height))
         (x0 (* column width))
         (y0 (* row height))
         (key (list 'margin palette width height size region row column focused)))
    (or (etaf-go--cache-get cache key)
        (let* ((label (etaf-go--margin-label size region row column))
               (body (concat
                      (etaf-ui-svg-rect x0 y0 width height
                                        :fill (plist-get palette :go-margin))
                      (when label
                        (etaf-ui-svg-text (+ x0 (/ width 2.0))
                                          (+ y0 (/ height 2.0) (* height 0.20))
                                          label
                                          :font-family "Helvetica, Arial, sans-serif"
                                          :font-weight 600
                                          :font-size (* height 0.56)
                                          :text-anchor "middle"
                                          :fill (plist-get palette :go-line)))
                      (when focused
                        (etaf-ui-svg-rect (+ x0 2) (+ y0 2)
                                          (max 1 (- width 4))
                                          (max 1 (- height 4))
                                          :fill "none"
                                          :stroke (plist-get palette :go-focus)
                                          :stroke-width 1
                                          :stroke-dasharray "2 2")))))
          (etaf-go--cache-put
           cache key
           (etaf-ui-svg-image body width height
                              (list x0 y0 width height)))))))

(defun etaf-go--intersection-art (cache palette metrics size row column
                                        position marks show-numbers territory
                                        focused)
  "Return cached artwork for one intersection.
TERRITORY is an ownership vector from `etaf-go--territory', or nil."
  (let* ((palette (etaf-go--palette-value palette))
         (width (plist-get metrics :width))
         (height (plist-get metrics :height))
         (board-row (1- row))
         (board-column (1- column))
         (index (+ (* board-row size) board-column))
         (board (plist-get position :board))
         (numbers (plist-get position :numbers))
         (piece (aref board index))
         (number (aref numbers index))
         (owner (and territory (aref territory index)))
         (last (eql index (plist-get position :last)))
         (point-marks (cdr (assoc index marks)))
         (star (cl-member (cons board-row board-column)
                          (etaf-go--star-points size) :test #'equal))
         (key (list 'stone palette width height size board-row board-column
                    piece number owner last point-marks show-numbers focused)))
    (or (etaf-go--cache-get cache key)
        (let* ((x0 (* column width))
               (y0 (* row height))
               (cx (+ x0 (/ width 2.0)))
               (cy (+ y0 (/ height 2.0)))
               (radius (* (min width height) 0.46))
               (line (plist-get palette :go-line))
               (thickness (if (or (= board-row 0) (= board-row (1- size))
                                  (= board-column 0) (= board-column (1- size)))
                              1.1 0.8))
               (body (concat
                      (etaf-ui-svg-rect x0 y0 width height
                                        :fill (plist-get palette :go-board))
                      (when (> board-column 0)
                        (etaf-ui-svg-line x0 cy cx cy :stroke line
                                          :stroke-width thickness :stroke-linecap "round"))
                      (when (< board-column (1- size))
                        (etaf-ui-svg-line cx cy (+ x0 width) cy :stroke line
                                          :stroke-width thickness :stroke-linecap "round"))
                      (when (> board-row 0)
                        (etaf-ui-svg-line cx y0 cx cy :stroke line
                                          :stroke-width thickness :stroke-linecap "round"))
                      (when (< board-row (1- size))
                        (etaf-ui-svg-line cx cy cx (+ y0 height) :stroke line
                                          :stroke-width thickness :stroke-linecap "round"))
                      (when star
                        (etaf-ui-svg-circle cx cy (max 1.0 (* (min width height) 0.065))
                                            :fill line))
                      (when (and (= piece 0) (integerp owner) (> owner 0))
                        (etaf-ui-svg-rect
                         (- cx (* radius 0.40)) (- cy (* radius 0.40))
                         (* radius 0.80) (* radius 0.80)
                         :fill (plist-get palette
                                          (if (= owner etaf-go--black)
                                              :go-black :go-white))
                         :stroke line :stroke-width 0.5 :opacity 0.55))
                      (when (> piece 0)
                        (etaf-go--stone-svg palette cx cy radius piece))
                      (when (and show-numbers (> number 0) (> piece 0))
                        (etaf-go--number-svg palette cx cy radius piece number))
                      (when point-marks
                        (etaf-go--mark-svg palette cx cy radius piece point-marks))
                      (when (and last (> piece 0))
                        (etaf-ui-svg-circle cx cy (max 0.5 (- radius 1.15))
                                            :fill "none"
                                            :stroke (plist-get palette :go-last)
                                            :stroke-width 1.4))
                      (when focused
                        (etaf-ui-svg-circle cx cy (* (min width height) 0.35)
                                            :fill "none"
                                            :stroke (plist-get palette :go-focus)
                                            :stroke-width 1
                                            :stroke-dasharray "2 2")))))
          (etaf-go--cache-put
           cache key
           (etaf-ui-svg-image body width height
                              (list x0 y0 width height)))))))

(etaf-define-component etaf-go--board
    (&key size position palette metrics marks show-numbers territory on-press
          view)
  "Render the Go board as a SIZE square with one coordinate margin.
VIEW, a (TOP LEFT HEIGHT WIDTH) region, shows only a local part of the
board so its cells can be larger.  TERRITORY, when non-nil, is an
ownership vector that paints the score estimate on empty points."
  :bindings ((cache (cons (make-hash-table :test #'equal)
                          (make-hash-table :test #'equal)))
             (cursor (etaf-ref nil :test #'equal))
             (region (or view (etaf-go--region-full size)))
             (view-height (etaf-go--region-height region))
             (view-width (etaf-go--region-width region)))
  :view
  (etaf-tile-grid
   :ref 'go-board :rows (+ view-height 2) :columns (+ view-width 2)
   :cell-width (plist-get metrics :columns)
   :cell-height (plist-get metrics :lines)
   :region (list :id 'go-board :label "围棋棋盘" :key "b"
                 :enter (intern (format "go-cell-%d-%d"
                                        (1+ (/ view-height 2))
                                        (1+ (/ view-width 2)))))
   :cell-ref (lambda (row column) (intern (format "go-cell-%d-%d" row column)))
   :cell-image
   (when (plist-get metrics :graphic)
     (lambda (row column)
       (let ((point (etaf-go--grid-point row column size region))
             (focused (equal (cons row column) (etaf-value cursor))))
         (if point
             (etaf-go--intersection-art
              cache palette metrics size row column
              (etaf-value position) (etaf-value marks)
              (etaf-value show-numbers) (etaf-value territory) focused)
           (etaf-go--margin-art cache palette metrics size row column
                                focused region)))))
   :cell-text
   (lambda (row column)
     (let ((point (etaf-go--grid-point row column size region)))
       (cond
        (point
         (let* ((index (+ (* (car point) size) (cdr point)))
                (piece (aref (plist-get (etaf-value position) :board) index))
                (star (cl-member point (etaf-go--star-points size) :test #'equal)))
           (etaf-go--text-glyph size (car point) (cdr point) piece star)))
        (t (or (etaf-go--margin-label size region row column) " ")))))
   :cell-background
   (lambda (row column)
     (let* ((point (etaf-go--grid-point row column size region))
            (index (and point (+ (* (car point) size) (cdr point))))
            (palette (etaf-go--palette-value palette))
            (last (and index (eql index (plist-get (etaf-value position) :last)))))
       (cond
        ((and last (not (plist-get metrics :graphic)))
         (plist-get palette :ui-warning-bg))
        ((and index (not (plist-get metrics :graphic)))
         (etaf-go--territory-background palette (etaf-value territory) index))
        (t (plist-get palette :go-board)))))
   :cell-color
   (lambda (row column)
     (let* ((point (etaf-go--grid-point row column size region))
            (index (and point (+ (* (car point) size) (cdr point))))
            (piece (and index (aref (plist-get (etaf-value position) :board) index))))
       (plist-get (etaf-go--palette-value palette)
                  (cond ((null point) :go-line)
                        ((= piece etaf-go--black) :ui-fg)
                        ((= piece etaf-go--white) :ui-fg)
                        (t :go-line)))))
   :cell-label
   (lambda (row column)
     (let ((point (etaf-go--grid-point row column size region)))
       (if point
           (let* ((index (+ (* (car point) size) (cdr point)))
                  (state (etaf-value position))
                  (piece (aref (plist-get state :board) index))
                  (number (aref (plist-get state :numbers) index))
                  (last (eql index (plist-get state :last))))
             (concat
              (etaf-go--coord-name size (car point) (cdr point))
              " · "
              (cond ((= piece etaf-go--black) "黑子")
                    ((= piece etaf-go--white) "白子")
                    (t "空点"))
              (if (> number 0) (format " · 第 %d 手" number) "")
              (if last " · 最后一手" "")
              (let ((owner (let ((territory (etaf-value territory)))
                             (and territory (aref territory index)))))
                (if (and owner (> owner 0) (= piece 0))
                    (format " · %s领地"
                            (etaf-go--color-name owner))
                  ""))
              (let ((values (cdr (assoc index (etaf-value marks)))))
                (if values " · 有标记" ""))))
         (cond
          ((and (= row 0) (= column 0)) "棋盘角")
          ((and (= row 0) (>= column 1) (<= column view-width))
           (concat "列 "
                   (etaf-go--column-name
                    (+ (nth 1 region) (1- column)))))
          ((and (= column 0) (>= row 1) (<= row view-height))
           (format "第 %d 行"
                   (- size (+ (nth 0 region) (1- row)))))
          (t "棋盘边")))))
   :on-cell-focus
   (lambda (row column)
     (setf (etaf-value cursor) (cons row column)))
   :on-cell-press
   (lambda (row column)
     (when-let* ((point (etaf-go--grid-point row column size region)))
       (funcall on-press (car point) (cdr point))))))

(defun etaf-go--mode-name (mode)
  "Return the Chinese name of MODE."
  (pcase mode
    ('play "对弈")
    ('practice "死活")
    ('study "研究")
    ('edit "编辑")
    (_ "打谱")))

(defun etaf-go--turn-text (position mode practice-color
                                    &optional edit-tool game-over engine-color)
  "Describe whose turn it is in POSITION for MODE.
EDIT-TOOL is the active edit tool in edit mode.  GAME-OVER, when
non-nil, replaces the turn line with the finished game's result.
ENGINE-COLOR, when non-nil, marks the side the engine plays."
  (let ((to-play (plist-get position :to-play)))
    (cond
     ((plist-get position :error) "棋谱回放中断")
     ((eq mode 'play)
      (or game-over
          (format "%s棋行棋 · %s"
                  (etaf-go--color-name to-play)
                  (pcase engine-color
                    ('nil "本地对弈")
                    (color (if (= color to-play)
                               "电脑" "轮到你"))))))
     ((eq mode 'practice)
      (format "轮到你 · 执%s"
              (etaf-go--color-name (or practice-color to-play))))
     ((eq mode 'edit)
      (format "编辑 · 工具：%s" (etaf-go--tool-name edit-tool)))
     ((= to-play etaf-go--black) "黑棋行棋")
     ((= to-play etaf-go--white) "白棋行棋")
     (t "—"))))

(defun etaf-go--last-line (position size)
  "Describe the previous move in POSITION on a SIZE board."
  (let ((last (plist-get position :last)))
    (when last
      (let ((color (plist-get position :last-color))
            (captured (plist-get position :last-captured)))
        (format "上一手：%s %s%s"
                (etaf-go--color-name color)
                (if (eq last 'pass) "停一手" (etaf-go--point-name size last))
                (if (and captured (> captured 0))
                    (format "，提 %d 子" captured)
                  ""))))))

(defun etaf-go--captures-line (position)
  "Return the capture counts of POSITION."
  (let ((captures (plist-get position :captures)))
    (format "提子  黑 %d · 白 %d" (car captures) (cdr captures))))

(defun etaf-go--score-line (score)
  "Return the area score summary of SCORE, or nil."
  (when score
    (let* ((margin (plist-get score :margin))
           (leader (if (>= margin 0) "黑" "白"))
           (amount (abs (float margin)))
           (head (format "数子 黑%d 白%d 贴%.1f"
                         (plist-get score :black)
                         (plist-get score :white)
                         (float (plist-get score :komi)))))
      (if (< amount 0.05)
          (concat head " · 和棋")
        (format "%s · %s胜%.1f目" head leader amount)))))

(defun etaf-go--score-result (score)
  "Return a Chinese result string for area SCORE."
  (let ((margin (plist-get score :margin)))
    (cond
     ((> margin 0.05) (format "黑胜 %.1f 目" margin))
     ((< margin -0.05) (format "白胜 %.1f 目" (- margin)))
     (t "和棋"))))

(defun etaf-go--sgf-result-score (score)
  "Return SCORE as an SGF result value such as \"B+3.5\".
SGF wants the winner letter, a plus sign and the margin; a draw is 0."
  (let ((margin (plist-get score :margin)))
    (cond
     ((> margin 0.05) (format "B+%.1f" margin))
     ((< margin -0.05) (format "W+%.1f" (- margin)))
     (t "0"))))

(defun etaf-go--sgf-result-resign (color)
  "Return the SGF result for COLOR resigning.
The opponent wins by resignation, which SGF writes as \"B+R\" or \"W+R\"."
  (if (= color etaf-go--black) "W+R" "B+R"))

(defun etaf-go--record-result (root result)
  "Store RESULT, an SGF result value, on ROOT and refresh the session info.
Only the root carries RE, as the SGF convention requires."
  (etaf-go--set-prop root "RE" (list result)))

(defun etaf-go--refresh-session-info (session)
  "Recompute SESSION's one-line description after the tree changed."
  (setf (etaf-go--session-info session)
        (etaf-go--game-info (etaf-go--session-root session))))

(defun etaf-go--path-moves (path size)
  "Return the moves on PATH as entries with their SGF prefixes."
  (let ((number 0)
        entries)
    (cl-loop for node in path
             for index from 0 do
             (let ((move (etaf-go--node-move node size)))
               (when move
                 (cl-incf number)
                 (push (list :index index :number number
                             :color (plist-get move :color)
                             :label (format "%s %s"
                                            (etaf-go--color-name (plist-get move :color))
                                            (etaf-go--point-name size (plist-get move :point)))
                             :path (cl-subseq path 0 (1+ index)))
                       entries))))
    (nreverse entries)))

(defun etaf-go--child-entries (node size)
  "Return NODE's children as variation entries."
  (when node
    (let ((index 0))
      (mapcar
       (lambda (child)
         (cl-incf index)
         (let* ((move (etaf-go--node-move child size))
                (name (etaf-go--prop1 child "N"))
                (comment (etaf-go--prop1 child "C"))
                (label (cond
                        (move
                         (format "%d. %s %s"
                                 index
                                 (etaf-go--color-name (plist-get move :color))
                                 (etaf-go--point-name size (plist-get move :point))))
                        (name (format "%d. %s" index name))
                        (comment
                         (format "%d. %s" index
                                 (truncate-string-to-width
                                  (replace-regexp-in-string
                                   "[\n\r]+" " " comment)
                                  18 nil nil "…")))
                        (t (format "%d. 注释" index)))))
           (list :index index :node child :label label)))
       (etaf-go--node-children node)))))

(defun etaf-go--record-directory ()
  "Return the configured SGF directory when it exists."
  (let ((directory (bound-and-true-p etaf-go-directory)))
    (if (and (stringp directory) (file-directory-p directory))
        directory
      default-directory)))

(defun etaf-go--advance (path-ref hint-ref)
  "Follow the first child from the current position."
  (let* ((path (etaf-value path-ref))
         (child (car (etaf-go--node-children (car (last path))))))
    (if child
        (setf (etaf-value path-ref) (append path (list child))
              (etaf-value hint-ref) nil)
      (setf (etaf-value hint-ref) "已经是最后一手"))))

(defun etaf-go--next-command (mode-ref path-ref hint-ref practice-color size)
  "Advance according to MODE without mixing study moves into the record."
  (pcase (etaf-value mode-ref)
    ('practice
     (etaf-go--practice-next path-ref hint-ref practice-color size))
    (_
     (etaf-go--advance path-ref hint-ref))))

(defun etaf-go--advance-transparent (path-ref size)
  "Step through single comment-only children before a practice move."
  (let ((guard 0))
    (while (and (< guard 256)
                (let* ((node (car (last (etaf-value path-ref))))
                       (children (etaf-go--solution-children node))
                       (child (car children)))
                  (when (and child
                             (null (cdr children))
                             (null (etaf-go--node-move child size)))
                    (setf (etaf-value path-ref)
                          (append (etaf-value path-ref) (list child)))
                    t)))
      (cl-incf guard))))

(defun etaf-go--pop-path (path-ref hint-ref)
  "Step back one SGF node."
  (let ((path (etaf-value path-ref)))
    (if (> (length path) 1)
        (setf (etaf-value path-ref) (butlast path)
              (etaf-value hint-ref) nil)
      (setf (etaf-value hint-ref) "已经是开局"))))

(defun etaf-go--practice-undo (path-ref hint-ref practice-color size)
  "Step back to the previous decision point in practice."
  (let* ((path (etaf-value path-ref))
         (color (or practice-color etaf-go--black)))
    (if (<= (length path) 1)
        (setf (etaf-value hint-ref) "已经是开局")
      (setq path (butlast path))
      (while (and (> (length path) 1)
                  (let ((move (etaf-go--node-move (car (last path)) size)))
                    (or (null move)
                        (/= (plist-get move :color) color))))
        (setq path (butlast path)))
      (setf (etaf-value path-ref) path
            (etaf-value hint-ref) "已回到上一手"))))

(defun etaf-go--undo (mode-ref path-ref dirty-ref hint-ref practice-color
                              size game-over-ref pass-count-ref play-start-ref)
  "Undo one move appropriate to the current mode."
  (pcase (etaf-value mode-ref)
    ('practice
     (etaf-go--practice-undo path-ref hint-ref practice-color size))
    ('play
     (etaf-go--play-undo path-ref dirty-ref hint-ref game-over-ref
                         pass-count-ref play-start-ref size))
    (_ (etaf-go--pop-path path-ref hint-ref))))

(defun etaf-go--reset (mode-ref path-ref dirty-ref hint-ref practice-start
                               root game-over-ref pass-count-ref play-start-ref
                               show-score-ref)
  "Return to the starting position of the current mode."
  (let ((mode (etaf-value mode-ref)))
    (pcase mode
      ('play
       (etaf-go--play-restart path-ref dirty-ref hint-ref game-over-ref
                              pass-count-ref play-start-ref show-score-ref))
      ('replay
       (setf (etaf-value path-ref) (list root)))
      (_
       (setf (etaf-value path-ref)
             (copy-sequence (etaf-value practice-start)))))
    (unless (eq mode 'play)
      (setf (etaf-value hint-ref) "已重置"))))

(defun etaf-go--end (path-ref hint-ref root)
  "Follow the main line to its last node."
  (let ((path (list root))
        (guard 0))
    (while (and (< guard 2048)
                (let ((child (car (etaf-go--node-children (car (last path))))))
                  (when child
                    (setq path (append path (list child)))
                    t)))
      (cl-incf guard))
    (setf (etaf-value path-ref) path
          (etaf-value hint-ref) "已到末手")))

(defun etaf-go--auto-reply (path-ref user-color size)
  "Follow the SGF's replies until the next move of USER-COLOR.
Only branches that are not marked as failure lines are followed."
  (when user-color
    (let ((guard 0))
      (while (and (< guard 512)
                  (let* ((node (car (last (etaf-value path-ref))))
                         (children (etaf-go--practice-children node))
                         (child (car children)))
                    (cond
                     ((null child) nil)
                     ((null (etaf-go--node-move child size))
                      (setf (etaf-value path-ref)
                            (append (etaf-value path-ref) (list child)))
                      t)
                     ((/= (plist-get (etaf-go--node-move child size) :color)
                          user-color)
                      (setf (etaf-value path-ref)
                            (append (etaf-value path-ref) (list child)))
                      t)
                     (t nil))))
        (cl-incf guard)))))

(defconst etaf-go--right-comment-pattern
  "\\(正解\\|正确\\|妙手\\|好手\\|correct\\|solution\\)"
  "Words marking a variation as a correct answer.
Many problem collections keep alternative answers and failure lines as
siblings; a comment containing one of these words says which is which.
When a comment holds both kinds of word, the earlier occurrence wins, so
\"正解；否则失败\" stays a correct answer and \"失败，不是正解\" stays wrong.")

(defconst etaf-go--wrong-comment-pattern
  "\\(失败\\|错误\\|不正确\\|不行\\|不是正解\\|俗手\\|坏棋\\|无理\\|wrong\\|failure\\|incorrect\\)"
  "Words marking a variation as a failure line.")

(defun etaf-go--comment-mark (node)
  "Return `right', `wrong' or nil according to NODE's comment."
  (let* ((text (or (etaf-go--prop1 node "C") ""))
         (right (and (string-match etaf-go--right-comment-pattern text)
                     (match-beginning 0)))
         (wrong (and (string-match etaf-go--wrong-comment-pattern text)
                     (match-beginning 0))))
    (cond
     ((and right (or (null wrong) (< right wrong))) 'right)
     (wrong 'wrong)
     (t nil))))

(defun etaf-go--practice-children (node)
  "Return NODE's solution children that may count as answers.
Children marked as failure lines are dropped.  When any child is marked
as a correct answer, only those remain, so an imported set that stores
both answers and failures is judged correctly.  Without any marking the
unmarked children are kept, which still accepts several first moves."
  (let* ((children (etaf-go--solution-children node))
         (marked (cl-remove-if-not
                  (lambda (child) (eq (etaf-go--comment-mark child) 'right))
                  children)))
    (if marked
        marked
      (cl-remove-if (lambda (child) (eq (etaf-go--comment-mark child) 'wrong))
                    children))))

(defun etaf-go--practice-done-p (node)
  "Return non-nil when NODE has no answer left in practice."
  (null (etaf-go--practice-children node)))

(defun etaf-go--practice-press (path-ref hint-ref practice-color size point
                                          &optional session)
  "Check a practice move at POINT against the current SGF node.
Every answer for the same first move is accepted.  SESSION, when given,
records the attempt and the solve in the progress file."
  (etaf-go--advance-transparent path-ref size)
  (let* ((path (etaf-value path-ref))
         (node (car (last path)))
         (color (or practice-color etaf-go--black))
         (children (etaf-go--practice-children node))
         (match (cl-find-if
                 (lambda (child)
                   (let ((move (etaf-go--node-move child size)))
                     (and move
                          (= (plist-get move :color) color)
                          (eql (plist-get move :point) point))))
                 children)))
    (cond
     ((null children)
      (setf (etaf-value hint-ref) "这一支已经结束，按 r 重来"))
     (match
      (setf (etaf-value path-ref) (append path (list match)))
      (etaf-go--auto-reply path-ref color size)
      (let ((done (etaf-go--practice-done-p
                   (car (last (etaf-value path-ref))))))
        (setf (etaf-value hint-ref) (if done "正解完成" "正解！"))
        (when (and done session)
          (etaf-go--record-practice session))))
     (t
      (when session
        (etaf-go--record-practice-attempt session))
      (setf (etaf-value hint-ref)
            (format "%s 不是正解，再想想（按 m 可切到研究模式试下）"
                    (etaf-go--point-name size point)))))))

(defun etaf-go--recordable-problem-p (session)
  "Return non-nil when SESSION came from a file that can be tracked."
  (let ((file (etaf-go--session-file session)))
    (and (stringp file) (file-regular-p file))))

(defun etaf-go--record-practice (session)
  "Record that SESSION's problem was solved."
  (when (etaf-go--recordable-problem-p session)
    (ignore-errors
      (etaf-go-progress-record (etaf-go--session-file session) 'solved))))

(defun etaf-go--record-practice-attempt (session)
  "Record that a wrong move was tried on SESSION's problem."
  (when (etaf-go--recordable-problem-p session)
    (ignore-errors
      (etaf-go-progress-record (etaf-go--session-file session) 'attempts))))

(defun etaf-go--study-node (color point size)
  "Return an SGF node for a study move of COLOR at POINT on a SIZE board.
The node carries the private `EG' property so practice mode ignores it."
  (etaf-go--make-node
   :props (list (cons (if (= color etaf-go--black) "B" "W")
                      (list (etaf-go--sgf-coord size point)))
                '("EG" . ("1")))))

(defun etaf-go--study-play (path-ref dirty-ref hint-ref position size point)
  "Play a free move at POINT in study mode, recorded in the SGF tree."
  (let* ((state (etaf-value position))
         (color (plist-get state :to-play))
         (result (etaf-go--play (plist-get state :board) size color
                                point (plist-get state :ko))))
    (if (not (plist-get result :ok))
        (setf (etaf-value hint-ref) (plist-get result :reason))
      (let* ((path (etaf-value path-ref))
             (parent (car (last path)))
             (node (etaf-go--study-node color point size)))
        (setf (etaf-go--node-children parent)
              (append (etaf-go--node-children parent) (list node)))
        (setf (etaf-value path-ref) (append path (list node))
              (etaf-value dirty-ref) t
              (etaf-value hint-ref)
              (format "%s %s（已写入棋谱，u 撤销，x 删除分支）"
                      (etaf-go--color-name color)
                      (etaf-go--point-name size point)))))))

(defun etaf-go--move-node (color point size)
  "Return a plain SGF node for a played move of COLOR at POINT."
  (etaf-go--make-node
   :props (list (cons (if (= color etaf-go--black) "B" "W")
                      (list (etaf-go--sgf-coord size point))))))

(defun etaf-go--trailing-passes (path size)
  "Count consecutive pass moves at the end of PATH on a SIZE board."
  (let ((count 0)
        (nodes (reverse path)))
    (while (and nodes
                (let ((move (etaf-go--node-move (car nodes) size)))
                  (and move (eq (plist-get move :point) 'pass))))
      (cl-incf count)
      (setq nodes (cdr nodes)))
    count))

(defun etaf-go--play-press (path-ref dirty-ref hint-ref game-over-ref
                                     pass-count-ref size position point)
  "Play POINT for the side to move in a local game.
PATH-REF, DIRTY-REF and HINT-REF are the application refs.  GAME-OVER-REF
blocks moves after the result, and PASS-COUNT-REF resets on a move."
  (if (etaf-value game-over-ref)
      (setf (etaf-value hint-ref)
            "对局已结束；u 撤销上一手，或 M-x etaf-go-play 新开一局")
    (let* ((state (etaf-value position))
           (color (plist-get state :to-play))
           (result (etaf-go--play (plist-get state :board) size color
                                  point (plist-get state :ko))))
      (if (not (plist-get result :ok))
          (setf (etaf-value hint-ref) (plist-get result :reason))
        (let* ((path (etaf-value path-ref))
               (parent (car (last path)))
               (node (etaf-go--move-node color point size)))
          (setf (etaf-go--node-children parent)
                (append (etaf-go--node-children parent) (list node)))
          (setf (etaf-value path-ref) (append path (list node))
                (etaf-value dirty-ref) t
                (etaf-value pass-count-ref) 0
                (etaf-value hint-ref)
                (format "%s %s"
                        (etaf-go--color-name color)
                        (etaf-go--point-name size point))))))))

(defun etaf-go--finish-game (game-over-ref hint-ref session root result sgf-result)
  "Finish the game: record SGF-RESULT on ROOT and show RESULT.
The turn line reads GAME-OVER-REF, so both the Chinese text and the SGF
value are set together; the session description is refreshed because RE
appears in it."
  (etaf-go--record-result root sgf-result)
  (etaf-go--refresh-session-info session)
  (setf (etaf-value game-over-ref) result
        (etaf-value hint-ref) result))

(defun etaf-go--play-pass (path-ref dirty-ref hint-ref game-over-ref
                                    pass-count-ref show-score-ref
                                    size position session)
  "Pass in a local game; two consecutive passes end it."
  (if (etaf-value game-over-ref)
      (setf (etaf-value hint-ref) "对局已经结束")
    (let* ((state (etaf-value position))
           (color (plist-get state :to-play))
           (path (etaf-value path-ref))
           (parent (car (last path)))
           (node (etaf-go--move-node color 'pass size))
           (new-path (append path (list node)))
           (passes (1+ (or (etaf-value pass-count-ref) 0))))
      (setf (etaf-go--node-children parent)
            (append (etaf-go--node-children parent) (list node)))
      (setf (etaf-value path-ref) new-path
            (etaf-value dirty-ref) t
            (etaf-value pass-count-ref) passes
            (etaf-value hint-ref)
            (format "%s 停一手" (etaf-go--color-name color)))
      (when (>= passes 2)
        (let* ((root (car new-path))
               (after (etaf-go--position size root new-path))
               (score (etaf-go--score (plist-get after :board) size
                                      (etaf-go--komi root)))
               (result (etaf-go--score-result score)))
          (setf (etaf-value show-score-ref) t)
          (etaf-go--finish-game
           game-over-ref hint-ref session root
           (format "双方停一手，对局结束：%s" result)
           (etaf-go--sgf-result-score score))
          (setf (etaf-value path-ref) (copy-sequence new-path)))))))

(defun etaf-go--play-resign (hint-ref game-over-ref dirty-ref session position)
  "Resign the game for the side to move."
  (if (etaf-value game-over-ref)
      (setf (etaf-value hint-ref) "对局已经结束")
    (let* ((state (etaf-value position))
           (color (plist-get state :to-play))
           (winner (etaf-go--opponent color))
           (name (etaf-go--color-name winner)))
      (when (etaf-confirm-dialog
             (format "%s 认输，判定%s中盘胜？"
                     (etaf-go--color-name color) name))
        (setf (etaf-value dirty-ref) t)
        (etaf-go--finish-game
         game-over-ref hint-ref session
         (etaf-go--session-root session)
         (format "%s认输，%s中盘胜" (etaf-go--color-name color) name)
         (etaf-go--sgf-result-resign color))))))

(defun etaf-go--play-undo (path-ref dirty-ref hint-ref game-over-ref
                                    pass-count-ref play-start-ref size)
  "Remove the last move of the current local game."
  (let ((path (etaf-value path-ref))
        (start (etaf-value play-start-ref)))
    (if (<= (length path) (length start))
        (setf (etaf-value hint-ref) "已经是对局开始")
      (let* ((parent (nth (- (length path) 2) path))
             (node (car (last path)))
             (move (etaf-go--node-move node size))
             (new-path (butlast path)))
        (setf (etaf-go--node-children parent)
              (cl-remove-if (lambda (child) (eq child node))
                            (etaf-go--node-children parent)))
        (setf (etaf-value path-ref) new-path
              (etaf-value dirty-ref) t
              (etaf-value game-over-ref) nil
              (etaf-value pass-count-ref)
              (etaf-go--trailing-passes new-path size)
              (etaf-value hint-ref)
              (format "已撤销：%s %s"
                      (etaf-go--color-name (plist-get move :color))
                      (etaf-go--point-name size (plist-get move :point))))))))

(defun etaf-go--play-restart (path-ref dirty-ref hint-ref game-over-ref
                                       pass-count-ref play-start-ref
                                       show-score-ref)
  "Discard the moves played from PLAY-START-REF and restart the game."
  (let ((path (etaf-value path-ref))
        (start (etaf-value play-start-ref)))
    (if (<= (length path) (length start))
        (setf (etaf-value hint-ref) "已经是对局开始")
      (when (etaf-confirm-dialog "重新开始这一局？")
        (let ((parent (car (last start)))
              (child (nth (length start) path)))
          (setf (etaf-go--node-children parent)
                (cl-remove-if (lambda (candidate) (eq candidate child))
                              (etaf-go--node-children parent))))
        (setf (etaf-value path-ref) (copy-sequence start)
              (etaf-value game-over-ref) nil
              (etaf-value pass-count-ref) 0
              (etaf-value show-score-ref) nil
              (etaf-value dirty-ref) t
              (etaf-value hint-ref) "重新开始")))))

(defun etaf-go--pass-command (mode-ref path-ref dirty-ref hint-ref position size
                                       game-over-ref pass-count-ref show-score-ref
                                       session)
  "Pass in a local game or study; explain why other modes cannot."
  (pcase (etaf-value mode-ref)
    ('play
     (etaf-go--play-pass path-ref dirty-ref hint-ref game-over-ref
                         pass-count-ref show-score-ref size position session))
    ('study
     (let* ((state (etaf-value position))
            (color (plist-get state :to-play))
            (path (etaf-value path-ref))
            (parent (car (last path)))
            (node (etaf-go--study-node color 'pass size)))
       (setf (etaf-go--node-children parent)
             (append (etaf-go--node-children parent) (list node)))
       (setf (etaf-value path-ref) (append path (list node))
             (etaf-value dirty-ref) t
             (etaf-value hint-ref)
             (format "%s 停一手（已写入棋谱，u 撤销）"
                     (etaf-go--color-name color)))))
    (_
     (setf (etaf-value hint-ref) "只有对弈或研究模式可以停一手"))))

(defun etaf-go--press (mode-ref path-ref dirty-ref hint-ref practice-color
                                edit-tool game-over-ref pass-count-ref
                                size position point
                                &optional session show-score-ref)
  "Handle a board press at POINT according to the current mode.
SESSION and SHOW-SCORE-REF let a local move hand the turn to the engine."
  (pcase (etaf-value mode-ref)
    ('play
     (let ((before (length (etaf-value path-ref))))
       (etaf-go--play-press path-ref dirty-ref hint-ref game-over-ref
                            pass-count-ref size position point)
       (when (and session
                  (> (length (etaf-value path-ref)) before))
         (etaf-go--engine-schedule session mode-ref game-over-ref path-ref
                                   dirty-ref hint-ref pass-count-ref
                                   show-score-ref position 0.05))))
    ('practice
     (etaf-go--practice-press path-ref hint-ref practice-color size point
                              session))
    ('study
     (etaf-go--study-play path-ref dirty-ref hint-ref position size point))
    ('edit
     (etaf-go--edit-point edit-tool path-ref dirty-ref hint-ref
                          size (car (last (etaf-value path-ref))) point))
    (_
     (setf (etaf-value hint-ref)
           "打谱模式只读：n/p 翻页，m 进入死活练习、研究或编辑模式"))))

(defun etaf-go--practice-next (path-ref hint-ref practice-color size)
  "Play the SGF's next solution move as a hint.
When several answers exist the hint names how many there are."
  (etaf-go--advance-transparent path-ref size)
  (let* ((path (etaf-value path-ref))
         (node (car (last path)))
         (children (etaf-go--practice-children node))
         (color (or practice-color (etaf-go--solution-color node size)))
         (candidates
          (if color
              (cl-remove-if-not
               (lambda (child)
                 (let ((move (etaf-go--node-move child size)))
                   (and move (= (plist-get move :color) color))))
               children)
            children)))
    (cond
     ((null children)
      (setf (etaf-value hint-ref) "已经是终点了，按 r 重来"))
     (t
      (let* ((candidate (or (car candidates) (car children)))
             (move (etaf-go--node-move candidate size)))
        (setf (etaf-value path-ref) (append path (list candidate)))
        (when move
          (setf (etaf-value hint-ref)
                (format "提示：%s %s%s"
                        (etaf-go--color-name (plist-get move :color))
                        (etaf-go--point-name size (plist-get move :point))
                        (if (> (length candidates) 1)
                            (format "（共 %d 个正解，走其中一个）"
                                    (length candidates))
                          ""))))
        (etaf-go--auto-reply path-ref color size)
        (when (etaf-go--practice-done-p (car (last (etaf-value path-ref))))
          (setf (etaf-value hint-ref)
                (if (> (length candidates) 1)
                    "正解完成（还有别的正解）"
                  "正解完成"))))))))

(defun etaf-go--hint (mode-ref path-ref hint-ref practice-color size)
  "Give a hint, or explain that hints are a practice feature."
  (if (eq (etaf-value mode-ref) 'practice)
      (etaf-go--practice-next path-ref hint-ref practice-color size)
    (setf (etaf-value hint-ref) "提示只在死活模式下可用")))

(defun etaf-go--cycle-mode (mode-ref path-ref hint-ref
                                      practice-start-ref practice-color-ref
                                      zoom-ref position size)
  "Cycle replay, practice and study modes.
Entering practice turns on its local view; `z' toggles it."
  (let* ((current (etaf-value mode-ref))
         (next (pcase current
                 ('replay 'practice)
                 ('practice 'study)
                 (_ 'replay))))
    (when (memq next '(practice study))
      (setf (etaf-value practice-start-ref)
            (copy-sequence (etaf-value path-ref))))
    (when (eq next 'practice)
      (setf (etaf-value practice-color-ref)
            (or (etaf-go--solution-color
                 (car (last (etaf-value path-ref))) size)
                (plist-get (etaf-value position) :to-play)
                etaf-go--black)
            (etaf-value zoom-ref) t))
    (setf (etaf-value mode-ref) next
          (etaf-value hint-ref)
          (pcase next
            ('practice "死活练习：找出 SGF 的正解；n 提示，r 重来")
            ('study "自由研究：点击棋盘落子即写入棋谱分支；u 撤销，s 停一手")
            (_ "打谱模式：n/p 前后翻页，v 选择变化")))
    ;; Practice may zoom into a smaller grid, which can afford a larger
    ;; face; replay and study use the whole board.
    (etaf-go--notify-scale-change)))

(defun etaf-go--open-variations (mode-ref variations-open-ref variations hint-ref)
  "Open the variation chooser unless practice would reveal the answer."
  (cond
   ((eq (etaf-value mode-ref) 'practice)
    (setf (etaf-value hint-ref) "死活模式下不显示变化；按 n 获得提示"))
   ((null (etaf-value variations))
    (setf (etaf-value hint-ref) "这里没有后续变化"))
   (t
    (setf (etaf-value variations-open-ref) t))))

(defun etaf-go--choose-variation (path-ref hint-ref variations-open-ref child)
  "Follow CHILD from the current position."
  (setf (etaf-value path-ref)
        (append (etaf-value path-ref) (list child))
        (etaf-value hint-ref) nil
        (etaf-value variations-open-ref) nil))

(defun etaf-go--jump (path-ref hint-ref target)
  "Jump to TARGET, a root-first path."
  (setf (etaf-value path-ref) (copy-sequence target)
        (etaf-value hint-ref) nil))

(defun etaf-go--delete-branch (mode-ref path-ref dirty-ref hint-ref)
  "Remove the last node of the current path from the record."
  (let ((path (etaf-value path-ref)))
    (if (or (memq (etaf-value mode-ref) '(practice play))
            (<= (length path) 1))
        (setf (etaf-value hint-ref)
              (pcase (etaf-value mode-ref)
                ('practice "死活模式下不能删除分支")
                ('play "对弈模式请用 u 撤销")
                (_ "已经是开局，没有可删除的分支")))
      (let* ((parent (nth (- (length path) 2) path))
             (node (car (last path)))
             (label (etaf-go--node-name node)))
        (if (and (etaf-confirm-dialog
                  (format "删除分支%s？" (if label (concat "：" label) "")))
                 (cl-member node (etaf-go--node-children parent) :test #'eq))
            (progn
              (setf (etaf-go--node-children parent)
                    (cl-remove-if (lambda (child) (eq child node))
                                  (etaf-go--node-children parent)))
              (setf (etaf-value path-ref) (butlast path)
                    (etaf-value dirty-ref) t
                    (etaf-value hint-ref) "分支已删除"))
          (setf (etaf-value hint-ref) "已取消"))))))

(defun etaf-go--node-name (node)
  "Return NODE's description for prompts."
  (let ((props (and node (etaf-go--node-props node))))
    (cond
     ((and props (assoc "B" props))
      (concat "黑 " (or (etaf-go--prop1 node "C") "")))
     ((and props (assoc "W" props))
      (concat "白 " (or (etaf-go--prop1 node "C") "")))
     (t nil))))

(defun etaf-go--open-comment (comment-open-ref comment-ref node)
  "Open the comment editor for NODE."
  (setf (etaf-value comment-ref)
        (or (etaf-go--prop1 node "C") ""))
  (setf (etaf-value comment-open-ref) t))

(defun etaf-go--submit-comment (path-ref comment-open-ref comment-ref
                                       dirty-ref hint-ref node)
  "Write the edited comment into NODE and close the editor."
  (let ((text (etaf-value comment-ref)))
    (etaf-go--set-prop node "C" (and text
                                     (not (string-empty-p text))
                                     (list text)))
    ;; The node changed in place; replacing the path with a copy makes the
    ;; comment, marks and variation views recompute.
    (setf (etaf-value path-ref) (copy-sequence (etaf-value path-ref)))
    (setf (etaf-value comment-open-ref) nil
          (etaf-value dirty-ref) t
          (etaf-value hint-ref)
          (if (and text (not (string-empty-p text)))
              "注释已保存到当前节点" "注释已删除"))))

(defun etaf-go--write-session (session file)
  "Write SESSION's tree to FILE, atomically, and return FILE."
  (let* ((text (etaf-go--sgf-serialize (etaf-go--session-root session)))
         (temporary (make-temp-file "etaf-go"))
         (parent (file-name-directory file))
         (modes (and (file-exists-p file) (file-modes file))))
    (when (and parent (not (file-directory-p parent)))
      (make-directory parent t))
    (unwind-protect
        (progn
          (write-region text nil temporary nil 'silent)
          (when modes (set-file-modes temporary modes))
          (rename-file temporary file t))
      (when (file-exists-p temporary)
        (delete-file temporary)))
  file))

(defun etaf-go--save-command (session dirty-ref hint-ref &optional ask)
  "Save SESSION to its file, asking for a new name with ASK non-nil."
  (let* ((current (etaf-go--session-file session))
         (default (and (stringp current) (file-name-absolute-p current)
                       current))
         (known (and (not ask) default (file-exists-p default)
                     (not (file-directory-p default))))
         (file (if known
                   default
                 (etaf-call-with-input
                  (lambda ()
                    (read-file-name
                     "保存棋谱: "
                     (and default (or (file-directory-p default)
                                      (file-name-directory default)))
                     default nil))))))
    (if (and (file-exists-p file)
             (not known)
             (not (etaf-confirm-dialog (format "%s 已存在，覆盖？" file))))
        (setf (etaf-value hint-ref) "已取消保存")
      (etaf-go--write-session session file)
      (setf (etaf-go--session-file session) file)
      (setf (etaf-value dirty-ref) nil
            (etaf-value hint-ref) (format "已保存到 %s" file)))))

(defun etaf-go--maybe-close (dirty-ref)
  "Close the application, confirming unsaved changes first."
  (if (and (etaf-value dirty-ref)
           (not (etaf-confirm-dialog "有未保存的改动，丢弃并关闭？")))
      nil
    (etaf-go-close)))

(defun etaf-go--toggle-zoom (mode-ref zoom-ref hint-ref region-ref size)
  "Toggle the practice local view, or explain why it is unavailable."
  (cond
   ((not (eq (etaf-value mode-ref) 'practice))
    (setf (etaf-value hint-ref) "局部放大只用于死活模式"))
   ((not (display-graphic-p))
    (setf (etaf-value hint-ref) "局部放大需要图形界面"))
   (t
    (setf (etaf-value zoom-ref) (not (etaf-value zoom-ref)))
    (if (etaf-value zoom-ref)
        (let ((region (etaf-value region-ref)))
          (setf (etaf-value hint-ref)
                (if (equal region (etaf-go--region-full size))
                    "全盘（题目覆盖范围较大）"
                  (format "局部放大 %d×%d"
                          (etaf-go--region-width region)
                          (etaf-go--region-height region)))))
      (setf (etaf-value hint-ref) "显示全盘"))
    (etaf-go--notify-scale-change))))

(defun etaf-go--engine-gtp-color (color)
  "Return COLOR as the GTP colour word."
  (if (= color etaf-go--black) "b" "w"))

(defun etaf-go--engine-gtp-vertex (size point)
  "Return POINT's GTP vertex on a SIZE board, or `pass'."
  (if (eq point 'pass)
      "pass"
    (format "%s%d"
            (string (aref etaf-go--column-letters (% point size)))
            (- size (/ point size)))))

(defun etaf-go--engine-moves (path size)
  "Return PATH's moves as GTP (COLOUR . VERTEX) pairs."
  (let (moves)
    (dolist (node path)
      (when-let* ((move (etaf-go--node-move node size)))
        (push (cons (etaf-go--engine-gtp-color (plist-get move :color))
                    (etaf-go--engine-gtp-vertex size (plist-get move :point)))
              moves)))
    (nreverse moves)))

(defun etaf-go--engine-sync (session path)
  "Bring SESSION's engine to the position at PATH.
The engine is only replayed from the start, which keeps one client path
for every engine and avoids incremental-desync bugs."
  (etaf-go-gtp-sync-position
   (etaf-go--session-engine session)
   (etaf-go--session-size session)
   (etaf-go--komi (etaf-go--session-root session))
   (etaf-go--engine-moves path (etaf-go--session-size session))))

(defun etaf-go--engine-request-move (session path)
  "Ask SESSION's engine for the move at PATH.
Return (COLOR . POINT) where POINT may be `pass' or `resign'."
  (let* ((size (etaf-go--session-size session))
         (state (etaf-go--position size (etaf-go--session-root session) path))
         (color (plist-get state :to-play)))
    (etaf-go--engine-sync session path)
    (let* ((vertex (etaf-go-gtp-genmove
                    (etaf-go--session-engine session)
                    (etaf-go--engine-gtp-color color)))
           (point (etaf-go-gtp-vertex-index size (or vertex "pass"))))
      (cond
       ((eq point 'resign) (cons color 'resign))
       ((null point)
        (signal 'etaf-go-gtp-error
                (list (format "引擎给出了无法识别的位置：%s" vertex))))
       (t (cons color point))))))

(defun etaf-go--engine-play (session path-ref dirty-ref hint-ref game-over-ref
                                     pass-count-ref show-score-ref path)
  "Play one engine move at PATH and record it.
Return non-nil when a move was played."
  (let* ((size (etaf-go--session-size session))
         (move (etaf-go--engine-request-move session path))
         (color (car move))
         (point (cdr move)))
    (cond
     ((eq point 'resign)
      (let ((winner (etaf-go--color-name (etaf-go--opponent color))))
        (etaf-go--finish-game
         game-over-ref hint-ref session
         (etaf-go--session-root session)
         (format "%s中盘胜（引擎认输）" winner)
         (etaf-go--sgf-result-resign color)))
      t)
     (t
      (let* ((node (etaf-go--move-node color point size))
             (parent (car (last path)))
             (new-path (append path (list node))))
        (setf (etaf-go--node-children parent)
              (append (etaf-go--node-children parent) (list node)))
        (setf (etaf-value path-ref) new-path
              (etaf-value dirty-ref) t
              (etaf-value pass-count-ref)
              (if (eq point 'pass)
                  (1+ (or (etaf-value pass-count-ref) 0))
                0)
              (etaf-value hint-ref)
              (format "%s %s（%s）"
                      (etaf-go--color-name color)
                      (etaf-go--point-name size point)
                      (or (etaf-go-gtp-name (etaf-go--session-engine session))
                          "引擎")))
        (when (>= (etaf-value pass-count-ref) 2)
          (let* ((root (car new-path))
                 (after (etaf-go--position size root new-path))
                 (score (etaf-go--score (plist-get after :board) size
                                        (etaf-go--komi root)))
                 (result (etaf-go--score-result score)))
            (setf (etaf-value show-score-ref) t)
            (etaf-go--finish-game
             game-over-ref hint-ref session root
             (format "双方停一手，对局结束：%s" result)
             (etaf-go--sgf-result-score score))))
        t)))))

(defun etaf-go--engine-turn-p (session mode game-over position)
  "Return non-nil when the engine should move now."
  (and session
       (etaf-go--session-engine session)
       (eq mode 'play)
       (null game-over)
       (eql (etaf-go--session-engine-color session)
            (plist-get position :to-play))))

(defun etaf-go--maybe-engine-move (session mode game-over-ref path-ref
                                           dirty-ref hint-ref pass-count-ref
                                           show-score-ref position)
  "Let the engine move when it is its turn."
  (when (etaf-go--engine-turn-p session mode (etaf-value game-over-ref)
                                (etaf-value position))
    (let ((path (etaf-value path-ref)))
      (condition-case error
          (etaf-go--engine-play session path-ref dirty-ref hint-ref game-over-ref
                                pass-count-ref show-score-ref path)
        (etaf-go-gtp-error
         (setf (etaf-value hint-ref)
               (format "引擎出错：%s"
                       (or (cadr error) (error-message-string error)))))))))

(defun etaf-go--engine-close (session)
  "Stop SESSION's engine when it has one."
  (when (and session (etaf-go--session-engine session))
    (ignore-errors (etaf-go-gtp-stop (etaf-go--session-engine session)))
    (setf (etaf-go--session-engine session) nil
          (etaf-go--session-engine-color session) nil)))

(defun etaf-go--engine-schedule (session mode-ref game-over-ref path-ref
                                         dirty-ref hint-ref pass-count-ref
                                         show-score-ref position-ref
                                         &optional delay)
  "Arrange one engine turn after DELAY seconds.
The move runs outside ETAF's event and render context, so it may read the
application refs freely and update the SGF tree."
  (when (etaf-go--session-engine session)
    (run-at-time
     (or delay 0.05) nil
     (lambda ()
       (ignore-errors
         (let ((state (etaf-value position-ref)))
           (when (and (memq (etaf-value mode-ref) '(play))
                      (null (etaf-value game-over-ref))
                      (etaf-go--session-engine-color session)
                      (eql (etaf-go--session-engine-color session)
                           (plist-get state :to-play)))
             (etaf-go--maybe-engine-move
              session (etaf-value mode-ref) game-over-ref path-ref
              dirty-ref hint-ref pass-count-ref show-score-ref position-ref))))))))

(defun etaf-go--toggle-theme (appearance-ref)
  "Switch between the light and dark palettes."
  (setf (etaf-value appearance-ref)
        (if (eq (etaf-value appearance-ref) 'dark) 'light 'dark)))

(defun etaf-go--cycle-cell-lines (cell-lines-ref hint-ref &optional session)
  "Cycle the graphical cell height through auto, 1, 2, 3 and 4 lines.
SESSION, when given, remembers the choice and drops the fitted face scale:
a forced line height means the caller accepts scrolling for a larger cell,
so the automatic scale must not quietly shrink it."
  (let* ((values '(auto 1 2 3 4))
         (next (or (cadr (memq (etaf-value cell-lines-ref) values)) 'auto)))
    (setf (etaf-value cell-lines-ref) next
          (etaf-value hint-ref)
          (if (eq next 'auto)
              "棋盘 cell 行数：自动"
            (format "棋盘 cell 行数：%d（窗口不够高时会滚动）" next)))
    (when session
      (setf (etaf-go--session-cell-lines session) next)
      (when (integerp next)
        (etaf-go--apply-scale 1.0))
      ;; Switching to auto keeps the scale at 1.0, so ask the fit callback
      ;; to choose a scale for the current window.
      (etaf-go--notify-scale-change))))

(defun etaf-go--load (mode-ref dirty-ref)
  "Read another SGF file in a new buffer and close this one."
  (let ((file (etaf-call-with-input
               (lambda ()
                 (read-file-name "打开 SGF 棋谱: "
                                 (etaf-go--record-directory) nil t "sgf")))))
    (when (and file (file-regular-p file))
      (let ((old (current-buffer))
            (practice (eq (etaf-value mode-ref) 'practice)))
        (run-at-time
         0 nil
         (lambda ()
           (etaf-go--open-file file practice)
           (when (and (buffer-live-p old)
                      (etaf-go--session-p (etaf-app-state old))
                      (or (not (etaf-value dirty-ref))
                          (etaf-confirm-dialog
                           "当前棋谱有未保存的改动，关闭？")))
             (etaf-go-close old))))))))

(defun etaf-go--new-record (dirty-ref &optional play)
  "Read a board size and open a new record, confirming unsaved work.
With PLAY non-nil start a new local game instead of an editor record."
  (let ((size (etaf-call-with-input (lambda () (read-number "棋盘大小: " 19)))))
    (when (and (integerp size) (<= 2 size 25)
               (or (not (etaf-value dirty-ref))
                   (etaf-confirm-dialog
                    (if play "当前对局未保存，新开一局？"
                      "当前棋谱有未保存的改动，新建？"))))
      (let ((old (current-buffer)))
        (run-at-time
         0 nil
         (lambda ()
           (if play (etaf-go-play size) (etaf-go-new size))
           (when (and (buffer-live-p old)
                      (etaf-go--session-p (etaf-app-state old)))
             (etaf-go-close old))))))))

(etaf-define-component etaf-go--app (&key session)
  "Render one Go session."
  :bindings
  ((appearance
    (etaf-ref (if (eq (bound-and-true-p etaf-go-appearance) 'auto)
                  (if (eq (frame-parameter nil 'background-mode) 'dark)
                      'dark 'light)
                (or (bound-and-true-p etaf-go-appearance) 'light))))
   (theme
    (etaf-computed
     (lambda ()
       (if (eq (etaf-value appearance) 'dark)
           etaf-go--dark-theme
         etaf-go--light-theme))
     :test #'equal))
   (root (etaf-go--session-root session))
   (size (etaf-go--session-size session))
   (cell-lines (etaf-ref (or (etaf-go--session-cell-lines session)
                             etaf-go-cell-lines)))
   (path (etaf-ref (list root)))
   (mode (etaf-ref (etaf-go--session-mode session)))
   (practice-start (etaf-ref (list root)))
   (zoom (etaf-ref (eq (etaf-go--session-mode session) 'practice)))
   (region
    (etaf-computed
     (lambda ()
       (let ((region
              (if (and (etaf-value zoom)
                       (eq (etaf-value mode) 'practice)
                       (display-graphic-p))
                  (etaf-go--problem-region (etaf-value practice-start) size)
                (etaf-go--region-full size))))
         ;; The fit callback runs outside the component; remember the board
         ;; actually drawn so a local view can be scaled for its own grid.
         (setq-local etaf-go--region region)
         region))
     :test #'equal))
   (metrics
    (etaf-computed
     (lambda ()
       (let ((etaf-go-cell-lines (etaf-value cell-lines)))
         (etaf-go--board-metrics size (etaf-value region))))
     :test #'equal))
   (practice-color
    (etaf-ref (or (etaf-go--solution-color root size)
                  (etaf-go--initial-to-play root))))
   (play-start (etaf-ref (list root)))
   (game-over (etaf-ref nil))
   (pass-count (etaf-ref 0))
   (hint (etaf-ref nil))
   (dirty (etaf-ref nil))
   (show-numbers (etaf-ref nil))
   (show-score (etaf-ref nil))
   (edit-tool (etaf-ref 'black))
   (variations-open (etaf-ref nil))
   (comment-open (etaf-ref nil))
   (comment-draft (etaf-ref nil))
   (position
    (etaf-computed
     (lambda ()
       (etaf-go--position size root (etaf-value path)))
     :test #'equal))
   (engine-turn
    (etaf-computed
     (lambda ()
       (let ((state (etaf-go--position size root (etaf-value path))))
         (and (etaf-go--session-engine session)
              (eq (etaf-value mode) 'play)
              (null (etaf-value game-over))
              (eql (etaf-go--session-engine-color session)
                   (plist-get state :to-play)))))
     :test #'eq))
   (territory
    (etaf-computed
     (lambda ()
       (when (etaf-value show-score)
         (etaf-go--territory (plist-get (etaf-value position) :board) size)))
     :test #'equal))
   (score
    (etaf-computed
     (lambda ()
       (when (etaf-value show-score)
         (etaf-go--score (plist-get (etaf-value position) :board) size
                         (etaf-go--komi root))))
     :test #'equal))
   (marks
    (etaf-computed
     (lambda ()
       (etaf-go--node-marks (car (last (etaf-value path))) size))
     :test #'equal))
   (moves
    (etaf-computed
     (lambda () (etaf-go--path-moves (etaf-value path) size))
     :test #'equal))
   (variations
    (etaf-computed
     (lambda ()
       (etaf-go--child-entries (car (last (etaf-value path))) size))
     :test #'equal))
   (answers
    (etaf-computed
     (lambda ()
       (length (etaf-go--practice-children (car (last (etaf-value path))))))
     :test #'eql))
   (comment
    (etaf-computed
     (lambda ()
       (etaf-go--prop1 (car (last (etaf-value path))) "C"))
     :test #'equal))
   (node-name
    (etaf-computed
     (lambda ()
       (etaf-go--prop1 (car (last (etaf-value path))) "N"))
     :test #'equal))
   (viewport
    (etaf-computed (lambda () (etaf-viewport)) :test #'equal))
   (compact
    (etaf-computed
     (lambda ()
       (let ((metrics (etaf-value metrics)))
         (if (plist-member metrics :compact)
             (plist-get metrics :compact)
           ;; A text frame has no measured cell, so stack the panel on a
           ;; narrow window from the viewport's character columns.
           (let* ((width (car (etaf-value viewport)))
                  (char-width (or (plist-get metrics :char-width)
                                  (frame-char-width))))
             (and width
                  (< (floor width (max 1 char-width)) 78))))))
     :test #'eq)))
  :setup
  (progn
    (etaf-theme-provide theme)
    (etaf-go--engine-schedule session mode game-over path dirty hint
                              pass-count show-score position 0.3)
    nil)
  :view
  (flex :width '(vw 100) :height '(vh 100) :flex-direction 'column
    :background-color (etaf-go--color :ui-bg)
    :color (etaf-go--color :ui-fg)
    :aria-label "围棋"
    :on-dismiss (lambda () (setf (etaf-value variations-open) nil))
    :help '("对弈是本地双人：点击棋盘轮流落子，s 停一手，R 认输，u 撤销。"
            "双方连续停一手后结束，自动显示数子结果；r 重新开始这一局。"
            "打谱用 n/p 前后翻页，G 跳到末手，v 选择变化。"
            "死活练习中点击棋盘作答，n 提示下一步，答错不会落子。"
            "同一节点下多个同色同点的子节点都算正解；注释含「失败/不正确」等词的变化按失败图跳过。"
            "研究模式点击棋盘落子，分支直接写入棋谱：u 撤销，s 停一手，x 删除分支。"
            "e 进入编辑模式：先选工具再点棋盘，可摆黑/白子、擦除、加三角/方块/圆圈/叉或标签；"
            "P 切换行棋方。新建棋谱后用编辑模式摆题，再保存成 .sgf。"
            "S 显示数子法形势判断；z 在死活模式下局部放大/全盘；L 循环 cell 行数。"
            "c 编辑注释；w 保存，W 另存为；N 新建；o 打开。"
            "提子、打劫和禁着点由内置规则判断；坐标列不包含 I。")
    :keys
    (list
      (list "n"
            (lambda ()
              (etaf-go--next-command mode path hint
                                     (etaf-value practice-color) size))
            "下一手")
      (list "p"
            (lambda ()
              (etaf-go--undo mode path dirty hint
                             (etaf-value practice-color) size
                             game-over pass-count play-start))
            "上一手")
      (list "g"
            (lambda ()
              (etaf-go--reset mode path dirty hint practice-start root
                              game-over pass-count play-start show-score))
            "开局")
      (list "G"
            (lambda ()
              (if (eq (etaf-value mode) 'practice)
                  (setf (etaf-value hint) "死活模式请一手一手找；按 n 获得提示")
                (etaf-go--end path hint root)))
            "末手")
      (list "m"
            (lambda ()
              (etaf-go--cycle-mode mode path hint
                                   practice-start practice-color zoom position size))
            "切换模式")
      (list "e"
            (lambda ()
              (if (eq (etaf-value mode) 'play)
                  (setf (etaf-value hint)
                        "对弈模式不能编辑；保存后可用编辑模式打开")
                (setf (etaf-value mode) 'edit
                      (etaf-value hint)
                      "编辑模式：选工具后点击棋盘改动摆子/标记；P 行棋方；w 保存")))
            "编辑模式")
      (list "u"
            (lambda ()
              (etaf-go--undo mode path dirty hint
                             (etaf-value practice-color) size
                             game-over pass-count play-start))
            "撤销")
      (list "r"
            (lambda ()
              (etaf-go--reset mode path dirty hint practice-start root
                              game-over pass-count play-start show-score))
            "重来")
      (list "v"
            (lambda ()
              (etaf-go--open-variations mode variations-open variations hint))
            "变化")
      (list "a"
            (lambda ()
              (etaf-go--hint mode path hint
                             (etaf-value practice-color) size))
            "提示")
      (list "t"
            (lambda ()
              (setf (etaf-value show-numbers)
                    (not (etaf-value show-numbers))))
            "手数")
      (list "S"
            (lambda ()
              (setf (etaf-value show-score)
                    (not (etaf-value show-score)))
              (setf (etaf-value hint)
                    (if (etaf-value show-score)
                        "数子法形势（未终局仅供参照）；S 关闭"
                      nil)))
            "形势判断")
      (list "P"
            (lambda ()
              (if (eq (etaf-value mode) 'edit)
                  (let ((next (etaf-go--toggle-player
                               (car (last (etaf-value path))))))
                    (setf (etaf-value dirty) t
                          (etaf-value path) (copy-sequence (etaf-value path))
                          (etaf-value hint)
                          (format "行棋方：%s"
                                  (if (equal next "B") "黑" "白"))))
                (setf (etaf-value hint) "P 只在编辑模式切换行棋方")))
            "行棋方")
      (list "L"
            (lambda () (etaf-go--cycle-cell-lines cell-lines hint session))
            "棋盘 cell 行数")
      (list "z"
            (lambda ()
              (etaf-go--toggle-zoom mode zoom hint region size))
            "局部放大")
      (list "T"
            (lambda () (etaf-go--toggle-theme appearance))
            "主题")
      (list "s"
            (lambda ()
              (etaf-go--pass-command mode path dirty hint position size
                                     game-over pass-count show-score session))
            "停一手")
      (list "R"
            (lambda ()
              (etaf-go--play-resign hint game-over dirty session position))
            "认输")
      (list "c"
            (lambda ()
              (etaf-go--open-comment comment-open comment-draft
                                     (car (last (etaf-value path)))))
            "编辑注释")
      (list "x"
            (lambda ()
              (etaf-go--delete-branch mode path dirty hint))
            "删除分支")
      (list "w"
            (lambda ()
              (etaf-go--save-command session dirty hint))
            "保存")
      (list "W"
            (lambda ()
              (etaf-go--save-command session dirty hint t))
            "另存为")
      (list "o" (lambda () (etaf-go--load mode dirty)) "打开棋谱")
      (list "N"
            (lambda ()
              (etaf-go--new-record dirty
                                   (eq (etaf-value mode) 'play)))
            "新建")
      (list "<f1>" #'etaf-help "帮助")
      (list "q" (lambda () (etaf-go--maybe-close dirty)) "关闭"))
    (flex :width 'stretch :flex-grow 1 :justify-content 'center
      :align-items 'stretch
      :padding '((lh 1) (ch 2))
      (flex :width 'stretch :flex-direction (if (etaf-value compact) 'column 'row)
        (column :ref 'go-root :flex-shrink 0 :height 'stretch
          :width (list 'ch
                       (max 36
                            (etaf-go--board-width-ch
                             (etaf-value metrics)
                             (etaf-value region))))
          :item-gap '(lh 0)
          (flex :width 'stretch :justify-content 'space-between
            :align-items 'center
            (etaf-label :text (etaf-go--session-title session) :variant 'strong)
            (text :color (etaf-go--color :ui-muted-fg)
              (expr (format "%s · %d 手%s"
                            (etaf-go--mode-name (etaf-value mode))
                            (plist-get (etaf-value position) :move-count)
                            (if (etaf-value dirty) " · 未保存" "")))))
          (etaf-alert :if (plist-get (etaf-value position) :error)
            :title "棋谱回放中断"
            :message (plist-get (etaf-value position) :error)
            :variant 'danger)
          (flex :width 'stretch :justify-content 'center
            (etaf-go--board :size size :position position :palette theme
              :metrics (etaf-value metrics) :marks marks
              :show-numbers show-numbers
              :territory territory
              :view (etaf-value region)
              :on-press
              (lambda (row column)
                (etaf-go--press mode path dirty hint
                                (etaf-value practice-color)
                                edit-tool game-over pass-count
                                size position (+ (* row size) column)
                                session show-score))))
          (flex :if (eq (etaf-value mode) 'edit)
            :width 'stretch :justify-content 'center
            (etaf-button :for (entry etaf-go--edit-tools)
              :key (car entry)
              :ref (intern (format "go-tool-%s" (car entry)))
              :label (cadr entry)
              :aria-label (caddr entry)
              :variant (if (eq (car entry) (etaf-value edit-tool))
                           'secondary 'ghost)
              :aria-selected (eq (car entry) (etaf-value edit-tool))
              :on-press
              (lambda ()
                (setf (etaf-value edit-tool) (car entry)
                      (etaf-value hint)
                      (format "工具：%s" (caddr entry)))))
            (etaf-button :label "行" :aria-label "行棋方"
              :ref 'go-player :variant 'ghost
              :on-press
              (lambda ()
                (let ((next (etaf-go--toggle-player
                             (car (last (etaf-value path))))))
                  (setf (etaf-value dirty) t
                        (etaf-value path) (copy-sequence (etaf-value path))
                        (etaf-value hint)
                        (format "行棋方：%s"
                                (if (equal next "B") "黑" "白")))))))
          (flex :width 'stretch :justify-content 'center
            (text :color (etaf-go--color :ui-accent)
              (expr (or (etaf-value hint)
                        (etaf-go--last-line (etaf-value position) size)
                        (pcase (etaf-value mode)
                          ('play "轮流落子 · s 停一手 · R 认输 · u 撤销")
                          ('practice "点击棋盘作答 · n 提示 · r 重来")
                          ('edit "选工具后点棋盘 · P 行棋方 · w 保存")
                          (_ "n/p 翻页 · m 切换模式 · ? 帮助"))))))
          (box :flex-grow 1 :flex-basis '(lh 0) :min-height '(lh 0))
          (flex :width 'stretch :justify-content 'center
            (etaf-button
             :label (format "模式·%s" (etaf-go--mode-name (etaf-value mode)))
             :ref 'go-mode :variant 'secondary
             :on-press
             (lambda ()
               (etaf-go--cycle-mode mode path hint
                                    practice-start practice-color zoom position size)))
            (etaf-button :label "编辑" :ref 'go-edit
              :variant (if (eq (etaf-value mode) 'edit) 'secondary 'ghost)
              :aria-selected (eq (etaf-value mode) 'edit)
              :disabled (eq (etaf-value mode) 'play)
              :on-press
              (lambda ()
                (if (eq (etaf-value mode) 'edit)
                    (setf (etaf-value mode) 'replay
                          (etaf-value hint) "已回到打谱模式")
                  (setf (etaf-value mode) 'edit
                        (etaf-value hint)
                        "编辑模式：选工具后点击棋盘改动摆子/标记；P 行棋方；w 保存"))))
            (etaf-button :label "提示" :ref 'go-hint :variant 'ghost
              :disabled (not (eq (etaf-value mode) 'practice))
              :on-press
              (lambda ()
                (etaf-go--hint mode path hint
                               (etaf-value practice-color) size)))
            (etaf-button :label "手数" :ref 'go-numbers
              :variant (if (etaf-value show-numbers) 'secondary 'ghost)
              :aria-selected (etaf-value show-numbers)
              :on-press
              (lambda ()
                (setf (etaf-value show-numbers)
                      (not (etaf-value show-numbers)))))
            (etaf-button :label "主题" :ref 'go-theme :variant 'ghost
              :on-press
              (lambda () (etaf-go--toggle-theme appearance)))
            (etaf-button :label "形势" :ref 'go-score
              :variant (if (etaf-value show-score) 'secondary 'ghost)
              :aria-selected (etaf-value show-score)
              :on-press
              (lambda ()
                (setf (etaf-value show-score)
                      (not (etaf-value show-score))))))
          (flex :width 'stretch :justify-content 'center
            (etaf-button :label "开局" :ref 'go-start :variant 'ghost
              :on-press
              (lambda ()
                (etaf-go--reset mode path dirty hint practice-start root
                                game-over pass-count play-start show-score)))
            (etaf-button :label "上一手" :ref 'go-prev :variant 'ghost
              :on-press
              (lambda ()
                (etaf-go--undo mode path dirty hint
                               (etaf-value practice-color) size
                               game-over pass-count play-start)))
            (etaf-button :label "下一手" :ref 'go-next :variant 'ghost
              :on-press
              (lambda ()
                (etaf-go--next-command mode path hint
                                    (etaf-value practice-color) size)))
            (etaf-button :label "末手" :ref 'go-end :variant 'ghost
              :disabled (eq (etaf-value mode) 'practice)
              :on-press
              (lambda ()
                (etaf-go--end path hint root))))
          (flex :width 'stretch :justify-content 'center
            (etaf-button :label "注释" :ref 'go-comment :variant 'ghost
              :on-press
              (lambda ()
                (etaf-go--open-comment comment-open comment-draft
                                       (car (last (etaf-value path))))))
            (etaf-button
              :label (if (etaf-value dirty) "保存*" "保存")
              :ref 'go-save
              :variant (if (etaf-value dirty) 'secondary 'ghost)
              :on-press
              (lambda ()
                (etaf-go--save-command session dirty hint)))
            (etaf-button :label "新建" :ref 'go-new :variant 'ghost
              :on-press
              (lambda ()
                (etaf-go--new-record dirty
                                     (eq (etaf-value mode) 'play))))
            (etaf-button :label "打开" :ref 'go-open :variant 'ghost
              :on-press (lambda () (etaf-go--load mode dirty)))
            (etaf-button :label "停一手" :ref 'go-pass :variant 'ghost
              :disabled (not (memq (etaf-value mode) '(play study)))
              :on-press
              (lambda ()
                (etaf-go--pass-command mode path dirty hint position size
                                       game-over pass-count show-score session)))
            (etaf-button :label "帮助" :ref 'go-help :variant 'ghost
              :on-press #'etaf-help)
            (etaf-button :label "关闭" :ref 'go-close :variant 'ghost
              :on-press (lambda () (etaf-go--maybe-close dirty)))))
        (box :if (not (etaf-value compact))
          :width '(ch 2) :flex-shrink 0)
        (column :width (if (etaf-value compact) 'stretch '(ch 34))
          :flex-grow (if (etaf-value compact) 0 1)
          :flex-shrink 0
          :height (if (etaf-value compact) nil 'stretch)
          :item-gap '(lh 1)
          (etaf-label :text "对局" :variant 'muted)
          (text :font-weight 'bold
            (expr (etaf-go--turn-text (etaf-value position)
                                      (etaf-value mode)
                                      (etaf-value practice-color)
                                      (etaf-value edit-tool)
                                      (etaf-value game-over)
                                      (etaf-go--session-engine-color session))))
          (etaf-button :if (and (eq (etaf-value mode) 'play)
                                (not (etaf-value game-over)))
            :label "认输" :ref 'go-resign :variant 'ghost
            :on-press
            (lambda ()
              (etaf-go--play-resign hint game-over dirty session position)))
          (etaf-label :if (not (string-empty-p (etaf-go--session-info session)))
            :text (truncate-string-to-width
                   (etaf-go--session-info session) 32 nil nil "…")
            :variant 'muted)
          (etaf-label :text (etaf-go--captures-line (etaf-value position))
            :variant 'muted)
          (etaf-label :if (etaf-value score)
            :text (or (etaf-go--score-line (etaf-value score)) "")
            :variant 'muted)
          (etaf-label :if (and (eq (etaf-value mode) 'practice)
                               (> (etaf-value answers) 1))
            :text (format "此处有 %d 个正解" (etaf-value answers))
            :variant 'muted)
          (etaf-label
           :text (if (eq (etaf-value mode) 'practice)
                     (format "第 %d 手" (plist-get (etaf-value position) :move-count))
                   (format "第 %d / %d 手"
                           (plist-get (etaf-value position) :move-count)
                           (max (etaf-go--session-main-count session)
                                (plist-get (etaf-value position) :move-count))))
           :variant 'muted)
          (etaf-button :label (format "行数·%s" (etaf-value cell-lines))
            :ref 'go-cell-lines :variant 'ghost
            :help-echo "循环切换棋盘 cell 行数（auto/1/2/3/4）"
            :on-press
            (lambda () (etaf-go--cycle-cell-lines cell-lines hint session)))
          (etaf-label :if (and (eq (etaf-value mode) 'practice)
                               (not (equal (etaf-value region)
                                           (etaf-go--region-full size))))
            :text (format "局部 %d×%d"
                          (etaf-go--region-width (etaf-value region))
                          (etaf-go--region-height (etaf-value region)))
            :variant 'muted)
          (etaf-button :if (eq (etaf-value mode) 'practice)
            :label (if (equal (etaf-value region) (etaf-go--region-full size))
                       "局部放大" "显示全盘")
            :ref 'go-zoom :variant 'ghost
            :disabled (not (display-graphic-p))
            :help-echo "局部放大 / 全盘（z）"
            :on-press
            (lambda () (etaf-go--toggle-zoom mode zoom hint region size)))
          (etaf-label :text "注释" :variant 'muted)
          (column :height '(lh 7) :overflow 'scroll :width 'stretch
            (etaf-label :if (etaf-value node-name)
              :text (etaf-value node-name) :variant 'strong)
            (text :color (etaf-go--color :ui-muted-fg)
              (expr (or (etaf-value comment) "（无注释）"))))
          (flex :width 'stretch :justify-content 'space-between
            :align-items 'center
            (etaf-label :text "走法" :variant 'muted)
            (etaf-button :label "变化" :ref 'go-variations :variant 'ghost
              :disabled (or (eq (etaf-value mode) 'practice)
                            (null (etaf-value variations)))
              :on-press
              (lambda ()
                (etaf-go--open-variations mode variations-open
                                          variations hint))))
          (column :height '(lh 10) :flex-grow 1 :overflow 'scroll
            :width 'stretch :item-gap '(lh 0)
            (etaf-button :for (entry (etaf-value moves))
              :key (plist-get entry :index)
              :label (format "%3d  %s"
                             (plist-get entry :number)
                             (plist-get entry :label))
              :variant (if (= (plist-get entry :index)
                              (1- (length (etaf-value path))))
                           'secondary 'ghost)
              :on-press
              (lambda ()
                (etaf-go--jump path hint (plist-get entry :path)))))
          (etaf-label :if (null (etaf-value moves))
            :text "还没有落子。" :variant 'muted))))
    (etaf-dialog :open variations-open :title "选择变化"
      :width '(ch 48) :height '(lh 16) :compact t :close-label "关闭"
      :on-close (lambda () (setf (etaf-value variations-open) nil))
      (column :height '(lh 12) :overflow 'scroll :width 'stretch
        :item-gap '(lh 0)
        (etaf-button :for (entry (etaf-value variations))
          :key (plist-get entry :index)
          :label (plist-get entry :label) :variant 'ghost
          :on-press
          (lambda ()
            (etaf-go--choose-variation path hint variations-open
                                       (plist-get entry :node))))))
    (etaf-dialog :open comment-open :title "编辑注释"
      :width '(ch 48) :height '(lh 12) :compact t :close-label "取消"
      :on-close (lambda () (setf (etaf-value comment-open) nil))
      (column :width 'stretch :item-gap '(lh 1)
        (etaf-text-input :bind comment-draft :label "注释" :rows 6
          :width 'stretch :ref 'go-comment-input :submit-on-return nil)
        (flex :width 'stretch :justify-content 'flex-end
          (etaf-button :label "保存注释" :ref 'go-comment-save
            :variant 'secondary
            :on-press
            (lambda ()
              (etaf-go--submit-comment path comment-open comment-draft
                                       dirty hint
                                       (car (last (etaf-value path)))))))))))

(defun etaf-go--home-close ()
  "Close the main menu."
  (interactive)
  (etaf-app-close (current-buffer)))

(defun etaf-go--home-run (function)
  "Run FUNCTION from the main menu, then close the menu.
FUNCTION runs just after the event so it may read from the minibuffer and
replace application buffers without closing one inside ETAF's event drain."
  (let ((home (current-buffer)))
    (run-at-time
     0 nil
     (lambda ()
       (funcall function)
       (when (and (buffer-live-p home)
                  (eq (etaf-app-state home) 'home))
         (etaf-app-close home))))))

(defun etaf-go--home-open ()
  "Open or raise the Go main menu and return its buffer."
  (let* ((existing (get-buffer "*Go*"))
         (name (if (and existing
                        (not (eq (etaf-app-state existing) 'home)))
                   (generate-new-buffer-name "*Go*")
                 "*Go*"))
         (buffer (get-buffer name)))
    (if (and buffer (eq (etaf-app-state buffer) 'home))
        (progn
          (pop-to-buffer buffer)
          buffer)
      (etaf-app-open name
                     (etaf-view (etaf-go--home-app))
                     :state 'home
                     :cursor nil
                     :on-fit (lambda (_window)
                               (setq-local line-spacing nil))
                     :on-close (lambda () nil))
      (get-buffer name))))

(etaf-define-component etaf-go--home-app ()
  "Render the Go main menu."
  :bindings
  ((appearance
    (etaf-ref (if (eq (bound-and-true-p etaf-go-appearance) 'auto)
                  (if (eq (frame-parameter nil 'background-mode) 'dark)
                      'dark 'light)
                (or (bound-and-true-p etaf-go-appearance) 'light))))
   (theme
    (etaf-computed
     (lambda ()
       (if (eq (etaf-value appearance) 'dark)
           etaf-go--dark-theme
         etaf-go--light-theme))
     :test #'equal)))
  :setup
  (progn (etaf-theme-provide theme) nil)
  :view
  (flex :width '(vw 100) :height '(vh 100) :flex-direction 'column
    :background-color (etaf-go--color :ui-bg)
    :color (etaf-go--color :ui-fg)
    :aria-label "围棋主界面"
    :help '("对弈是本地双人轮流落子；打谱、死活题和编辑摆题各自独立。"
            "本包不设置全局按键，请从 M-x 或你自己的配置启动。")
    :keys
    (list
      (list "q" #'etaf-go--home-close "关闭")
      (list "<f1>" #'etaf-help "帮助"))
    (box :flex-grow 1)
    (flex :justify-content 'center :align-items 'flex-start
      :padding '((lh 2) (ch 2))
      (column :width '(ch 58) :item-gap '(lh 1)
        (etaf-label :text "围棋" :variant 'strong)
        (etaf-label :text "对弈 · 打谱 · 死活题 · 编辑摆题" :variant 'muted)
        (etaf-label :text (if etaf-go-engine-program
                              (format "人机对弈（引擎：%s）"
                                      (if (stringp etaf-go-engine-program)
                                          (file-name-nondirectory
                                           etaf-go-engine-program)
                                        (or (car etaf-go-engine-program)
                                            "引擎")))
                            "本地双人对弈")
          :variant 'muted)
        (flex :width 'stretch :justify-content 'center
          (etaf-button :label "9 路" :ref 'go-home-play-9
            :on-press
            (lambda ()
              (etaf-go--home-run (lambda () (etaf-go-play 9)))))
          (etaf-button :label "13 路" :ref 'go-home-play-13
            :on-press
            (lambda ()
              (etaf-go--home-run (lambda () (etaf-go-play 13)))))
          (etaf-button :label "19 路" :ref 'go-home-play-19
            :on-press
            (lambda ()
              (etaf-go--home-run (lambda () (etaf-go-play 19))))))
        (flex :width 'stretch :justify-content 'center
          (etaf-button :label "本地双人" :ref 'go-home-play-local
            :on-press
            (lambda ()
              (etaf-go--home-run (lambda () (etaf-go-play-local 19))))))
        (etaf-label :text "棋谱与题目" :variant 'muted)
        (flex :width 'stretch :justify-content 'center
          (etaf-button :label "打开棋谱" :ref 'go-home-open
            :on-press
            (lambda () (etaf-go--home-run #'etaf-go-open-file)))
          (etaf-button :label "死活题" :ref 'go-home-problems
            :on-press
            (lambda () (etaf-go--home-run #'etaf-go-problems)))
          (etaf-button :label "新建棋谱" :ref 'go-home-new
            :on-press
            (lambda ()
              (etaf-go--home-run
               (lambda () (call-interactively #'etaf-go-new))))))
        (flex :width 'stretch :justify-content 'center
          (etaf-button :label "关闭" :ref 'go-home-close :variant 'ghost
            :on-press #'etaf-go--home-close))))
    (box :flex-grow 1)))

(provide 'etaf-go-view)
;;; etaf-go-view.el ends here
