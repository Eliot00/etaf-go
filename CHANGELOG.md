# Changelog

## 0.4.0

- Play against a GTP engine.  `etaf-go-gtp' is a small Go Text Protocol
  client, so KataGo, GNU Go, Leela Zero and any other GTP engine work.
  `etaf-go-play' starts an engine game when `etaf-go-engine-program' is set
  and a local two-player game otherwise; `etaf-go-play-local' always plays
  locally.  The engine moves for its side, replies after your move, resigns
  and passes, and its process is stopped when the game buffer closes.
- New user options: `etaf-go-engine-program`, `etaf-go-engine-arguments`,
  `etaf-go-engine-handicap`, `etaf-go-engine-size`, `etaf-go-engine-timeout`
  and `etaf-go-engine-komi`.
- The turn line names the engine side, and the main menu shows the engine.

## 0.3.4

- Practice mode now zooms into the problem in graphical frames: the view is
  the bounding box of the starting stones plus every move, setup point and
  mark in the solution tree, with a two-point margin and a five-intersection
  minimum.  A problem covering 80 percent of the board keeps the full view.
- `z` (or the 局部放大/显示全盘 button) toggles the local view in practice;
  the sidebar shows the visible size.  Entering practice through mode cycling
  turns it on.

## 0.3.3

- Practice accepts every recorded first move at a node, so a problem with
  several solutions no longer rejects valid answers.
- Variations whose comment contains 失败/错误/不正确/不行/不是正解/俗手/坏棋/无理
  (or wrong/failure/incorrect) are treated as failure lines instead of
  answers.  Hints and automatic replies skip them; the sidebar shows how many
  answers a node has.
- A comment containing both kinds of word is classified by the earlier
  occurrence, so "正解；否则失败" stays a correct answer.

## 0.3.2

- The application canvas fills the frame; spare width and height go to the
  side panel, comment and move list instead of empty margins, and the control
  rows sit at the bottom.
- `L` (or the 行数 button) cycles the graphical cell height through auto, 1,
  2, 3 and 4.  In a short window, two lines give a near-square 35x34 cell at
  the cost of scrolling.
- Ebox's display-metric cache is reset before fitting so the cell width and
  height are measured in the application buffer.

## 0.3.1

- The board now takes the window height above the title and padding instead
  of reserving eight control rows, so a 45-line frame gives a 19x19 board
  two-line cells (about 588x588 pixels on the default macOS metrics).
  Controls below the board may scroll in short frames.
- This also removes the worst rectangular cells: ratios such as 2.5 text
  pixels per character made one-line cells 25 percent taller than wide.

## 0.3.0

- Local two-player play: alternating turns, captures, simple ko, pass,
  resign, undo, restart, an area-score result after two passes, and saving
  the game tree as SGF.
- `etaf-go` and `etaf-go-open` now open a main menu for play, records,
  problems and authoring; `etaf-go-open-file` opens a record directly.
- `etaf-go-play` starts a local game and `etaf-go-new` still opens the
  position editor.
- Removed `etaf-go-demo` and the built-in demo command.
- The package installs no global key bindings.

## 0.2.1

- The graphical board now takes the largest near-square cell that fits the
  window instead of the smallest one, so it no longer sits at roughly half the
  frame size.  Cell selection reacts to frame resizes.
- Removed the blank line between the board column's rows and moved game info
  into the side panel, leaving height for the larger board.
- On frames where the side panel would force a smaller board, the panel moves
  below the board instead; the board keeps its size.

## 0.2.0

- Edit mode: set up black/white stones, erase, add triangle/square/circle/cross
  marks and labels, switch the player, and save the node back to SGF.
- `etaf-go-new` opens an empty board in edit mode for authoring problems.
- Area-score estimate with an on-board territory overlay and a move-list
  summary; toggle with `S`.
- SGF values containing `]` or `\` now escape and round-trip correctly.
- Repeated properties such as two `AB` groups are merged instead of dropping
  all but the first.
- Komi is read from `KM` (default 6.5) for the score estimate.

## 0.1.0

- Initial release.
- SGF (FF[4]) replay with variations, setup stones, comments, marks and move numbers.
- Life-and-death practice against the SGF solution, with hints and automatic replies.
- Free study mode with captures, simple ko, suicide prevention and pass.
- SVG board on graphical frames and a box-drawing fallback in terminals.
- Light and dark board palettes.
