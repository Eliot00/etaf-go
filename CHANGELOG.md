# Changelog

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
