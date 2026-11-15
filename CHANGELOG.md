# Changelog

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
