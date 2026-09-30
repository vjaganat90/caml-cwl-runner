cwlVersion: v1.2
class: CommandLineTool
doc: Writes its TMPDIR into the outdir, leaves a symlink from TMPDIR back to the outdir, then exits with `code`.
baseCommand:
  - sh
  - -c
  - 'printf %s "$TMPDIR" > tmpdir.txt; ln -s "$HOME" "$TMPDIR/outdir-link"; touch "$TMPDIR/scratch"; exit "$0"'
inputs:
  code: {type: int, inputBinding: {position: 1}}
outputs: []
