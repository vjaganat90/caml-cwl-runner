cwlVersion: v1.2
class: CommandLineTool
doc: In a container, HOME is the outdir, TMPDIR exists, and Directory inputs are visible.
requirements:
  DockerRequirement:
    dockerPull: alpine:latest
baseCommand:
  - sh
  - -c
  - '[ "$HOME" = "$(pwd)" ] && echo home-ok; [ -d "$TMPDIR" ] && echo tmp-ok; ls "$1"'
  - sh
inputs:
  d: {type: Directory, inputBinding: {position: 1}}
stdout: out.txt
outputs:
  out: stdout
