cwlVersion: v1.2
class: CommandLineTool
doc: In a container, HOME is the outdir and the working directory, and TMPDIR is a writable directory.
requirements:
  DockerRequirement:
    dockerPull: alpine:latest
baseCommand:
  - sh
  - -c
  - '[ "$HOME" = "$(pwd)" ] && echo home-ok; [ -d "$TMPDIR" ] && touch "$TMPDIR/probe" && echo tmp-ok'
inputs: []
stdout: out.txt
outputs:
  out: stdout
