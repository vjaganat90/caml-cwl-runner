cwlVersion: v1.2
class: CommandLineTool
doc: A staged File's location is a file URI.
baseCommand: [sh, -c, 'case "$1" in file://*) echo uri;; *) echo not-uri;; esac', sh]
inputs:
  f:
    type: File
    inputBinding: {position: 1, valueFrom: $(self.location)}
stdout: out.txt
outputs:
  out: stdout
