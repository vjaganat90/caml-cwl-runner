cwlVersion: v1.2
class: CommandLineTool
doc: A staged File's path is absolute.
baseCommand: [sh, -c, 'case "$1" in /*) echo absolute;; *) echo relative;; esac', sh]
inputs:
  f: {type: File, inputBinding: {position: 1}}
stdout: out.txt
outputs:
  out: stdout
