cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - "echo err >&2"
inputs: []
outputs:
  err:
    type: stderr
