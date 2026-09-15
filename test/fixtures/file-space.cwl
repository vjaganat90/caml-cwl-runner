cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - 'touch "a b.txt"'
inputs: []
outputs:
  f:
    type: File
    outputBinding:
      glob: "a b.txt"
