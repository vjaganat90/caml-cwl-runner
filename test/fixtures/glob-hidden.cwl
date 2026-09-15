cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - touch .hid vis
inputs: []
outputs:
  files:
    type: File[]
    outputBinding:
      glob: "*"
