cwlVersion: v1.2
class: CommandLineTool
baseCommand: [mkdir, d]
inputs: []
outputs:
  out:
    type: File
    outputBinding:
      glob: d
