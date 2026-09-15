cwlVersion: v1.2
class: CommandLineTool
baseCommand: [touch, f]
inputs: []
outputs:
  out:
    type: Directory
    outputBinding:
      glob: f
