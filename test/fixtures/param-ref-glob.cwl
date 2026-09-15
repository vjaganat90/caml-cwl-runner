cwlVersion: v1.2
class: CommandLineTool
baseCommand: touch
inputs:
  name:
    type: string
    inputBinding:
      position: 1
outputs:
  f:
    type: File
    outputBinding:
      glob: $(inputs.name)
