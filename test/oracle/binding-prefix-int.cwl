cwlVersion: v1.2
class: CommandLineTool
baseCommand: echo
inputs:
  x:
    type: string
    default: hi
    inputBinding:
      prefix: 5
outputs: []
