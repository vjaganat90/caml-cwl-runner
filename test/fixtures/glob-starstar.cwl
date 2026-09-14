cwlVersion: v1.2
class: CommandLineTool
baseCommand: [ln, -s, /, escape]
inputs: []
outputs:
  files:
    type:
      type: array
      items: File
    outputBinding:
      glob: "**"
