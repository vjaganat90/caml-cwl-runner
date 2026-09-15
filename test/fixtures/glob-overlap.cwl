cwlVersion: v1.2
class: CommandLineTool
baseCommand: [touch, a.txt]
inputs: []
outputs:
  files:
    type: File[]
    outputBinding:
      glob: ["*.txt", "a.txt"]
