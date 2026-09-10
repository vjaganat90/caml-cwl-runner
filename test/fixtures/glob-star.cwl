cwlVersion: v1.2
class: CommandLineTool
baseCommand: touch
arguments: [a.txt, b.txt]
inputs: []
outputs:
  files:
    type: File[]
    outputBinding:
      glob: "*.txt"
