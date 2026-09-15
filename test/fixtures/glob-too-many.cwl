cwlVersion: v1.2
class: CommandLineTool
baseCommand: [touch, a.txt, b.txt]
inputs: []
outputs:
  out:
    type: File
    outputBinding:
      glob: "*.txt"
