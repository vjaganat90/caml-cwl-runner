cwlVersion: v1.2
class: CommandLineTool
baseCommand: [touch, a.txt, b.dat]
inputs: []
outputs:
  files:
    type: File[]
    outputBinding:
      glob: [a.txt, b.dat]
