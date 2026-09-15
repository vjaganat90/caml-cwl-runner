cwlVersion: v1.2
class: CommandLineTool
baseCommand: [touch, foo.txt]
inputs: []
outputs:
  f:
    type: File
    outputBinding:
      glob: foo.txt
