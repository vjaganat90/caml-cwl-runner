cwlVersion: v1.2
class: CommandLineTool
baseCommand: [touch, foo]
inputs: []
outputs:
  f:
    type: File
    outputBinding:
      glob: foo
