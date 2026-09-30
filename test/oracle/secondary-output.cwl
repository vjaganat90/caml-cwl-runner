cwlVersion: v1.2
class: CommandLineTool
doc: Output secondaryFiles are collected next to the primary.
baseCommand: [touch, a.txt, a.txt.idx]
inputs: []
outputs:
  f:
    type: File
    secondaryFiles: [.idx]
    outputBinding: {glob: a.txt}
