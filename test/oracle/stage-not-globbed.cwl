cwlVersion: v1.2
class: CommandLineTool
doc: Staged inputs are not outputs; the glob sees only what the tool wrote.
baseCommand: [touch, b.txt]
inputs:
  f: File
outputs:
  outs:
    type: File[]
    outputBinding: {glob: "*.txt"}
