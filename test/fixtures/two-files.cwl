cwlVersion: v1.2
class: CommandLineTool
baseCommand: cat
stdout: out.txt
inputs:
  a:
    type: File
    inputBinding: {position: 1}
  b:
    type: File
    inputBinding: {position: 2}
outputs:
  out:
    type: stdout
