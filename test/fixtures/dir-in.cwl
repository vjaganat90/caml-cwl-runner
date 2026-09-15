cwlVersion: v1.2
class: CommandLineTool
baseCommand: echo
stdout: out.txt
inputs:
  d:
    type: Directory
    inputBinding:
      position: 1
outputs:
  out:
    type: stdout
