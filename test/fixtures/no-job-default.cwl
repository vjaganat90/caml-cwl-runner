cwlVersion: v1.2
class: CommandLineTool
baseCommand: echo
inputs:
  msg:
    type: string
    default: from-default
    inputBinding: {}
stdout: out.txt
outputs:
  out: stdout
