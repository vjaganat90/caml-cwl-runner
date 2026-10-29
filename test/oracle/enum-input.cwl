cwlVersion: v1.2
class: CommandLineTool
doc: An enum input takes only its symbols.
baseCommand: echo
inputs:
  e:
    type: {type: enum, symbols: [a, b]}
    inputBinding: {}
stdout: out.txt
outputs:
  out: stdout
