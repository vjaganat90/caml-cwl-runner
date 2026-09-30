cwlVersion: v1.2
class: CommandLineTool
doc: A long above 2^53 keeps every digit.
baseCommand: echo
inputs:
  n: {type: long, inputBinding: {}}
stdout: out.txt
outputs:
  out: stdout
