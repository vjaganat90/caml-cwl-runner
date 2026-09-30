cwlVersion: v1.2
class: CommandLineTool
doc: In the map form of inputs, a list value is a union type.
baseCommand: echo
inputs:
  x: ["null", string]
stdout: out.txt
outputs:
  out: stdout
