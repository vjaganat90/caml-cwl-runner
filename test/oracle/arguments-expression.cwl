cwlVersion: v1.2
class: CommandLineTool
doc: A string in arguments containing $(...) is an expression.
baseCommand: echo
arguments: [$(inputs.f.basename)]
inputs:
  f: File
stdout: out.txt
outputs:
  out: stdout
