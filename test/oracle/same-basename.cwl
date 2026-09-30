cwlVersion: v1.2
class: CommandLineTool
doc: Two inputs with the same basename from different directories are valid.
baseCommand: cat
inputs:
  a: {type: File, inputBinding: {position: 1}}
  b: {type: File, inputBinding: {position: 2}}
stdout: out.txt
outputs:
  out: stdout
