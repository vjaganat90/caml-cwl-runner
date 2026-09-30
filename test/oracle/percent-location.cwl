cwlVersion: v1.2
class: CommandLineTool
doc: location is a URI; percent escapes decode (%61 is a).
baseCommand: cat
inputs:
  f: {type: File, inputBinding: {}}
stdout: out.txt
outputs:
  out: stdout
