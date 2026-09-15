cwlVersion: v1.2
class: CommandLineTool
baseCommand: cat
stdin: $(inputs.f.path)
stdout: out.txt
inputs:
  f:
    type: File
outputs:
  out:
    type: stdout
