cwlVersion: v1.2
class: CommandLineTool
baseCommand: cat
stdin: $(inputs.d.listing[0].listing[0].path)
stdout: out.txt
inputs:
  d: Directory
outputs:
  out:
    type: stdout
