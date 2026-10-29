cwlVersion: v1.2
class: CommandLineTool
baseCommand: cat
stdin: $(inputs.f.path)
stdout: out.txt
inputs:
  f:
    type: File
    default: {class: File, location: data.txt}
outputs:
  out:
    type: stdout
