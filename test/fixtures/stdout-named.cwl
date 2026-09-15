cwlVersion: v1.2
class: CommandLineTool
baseCommand: echo
arguments: [hi]
stdout: $(inputs.name)
inputs:
  name:
    type: string
outputs:
  out:
    type: stdout
