cwlVersion: v1.2
class: CommandLineTool
doc: A relative location in a default resolves against the tool document.
baseCommand: cat
inputs:
  f:
    type: File
    default: {class: File, location: data.txt}
    inputBinding: {}
stdout: out.txt
outputs:
  out: stdout
