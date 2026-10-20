cwlVersion: v1.2
class: CommandLineTool
baseCommand: cat
stdout: out.txt
inputs:
  d:
    type: Directory
    inputBinding:
      valueFrom: $(self.listing[0].listing[0].path)
outputs:
  out:
    type: stdout
