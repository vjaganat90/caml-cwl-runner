cwlVersion: v1.2
class: CommandLineTool
doc: valueFrom on an array binding replaces the items.
baseCommand: echo
inputs:
  xs:
    type: string[]
    inputBinding:
      valueFrom: $(self.length)
stdout: out.txt
outputs:
  out: stdout
