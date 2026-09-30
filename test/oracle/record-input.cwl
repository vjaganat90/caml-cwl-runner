cwlVersion: v1.2
class: CommandLineTool
doc: Record fields bind like inputs.
baseCommand: echo
inputs:
  r:
    type:
      type: record
      fields:
        a: {type: string, inputBinding: {}}
stdout: out.txt
outputs:
  out: stdout
