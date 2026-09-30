cwlVersion: v1.2
class: CommandLineTool
doc: An empty array with a prefix emits nothing, not a bare prefix.
baseCommand: echo
inputs:
  xs:
    type: string[]
    inputBinding:
      prefix: -x
stdout: out.txt
outputs:
  out: stdout
