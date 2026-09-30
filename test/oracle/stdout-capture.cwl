cwlVersion: v1.2
class: CommandLineTool
doc: Control case ccr already passes, so a harness break shows up as a regression.
baseCommand: echo
inputs:
  s: {type: string, inputBinding: {}}
stdout: out.txt
outputs:
  out: stdout
