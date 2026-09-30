cwlVersion: v1.2
class: CommandLineTool
doc: An array input with no inputBinding contributes nothing to argv.
baseCommand: echo
inputs:
  xs: string[]
stdout: out.txt
outputs:
  out: stdout
