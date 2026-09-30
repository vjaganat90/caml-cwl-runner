cwlVersion: v1.2
class: CommandLineTool
doc: CWL documents are YAML 1.2, where a plain `no` is a string.
baseCommand: echo
inputs:
  s: {type: string, inputBinding: {}}
stdout: out.txt
outputs:
  out: stdout
