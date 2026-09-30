cwlVersion: v1.2
class: CommandLineTool
doc: CWL documents are YAML 1.2, where a plain `no` is not a boolean.
baseCommand: echo
inputs:
  s: {type: string, default: no, inputBinding: {}}
stdout: out.txt
outputs:
  out: stdout
