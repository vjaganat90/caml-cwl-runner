cwlVersion: v1.2
class: CommandLineTool
doc: A YAML job file is read as YAML 1.2, where a plain `no` is a string.
baseCommand: echo
inputs:
  s: {type: string, inputBinding: {}}
stdout: out.txt
outputs:
  out: stdout
