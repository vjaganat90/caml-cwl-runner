cwlVersion: v1.2
class: CommandLineTool
doc: A Directory with a relative location is usable from the tool's cwd.
baseCommand: ls
inputs:
  d: {type: Directory, inputBinding: {}}
stdout: out.txt
outputs:
  out: stdout
