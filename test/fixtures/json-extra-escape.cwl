cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - 'echo "{\"x\":{\"class\":\"File\",\"path\":\"../evil\"}}" > cwl.output.json'
inputs: []
outputs: []
