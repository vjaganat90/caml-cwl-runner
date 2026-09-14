cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - 'echo "{\"f\":{\"class\":\"File\",\"path\":\"../evil\"}}" > cwl.output.json'
inputs: []
outputs:
  f:
    type: File
