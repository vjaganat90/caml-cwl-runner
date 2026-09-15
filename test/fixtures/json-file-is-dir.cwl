cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - 'mkdir d && echo "{\"f\":{\"class\":\"File\",\"path\":\"d\"}}" > cwl.output.json'
inputs: []
outputs:
  f:
    type: File
