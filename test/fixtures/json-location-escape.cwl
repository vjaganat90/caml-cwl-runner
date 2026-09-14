cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - 'echo "{\"f\":{\"class\":\"File\",\"location\":\"file:///etc/passwd\"}}" > cwl.output.json'
inputs: []
outputs:
  f:
    type: File
