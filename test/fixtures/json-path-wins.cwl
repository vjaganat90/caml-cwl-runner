cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - 'touch a.txt && echo "{\"f\":{\"class\":\"File\",\"path\":\"a.txt\",\"location\":\"file:///etc/passwd\",\"format\":\"http://edamontology.org/format_2330\"}}" > cwl.output.json'
inputs: []
outputs:
  f:
    type: File
