cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - 'touch a.txt && echo "{\"out\":{\"class\":\"File\",\"path\":\"a.txt\"}}" > cwl.output.json'
inputs: []
outputs:
  out:
    type: File
