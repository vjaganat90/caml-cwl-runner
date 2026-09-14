cwlVersion: v1.2
class: CommandLineTool
baseCommand: [ln, -s, /etc/passwd, evil]
inputs: []
outputs:
  f:
    type: File
    outputBinding:
      glob: "*"
