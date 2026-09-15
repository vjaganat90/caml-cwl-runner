cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - mkdir d && touch a.txt
inputs: []
outputs:
  files:
    type: File[]
    outputBinding:
      glob: "*"
