cwlVersion: v1.2
class: CommandLineTool
baseCommand: [ "true" ]
inputs: []
outputs:
  files:
    type: File[]
    outputBinding:
      glob: nope.txt
