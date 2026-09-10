cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - 'echo "{\"n\": 1}" > cwl.output.json'
inputs: []
outputs:
  n:
    type: int
    outputBinding:
      glob: nope.txt
