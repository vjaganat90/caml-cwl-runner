cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - mkdir -p d && echo x > d/f
inputs: []
outputs:
  d:
    type: Directory
    outputBinding:
      glob: d
