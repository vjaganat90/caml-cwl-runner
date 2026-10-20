cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - mkdir d && ln -s /etc/passwd d/evil
inputs: []
outputs:
  d:
    type: Directory
    outputBinding:
      glob: d
