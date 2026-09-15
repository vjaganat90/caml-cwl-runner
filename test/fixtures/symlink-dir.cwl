cwlVersion: v1.2
class: CommandLineTool
baseCommand: [sh, -c]
arguments:
  - mkdir d && echo x > d/a.txt && ln -s d link
inputs: []
outputs:
  f:
    type: File
    outputBinding:
      glob: "link/*"
