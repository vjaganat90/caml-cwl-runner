cwlVersion: v1.2
class: CommandLineTool
requirements:
  DockerRequirement:
    dockerPull: alpine
baseCommand: echo
arguments:
  - valueFrom: $(inputs.f.location)
inputs:
  f:
    type: File
    inputBinding: {}
outputs: []
