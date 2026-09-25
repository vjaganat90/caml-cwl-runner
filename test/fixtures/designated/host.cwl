cwlVersion: v1.2
class: CommandLineTool
requirements:
  DockerRequirement:
    dockerPull: alpine
baseCommand: echo
arguments:
  - valueFrom: $(runtime.outdir)
inputs: []
outputs:
  f:
    type: File
