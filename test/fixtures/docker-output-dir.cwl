cwlVersion: v1.2
class: CommandLineTool
requirements:
  - class: DockerRequirement
    dockerPull: alpine
    dockerOutputDirectory: /out
baseCommand: echo
inputs: []
outputs: []
