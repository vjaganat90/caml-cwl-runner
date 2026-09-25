cwlVersion: v1.2
class: CommandLineTool
requirements:
  - class: DockerRequirement
    dockerPull: alpine
  - class: DockerRequirement
    dockerPull: alpine
    dockerOutputDirectory: /other
baseCommand: echo
inputs: []
outputs: []
