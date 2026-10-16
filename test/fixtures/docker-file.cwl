cwlVersion: v1.2
class: CommandLineTool
requirements:
  - class: DockerRequirement
    dockerFile: "FROM alpine:latest\n"
    dockerImageId: @TAG@
baseCommand: [echo, built]
inputs: []
outputs: []
