cwlVersion: v1.2
class: CommandLineTool
requirements:
  - class: DockerRequirement
    dockerFile: "FROM alpine:latest\n"
    dockerImageId: ccr-dockerfile-test
baseCommand: [echo, built]
inputs: []
outputs: []
