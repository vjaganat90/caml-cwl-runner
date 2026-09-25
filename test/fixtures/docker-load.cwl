cwlVersion: v1.2
class: CommandLineTool
requirements:
  DockerRequirement:
    dockerLoad: @ARCHIVE@
baseCommand: [echo, loaded]
inputs: []
outputs: []
