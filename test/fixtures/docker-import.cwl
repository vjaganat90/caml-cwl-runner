cwlVersion: v1.2
class: CommandLineTool
requirements:
  DockerRequirement:
    dockerImport: @ARCHIVE@
    dockerImageId: @TAG@
baseCommand: [echo, imported]
inputs: []
outputs: []
