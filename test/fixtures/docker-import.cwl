cwlVersion: v1.2
class: CommandLineTool
requirements:
  DockerRequirement:
    dockerImport: @ARCHIVE@
    dockerImageId: ccr-import-test
baseCommand: [echo, imported]
inputs: []
outputs: []
