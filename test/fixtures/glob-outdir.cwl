cwlVersion: v1.2
class: CommandLineTool
baseCommand: [ "true" ]
inputs: []
outputs:
  d:
    type: Directory
    outputBinding:
      glob: $(runtime.outdir)
