cwlVersion: v1.2
class: CommandLineTool
requirements:
  DockerRequirement:
    dockerPull: alpine
    dockerOutputDirectory: /other
baseCommand: [sh, -c]
arguments:
  - 'printf %s "$0" > where.txt; touch "$0/thing"'
  - valueFrom: $(runtime.outdir)
inputs: []
outputs:
  where:
    type: File
    outputBinding:
      glob: where.txt
  thing:
    type: File
    outputBinding:
      glob: thing
