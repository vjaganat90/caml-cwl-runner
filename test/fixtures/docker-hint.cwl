cwlVersion: v1.2
class: CommandLineTool
hints:
  - class: DockerRequirement
    dockerPull: alpine
baseCommand: echo
stdout: out.txt
inputs:
  message:
    type: string
    inputBinding:
      position: 1
outputs:
  example_out:
    type: stdout
