cwlVersion: v1.2
$graph:
  - id: main
    class: CommandLineTool
    baseCommand: echo
    inputs: []
    outputs: []
  - id: "#other"
    class: CommandLineTool
    baseCommand: ["true"]
    inputs: []
    outputs: []
