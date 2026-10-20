cwlVersion: v1.2
class: CommandLineTool
baseCommand: "true"
inputs:
  n: {type: int, default: 3}
outputs:
  n: {type: int, outputBinding: {outputEval: $(inputs.n)}}
