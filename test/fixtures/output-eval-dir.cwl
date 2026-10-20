cwlVersion: v1.2
class: CommandLineTool
doc: Returns its staged Directory literal as an output.
baseCommand: "true"
inputs:
  d: Directory
outputs:
  out: {type: Directory, outputBinding: {outputEval: $(inputs.d)}}
