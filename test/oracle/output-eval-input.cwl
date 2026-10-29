cwlVersion: v1.2
class: CommandLineTool
doc: An outputEval that returns a staged input reports a File that outlives the run.
baseCommand: "true"
inputs:
  f: File
outputs:
  out: {type: File, outputBinding: {outputEval: $(inputs.f)}}
