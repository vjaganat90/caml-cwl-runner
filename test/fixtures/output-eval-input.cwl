cwlVersion: v1.2
class: CommandLineTool
doc: Returns its staged input as an output, beside an outdir file of that name.
baseCommand: [touch, stdin-hello.txt]
inputs:
  f: File
outputs:
  out: {type: File, outputBinding: {outputEval: $(inputs.f)}}
