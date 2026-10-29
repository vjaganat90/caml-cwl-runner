cwlVersion: v1.2
class: CommandLineTool
doc: outputEval with no glob.
baseCommand: "true"
inputs: []
outputs:
  n: {type: int, outputBinding: {outputEval: $(self.length)}}
