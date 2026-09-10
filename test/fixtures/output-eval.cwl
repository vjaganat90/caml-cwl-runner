cwlVersion: v1.2
class: CommandLineTool
baseCommand: [ "true" ]
inputs: []
outputs:
  out:
    type: File
    outputBinding:
      glob: "*.txt"
      outputEval: ${ return self; }
