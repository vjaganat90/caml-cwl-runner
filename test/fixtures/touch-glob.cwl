cwlVersion: v1.2
class: CommandLineTool
baseCommand: [touch, hello.txt]
inputs: []
outputs:
  out:
    type: File
    outputBinding:
      glob: hello.txt
