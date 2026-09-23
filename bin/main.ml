open Cmdliner

let version = "0.1.0"

let run outdir quiet processfile jobfile =
  match processfile with
  | None ->
      Printf.eprintf "ccr: missing process file\n";
      exit 2
  | Some tool_path -> (
      match jobfile with
      | None ->
          Printf.eprintf "ccr: missing job file\n";
          exit 2
      | Some job_path -> (
          Eio_main.run @@ fun env ->
          let local = Cwl.Runtime.local env in
          let docker image = Cwl.Runtime.docker env image in
          match Cwl.run local ~docker ?outdir tool_path job_path with
          | Error (Cwl.Error.Unsupported { feature }) ->
              Printf.eprintf "ccr: unsupported feature: %s\n" feature;
              exit 33
          | Error e ->
              Printf.eprintf "ccr: %s\n" (Cwl.Error.to_string e);
              exit 1
          | Ok ann ->
              if not quiet then
                List.iter
                  (fun d ->
                    Printf.eprintf "ccr: %s\n"
                      (Cwl.Error.diagnostic_to_string d))
                  ann.diagnostics;
              print_endline (Cwl.Type.object_to_json ann.value)))

let outdir =
  let doc = "Output directory" in
  Arg.(value & opt (some string) None & info [ "outdir" ] ~docv:"DIR" ~doc)

let quiet =
  let doc = "No diagnostic output" in
  Arg.(value & flag & info [ "quiet" ] ~doc)

let processfile =
  let doc = "CWL process document" in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"PROCESS" ~doc)

let jobfile =
  let doc = "CWL input object" in
  Arg.(value & pos 1 (some string) None & info [] ~docv:"JOB" ~doc)

let term = Term.(const run $ outdir $ quiet $ processfile $ jobfile)
let info = Cmd.info "ccr" ~version ~doc:"CWL v1.2 runner"
let () = exit (Cmd.eval (Cmd.v info term))
