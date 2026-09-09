open Cmdliner

let version = "0.1.0"

let run outdir quiet processfile jobfile =
  ignore (outdir, quiet);
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
          match Cwl.command_line ~tool_path ~job_path with
          | Error e ->
              Printf.eprintf "ccr: %s\n" (Cwl.Error.to_string e);
              exit 1
          | Ok ann ->
              List.iter
                (fun d ->
                  Printf.eprintf "ccr: %s\n" (Cwl.Error.diagnostic_to_string d))
                ann.diagnostics;
              Printf.eprintf "ccr: execution not implemented\n";
              exit 33))

let outdir =
  let doc = "Output directory (unused in slice 1)" in
  Arg.(value & opt (some string) None & info [ "outdir" ] ~docv:"DIR" ~doc)

let quiet =
  let doc = "No diagnostic output (unused in slice 1)" in
  Arg.(value & flag & info [ "quiet" ] ~doc)

let processfile =
  let doc = "CWL process document" in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"PROCESS" ~doc)

let jobfile =
  let doc = "CWL input object" in
  Arg.(value & pos 1 (some string) None & info [] ~docv:"JOB" ~doc)

let term = Term.(const run $ outdir $ quiet $ processfile $ jobfile)

let info =
  Cmd.info "ccr" ~version
    ~doc:"CWL v1.2 runner (argv only in this slice; execution is unimplemented)"

let () = exit (Cmd.eval (Cmd.v info term))
