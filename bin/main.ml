(** The [ccr] command. It reads the flags, starts the Eio scheduler, runs one
    process through [Cwl.run], prints the output object as JSON on stdout, and
    turns an error into an exit code: 33 for an unsupported feature, 1 for
    anything else. No CWL logic lives here. *)

open Cmdliner

let version = "0.1.0"

let run outdir quiet rm_tmpdir tool_path job =
  Eio_main.run @@ fun env ->
  let local = Runtime.local env in
  let docker image = Runtime.docker env image in
  match Cwl.run local ~docker ?outdir ~rm_tmpdir tool_path job with
  | Error (Cwl.Error.Unsupported { feature }) ->
      Printf.eprintf "ccr: unsupported feature: %s\n" feature;
      33
  | Error e ->
      Printf.eprintf "ccr: %s\n" (Cwl.Error.to_string e);
      1
  | Ok ann ->
      if not quiet then
        List.iter
          (fun d ->
            Printf.eprintf "ccr: %s\n" (Cwl.Error.diagnostic_to_string d))
          ann.diagnostics;
      print_endline
        (Cwl.Type.to_json ~sorted:false (Cwl.Untyped_tree.Object ann.value));
      0

let outdir =
  let doc = "Output directory" in
  Arg.(value & opt (some string) None & info [ "outdir" ] ~docv:"DIR" ~doc)

let quiet =
  let doc = "No diagnostic output" in
  Arg.(value & flag & info [ "quiet" ] ~doc)

let rm_tmpdir =
  let rm =
    Arg.info [ "rm-tmpdir" ]
      ~doc:"Delete the tool's temporary directory after the run (default)."
  in
  let leave =
    Arg.info [ "leave-tmpdir" ] ~doc:"Keep the tool's temporary directory."
  in
  Arg.(value & vflag true [ (true, rm); (false, leave) ])

let processfile =
  let doc = "CWL process document" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PROCESS" ~doc)

let jobfile =
  let doc = "CWL input object. Omitted means an empty object." in
  Arg.(value & pos 1 (some string) None & info [] ~docv:"JOB" ~doc)

let term = Term.(const run $ outdir $ quiet $ rm_tmpdir $ processfile $ jobfile)
let info = Cmd.info "ccr" ~version ~doc:"CWL v1.2 runner"

(* 33 is the only special code; usage errors and escaped exceptions are 1. *)
let () =
  match Cmd.eval_value (Cmd.v info term) with
  | Ok (`Ok code) -> exit code
  | Ok (`Version | `Help) -> exit 0
  | Error _ -> exit 1
