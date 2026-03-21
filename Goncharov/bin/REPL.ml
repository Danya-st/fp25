[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

let extract_steps line =
  let prefix = ":max_steps" in
  let rest =
    String.sub line (String.length prefix) (String.length line - String.length prefix)
    |> String.trim
  in
  int_of_string rest
;;


let run_line max_steps line = Interpret.parse_and_run ~max_steps line

let rec repl ~(max_steps : int) =
  print_string "repl_miniml> ";
  Out_channel.flush stdout;
  match In_channel.input_line stdin with
  | None -> ()
  | Some line ->
    (match String.trim line with
     | line when String.equal line "" -> repl ~max_steps
     | line when String.equal line ":quit" || String.equal line ":q" -> ()
     | line when String.starts_with ~prefix:":max_steps" line ->
       repl ~max_steps:(extract_steps line)
     | _ ->
       run_line max_steps line;
       repl ~max_steps)
;;

let () =
  repl ~max_steps:10000
;;
