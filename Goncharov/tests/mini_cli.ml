[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib

let () =
  let input = In_channel.input_all In_channel.stdin |> String.trim in
  Interpret.parse_and_run input
;;