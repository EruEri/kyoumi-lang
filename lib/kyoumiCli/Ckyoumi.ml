(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kyoumi                                                                *)
(* Copyright (C) 2023 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* Kyoumi is free software: you can redistribute it and/or modify it under the terms          *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* Kyoumi is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;        *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with Kyoumi.       *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open Cmdliner

let name = "kyoumi"

type t = {
  output: string option;
  files: string list
}


let files_term = 
  Arg.(
    non_empty
    & pos_all (non_dir_file) [] 
    & info [] ~docv:"filename"
  )


let output_term = 
  Arg.(
    value
    & opt (some string) None
    & info ["o"] ~docv:""
  )


let cmd_term run = 
  let combine output files = 
    run @@ {output; files}
  in
  Term.(const combine
    $ output_term
    $ files_term
  )

let doc = "The Kyoumi compiler"

let man = [
  `S Manpage.s_description;
]

let kyoumic run =
  let info =
    Cmd.info ~doc ~man ~version:(Ccommon.version) name
  in
  Cmd.v info (cmd_term run)


let run cmd = 
  let {output; files} = cmd in
  let `Kyofile kyo_files, `Cfile c_file, `CObjfile cobj_files = match Ccommon.split_file files with
    | Ok files -> files
    | Error e -> raise @@ KyoumiFrontend.Error.unsupported_file e 
  in
  let kyo_program = 
    match KyoumiFrontend.Create.kyo_program kyo_files with
    | Ok kyo_program -> kyo_program
    | Error lexer -> raise @@ KyoumiFrontend.Error.lexer_error lexer
  in
  let () = ignore (output, c_file, cobj_files, kyo_program) in
  ()


let command = kyoumic run

let eval () = Cmd.eval command