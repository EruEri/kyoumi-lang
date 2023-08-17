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

let rec kyo_program ~acc = function
| [] -> Result.ok @@ List.rev acc
| kyofile::q -> 
  let (let*) = Result.bind in
  let* kyo_module = In_channel.with_open_bin kyofile (fun ic ->
    let lexbuf = Lexing.from_channel ic in
    KyoumiParserToken.parse lexbuf (KyoumiParser.Incremental.kyo_module lexbuf.lex_curr_p)
  ) in
  kyo_program ~acc:(KyoumiAst.{filename = kyofile; kyo_module}::acc) q

let kyo_program = kyo_program ~acc:[]
  