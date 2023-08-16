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

type position = {
  start_position : Lexing.position;
  end_position : Lexing.position;
}

type 'a location = { value : 'a; position : position }

let map f location = { location with value = f location.value }
let map_use f location = { value = f location; position = location.position }
let value { value; _ } = value

let values list = List.map value list
let position { position; _ } = position


let located_value start_position end_position value =
  { value; position = { start_position; end_position } }

let current_position lexbuf =
  let open Lexing in
  { start_position = lexbuf.lex_start_p; end_position = lexbuf.lex_curr_p }