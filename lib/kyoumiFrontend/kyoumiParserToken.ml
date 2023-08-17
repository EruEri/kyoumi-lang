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

open Lexing
module I = KyoumiParser.MenhirInterpreter

let error_info env =
  match I.stack env with
  | (lazy Nil) ->
      ("Invalid syntax", None)
  | (lazy (Cons (I.Element (state, _element, _, _), _))) -> (
      let nb_state = I.number state in
      try (KyoumiParserMessage.message nb_state, Some nb_state)
      with Not_found ->
        ("invalid syntax (no specific message for this eror)", None)
    )

let rec parse lexbuf (checkpoint : KyoumiAst.kyo_module I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env -> (
      try
        let token = KyoumiLexer.token lexbuf in
        let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
        let checkpoint = I.offer checkpoint (token, startp, endp) in
        parse lexbuf checkpoint
      with
      | KyoumiError.Raw_Lexer_Error e ->
          Result.Error e
      | _ ->
          failwith "Uncatched Lexer Error"
    )
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse lexbuf checkpoint
  | I.HandlingError env ->
      let position = Util.Position.current_position lexbuf in
      let current_lexeme = Lexing.lexeme lexbuf in
      let err, state = error_info env in
      Result.error
        @@ KyoumiError.Syntax_Error { position; current_lexeme; message = err; state }
  | I.Accepted v ->
      Ok v
  | I.Rejected ->
      let position = Util.Position.current_position lexbuf in
      let current_lexeme = Lexing.lexeme lexbuf in
      Result.error
        @@ KyoumiError.Syntax_Error
            {
              position;
              current_lexeme;
              message = "Parser reject the input";
              state = None;
            }
        