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

{
    open Lexing
    open KyoumiParser
    open KyoumiError

    let lkeywords = 
    [
        ("as", AS); ("effect", EFFECT); ("end", END); ("external", EXTERNAL); ("eq", CMP_EQUAL); ("false", FALSE); ("fn", FUNCTION); ("fun", ANON_FUNCTION);
        ("gt", CMP_GREATER); ("handler", HANDLER ); ("in", IN); ("lt", CMP_LESS); ("let", LET); ("match", MATCH); ("open", OPEN); ("perform", PERFORM);
        ("true", TRUE); ("type", TYPE); ("resume", RESUME); ("val", VAL);("while", WHILE); ("with", WITH)
    ]
    let keywords = Hashtbl.of_seq @@ List.to_seq lkeywords

    let current_position = Util.Position.current_position
}

let digit = ['0'-'9']
let loLetter = ['a'-'z']
let upLetter = ['A'-'Z']
let lower_identifier = loLetter+
let identifiant = (loLetter | '_') (loLetter | upLetter | digit | "_")*
let module_identifier = (upLetter) (loLetter | digit | "_" | upLetter )*
let escaped_char =  ['n' 'r' 't' '\\' '\'' '\"']
let float_literal = (digit) (digit | "_" )* (('.') (digit | "_")*  | ('e' | 'E')  ('+' | '-') digit (digit | '_')*)
let decimal_integer = digit (digit | '_')*
let hex_integer = '0' ('x' | 'X') (digit | ['a'-'f'] | ['A'-'F']) (digit | ['a'-'f'] | ['A'-'F'] | '_')*
let octal_intger = '0' ('o' | 'O') (['0'-'7']) (['0'-'7'] | '_')*
let binary_integer = '0' ('b' | 'B') ('0' | '1') ('0' | '1' | '_')*
let number = decimal_integer | hex_integer | octal_intger | binary_integer
let hexa_char = '\\' 'x' (digit | ['a'-'f'] | ['A'-'F']) (digit | ['a'-'f'] | ['A'-'F'])
let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']
let whitespace = [' ' '\t' '\r' '\n']+
let infix_symbol = ['|' '=' '<' '>' '^' '&' '+' '-' '*' '/' '$' '%' '~']
let prefix_symbol = [ '!' '?']
let operator_symbol = ['|' '=' '<' '>' '^' '&' '+' '-' '*' '/' '$' '%' '~' '!' '?']
let not_identifier = [^ 'a'-'z']


rule token = parse
| newline {
    let () = Lexing.new_line lexbuf in
    token lexbuf
}
| blank+ { token lexbuf }
| "(" { LPARENT }
| ")" { RPARENT }
| "{" { LBRACE }
| "}" { RBRACE }
| ";" { SEMICOLON }
| ":" { COLON }
| "::" { DOUBLECOLON }
| "," { COMMA }
| "." { DOT }
| "_" { WILDCARD }
| "`"  not_identifier {
    BACKTICK
}
| "`" {
    polymorphic_eff lexbuf
 }
| "'" { 
    polymorphic_var lexbuf
}
| "@" { built_in_function lexbuf }
| (infix_symbol as i) (operator_symbol* as os) as all {
    match i with
    | '|' when os = String.empty -> PIPE
    | '&' when os = String.empty -> AMPERSAND
    | '=' when os = String.empty -> EQUAL
    (* | '=' when all = "=>" -> EQUAL_SUP *)
    | '-' when all = "->" -> MINUS_SUP
    | '/' when all = "//" -> single_line_comment lexbuf
    | '/' when all = "/*" ->
        multiple_line_comment lexbuf
    | '|' -> INFIX_PIPE all
    | '&' -> INFIX_AMPERSAND all
    | '=' -> INFIX_EQUAL all
    | '<' -> INFIX_INF all 
    | '>' -> INFIX_SUP all
    | '^' -> INFIX_CARET all
    | '+' -> INFIX_PLUS all
    | '-' -> INFIX_MINUS all
    | '*' -> INFIX_MULT all
    | '/' -> INFIX_DIV all
    | '$' -> INFIX_DOLLAR all
    | '%' -> INFIX_PERCENT all
    | '~' -> INFIX_TILDE all
    | _ -> failwith "Unreachable: no other infix characters"

}
| (prefix_symbol as i) (operator_symbol* as os) as all {
    let () = ignore os in
    match i with
    | '!' -> PREFIX_EXCLA all
    | '?' -> PREFIX_QUESTIONMARK all
    | _ -> failwith "Unreachable: no other prefix characters"
}
| "[" { LSQBRACE }
| "]" { RSQBRACE }
| (float_literal as f) {
    Float_lit (float_of_string f)
}
| module_identifier as s {
    Module_IDENT s
}
| identifiant as s {
    try 
        Hashtbl.find keywords s
    with Not_found -> IDENT s
}
| (number as n) {
    Integer_lit (int_of_string n)
}
| eof { EOF }
and polymorphic_var = parse
| lower_identifier as s {
    PolymorphicVar s
}
and polymorphic_eff = parse
| lower_identifier as s {
    PolymorphicEff s
}
and read_string buffer = parse
| '"' { String_lit (Buffer.contents buffer) }
| (hexa_char as s) {
    let s_len = String.length s in
    let s_number = String.sub s 1 (s_len - 1) in
    let code =  int_of_string ("0" ^  s_number) in
    let char = Char.chr code in
    let escaped = char |> Printf.sprintf "%c" |> String.escaped in
    let () = Buffer.add_string buffer escaped in 
    read_string buffer lexbuf 
}
| '\\' ( escaped_char as c ){ 
    let () = if c = '\\' then () else Buffer.add_char buffer '\\' in
    let () = Buffer.add_char buffer c in
    read_string buffer lexbuf 
}
| '\\' { 
    let le = Unexpected_escaped_char ((current_position lexbuf) , ( lexeme lexbuf) ) in
    raise @@ raw_lexer_error @@ le  
}

| _ as c { 
    if c = '\n' then 
        let () = Lexing.new_line lexbuf in
        read_string buffer lexbuf
    else
        let () = Buffer.add_char buffer c in
        read_string buffer lexbuf 
}
| eof {
    raise @@ raw_lexer_error @@ Unclosed_string (current_position lexbuf)
}
and single_line_comment = parse
| newline {  
    let () = Lexing.new_line lexbuf in
    token lexbuf 
}
| _ { single_line_comment lexbuf}
| eof { EOF }
and multiple_line_comment = parse 
| "*/" { token lexbuf }
| newline { 
    let () = Lexing.new_line lexbuf in
    multiple_line_comment lexbuf
}
| _ { multiple_line_comment lexbuf }
| eof {
    raise @@ raw_lexer_error @@ Unclosed_comment (current_position lexbuf)
}
and built_in_function = parse
| identifiant as s {
    match Hashtbl.find_opt keywords s with
    | None -> BUILTIN s
    | Some _ -> (Invalid_keyword_for_build_in_function ((current_position lexbuf) , s) |> raw_lexer_error |> raise )
}
| _ as lit {
     (Invalid_litteral_for_build_in_function ( current_position lexbuf ,lit)  |> raw_lexer_error |> raise )
}
| eof {  Not_finished_built_in_function (current_position lexbuf)  |> raw_lexer_error |> raise }
