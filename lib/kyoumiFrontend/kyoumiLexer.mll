{
    open Lexing
    open KyoumiParser

    let lkeywords = 
    [
        ("and", FULLAND); ("as", AS); ("effect", EFFECT); ("external", EXTERNAL); ("eq", CMP_EQUAL); ("false", FALSE); ("fn", FUNCTION); ("fun", ANON_FUNCTION);
        ("gt", CMP_GREATER); ("lt", CMP_LESS); ("let", LET); ("match", ); ("true", TRUE); ("type", TYPE);
        ("val", VAL);("while", WHILE);
    ]
    let keywords = Hashtbl.of_seq @@ List.to_seq lkeywords
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
| "`" {
    polymorphic_eff lexbuf
 }
| "'" { 
    polymorphic_var lexbuf
}
| "@" { built_in_function lexbuf }
| "=" { EQUAL }
| "&"  { AMPERSAND }
| "&&" { AND }
| "||" { OR }
| "|>" { PIPESUP }
| "==" { DOUBLEQUAL }
| "<=>" { INF_EQ_SUP }
| "!=" { DIF }
| "+" { PLUS }
| "-" { MINUS }
| "*" { MULT } 
| "/" { DIV }
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
    Integer_lit (Int.of_string n)
}
| eof { EOF }
rule polymorphic_var = parse
| lower_identifier as s {
    PolymorphicVar s
}
rule polymorphic_eff = parse
| lower_identifier as s {
    PolymorphicEff s
}