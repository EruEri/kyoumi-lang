open Util.Position

type lexer_error = 
  | Forbidden_char of position*char
  | Unexpected_escaped_char of position*string
  | Invalid_keyword_for_build_in_function of position*string
  | Invalid_litteral_for_build_in_function of position*char
  | Not_finished_built_in_function of position
  | Unclosed_string of position
  | Unclosed_comment of position
  | Char_out_of_range of position * int
  | Char_Error of position
  | Syntax_Error of {
      position: position;
      current_lexeme: string;
      message: string;
      state: int option
  }

exception Raw_Lexer_Error of lexer_error

let raw_lexer_error e = Raw_Lexer_Error e
