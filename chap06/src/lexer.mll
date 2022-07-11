{
open Parser

exception SyntaxError of string
}

let white = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_']+['0'-'9']*

rule read = 
  parse
  | white { read lexbuf }
  | "array" { ARRAY }
  | "break" { BREAK }
  | "do" { DO }
  | "else" { ELSE }
  | "then" { THEN }
  | "for" { FOR }
  | "while" { WHILE }
  | "end" { END }
  | "function" { FUNCTION }
  | "if" { IF }
  | "to" { TO }
  | "type" { TYPE }
  | "var" { VAR }
  | "let" { LET }
  | "in" { IN }
  | "of" { OF }
  | "nil" { NIL }
  | ":=" { ASSIGN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "&" { AND }
  | "|" { OR }
  | "<" { LT }
  | "<=" { LE }
  | ">" { GT }
  | ">=" { GE }
  | "=" { EQ }
  | "<>" { NE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "." { DOT }
  | ":" { COLON }
  | "," { COMMA }
  | ";" { SEMI }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | "/*" { comment 1 lexbuf }
  | eof { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' ' ' { Buffer.add_char buf ' '; read_string buf lexbuf }
  | "\\" (digit digit digit as x) {
    let num = int_of_string x in
      if num >= 0 && num <= 255 then Buffer.add_char buf (Char.chr num)
      else raise (SyntaxError ("error escape number."));
    read_string buf lexbuf
  }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and comment level =
  parse
  | "/*" { comment (level + 1) lexbuf }
  | "*/" { if level = 1 then (read lexbuf) else (comment (level - 1) lexbuf) }
  | eof { raise (SyntaxError ("File ended with unclosed comment.")) }
  | _ { comment level lexbuf }