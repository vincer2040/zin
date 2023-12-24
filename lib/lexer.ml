type t =
  { input : string
  ; pos : int
  ; ch : char option
  }
[@@deriving show]

let init input =
  if String.length input == 0
  then { input; pos = 1; ch = None }
  else { input; pos = 1; ch = Some (String.get input 0) }
;;

let rec next_token lexer =
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | None -> lexer, None
  | Some ch ->
    (match ch with
     | '+' -> advance lexer, Some Token.Plus
     | '*' -> advance lexer, Some Token.Asterisk
     | '/' -> advance lexer, Some Token.Slash
     | '(' -> advance lexer, Some Token.LParen
     | ')' -> advance lexer, Some Token.RParen
     | '{' -> advance lexer, Some Token.LSquirly
     | '}' -> advance lexer, Some Token.RSquirly
     | ';' -> advance lexer, Some Token.Semicolon
     | ',' -> advance lexer, Some Token.Comma
     | '_' -> advance lexer, Some Token.Underscore
     | '|' -> advance lexer, Some Token.Bar
     | '<' -> advance lexer, Some Token.Lt
     | '>' -> advance lexer, Some Token.Gt
     | ':' ->
       let lexer, tok =
         if_peeked lexer ':' ~matched:Token.DoubleColon ~default:Token.Colon
       in
       advance lexer, Some tok
     | '.' ->
       let lexer, tok =
         if_peeked lexer '.' ~matched:Token.Range ~default:Token.Dot
       in
       if tok == Token.Range
       then (
         let lexer, tok =
           if_peeked
             lexer
             '='
             ~matched:Token.InclusiveRange
             ~default:Token.Range
         in
         advance lexer, Some tok)
       else advance lexer, Some tok
     | '-' ->
       let lexer, tok =
         if_peeked lexer '>' ~matched:Token.Arrow ~default:Token.Minus
       in
       advance lexer, Some tok
     | '=' ->
       let lexer, tok =
         if_peeked lexer '=' ~matched:Token.Eq ~default:Token.Assign
       in
       advance lexer, Some tok
     | '!' ->
       let lexer, tok =
         if_peeked lexer '=' ~matched:Token.NotEq ~default:Token.Bang
       in
       advance lexer, Some tok
     | ch when is_letter ch ->
       let lexer, ident = read_ident lexer in
       lexer, Some (Token.lookup_ident ident)
     | ch when is_digit ch ->
       let lexer, int_string = read_int lexer in
       lexer, Some (Token.Int int_string)
     | _ -> advance lexer, Some Token.Illegal)

and advance lexer =
  if lexer.pos >= String.length lexer.input
  then { lexer with ch = None }
  else
    { lexer with
      pos = lexer.pos + 1
    ; ch = Some (String.get lexer.input lexer.pos)
    }

and read_while lexer start cond =
  let rec loop l end_pos =
    match l.ch with
    | None -> l, end_pos
    | Some ch -> if cond ch then loop (advance l) (end_pos + 1) else l, end_pos
  in
  let l, end_pos = loop lexer 0 in
  l, start, end_pos

and if_peeked lexer ch ~matched ~default =
  match peek_char lexer with
  | None -> lexer, default
  | Some peek when peek == ch -> advance lexer, matched
  | Some _ -> lexer, default

and peek_char lexer =
  if lexer.pos >= String.length lexer.input
  then None
  else Some (String.get lexer.input lexer.pos)

and read_ident lexer =
  let l, start_pos, end_pos =
    read_while lexer (lexer.pos - 1) is_valid_ident_char
  in
  let ident = String.sub lexer.input start_pos end_pos in
  l, ident

and read_int lexer =
  let l, start_pos, end_pos = read_while lexer (lexer.pos - 1) is_digit in
  let int_string = String.sub lexer.input start_pos end_pos in
  l, int_string

and skip_whitespace lexer =
  let lexer, _, _ = read_while lexer (lexer.pos - 1) is_whitespace in
  lexer

and is_whitespace ch =
  match ch with
  | ' ' -> true
  | '\t' -> true
  | '\r' -> true
  | '\n' -> true
  | _ -> false

and is_valid_ident_char ch = is_digit ch || is_letter ch

and is_digit ch =
  match ch with
  | '0' .. '9' -> true
  | _ -> false

and is_letter ch =
  match ch with
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | '_' -> true
  | _ -> false
;;
