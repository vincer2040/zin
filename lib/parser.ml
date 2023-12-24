let ( let* ) res f = Result.bind res f

type t =
  { lexer : Lexer.t
  ; cur : Token.token option
  ; next : Token.token option
  ; errors : string list
  }

let next_token parser =
  let lexer, next = Lexer.next_token parser.lexer in
  { parser with lexer; cur = parser.next; next }
;;

let chomp_semicolon parser =
  match parser.next with
  | None -> parser
  | Some tok ->
    (match tok with
     | Token.Semicolon -> next_token parser
     | _ -> parser)
;;

let init lexer =
  { lexer; cur = None; next = None; errors = [] } |> next_token |> next_token
;;

let rec parse parser =
  let rec parse' parser statements =
    match parser.cur with
    | None -> Ok (List.rev statements)
    | Some _ ->
      let* parser, stmt = parse_statement parser in
      parse' (advance parser) (stmt :: statements)
  in
  let* statements = parse' parser [] in
  let m = Ast.Main { statements; types = Util.StringMap.empty } in
  Ok m

and advance parser = next_token parser

and parse_statement parser =
  match parser.cur with
  | None -> Error "parser.cur is none"
  | Some tok ->
    (match tok with
     | Let -> Error "todo"
     | Return -> Error "todo"
     | _ -> parse_expression parser)

and parse_expression parser =
  let* exp =
    match parser.cur with
    | None -> Error "parser.cur is none"
    | Some tok ->
      (match tok with
       | Token.Int int_str ->
         let int_val = int_of_string int_str in
         let exp = Ast.Expression (Ast.Integer int_val) in
         Ok exp
       | Token.Ident ident_str -> parse_ident_expression ident_str
       | _ -> Error "todo")
  in
  let parser = chomp_semicolon parser in
  Ok (parser, exp)

and parse_ident_expression value =
  let ident = Ast.Ident { value; data_type = Ast.Infer } in
  let exp = Ast.Expression ident in
  Ok exp
;;
