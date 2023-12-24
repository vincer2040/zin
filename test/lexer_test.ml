open Zin

let test_next_token () =
  let input =
    "let five = 5;\n\
     let ten = 10;\n\
     fn add(x, y) -> u32 {\n\
    \    x + y\n\
     }\n\
     let result = add(five, ten);\n\
     10 == 10;\n\
     10 != 9;\n\
     if 9 < 10 {\n\
    \   return true;\n\
     } else {\n\
    \   return false;\n\
     }\n\
     match five {\n\
    \   1 -> true,\n\
    \   2 | 3  -> true,\n\
    \   13 ..= 19 -> true,\n\
    \   _ -> false,\n\
     }\n\
     13 .. 19;\n\
     struct MyStruct {\n\
    \   num: u32;\n\
    \   is_even: bool;\n\
     }\n\
     enum IpAddr {\n\
    \   V4(IpV4Addr),\n\
    \   V6(IpV6Addr),\n\
     }\n\
     type MyType = i32;\n\
     fn main() {}"
  in
  let exps =
    [ Token.Let
    ; Token.Ident "five"
    ; Token.Assign
    ; Token.Int "5"
    ; Token.Semicolon
    ; Token.Let
    ; Token.Ident "ten"
    ; Token.Assign
    ; Token.Int "10"
    ; Token.Semicolon
    ; Token.Function
    ; Token.Ident "add"
    ; Token.LParen
    ; Token.Ident "x"
    ; Token.Comma
    ; Token.Ident "y"
    ; Token.RParen
    ; Token.Arrow
    ; Token.U32
    ; Token.LSquirly
    ; Token.Ident "x"
    ; Token.Plus
    ; Token.Ident "y"
    ; Token.RSquirly
    ; Token.Let
    ; Token.Ident "result"
    ; Token.Assign
    ; Token.Ident "add"
    ; Token.LParen
    ; Token.Ident "five"
    ; Token.Comma
    ; Token.Ident "ten"
    ; Token.RParen
    ; Token.Semicolon
    ; Token.Int "10"
    ; Token.Eq
    ; Token.Int "10"
    ; Token.Semicolon
    ; Token.Int "10"
    ; Token.NotEq
    ; Token.Int "9"
    ; Token.Semicolon
    ; Token.If
    ; Token.Int "9"
    ; Token.Lt
    ; Token.Int "10"
    ; Token.LSquirly
    ; Token.Return
    ; Token.True
    ; Token.Semicolon
    ; Token.RSquirly
    ; Token.Else
    ; Token.LSquirly
    ; Token.Return
    ; Token.False
    ; Token.Semicolon
    ; Token.RSquirly
    ; Token.Match
    ; Token.Ident "five"
    ; Token.LSquirly
    ; Token.Int "1"
    ; Token.Arrow
    ; Token.True
    ; Token.Comma
    ; Token.Int "2"
    ; Token.Bar
    ; Token.Int "3"
    ; Token.Arrow
    ; Token.True
    ; Token.Comma
    ; Token.Int "13"
    ; Token.InclusiveRange
    ; Token.Int "19"
    ; Token.Arrow
    ; Token.True
    ; Token.Comma
    ; Token.Underscore
    ; Token.Arrow
    ; Token.False
    ; Token.Comma
    ; Token.RSquirly
    ; Token.Int "13"
    ; Token.Range
    ; Token.Int "19"
    ; Token.Semicolon
    ; Token.Struct
    ; Token.Ident "MyStruct"
    ; Token.LSquirly
    ; Token.Ident "num"
    ; Token.Colon
    ; Token.U32
    ; Token.Semicolon
    ; Token.Ident "is_even"
    ; Token.Colon
    ; Token.Bool
    ; Token.Semicolon
    ; Token.RSquirly
    ; Token.Enum
    ; Token.Ident "IpAddr"
    ; Token.LSquirly
    ; Token.Ident "V4"
    ; Token.LParen
    ; Token.Ident "IpV4Addr"
    ; Token.RParen
    ; Token.Comma
    ; Token.Ident "V6"
    ; Token.LParen
    ; Token.Ident "IpV6Addr"
    ; Token.RParen
    ; Token.Comma
    ; Token.RSquirly
    ; Token.Type
    ; Token.Ident "MyType"
    ; Token.Assign
    ; Token.I32
    ; Token.Semicolon
    ; Token.Function
    ; Token.Main
    ; Token.LParen
    ; Token.RParen
    ; Token.LSquirly
    ; Token.RSquirly
    ]
  in
  let lexer = Lexer.init input in
  let rec lex l acc =
    let l, tok = Lexer.next_token l in
    match tok with
    | Some t -> lex l (t :: acc)
    | None -> List.rev acc
  in
  let gots = lex lexer [] in
  List.iter2
    (fun got exp ->
      let sgot = Token.show_token got in
      let sexp = Token.show_token exp in
      if String.compare sgot sexp != 0
      then (
        let s =
          Printf.sprintf
            "expected next token to be %s, got %s instead"
            (Token.show_token exp)
            (Token.show_token got)
        in
        failwith s))
    gots
    exps
;;

let _ =
  let open Alcotest in
  run
    "lexer"
    [ "next token", [ test_case "next token" `Quick test_next_token ] ]
;;
