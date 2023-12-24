open Zin

type int_test =
  { input : string
  ; exp_num : int
  }

let test_parse_integers () =
  let tests =
    [ { input = "5;"; exp_num = 5 }; { input = "10;"; exp_num = 10 } ]
  in
  let run_int_test test =
    let lexer = Lexer.init test.input in
    let parser = Parser.init lexer in
    let main = Parser.parse parser in
    match main with
    | Ok (Ast.Main m) ->
      let statements = m.statements in
      let len = List.length statements in
      if len != 1
      then failwith "expected statements length to be 1"
      else (
        let stmt = List.nth statements 0 in
        let exp =
          match stmt with
          | Ast.Expression e -> e
          | _ -> failwith "expected expression"
        in
        let got_int =
          match exp with
          | Ast.Integer i -> i
          | _ -> failwith "expected integer expression"
        in
        if got_int != test.exp_num
        then (
          let s = Printf.sprintf "expected %d to be %d" test.exp_num got_int in
          failwith s)
        else ())
    | Ok _ -> failwith "expected main"
    | Error err -> failwith err
  in
  List.iter run_int_test tests
;;

let test_parse_idents () =
  let input = "foobar;" in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let res = Parser.parse parser in
  match res with
  | Error e -> failwith e
  | Ok (Ast.Main m) ->
    let statements = m.statements in
    if List.length statements != 1
    then failwith "expected statements length to be 1"
    else (
      let stmt = List.nth statements 0 in
      match stmt with
      | Expression e ->
        let ident =
          match e with
          | Ident i -> i
          | _ -> failwith "expected identifier"
        in
        if String.compare ident.value "foobar" != 0
        then (
          let s = Printf.sprintf "expected foobar, got %s" ident.value in
          failwith s)
        else ()
      | _ -> failwith "expected expression")
  | _ -> failwith "expected main"
;;

let _ =
  let open Alcotest in
  run
    "parser"
    [ "parse integers", [ test_case "integers" `Quick test_parse_integers ]
    ; "parse identifiers", [ test_case "identifiers" `Quick test_parse_idents ]
    ]
;;
