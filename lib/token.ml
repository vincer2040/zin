type token =
  | Illegal
  | Assign
  | Plus
  | Minus
  | Asterisk
  | Slash
  | Bang
  | Lt
  | Gt
  | Eq
  | NotEq
  | LParen
  | RParen
  | LSquirly
  | RSquirly
  | Semicolon
  | Comma
  | Bar
  | Arrow
  | Underscore
  | Colon
  | Dot
  | Range
  | InclusiveRange
  | Let
  | Function
  | If
  | Else
  | Match
  | Return
  | True
  | False
  | U8
  | U16
  | U32
  | U64
  | I8
  | I16
  | I32
  | I64
  | Bool
  | Enum
  | Type
  | Struct
  | Main
  | Ident of string
  | Int of string
[@@deriving show, eq]

let lookup_ident ident =
  match ident with
  | "if" -> If
  | "fn" -> Function
  | "u8" -> U8
  | "i8" -> I8
  | "u16" -> U16
  | "i16" -> I16
  | "u32" -> U32
  | "i32" -> I32
  | "u64" -> U64
  | "I64" -> I64
  | "let" -> Let
  | "bool" -> Bool
  | "type" -> Type
  | "enum" -> Enum
  | "main" -> Main
  | "else" -> Else
  | "true" -> True
  | "match" -> Match
  | "false" -> False
  | "struct" -> Struct
  | "return" -> Return
  | _ -> Ident ident
;;
