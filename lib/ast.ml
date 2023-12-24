type node =
  | Main of main
  | Statement of statement

and statement =
  | Let of
      { name : identifier
      ; exp : expression
      }
  | Return of expression
  | Expression of expression

and expression =
    | Integer of int
    | Ident of identifier

and identifier =
  { value : string
  ; data_type : data_type
  }

and data_type =
  | Infer
  | U8
  | U16
  | U32
  | U64
  | I8
  | I16
  | I32
  | I64
  | Bool
  | Function of data_type
  | Struct of data_type Util.StringMap.t
  | Enum of data_type Util.StringMap.t

and main =
  { statements : statement list
  ; types : data_type Util.StringMap.t
  }
