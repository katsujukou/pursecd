type proper_name = ProperName of string

type module_name = ModuleName of string

type 'a qualified = Qualified of module_name option * 'a 

let map_qualified f (Qualified (mn, a)) = Qualified (mn, f a)

type ident = Ident of string

type re_exort = ReExport of module_name * ident

type 'a prop = Prop of string * 'a

let map_prop f (Prop (k, v)) = Prop (k, f v)

let fold_left_prop f acc (Prop (_, v)) = f acc v 

let fold_right_prop f acc (Prop (_, v)) = f v acc

let prop_key (Prop (k, _)) = k 

let prop_value (Prop (_, v)) = v

let find_prop prop props = props
  |> List.find_opt (fun (Prop (k, _)) -> k == prop)
  |> Option.map prop_value

type 'a literal =
  | LitInt of int
  | LitNumber of float 
  | LitString of string
  | LitChar of char
  | LitBoolean of bool
  | LitArray of 'a list 
  | LitRecord of 'a prop list 
  
let map_literal f = function
  | LitInt i -> LitInt i 
  | LitNumber f -> LitNumber f 
  | LitString s -> LitString s 
  | LitChar c -> LitChar c 
  | LitBoolean b -> LitBoolean b 
  | LitArray xs -> LitArray (List.map f xs)
  | LitRecord ps -> LitRecord (List.map (map_prop f) ps)

let lit_int i = LitInt i
let lit_number f = LitNumber f
let lit_string s = LitString s
let lit_bool b = LitBoolean b
let lit_char c = LitChar c
let lit_array xs = LitArray xs
let lit_record props = LitRecord props

type 'a binder =
  | BinderNull of 'a
  | BinderVar of 'a * ident
  | BinderNamed of 'a * ident  * 'a binder 
  | BinderLit of 'a * 'a binder literal
  | BinderConstructor of 'a * (proper_name qualified) * (ident qualified) * ('a binder list)

let rec map_binder f = function
    | BinderNull a -> BinderNull (f a)
    | BinderVar (a, v) -> BinderVar (f a, v)
    | BinderNamed (a, i, b) -> BinderNamed (f a, i, map_binder f b)
    | BinderLit (a, lit) -> BinderLit (f a, map_literal (map_binder f) lit)
    | BinderConstructor (a, tyn, constr, args) -> BinderConstructor (f a, tyn, constr, List.map (map_binder f) args)

type 'a bind = 
  | NonRec of 'a binding
  | Rec of 'a binding list 

and 'a binding = Binding of 'a * ident * 'a expr
      
and 'a expr =
  | ExprVar of 'a * ident qualified
  | ExprLit of 'a * 'a expr literal 
  | ExprConstructor of 'a * proper_name * ident * string list
  | ExprAccessor of 'a * 'a expr * string
  | ExprUpdate of 'a * 'a expr * (('a expr) prop) list
  | ExprAbs of 'a * ident * 'a expr
  | ExprApp of 'a * 'a expr * 'a expr
  | ExprCase of 'a * 'a expr list * 'a case_alternative list
  | ExprLet of 'a * 'a bind list * 'a expr
  
and 'a case_alternative = CaseAlternative of 'a binder list * 'a case_guard
      
and 'a case_guard = 
      | Unconditional of 'a expr 
      | Guarded of 'a guard list 
            
and 'a guard = Guard of 'a expr * 'a expr
      
type 'a import = Import of 'a * module_name
      
type re_export = ReExport of module_name * ident

type comment =
  | LineComment of string
  | BlockCommnet of string
  [@@deriving show, eq]

let line_comment com = LineComment com
let block_comment com = BlockCommnet com

type 'a corefn_module = {
  name: module_name;
  path: string;
  span: Source.span;
  imports: 'a import list;
  exports: ident list;
  re_exports: re_export list;
  decls: 'a bind list;
  foreign: ident list;
  comments: comment list
}

type constructor_type = 
  | SumType
  | ProductType
  
type meta = 
  | IsConstructor of constructor_type * ident list
  | IsNewtype
  | IsTypeClassConstructor
  | IsForeign
  | IsWhere
  | IsSyntheticApp
  
type ann = {
  span: Source.span;
  meta: meta option
}

let empty_ann = {
  span = Source.empty_span;
  meta = None;
}

module Decoder = struct
  type json = Yojson.Safe.t

  type decode_error = 
    | TypeMismatch of string
    | MissingValue
    | AtKey of string * decode_error
    | AtIndex of int * decode_error
    | Named of string * decode_error
    | UnexpectedValue of json

  let string_of_decode_error err = 
    let rec go = function
    | TypeMismatch ty -> "  Expected value of type '" ^ ty ^ "'."
    | UnexpectedValue j -> "  Unexpected value " ^ Yojson.Safe.to_string j ^ "."
    | AtKey (key, inner) -> "  At object key \"" ^ key ^ "\":\n" ^ go inner 
    | AtIndex (ix, inner) -> "  At array index " ^ string_of_int ix ^ ":\n" ^ go inner
    | Named (name, inner) -> "  Under " ^ name ^ ":\n" ^ go inner 
    | MissingValue -> "  No value was found."
    in
    "An error occurred while decoding a JSON value:\n" ^ go err

  type 'a t = ('a, decode_error) result

  type 'a decoder = json -> 'a t

  let return = Result.ok
  let throw = Result.error

  let (let*) = Result.bind
  
  let (<$>) = Result.map

  let (|$>) a b = Result.map b a

  let (>=>) f g = fun a -> 
    let* x = f a in
    g x

  let alt a kb =
     match a with
     | Ok _ -> a
     | _ -> kb ()

  let (<|>) = alt

  let rec traverse_list (f : 'a -> 'b t) (xs: 'a list) : 'b list t =
    match xs with
    | [] -> return []
    | (h::tail) ->
      let* x = f h in
      let* rest = traverse_list f tail in
      return (x::rest)
  
  let traverse_keyed (f : 'a -> 'b t) (keyed : (string * 'a)) : (string * 'b) t =
    let (k, v) = keyed in
    let* a = f v in
    return (k, a)
  
  let match_json json
    ?on_null
    ?on_int
    ?on_float
    ?on_string
    ?on_bool
    ?on_list
    ?on_assoc
    ?fail
    ()
    =
    let err = fail |> Option.value ~default:(UnexpectedValue json) in
    let handle h v =
      match h with
      | Some h' -> h' v
      | None -> Result.Error err 
    in
    match json with
    | `Null -> handle on_null ()
    | `Int i -> handle on_int i
    | `Float f -> handle on_float f 
    | `String s -> handle on_string s
    | `Bool b -> handle on_bool b
    | `List v -> handle on_list v
    | `Assoc v -> handle on_assoc v
    | _ -> Result.Error (UnexpectedValue json)
  
  let decode_int json = match_json json ~on_int:Result.ok ~fail:(TypeMismatch "int") ()
  let decode_float json = match_json json ~on_float:Result.ok ~fail:(TypeMismatch "float") ()
  let decode_string json = match_json json ~on_string:Result.ok ~fail:(TypeMismatch "string") ()
  let decode_bool json = match_json json ~on_bool:Result.ok ~fail:(TypeMismatch "bool") ()
  let decode_json_list json = match_json json ~on_list:Result.ok ~fail:(TypeMismatch "list") ()
  let decode_json_object json = match_json json ~on_assoc:Result.ok ~fail:(TypeMismatch "object") ()

  let get_field decoder (obj : (string * json) list) field = 
    match obj |> List.find_opt (fun (k, _) -> k = field) with
    | None -> Result.Error (AtKey (field, MissingValue))
    | Some (k, json) -> decoder json |> Result.map_error (fun e -> AtKey (k, e))

  let get_field_optional' decoder (obj : (string * json) list) field =
    match obj |> List.find_opt (fun (k, _) -> k = field) with
    | None -> return None
    | Some (_, `Null) -> return None
    | Some (k, json) -> decoder json
        |$> Option.some
        |> Result.map_error (fun e -> AtKey (k, e))

  let decode_list decoder json =
    match decode_json_list json with
    | Error err -> Error err
    | Ok jsons ->
      let rec go prev rest = match prev, rest with
        | Error err, _ -> Error err
        | Ok js, [] -> Ok js
        | Ok js, h::tail -> (match decoder h with
          | Ok v -> go (Result.ok @@ v::js) tail
          | Error err -> Error err
          )
      in go (Ok []) jsons |> Result.map List.rev
  
  let decode_comment json = 
    let* obj = decode_json_object json in
    line_comment <$> get_field decode_string obj "LineComment"
      <|> fun _ -> block_comment <$> get_field decode_string obj "BlockComment"

  let decode_record json = 
    let decode_prop decoder j = 
      let* arr = decode_json_list j in
      match arr with
      | [a; b] -> 
        let* prop = decode_string a in
        let* value = decoder b in 
        return @@ Prop(prop, value)
      | _ ->
        throw @@ TypeMismatch "tuple"
    in
    json |> decode_prop |> decode_list

  let decode_literal dec json =
    let* obj = decode_json_object json in
    let* typ = get_field decode_string obj "literalType" in
    match typ with
    | "IntLiteral" -> lit_int <$> get_field decode_int obj "value"
    | "NumberLiteral" -> lit_number <$> get_field decode_float obj "value"
    | "StringLiteral" -> lit_string <$> get_field decode_string obj "value"
    | "CharLiteral" -> (
        let* str = get_field decode_string obj "value" in
        match List.init (String.length str) (String.get str) with
        | [ch] -> return @@ lit_char ch
        | _ -> throw @@ TypeMismatch "char"
        )
    | "BooleanLiteral" -> lit_bool <$> get_field decode_bool obj "value"
    | "ArrayLiteral" -> lit_array <$> get_field (decode_list dec) obj "value"
    | "ObjectLiteral" -> lit_record <$> get_field (decode_record dec) obj "value"
    | _ -> throw @@ TypeMismatch "literal"
  
  let decode_ident json =
    let* str = decode_string json in
    return @@ Ident str

  let decode_module_name json =
    json
      |> decode_list decode_string
      |$> fun paths -> ModuleName (String.concat "." paths)

  let decode_proper_name json =
    let* str = decode_string json in
    return @@ ProperName str

  let decode_qualified k json =
    let* obj = decode_json_object json in
    let* moduleName = get_field_optional' decode_module_name obj "moduleName" in
    let* identifier = get_field k obj "identifier" in
    return @@ Qualified (moduleName, identifier)

  let rec decode_binder dec_ann json =
    let* obj = decode_json_object json in
    let* ann = get_field dec_ann obj "annotation" in
    let* typ = get_field decode_string obj "binderType" in
    match typ with
    | "NullBinder" -> return @@ BinderNull ann
    | "VarBinder" -> get_field decode_ident obj "identifier" |$> (fun b -> BinderVar (ann, b))
    | "LiteralBinder" ->
        get_field (decode_literal (decode_binder dec_ann)) obj "literal"
        |$> fun l -> BinderLit (ann, l)
    | "ConstructorBinder" ->
      let* tyn = get_field (decode_qualified decode_proper_name) obj "typeName" in
      let* ctn = get_field (decode_qualified decode_ident) obj "constructorName" in
      let* binders = get_field (decode_list (decode_binder dec_ann)) obj "binders" in
      return @@ BinderConstructor(ann, tyn, ctn, binders)
    | "NamedBinder" ->
      let* ident = get_field decode_ident obj "identifier" in
      let* binder = get_field (decode_binder dec_ann) obj "binder" in
      return @@ BinderNamed (ann, ident, binder)
    | _ -> throw @@ TypeMismatch "binder"

  let rec decode_expr dec_ann json =
    let* obj = decode_json_object json in
    let* ann = get_field dec_ann obj "annotation" in
    let* typ = get_field decode_string obj "type" in
    match typ with
    | "Var" ->
      let* value = get_field (decode_qualified decode_ident) obj "value" in
      return @@ ExprVar(ann, value)
    | "Literal" ->
      let* lit = get_field (decode_literal (decode_expr dec_ann)) obj "value" in
      return @@ ExprLit (ann, lit)
    | "Constructor" ->
      let* tyn = get_field decode_proper_name obj "typeName" in
      let* con = get_field decode_ident obj "constructorName" in
      let* is = get_field (decode_list decode_string) obj "fieldNames" in
      return @@ ExprConstructor (ann, tyn, con, is)
    | "Accessor" ->
      let* e = get_field (decode_expr dec_ann) obj "expression" in
      let* f = get_field decode_string obj "fieldName" in
      return @@ ExprAccessor (ann, e, f)
    | "ObjectUpdate" ->
        let* e = get_field (decode_expr dec_ann) obj "expression" in
        let* us = get_field (decode_record (decode_expr dec_ann)) obj "updates" in
        return @@ ExprUpdate (ann, e, us)
    | "Abs" ->
        let* idn = get_field decode_ident obj "argument" in
        let* e = get_field (decode_expr dec_ann) obj "body" in
        return @@ ExprAbs (ann, idn, e)
    | "App" ->
        let* e1 = get_field (decode_expr dec_ann) obj "abstraction" in
        let* e2 = get_field (decode_expr dec_ann) obj "argument" in
        return @@ ExprApp (ann, e1, e2)
    | "Let" ->
        let* bs = get_field (decode_list (decode_bind dec_ann)) obj "binds" in
        let* e = get_field (decode_expr dec_ann) obj "expression" in
        return @@ ExprLet (ann, bs, e)
    | "Case" ->
        let* cs = get_field (decode_list (decode_expr dec_ann)) obj "caseExpressions" in
        let* cas = get_field (decode_list (decode_case_alternative dec_ann)) obj "caseAlternatives" in
        return @@ ExprCase (ann, cs, cas)
    | _ -> throw @@ TypeMismatch "expr"

  and decode_binding dec_ann obj =
    let* ann = get_field dec_ann obj "annotation" in
    let* ident = get_field decode_ident obj "identifier" in
    let* expr = get_field (decode_expr dec_ann) obj "expression" in
    return @@ Binding (ann, ident, expr)

  and decode_bind dec_ann json =
    let* obj = decode_json_object json in
    let* typ = get_field decode_string obj "bindType" in
    match typ with
    | "NonRec" ->
        let* x = decode_binding dec_ann obj in
        return @@ NonRec x
    | "Rec" ->
        let* bindings = get_field (decode_list (decode_json_object >=> decode_binding dec_ann)) obj "binds" in
        return @@ Rec bindings
    | _ -> throw@@ TypeMismatch "Bind"

  and decode_case_alternative dec_ann json =
    let* obj = decode_json_object json in
    let* binders = get_field (decode_list (decode_binder dec_ann)) obj "binders" in
    let* is_guarded = get_field decode_bool obj "isGuarded" in
    if is_guarded then
      let* es = get_field (decode_list (decode_guard dec_ann)) obj "expressions" in
      return @@ CaseAlternative (binders, (Guarded es))
    else
      let* e = get_field (decode_expr dec_ann) obj "expression" in
      return @@ CaseAlternative (binders, (Unconditional e))

  and decode_guard dec_ann json =
    let* obj = decode_json_object json in
    let* guard = get_field (decode_expr dec_ann) obj "guard" in
    let* expr = get_field (decode_expr dec_ann) obj "expression" in
    return @@ Guard(guard, expr)

  let decode_import decode_ann' json =
    let* obj = decode_json_object json in
    let* ann = get_field decode_ann' obj "annotation" in
    let* mn = get_field decode_module_name obj "moduleName" in
    return @@ Import (ann, mn)

  let decode_re_exports json =
    let* obj = decode_json_object json in
    let* all = traverse_list (traverse_keyed (decode_list decode_ident)) obj in
    let (>>=) = fun xs k -> List.(map k xs |> flatten) in
    return (all >>= fun (mn, idents) -> List.map (fun id -> ReExport ((ModuleName mn), id)) idents)

  let decode_pos json =
    let* arr = decode_list decode_int json in
    match arr with
    | [l; c] -> Ok Source.{ line = l; column = c }
    | _ -> throw @@ TypeMismatch "tuple"

  let decode_span path json =
    let* obj = decode_json_object json in
    let* st = get_field decode_pos obj "start" in
    let* ed = get_field decode_pos obj "end" in
    return Source.{ path = path; left = st; right = ed }

  let decode_constructor_type json =
    let* str = decode_string json in
    match str with
    | "ProductType" -> return ProductType
    | "SumType" -> return SumType
    | _ -> throw (TypeMismatch "constructor_type")

  let decode_meta json =
    let* obj = decode_json_object json in
    let* typ = get_field decode_string obj "metaType" in
    match typ with
    | "IsConstructor" ->
      let* ct = get_field decode_constructor_type obj "constructorType" in
      let* is = get_field (decode_list decode_ident) obj "identifiers" in
      return @@ IsConstructor (ct, is)
    | "IsNewtype" ->
      return IsNewtype
    | "IsTypeClassConstructor" ->
      return IsTypeClassConstructor
    | "IsForeign" ->
      return IsForeign
    | "IsWhere" ->
      return IsWhere
    | "IsSyntheticApp" ->
      return IsSyntheticApp
    | _ ->
      throw @@ TypeMismatch "meta"

  let decode_ann _path json =
    let* obj = decode_json_object json in
      (* Currently disabled because spans are not used and are a performance drain.
        let* span = get_field (decodeSourceSpan path) obj "sourceSpan" in *)
    let* meta = get_field_optional' decode_meta obj "meta" in
    return { span = Source.empty_span; meta = meta }

  let decode_module_with decode_ann' json =
    let* obj = decode_json_object json in
    let* name = get_field decode_module_name obj "moduleName" in
    let* path = get_field decode_string obj "modulePath" in
    let* span = get_field (decode_span path) obj "sourceSpan" in
    let* imports = get_field (decode_list (decode_import (decode_ann' path))) obj "imports" in
    let* exports = get_field (decode_list decode_ident) obj "exports" in
    let* re_exports = get_field decode_re_exports obj "reExports" in
    let* decls = get_field (decode_list (decode_bind (decode_ann' path))) obj "decls" in
    let* foreign = get_field (decode_list decode_ident) obj "foreign" in
    let* comments = get_field (decode_list decode_comment) obj "comments" in
    return
      { name = name; path = path; span = span; imports = imports
      ; exports = exports; re_exports = re_exports; decls = decls
      ; foreign = foreign; comments = comments
      }
  
  let decode_module = decode_module_with decode_ann

end