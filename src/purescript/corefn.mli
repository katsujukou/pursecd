type proper_name = ProperName of string

type module_name = ModuleName of string

type 'a qualified = Qualified of module_name option * 'a 

val map_qualified : ('a -> 'b) -> 'a qualified -> 'b qualified 

type ident = Ident of string

type re_exort = ReExport of module_name * ident

type 'a prop = Prop of string * 'a

val map_prop : ('a -> 'b) -> 'a prop -> 'b prop

val fold_left_prop : ('a -> 'b -> 'a) -> 'a -> 'b prop -> 'a

val fold_right_prop : ('a -> 'b -> 'b) -> 'b -> 'a prop -> 'b

val prop_key : 'a prop -> string

val prop_value : 'a prop -> 'a

val find_prop : string -> 'a prop list -> 'a option

type 'a literal =
  | LitInt of int
  | LitNumber of float 
  | LitString of string
  | LitChar of char
  | LitBoolean of bool
  | LitArray of 'a list 
  | LitRecord of 'a prop list 
  
val map_literal : ('a -> 'b) -> 'a literal -> 'b literal

type 'a binder =
  | BinderNull of 'a
  | BinderVar of 'a * ident
  | BinderNamed of 'a * ident  * 'a binder 
  | BinderLit of 'a * 'a binder literal
  | BinderConstructor of 'a * (proper_name qualified) * (ident qualified) * ('a binder list)

val map_binder : ('a -> 'b) -> 'a binder -> 'b binder 

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

val line_comment : string -> comment
val block_comment : string -> comment

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

val empty_ann : ann

module Decoder : sig
  type json = Yojson.Safe.t

  type decode_error = 
    | TypeMismatch of string
    | MissingValue
    | AtKey of string * decode_error
    | AtIndex of int * decode_error
    | Named of string * decode_error
    | UnexpectedValue of json

  val string_of_decode_error : decode_error -> string

  type 'a t = ('a, decode_error) result
  
  type 'a decoder = json -> 'a t
  
  val decode_module : json -> (ann corefn_module) t
  
  val decode_module_with : (string -> json -> 'a t) -> json -> ('a corefn_module) t
end