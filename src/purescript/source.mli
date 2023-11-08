type pos = {
  line: int;
  column: int;
}
[@@deriving eq, show]

type span = {
  path: string;
  left: pos;
  right: pos;
}
[@@deriving eq, show]

val empty_pos : pos 

val empty_span : span 

val span : span -> span -> span