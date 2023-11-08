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

let empty_pos = { line = 0; column = 0 }

let empty_span = { path = ""; left = empty_pos; right = empty_pos }

let span s1 s2 = { path = s1.path; left = s1.left; right = s2.right }
