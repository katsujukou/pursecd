
let () =
  let open Pursecd_purescript in
  let json = Yojson.Safe.from_file "/Users/koto/Projects/purescript-graphql-codefirst/output/GraphQL.Schema.Convert/corefn.json" in
  let emod = Corefn.Decoder.decode_module json in
  match emod with
  | Error e -> print_endline @@ Corefn.Decoder.string_of_decode_error e
  | Ok _ -> print_endline "Done!"