
open Forester_parser.Parse
open Forester_core
open Forester_core.Code
open Forester_core.Range
open Forester_core.Reporter
open Forester_compiler.Parse
open Ppx_yojson_conv_lib.Yojson_conv.Primitives


type intervalTree = 
    {
      name      : string;
      start_pos : int * int;
      end_pos   : int * int;
      children  : intervalTree list;
    }
    [@@deriving yojson, show ]
    [@@warning "-69"]

let rec create_interval_tree (name : string) (range : Range.t) (x : Code.t) =
    let rec folder = fun list a -> match (a.value, a.loc) with
    | (Subtree (Some nm, sn), Some loc) -> create_interval_tree nm loc sn :: list
    | (Subtree (_, sn), _) -> List.fold_left folder list sn
    | _ -> list
    in let subtrees = List.fold_left folder [] x 
    in  match (view range) with
      | `Range (a, b) -> 
        { name = name;
          start_pos = (begin_line_num range,begin_offset range);
          end_pos = (end_line_num range, end_offset range);
          children = subtrees; 
        } 

let () 
    = let tree = Sys.argv.(1) in
      easy_run (fun () -> 
      let lexbuf = Lexing.from_string tree in
      (* lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = path}; *)
      let code = parse lexbuf in
      match code with
        | Ok t        ->  (match (List.nth t 0).loc with
            | Some src -> let a = create_interval_tree "" src t 
                          in print_endline (Yojson.Safe.to_string (`List (List.map yojson_of_intervalTree a.children)))
            | _ -> print_endline "[]")
        | Error e -> emit_diagnostic e)

