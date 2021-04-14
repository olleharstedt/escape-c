(**
 * Testing if it works good to put strings in binary trees to backtrack easily.
 *)

type pre = string list
type post = string list
type string_tree = Content of string list | Pre of string_tree | Post of string_tree

(*
  No new scope, but needs to add to previous line.
    local p = new Point {1, 2};
    Assignment (
        id = p,
        type = ?
        expression = (
            New (
                type = Point,
                fields = Int 1, Int 2
            )
        )
    )

    typedef struct __Point {
        int x;
        int y;
    } Point;
    int main() {
        Point __p = {1, 2};
        Point* p = &__p;
    }

    [
        Stack_alloc (id, Struct_typ ("Point"), fields (Int 1, Int 2)),
        Assignment (
            id = p,
            typ = Struct_typ (Local, "Point"),
            expression = (
                New (
                    type = Point,
                    fields = Int 1, Int 2
                )
            )
        )
    ]

    let rec insert_stack_alloc (stmts : stmt list) = match stmts with
        | [] -> []
        | stmt::tail -> 

    match stmt with
    | Assignment (id, typ, exp) ->
        let (cont, pre, post) = match exp with 
            | New (typ, fields) ->
        in
        pre ^ "let id = " ^ cont ^ post

 *)

type stmt = Assignment | Stack_alloc

let print_stmts (stmts : stmt list) =
    List.iter (fun s -> match s with
        | Assignment -> print_endline "Assignment"
        | Stack_alloc -> print_endline "Stack_alloc"
    ) stmts

let rec insert_stack_alloc (stmts : stmt list) = match stmts with
    | [] -> []
    (* Fetch all New in assignment if locality is "local" *)
    (* TODO: match guard when contains_new *)
    (* TODO: let news = get_all_new_in_expr expr *)
    | Assignment::tail -> Stack_alloc :: Assignment :: insert_stack_alloc tail
    | Stack_alloc::_ -> failwith "Shouldn't visit Stack_alloc in this pass"

let () =
    let stmts = [Assignment; Assignment] in
    print_stmts stmts;
    print_endline "";
    let new_stmts = insert_stack_alloc stmts in
    print_stmts new_stmts
