(**
 * AST for EscapeLang
 *)

module Ast = struct
    type program = 
        | Declaration_list of declaration list

    and declaration =
        | Function of function_name * param list * statement list * typ
        | Struct of struct_name * struct_field list

    and function_name = string

    and struct_name = string

    and struct_field_name = string

    and struct_field = struct_field_name * typ

    and param =
        | Param of identifier * typ

    and identifier = string

    and region_name = string

    (**
     * TODO: Implement as kind 
     * @see https://ocaml.org/api/Bigarray.html
     *)
    and locality = Local | Region of region_name | Nonlocal

    and typ =
        | Int
        | Struct_typ of locality * struct_name

    and statement =
        | Struct_alloc of typ * identifier * struct_init
        | Assignment of typ * identifier * expression
        | Return of expression

    and struct_init = (struct_field * expression) list

    and expression =
        | Num of int
        | Plus of expression * expression
        (* TODO: "new" needs locality? *)
        | New of struct_name * struct_init
end

open Ast

(*

TODO: Struct
TODO: Escape via return
TODO: Escape via alias
TODO: Escape via array as in-argument
TODO: Escape via ref in in-argument

struct Point = {
    int x;
    int y;
}

// EscapeLang
Point new_point() {
    local p = new Point {1, 2};
    return p;
}

Point new_points() {
    local ps = [new Point {1, 2}, new Point {3, 4}];
    return ps;
}

// C
Point* new_point() {
    Point __p = {1, 2};
    Point* p = __p;
    return p;
}

Point new_point() {
    local p = new Point {1, 2};
    local q = p;
    return q;
}

void add_point(local Point[] points) {
    local p = new Point {1, 2};
    points[0] = p;
}

int main() {
    return 0;
}

Declaration_list [
    Function (
        "main",
        [],
        [
            Return (Num 0)
        ],
        Int
    )
]
 *)

module StackAllocPass : sig
    val insert_stack_alloc : program -> program
end = struct
    let rec insert_stack_alloc_stmts (stmts : statement list) : statement list = match stmts with
        | [] -> []
        | Assignment (typ, id, expr)::tail ->
            begin match expr with
                | New (struct_name, struct_init) ->
                       Struct_alloc (typ, id, struct_init)
                    :: Assignment (typ, id, expr)
                    :: insert_stack_alloc_stmts tail
                | _ -> failwith "Unsupported assignment structure"
            end
        | s::tail ->
            s :: insert_stack_alloc_stmts tail

    let insert_stack_alloc_decl (decl : declaration) : declaration = match decl with
        | Function (name, params, stmts, t) ->
            Function (name, params, insert_stack_alloc_stmts stmts, t)
        | d -> d

    (**
     * Find all "New" with locality "Local" and insert stack allocations on the line before.
     *
     * @param p program
     * @return program
     *)
    let insert_stack_alloc (p : program) : program = match p with
       | Declaration_list decls -> Declaration_list (List.map (fun decl -> insert_stack_alloc_decl decl) decls)
end

module CGeneratePass = struct
    let typ_to_c (t : typ) : string = match t with
        | Int -> "int"
        | Struct_typ (locality, t) -> t

    let new_to_c (struct_name : string) (struct_init : (struct_field * expression) list) : string = ""

    let expression_to_c (e : expression) : string = match e with
        | Num i -> string_of_int i
        | Plus (_, _) -> failwith "Not implemented: Plus"
        | New (struct_name, struct_init) -> new_to_c struct_name struct_init

    let assignment_to_c (typ : typ) (id : string) (expr : expression) : string =
        typ_to_c typ ^ " " ^ id ^ " = " ^ expression_to_c expr

    let statement_to_c (s : statement) : string = match s with
        | Return ex -> "return " ^ expression_to_c ex ^ ";\n"
        | Struct_alloc (typ, identifier, struct_init) ->
            begin match typ with
                | Struct_typ (Local, name) -> "struct alloc\n"
                | _ -> failwith "Invalid typ in Struct_alloc"
            end
        | Assignment (typ, id, expr) -> 
            begin match typ with
                | Struct_typ (Local, name) ->
                    assignment_to_c typ id expr
                | Int -> assignment_to_c typ id expr
                | _ -> failwith "Not iplemented"
            end

    let struct_field_to_c (field : struct_field) : string = match field with
        | (name, typ) -> typ_to_c typ ^ " " ^ name ^ ";\n"

    let struct_fields_to_c (fields : struct_field list) : string =
        let s = List.fold_left (fun carry struct_field -> carry ^ struct_field_to_c struct_field) "" fields in
        String.sub s 0 (String.length s - 1)

    let struct_to_c (name: string) (fields : struct_field list) : string =
        "typedef struct __" ^ name ^ "{\n" ^
        struct_fields_to_c fields ^ "\n" ^
        "} " ^ name ^ ";\n"

    let function_to_c (name: string) (params : param list) (stmts : statement list) (t: typ) =
        typ_to_c t ^ " " ^
        (* TODO: params *)
        name ^ "(" ^ ") {\n" ^
        (List.fold_left (fun carry stmt -> carry ^ statement_to_c stmt) "" stmts) ^
        "}\n"

    let declaration_to_c (d : declaration) : string = match d with
        | Function (name, params, stmts, t) ->
            function_to_c name params stmts t
        | Struct (name, fields) ->
            struct_to_c name fields

    (**
     * Loop the p AST and spit out C code as a string
     *
     * @param p program
     * @return string
     *)
    let program_to_c (p : program) : string = match p with
       | Declaration_list decls -> List.fold_left (fun carry decl -> carry ^ declaration_to_c decl) "" decls
end

(**
 * Compile with:
 *   ocamlc ast.ml
 *   ./a.out | gcc -xc - -o test1
 *)
let () =
    (*
    let test1 =
        Declaration_list [
            Function (
                "main",
                [],
                [
                    Return (Num 1)
                ],
                Int
            )
        ] in
    *)
    let test2 =
        Declaration_list [
            Struct (
                "Point",
                [
                    ("x", Int);
                    ("y", Int);
                ]
            );
            Function (
                "main",
                [],
                [
                    Assignment (
                        Struct_typ (Local, "Point"),
                        "p",
                        New (
                            "Point",
                            [
                                (("x", Int), Num 1);
                                (("y", Int), Num 2);
                            ]
                        )
                    );
                    Return (Num 1)
                ],
                Int
            )
        ] in
    let test2 = StackAllocPass.insert_stack_alloc test2 in
    print_endline (CGeneratePass.program_to_c test2)
