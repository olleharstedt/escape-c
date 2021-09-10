open Ast
open Printf

exception Parser_error of string
exception Lexer_error of string
exception Internal_error of string

(*

TODO: Struct
TODO: Escape via return
TODO: Escape via alias
TODO: Escape via array as in-argument
TODO: Escape via ref in in-argument
TODO: Use fold_lefti: let fold_lefti f acc xs = List.fold_left (fun (i, acc) x -> (i + 1, f i acc x)) (0, acc) xs`

struct Point = {
    int x;
    int y;
}

function newPoint(): Point
{
    local p = new Point {1, 2};
    return p;
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

function addPoint(local Point point): void
{
}

function newPoint(int x, int y) with r: Point
function newPoint(int x, int y): Point with r
{
    let p = new Point {x, y};
    return p;
}

int main() {
    return 0;
}

function main(): int
{
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

let global_struct_namespace : (string, struct_field list) Hashtbl.t = Hashtbl.create 10
type var_namespace = (string, typ) Hashtbl.t
type local_regions = (string, unit) Hashtbl.t

module type PASS = sig
    type return_t
    val run : program -> return_t
end

module AddStructToNamespacePass : (PASS with type return_t = unit) = struct
    type return_t = unit

    (**
     * Loop through AST and add all structs to the global namespace hashtable.
     *
     * @param p The program AST
     * @return return_t
     *)
    let run (p : program) : return_t = match p with
        | Declaration_list decls ->
            List.iter (fun decl -> match decl with
                | Struct (struct_name, struct_fields) ->
                    Hashtbl.add global_struct_namespace struct_name struct_fields
                | _ -> ()
            ) decls
end

(**
 * This pass checks so that no local variable escapes its scope.
 *)
module LocalEscapePass : (PASS with type return_t = unit) = struct
    type return_t = unit

    (**
     * Assuming variable is local
     *)
    let check_escape_by_return (variable_name : string) (stmts : statement list) =
        List.iter (fun stmt -> match stmt with
            | _ -> ()
        ) stmts

    (**
     * Check if any param has "local" locality, and aborts if it escapes.
     *)
    let check_params (params : param list) (stmts : statement list) : unit =
        List.iter (fun param -> match param with 
            (* TODO: Local string type? Only ref types should check escape, not value types *)
            | Param (Local, id, Struct_typ (_, _)) -> check_escape_by_return id stmts
            | Param _ -> ()
        ) params

    let run (p : program) : return_t = match p with
        | Declaration_list decls ->
            List.iter (fun decl -> match decl with
                | Function (_, params, stmts, _) ->
                    check_params params stmts
                | _ -> ()
            ) decls
end

(**
 * This pass adds stack allocations to the AST.
 *)
module StackAllocPass : (PASS with type return_t = program) = struct
    type return_t = program

    (**
     * Take an expression list, like "1, 2" inside Point {1, 2}, and make a struct_init of it
     *
     * @param name The name of the struct as saved in global namespace
     * @param exprs
     * @return struct_init
     * @TODO Split into separate pass?
     *)
    let exprs_to_struct_init (name : string) (exprs : expression list) : struct_init =
        let struct_fields = match Hashtbl.find_opt global_struct_namespace name with
            | Some s -> s
            | None -> failwith (sprintf "Could not find struct with name %s in global namespace" name)
        in
        (* Must initialize exact number of fields *)
        if List.length struct_fields = List.length exprs
        then
            List.mapi (fun i expr -> match expr with
                | Num n -> ((List.nth struct_fields i), Num n)
                | _ -> failwith "Can only initialize struct with int"
            ) exprs
        else
            failwith (sprintf "Number of initialized fields does not equal total number of fields instruct")

    (**
     * @param stmts
     * @return statement list
     * @TODO Can also be used for pool alloc?
     *)
    let rec insert_stack_alloc_stmts (stmts : statement list) : statement list = match stmts with
        | [] -> []
        | Assignment (typ, id, expr)::tail ->
            begin match expr with
                (* TODO: Nested "new" expressions, e.g. new Rectangle{new Point{1, 2}, new Point{3, 4}} *)
                | New (Struct_typ (Local, struct_name), exprs) ->
                       Struct_alloc (typ, id, exprs_to_struct_init struct_name exprs)
                    :: Assignment (typ, id, expr)
                    :: insert_stack_alloc_stmts tail
                (* For all other expressions, don't change anything *)
                | New (Struct_typ (Regional (Some r), struct_name), exprs) ->
                        Struct_pool_alloc (r, typ, id, exprs_to_struct_init struct_name exprs)
                    :: Assignment (typ, id, expr)
                    :: insert_stack_alloc_stmts tail
                | _ -> Assignment (typ, id, expr)::tail
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
    let run (p : program) : program = match p with
       | Declaration_list decls -> Declaration_list (List.map (fun decl -> insert_stack_alloc_decl decl) decls)
end

(**
 * Replace all Infer_me types with proper types.
 *)
module InferMePass : (PASS with type return_t = program) = struct
    type return_t = program

    (**
     * @param expr
     * @return typ
     *)
    let rec infer_expression (expr : expression) (ns : var_namespace) : typ = match expr with
        | Num _ -> Int
        | Plus (expr, expr2) ->
            let t1 = infer_expression expr ns in
            let t2 = infer_expression expr2 ns in
            if t1 = t2 then t1 else failwith "Expressions in Plus do not have same type"
        | New (struct_typ, exprs) ->
            struct_typ
            (*
            let types = List.map (fun e -> infer_expression e ns) exprs in
            let first_type = List.nth types 0 in
            if List.length (List.filter (fun t -> t <> first_type) types) = 0
            then first_type
            else failwith "Types in expression list do not all have same type"
            *)
        | Variable (locality, id) ->
            (* Check if variable is saved in namespace, and that it's not Infer_me *)
            match Hashtbl.find_opt ns id with
                | Some t -> 
                    begin match t with
                        | Infer_me -> failwith (sprintf "Variable %s has type Infer_me at wrong place" id)
                        | t -> t
                    end
                | None -> failwith (sprintf "Variable %s has no saved type in local namespace" id)


    (**
     * Infer type of statements inside function.
     * Also creates a local variable namespace.
     * TODO: Block scope?
     *
     * @param params Input arguments to function
     * @param stmts List of all statements inside function
     * @param t Return type of function
     * @return statement list Statements with inferred types
     *)
    let infer_statements (params : param list) (stmts : statement list) (t : typ) : statement list =
        let ns : var_namespace = Hashtbl.create 10 in
        List.map (fun s -> match s with
            | Assignment (Infer_me, id, expr) ->
                let new_type = infer_expression expr ns in
                Hashtbl.add ns id new_type;
                Assignment (new_type, id, expr)
            (*| Return of expression*)
            (* Ignore struct alloc *)
            | s -> s
        ) stmts

    (**
     * Run the pass
     *
     * @param p
     * @return program
     *)
    let run (p : program) : program = match p with
       | Declaration_list decls -> Declaration_list
            (List.map
                (fun decl -> match decl with
                    | Function (name, params, stmts, t) -> Function (name, params, infer_statements params stmts t, t)
                    | d -> d
                )
                decls
            )
end

(**
 * Replace all "Region None" with "Region Some <name>"
 *)
module InferRegionName : (PASS with type return_t = program) = struct
    type return_t = program

    (**
     * Function block
     *
     * @param params Input arguments to function
     * @param stmts List of all statements inside function
     * @param t Return type of function
     * @return statement list Statements with inferred types
     *)
    let infer_statements (params : param list) (stmts : statement list) (t : typ) : statement list =
        stmts

    (**
     * Run the pass
     *
     * @param p
     * @return program
     *)
    let run (p : program) : program = match p with
       | Declaration_list decls -> Declaration_list
            (List.map
                (fun decl -> match decl with
                    | Function (name, params, stmts, t) -> Function (name, params, infer_statements params stmts t, t)
                    | d -> d
                )
                decls
            )
end

(**
 * Loop the AST and spit out C code as a string
 *)
module GenerateCPass : (PASS with type return_t = string) = struct
    type return_t = string

    (**
     * Type to C
     *
     * @param t
     * @return string
     *)
    let typ_to_c (t : typ) : string = match t with
        | Int -> "int"
        | Struct_typ (_, t) -> t
        | Infer_me -> failwith "Cannot convert Infer_me to a C type"

    (**
     * New allocation to C
     *
     * @param struct_name
     * @param struct_init
     * @return string
     *)
    let new_to_c (struct_name : string) (struct_init : expression list) : string =
        ignore(struct_name);
        ignore(struct_init);
        ""

    (**
     * Expression to C
     *
     * @param e
     * @return string
     *)
    let expression_to_c (e : expression) : string = match e with
        | Num i -> string_of_int i
        | Plus (_, _) -> failwith "Not implemented: Plus"
        | New (Struct_typ (_, struct_name), exprs) -> new_to_c struct_name exprs
        | Variable (_, id) -> id
        | _ -> failwith "Not implemented: expression_to_c"

    (**
     * Assignment to C
     *
     * @param typ
     * @param id
     * @param expr
     * @return string
     *)
    let assignment_to_c (typ : typ) (id : string) (expr : expression) : string =
        ignore(typ);
        ignore(id);
        ignore(expr);
        ""
        (*typ_to_c typ ^ " " ^ id ^ " = " ^ expression_to_c expr ^ ";\n"*)
        (* TODO: Logic happens in struct_alloc *)

    (**
     * Struct init to C
     *
     * @param init
     * @return string
     *)
    let struct_init_to_c (init : struct_init) : string = 
        let s = List.fold_left (fun carry (field, expression) ->
            carry ^ begin match field, expression with
                | (name, Int), Num n ->
                    sprintf " .%s = %d" name n
                | _, _ -> failwith "Unsupported struct init"
            end
            ^ ","
        ) "" init
        in
        String.sub s 0 (String.length s - 1)

    (**
     * Statement to C
     *
     * @param s
     * @return string
     *)
    let statement_to_c (s : statement) (regs : local_regions) : string = match s with
        | Return ex -> 
                Hashtbl.fold (fun k v acc -> (sprintf "pool_destroy(%s);\n" k) ^ acc) regs "" ^
                "return " ^ expression_to_c ex ^ ";\n"
        | Struct_alloc (typ, identifier, struct_init) ->
            begin match typ with
                | Struct_typ (Local, struct_name) ->
                    sprintf "%s __%s = {%s};\n" struct_name identifier (struct_init_to_c struct_init)
                    ^ sprintf "%s *%s = &__%s;\n" struct_name identifier identifier
                | Struct_typ (Regional None, _) ->
                        failwith (sprintf "Cannot allocate to unnamed region - region name not inferred correctly?")
                | Struct_typ (l, _) -> failwith (sprintf "Invalid Struct_typ in Struct_alloc: %s" (show_locality l))
                | t -> failwith (sprintf "Invalid typ in Struct_alloc: %s" (show_typ t))
            end
        | Struct_pool_alloc (region_name, Struct_typ (_, struct_name), id, struct_init) ->
                sprintf "
if (pool_available(%s) < sizeof(%s)) {
    exit(127);
}
%s* %s = pool_alloc(%s, sizeof(%s));
"
                region_name
                struct_name
                struct_name
                id
                region_name
                struct_name
        | Assignment (typ, id, expr) -> 
            begin match typ with
                | Struct_typ (Local, _) ->
                    assignment_to_c typ id expr
                | Int -> assignment_to_c typ id expr
                | Infer_me -> failwith "Missing inference of assignment type"
                | _ -> "" (*failwith "Not implemented: Assignment, statement_to_c"*)
            end
        | New_region r ->
            (** TODO: Pool size *)
            Hashtbl.add regs r ();
            "POOL* " ^ r ^ " = pool_create(100);"
        | _ -> failwith "Not implemented: statement_to_c"

    (**
     * Struct field to C
     *
     * @param field
     * @return string
     *)
    let struct_field_to_c (field : struct_field) : string = match field with
        | (name, typ) -> typ_to_c typ ^ " " ^ name ^ ";\n"

    (**
     * Struct fields to C
     *
     * @param fields
     * @return string
     *)
    let struct_fields_to_c (fields : struct_field list) : string =
        let s = List.fold_left (fun carry struct_field -> carry ^ struct_field_to_c struct_field) "" fields in
        String.sub s 0 (String.length s - 1)

    (**
     * Struct to C
     *
     * @param name
     * @param fields
     * @return string
     *)
    let struct_to_c (name: string) (fields : struct_field list) : string =
        "typedef struct __" ^ name ^ "{\n" ^
        struct_fields_to_c fields ^ "\n" ^
        "} " ^ name ^ ";\n"

    (**
     * Function to C
     *
     * @param name
     * @param params
     * @param t
     * @return string
     *)
    let function_to_c (name: string) (params : param list) (stmts : statement list) (t: typ) : string =
        (* TODO: params *)
        ignore(params);
        let local_regions : local_regions = Hashtbl.create 10 in
        typ_to_c t ^ " " ^
        name ^ "(" ^ ") {\n" ^
        (** TODO: Factor out scope_to_c or block_to_c *)
        (** TODO: Region scope for if and loops? *)
        let stmts_c = List.fold_left (fun carry stmt -> carry ^ statement_to_c stmt local_regions) "" stmts in
        (* In case no return statement was written, destroy all pools before implicit return *)
        let last_destroy = Hashtbl.fold (fun k v acc -> (sprintf "pool_destroy(%s);\n" k) ^ acc) local_regions "" in
        stmts_c ^ last_destroy ^ "}\n"

    (**
     * Declaration to C
     *
     * @param d
     * @return string
     *)
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
    let run (p : program) : return_t = 
		(*
         * Include some memory pool code
         * @see https://stackoverflow.com/questions/11749386/implement-own-memory-pool
         *)
        "
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

typedef struct pool
{
  char * next;
  char * end;
} POOL;

POOL * pool_create( size_t size ) {
    POOL * p = (POOL*)malloc( size + sizeof(POOL) );
    p->next = (char*)&p[1];
    p->end = p->next + size;
    return p;
}

void pool_destroy( POOL *p ) {
    free(p);
}

// TODO: Not needed probably
void pool_destroy_many(int arg_count, ...) {
    va_list ap;
    va_start(ap, arg_count);
    for (int i = 2; i <= arg_count; i++) {
        POOL* p;
        p = va_arg(ap, POOL*);
        pool_destroy(p);
    }
    va_end(ap);
}

size_t pool_available( POOL *p ) {
    return p->end - p->next;
}

void * pool_alloc( POOL *p, size_t size ) {
    if( pool_available(p) < size ) return NULL;
    void *mem = (void*)p->next;
    p->next += size;
    return mem;
}
" ^
        match p with
            | Declaration_list decls -> List.fold_left (fun carry decl -> carry ^ declaration_to_c decl) "" decls
end

let string_of_token (token : Parser.token) : string =
    let open Parser in
    match token with
        | WITH -> "WITH"
        | STRUCT -> "STRUCT"
        | STRING_LITERAL -> "STRING_LITERAL"
        | SEMICOLON -> "SEMICOLON"
        | RPAREN -> "RPAREN"
        | RETURN -> "RETURN"
        | REGION -> "REGION"
        | RBRACK -> "RBRACE"
        | RBRACE -> "RBRACE"
        | PLUS -> "PLUS"
        | NEW -> "NEW"
        | NAME s  -> "NAME " ^ s
        | MINUS -> "MINUS"
        | LT -> "LT"
        | LPAREN -> "LPAREN"
        | LOCAL -> "LOCAL"
        | LET -> "LET"
        | LBRACK -> "LBRACK"
        | LBRACE -> "LBRACE"
        | INT i -> "INT" ^ string_of_int i
        | GT -> "GT"
        | EQEQ -> "EQEQ"
        | EQ -> "EQ"
        | EOF -> "EOF"
        | CONSTANT -> "CONSTANT"
        | _ -> failwith "Unknown token"


(**
 * Compile with:
 *   dune build comp.exe
 *
 * Run with:
 *   ./_build/default/comp.exe
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
    let ast =
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
                            Struct_typ (Local, "Point"),
                            [
                                Num 1;
                                Num 2;
                            ]
                        )
                    );
                    Return (Variable (Local, "p"))
                ],
                Int
            )
        ] in
    AddStructToNamespacePass.run ast;
    let ast = InferMePass.run ast in
    LocalEscapePass.run ast;
    let ast = StackAllocPass.run ast in
    print_endline (GenerateCPass.run ast);

    (* Testing lexer and parser *)
    let source = "
        struct Point = {int x; int y;}
        function main(): int {
            new region r;
            let p = new @Point{1, 2} in r;
            return 1;
        }
    " in
    (* NAME int NAME main LPAREN RPAREN LBRACE RETURN INT0 SEMICOLON RBRACE *)
    let linebuf = Lexing.from_string source in

    (*
    let rec dump_tokens linebuf =
        let token = Lexer.token linebuf in
        match token with
            | Parser.EOF -> ()
            | t ->
                printf "%s" ((string_of_token t) ^ " ");
                dump_tokens linebuf
    in
    *)

    (* Ignore error at end-of-text *)
    (*try dump_tokens linebuf with e -> ();*)

    (*Lexing.flush_input linebuf;*)
    (*let linebuf = Lexing.from_string source in*)

    (*print_endline (match (Lexer.token linebuf) with *)
    let ast = try (Parser.program Lexer.token linebuf) with
      | Lexer.Error msg ->
          let tok : string = Lexing.lexeme linebuf in
          raise (Lexer_error (sprintf "%s token = %s" msg tok))
      | Parser.Error ->
          let tok = Lexing.lexeme linebuf in
          raise (Parser_error (sprintf "tok = %s, Could not parse '%s': error at %c" tok source (String.get source (Lexing.lexeme_start linebuf))))
          (*raise (Parser_error (sprintf "Could not parse '%s'" source ))*)
      | Failure msg ->
          let open Lexing in
          print_endline msg;
          raise (Internal_error (sprintf "line = %d; col = %d" linebuf.lex_curr_p.pos_lnum linebuf.lex_curr_p.pos_cnum))
    in

    print_endline (show_program ast);
    AddStructToNamespacePass.run ast;
    let ast = InferMePass.run ast in
    LocalEscapePass.run ast;
    let ast = StackAllocPass.run ast in
    print_endline (show_program ast);
    print_endline (GenerateCPass.run ast)
