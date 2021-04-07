(**
 * AST for EscapeLang
 *)

type program = 
    | Declaration_list of declaration list

and declaration =
    | Function of function_name * param list * statement list * typ

and function_name = string

and param =
    | Param of identifier * typ

and identifier = string

and typ =
    | Int

and statement =
    | Assigment of identifier * expression
    | Return of expression

and expression =
    | Num of int
    | Plus of expression * expression

(*
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

let ast_to_c (ast : program) : string = 
    ""

let program_to_c (p : program) : string = match p with
   | _ -> ""

let typ_to_c (t : typ) : string = match t with
    | Int -> "int"

let expression_to_c (e : expression) : string = match e with
    | Num i -> string_of_int i
    | Plus (_, _) -> failwith "Not implemented: Plus"

let statement_to_c (s : statement) : string = match s with
    | Return ex -> expression_to_c ex
    | _ -> failwith "Not implemented"

let declaration_to_c (d : declaration) : string = match d with
    | Function (name, params, stmts, t) ->
        typ_to_c t ^ " " ^
        (* TODO: params *)
        name ^ "(" ^ ") {\n" ^
        (List.fold_left (fun carry stmt -> carry ^ statement_to_c stmt) "" stmts) ^
        "}\n"

