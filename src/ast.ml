(**
 * AST for EscapeLang
 *)

type program = 
    | Declaration_list of declaration list
[@@deriving show]

(**
 * TODO: Implement as kind 
 * @see https://ocaml.org/api/Bigarray.html
 *)
and locality =
    | Local
    | Region of region_name
    | Nonlocal
    | Unknown (* First pass might have un-propagated locality allocations; TODO: Use option? *)

(** TODO: Differ between value type and reference type? Always pass by reference except primitive types (int, string) *)
and typ =
    | Int
    | Struct_typ of locality * struct_name
    | Infer_me

and param =
    | Param of locality * identifier * typ

(** Top-level constructs *)
and declaration =
    | Function of function_name * param list * statement list * typ
    | Struct of struct_name * struct_field list

and function_name = string

and struct_name = string

and struct_field_name = string

and struct_field = struct_field_name * typ

and identifier = string

and region_name = string

and statement =
    (* Struct_alloc is an internal statement used by C pass *)
    | Struct_alloc of typ * identifier * struct_init
    (* let a = ...; *)
    | Assignment of typ * identifier * expression
    (* return ...; *)
    | Return of expression
    (* Function_call that returns void *)
    (* If-statement, or only if-expression *)
    (* While-loop *)

and struct_init = (struct_field * expression) list

and expression =
    | Num of int
    | Plus of expression * expression
    (* TODO: "new" needs locality? *)
    | New of struct_name * expression list
    | Variable of locality * identifier
    (*
    | Struct_access of expression * expression
    | Function_call of ...
    *)

let type_of_string s = match s with
    | "int" -> Int
    | s -> Struct_typ (Unknown, s)

let string_of_typ (t : typ) : string = match t with
    | Int -> "Int"
    | Struct_typ (_, _) -> "Struct_typ"
    | Infer_me -> "Infer_me"
