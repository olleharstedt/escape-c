/**
 * What escape-c should compile down to.
 *
 * Compile with (both steps are needed for full OCaml runtime to be included):
 *   ocamlopt -output-obj -o test.o dummy.ml
 *   cc src/benchmarks/binarytrees-escaped.c test.o -L`ocamlc -where` -lasmrun -lm -ldl
 *
 * @see https://caml.inria.fr/pub/docs/manual-ocaml/intfc.html
 * @see https://dev.realworldocaml.org/runtime-memory-layout.html
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

/*

	0 to No_scan_tag−1	A structured block (an array of OCaml objects). Each field is a value.
	Closure_tag			A closure representing a functional value. The first word is a pointer to a piece of code, the remaining words are value containing the environment.
	String_tag			A character string or a byte sequence.
	Double_tag			A double-precision floating-point number.
	Double_array_tag	An array or record of double-precision floating-point numbers.
	Abstract_tag		A block representing an abstract datatype.
	Custom_tag			A block representing an abstract datatype with user-defined finalization, comparison, hashing, serialization and deserialization functions attached. 

   type t =
   | A             (* First constant constructor -> integer "Val_int(0)" *)
   | B of string   (* First non-constant constructor -> block with tag 0 *)
   | C             (* Second constant constructor -> integer "Val_int(1)" *)
   | D of bool     (* Second non-constant constructor -> block with tag 1 *)
   | E of t * t    (* Third non-constant constructor -> block with tag 2 *)

	caml_alloc(n, t) returns a fresh block of size n with tag t. If t is less than No_scan_tag, then the fields of the block are initialized with a valid value in order to satisfy the GC constraints. 

    Store_field (b, n, v) stores the value v in the field number n of value b, which must be a block (i.e. Is_block(b) must be true).
*/

void foo(value v1, value v2, value v3)
{
    CAMLparam3 (v1, v2, v3);
    CAMLreturn0;
}


void init(void) {
	char* dummy = '\0';
	caml_main(&dummy);
}

int main(int argc, char ** argv)
{
	init();
    caml_startup(argv);
    return 1;
}
