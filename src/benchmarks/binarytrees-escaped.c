/**
 * What escape-c should compile down to.
 *
 * Compile with (both steps are needed for full OCaml runtime to be included):
 *   ocamlopt -output-obj -o test.o dummy.ml
 *   cc src/benchmarks/binarytrees-escaped.c test.o -L`ocamlc -where` -lasmrun -lm -ldl
 *
 * @see https://caml.inria.fr/pub/docs/manual-ocaml/intfc.html
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

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
