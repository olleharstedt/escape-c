/**
 * What escape-c should compile down to.
 *
 * Compile with:
 *   gcc -L/usr/lib/ocaml -lcamlrun -lm -ldl src/benchmarks/binarytrees-escaped.c
 *
 * @see https://caml.inria.fr/pub/docs/manual-ocaml/intfc.html
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>

void foo(value v1, value v2, value v3)
{
    CAMLparam3 (v1, v2, v3);
    CAMLreturn0;
}
