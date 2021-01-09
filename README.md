Toy language

* Only allow stack allocation with a fixed lifetime
* Ensure memory does not escape using escape analysis
* Integrate with PHP standard library

---

Tools:

* OCaml + Menhir
* Compile to C
* Integrate with PHP standard library

---

Features:

* Structs
* Variables
* Functions

Keep track on stack size with getrlimit? And fallback to malloc if too big? Runtime overhead.

Can use Rust instead, without malloc?

---

Example code:

```
// Struct
struct user = {
    id: int;
};
```
