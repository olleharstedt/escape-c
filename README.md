Toy language

* Only allow stack allocation
* Ensure memory does not escape using escape analysis
* Integrate with PHP standard library

---

Tools:

* OCaml + Menhir
* Compile to C

---

Features:

* Structs
* Functions

Keep track on stack size with getrlimit? And fallback to malloc if too big? Runtime overhead.
