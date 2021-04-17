Toy language

* Opt out of GC with non-escaping stack allocation
* Ensure variables do not escape using escape analysis
* Compiles to C

---

Tools:

* OCaml + Menhir
* Dune
* Compile to C
* Uses OCaml GC for blocked values
* Uses pure C struct for non-escaping values

---

Features:

* Three locality kinds: stack local, region local, non-local (garbage collected)
* Structs
* Variables
* Functions
* Pass-by-reference

TODO: Generics? Array of types

TODO: Alias graph with cyclic references (stack alloc)

TODO: Least privilige when pass-by-reference? Leads to ownership.

Keep track on stack size with getrlimit? And fallback to malloc if too big? Runtime overhead.

If refs can be null, flow-sensitive checking must be done.

---

Three memory strategies:

* Stack allocation (no value types, only references)
* Regions
* Reference count

For three scenarios:

* Stack when you know both size and scope (lifetime), or scope and clear upper bound (like max 3 bullet sprites on screen)
* Region when you know scope but not size
* GC when you don't know scope nor size

Also global variables for known size but no known scope?

```
local p = new Point {1, 2};     // Stack alloc, cannot escape
r = new Region;
let q = new Point {3, 4} in r;  // Region alloc, cannot escape region scope
let s = new Point {6, 7};       // GC, can escape scope
```

TODO: Interaction between different memories?

TODO: How to compile to C?

---

Can use Rust instead, without malloc? Rc or Arc without lifetime annotations?
 
Rust `no_std`: https://rust-embedded.github.io/book/intro/no-std.html

Three layout kinds?

* val - value types allocated on the stack with a fixed lifetime
* [&val - reference to a value type, constrained (only as function arguments to avoid copying? non-aliasing, cannot be used in arrays, collections, etc)]
* ref - reference to heap allocated boxed value, garbage collected

BUT: How to differ between value types that can be referenced, and those that cannot? Do we care?

ref types can only compose of other ref types. val types can only compose other val types.

Arrays are pointers in C, cannot return from a function if stack allocated. But can pass around.

```
function array_test() {
    local points = Point[10];
    let points_with_gc = Point[10];
    return points;  // Invalid because it's an array (or wrap in struct automatically?)
}
```

---

A struct in the code is compiled to both a native C struct (stack alloc) and an OCaml record (GC alloc).

That means they can't point to each other? Better to have OCaml values for everything, making the C-code less readable.

---

Example code:

```
// Struct
struct user = {
    id: int;
};

class MyClass {
    constructor() {
    }
    destructor() {
    }
}
```

```
struct Point {
    x: int;
    y: int;
};

struct Rectangle {
    top_corner: Point;
    bottom_corner: Point;
}

function new_rectangle(val Point p1, ref Point p2): Rectangle {
    // What should happen here?
    // How to differ between val Recangle and ref Rectangle here?
    return Rectangle {
        p1,
        p2  // Won't work, have to clone, or "collapse" to value type?
    };
}

function main(): int {
    p = Point {10, 20};      // Stack allocated
    q = ref Point {20, 30};  // Heap allocated, GC
    t = p.x + q.y;
    return t;
}
```

```
struct tree_node {
   ref tree_node left_Node;
   ref tree_node right_Node;
} tree_node;
```

---

http://zetcode.com/db/mysqlc/

* Can you do this without (explicit) malloc?

    // Can never create a pointer variable like this. Wrap in struct? Forbid malloc use?
    MYSQL_RES *result = mysql_store_result(con);
    // ... Do stuff
    mysql_free_result(result);

    struct MysqlRes = {
    }

    // MYSQL *con = mysql_init(NULL);
    con = mysql_init();
    con = ref mysql_init();  // Not allowed, should never be boxed. Or? If you want it to escape?
    mysql_close(con);  // What if con is boxed here? mysql_close() only accepts val MYSQL?

* Have pointer types only in lib code? And put free() in destructor/when it's out of scope.
 
```
#include <mysql.h>
#include <stdio.h>
#include <stdlib.h>

void finish_with_error(MYSQL *con)
{
  fprintf(stderr, "%s\n", mysql_error(con));
  mysql_close(con);
  exit(1);
}

int main(int argc, char **argv)
{
  MYSQL *con = mysql_init(NULL);

  if (con == NULL)
  {
      fprintf(stderr, "mysql_init() failed\n");
      exit(1);
  }

  if (mysql_real_connect(con, "localhost", "user12", "34klq*",
          "testdb", 0, NULL, 0) == NULL)
  {
      finish_with_error(con);
  }

  if (mysql_query(con, "SELECT * FROM cars"))
  {
      finish_with_error(con);
  }

  MYSQL_RES *result = mysql_store_result(con);

  if (result == NULL)
  {
      finish_with_error(con);
  }

  int num_fields = mysql_num_fields(result);

  MYSQL_ROW row;

  while ((row = mysql_fetch_row(result)))
  {
      for(int i = 0; i < num_fields; i++)
      {
          printf("%s ", row[i] ? row[i] : "NULL");
      }

      printf("\n");
  }

  mysql_free_result(result);
  mysql_close(con);

  exit(0);
}
```

## Resources

Example compiler

https://github.com/SOwens/example-compiler
