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
* All arguments to functions are passed by reference (except int/float)?
* Destructors?
* Constructor? Everything needs a default value (Rust).
* Exceptions? Result struct, new for each value (without generics/malloc)?
* Null?
* Mutability?
* Optional arguments? Only with NULL? Needs flow-sensitive typing or option type (can't without malloc/GC)
* Interfaces with structural typing?
* Memory pools from static buffers = malloc BUT can't use without pointers

Keep track on stack size with getrlimit? And fallback to malloc if too big? Runtime overhead.

If refs can be null, flow-sensitive checking must be done.

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
