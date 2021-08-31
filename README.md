Toy language

* Opt out of GC with non-escaping stack allocation
* Ensure variables do not escape using escape analysis
* Compiles to C

---

Tools:

* OCaml + Menhir
* Dune
* Compile to C
* Uses ref counting for non-local variables
* Uses pure C struct for local variables

---

Features:

* Three locality kinds: stack local, region local, non-local (garbage collected)
* Structs
* Variables
* Functions
* Pass-by-reference by default

TODO: Generics? Array of types

TODO: Alias graph with cyclic references (stack alloc)

TODO: Least privilige when pass-by-reference? Leads to ownership.

TODO: External locality kind, for mallocs() from the outside, which insert free() at end of scope?

TODO: Flow-sensitive for null types?

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

* Pass-by-reference by default (as in Java)

```
local p = new Point {1, 2};            // Stack alloc, cannot escape
let p = new Point {1, 2} in stack;     // Stack alloc, cannot escape
let p = new Point {1, 2} locally;
let p = new ~Point {1, 2};
loc p = new Point {1, 2};
let r = new Region;
let q = new Point {3, 4} in r;  // Region alloc, cannot escape region scope
new region;
let q = new @Point {3, 4};      // Can only be one region at a time
let s = new Point {6, 7};       // GC, can escape scope
let rect = new ~Rectangle{new ~Point{1, 2}, new ~Point{3, 4}};
let rect = new @Rectangle{new @Point{1, 2}, new @Point{3, 4}};
let area = rect_area(rect);
let dist = distance(new ~Point{1, 2}, new ~Point{3, 4});
let dist = distance(new Point{1, 2} in stack, new Point{3, 4} in stack);
let dist = distance(new Point{1, 2} in r, new Point{3, 4} in r);
let dist = distance(new Point{1, 2}, new Point{3, 4});
function area(shape): int with region <-- Assumes a region is created at top, and freed at bottom of function. Needed to use @-allocation.
What if user wants a hierachy of regions?

function main() with s, t
{
    if (rand()) {
        // Region MUST be block scoped
        new region r;
    }
    area() with r;
}

function distance(Point p1, Point p2)
{
    return square(abs(p1), abs(p2));
}

function rect_area(Rectangle r): float
{
    return r.p + r.q;
}
```

TODO: Interaction between different memories?

TODO: How to know which memory allocation strat is allowed for a function?

TODO: A function that takes three args: stack allocated, region alloc and ref count alloc. What does the func need to know?

TODO: Array concatenation for stack, reg and refcount alloc.

function foo(Stack s, Reg r, Refcount rc): void
{
}

---

Can use Rust instead, without malloc? Rc or Arc without lifetime annotations?
 
Rust `no_std`: https://rust-embedded.github.io/book/intro/no-std.html

Arrays are pointers in C, cannot return from a function if stack allocated. But can pass around.

```
function array_test() {
    local points = Point[10];
    let points_with_gc = Point[10];
    return points;  // Invalid because it's an array (or wrap in struct automatically?)
}
```

---

## Example code

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

function new_rectangle(Point p1, Point &p2): Rectangle {
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

```
function foo(local Point p): void {
    return p;  // Fails, local point cannot escape
}

void foo(local Point p) {
    local points = [p, p];
    return points;  // Fails
}

void foo(local Point p) {
    local q = p;
    return q;  // Fails
}

void foo(local Point p) {
    let q = p;  // Fails, cannot alias local from non-local variable
}

struct Address {
    int zipcode;
}

struct Person {
    int id;
    Address address;
}

void foo(local Person p) {
    return p.address;  // Fails, field in local variable
    return p.address.zipcode;  // OK, value type
    return 1 + p.address.zipcode;  // OK, value type
    return p.address.street;  // Fails
    return copy p.address.street;  // OK??
}

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

C parser in Menhir

https://github.com/jhjourdan/C11parser
