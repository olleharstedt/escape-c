#include <stdio.h>

typedef struct _point {
    int x, y;
} point;

point new_point() {
    return (point) {10, 20};
}

int main() {
    point* p = &(new_point());
    return 0;
}
