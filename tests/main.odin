package main

import "core:fmt"

foo :: proc(x: ^i32) {
  x^ = 420;
}

main :: proc() {
  y: i32;
  foo(&y);
  fmt.printf("%d\n", y);
}
