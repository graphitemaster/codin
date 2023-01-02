package main

import "core:fmt"

square :: #force_inline proc "contextless" (n: i64) -> i64 {
  return n * 2;
}

main :: proc "contextless" () {
  defer fmt.printf("Exiting ...\n");
  for x := 0; x < square(25); x += 1 {
    fmt.printf("Hello world: %d\n", x);
    if x >= square(5) {
      return;
    }
  }
}