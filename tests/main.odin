package main

import "core:fmt"

factorial :: proc(n: i64) -> i64 {
  defer fmt.printf("what")
  if n >= 1 do return n * factorial(n - 1)
  return 1
}

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