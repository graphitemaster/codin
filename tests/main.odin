package main

import "core:fmt"

factorial :: proc "contextless" (n: i32) -> i32 {
  if n >= 1 do return n * factorial(n - 1)
  return 1
}

main :: proc "contextless" () {
  fmt.printf("factorial of 21 is %d", factorial(21));
}