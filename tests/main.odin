package main

import "core:fmt"

foo :: proc(#any_int x: int) #no_bounds_check {
  printf("hello %d\n", x)
}

main :: proc() {
  foo(42)
}
