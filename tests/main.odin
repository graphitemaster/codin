package main

import "core:fmt"

foo :: proc() {
  fmt.printf("Hello world\n");
}

main :: proc() #no_bounds_check {
  for {}
  for i in 0..<10 {}
  for in 0..<10 {}
  for i := 0; i < 10; i += 1 {}
  for true {}
  a := 10;
  b, c := 10, 20;
  d := "hello world";
  foo();
}