package main

import "core:fmt"

main :: proc() {
  defer fmt.printf("Exiting ...\n");
  for x in 0..<10 {
    fmt.printf("Hello world %d\n", x);
    if x >= 5 {
      return;
    }
  }
}