package main

// TODO:
//  struct, union, enum
//  attributes (@static, @private, etc)
//  switch
//  expression directives (partial, assert, panic, unroll)
//  procedure groups
//  foreign decls

import "core:fmt"

m :: proc() -> int {
  return 0
}

x :: proc() {
 x, y, z := 10, 20, 30.0
 x = cast(int)z
 z = transmute(f64)y
 y = auto_cast z
 when true {
  defer x = 0
 } else {
  defer y = 1
 }
 if true {
  fmt.printf("Hello, world!\n")
 } else do x = 0
 for i in 0..<10 do x += m()
}