package main

// TODO:
//  distinct
//  struct, union, enum
//  attributes (@static, @private, etc)
//  switch
//  expression directives (partial, assert, panic, unroll)
//  procedure groups
//  foreign decls
//  implicit selector expressions (.ENUM inference)

import "core:fmt"

e :: proc() -> (x: [2]int) {
  return {};
}

b :: proc(x: int) {
  e := 0;
  {
    f := 0;
  }
}