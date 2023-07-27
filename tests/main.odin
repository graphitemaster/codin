package main

import "core:fmt"

main :: proc() #no_bounds_check {
  {
    x :: proc() {
      {

      }
    }
  }
}