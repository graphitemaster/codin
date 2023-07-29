package test

main :: proc() {
  x: #soa #simd [4][4]int
  #no_bounds_check x[0] = 1
  #no_bounds_check if true {
    x[1] = 0
  }
  call()
}