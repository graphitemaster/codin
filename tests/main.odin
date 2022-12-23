main :: proc() -> i32 {
  x, y, z: i32 = 10, 20, 30
  fmt.printf("Hello world %d\n", -x + y * z - 1);
}
