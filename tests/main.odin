main :: proc() -> i32 {
  x, y, z: i32 = 10, 20, 30;
  s: string = "world";
  fmt.printf("Hello %s %d modified for proof!\n", s, -x + y * z - 1);
}
