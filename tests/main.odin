bar :: proc() -> i32 {
  fmt.printf("called bar()\n");
}

main :: proc() -> i32 {
  false: i32 = 0;
  true: i32 = 1;
  x, y, z: i32 = 10, 20, 30;
  s: string = "world";
  fmt.printf("Hello %s %d modified for proof!\n", s, -x + y * z - 1);
  if true {
    fmt.printf("do print this\n");
    bar();
    if false {
      fmt.printf("do not print this\n");
    } else {
      fmt.printf("do print this\n");
    }
  } else do fmt.printf("do not print this\n");
}
