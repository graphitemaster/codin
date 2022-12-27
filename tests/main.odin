main :: proc() -> i32 {
	for x: int = 0; x < 10; x += 1 {
		fmt.printf("%d\n", x);
	}
}