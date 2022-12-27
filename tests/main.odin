main :: proc() -> i32 {
	defer fmt.printf("The end\n");
	defer {
		fmt.printf("woah\n");
		fmt.printf("nice\n");
	}
	for x: i32 = 0; x < 10; x += 1 {
		fmt.printf("%d\n", x);
		if x > 1 {
			return 10;
		}
	}
	defer fmt.printf("hi\n");
	return 1;
}
