package foo 

factorial :: proc(n: i32) -> i32 #no_bounds_check {
	if (n == 0) do return 1;
	return n * factorial(n - 1);
}

main :: proc() {
	fmt.printf("Factorial of 5 is %d\n", factorial(5));
}