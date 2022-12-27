main :: proc() -> i32 {
	fmt.printf("%s\n", #load("tests/main.odin", cstring));
}