# codin

An Odin to C compiler written in C.

### Building

To build on Linux
```
$ make
```

> On Linux you have some additional build options provided by the Makefile. Check the documentation at the top of the `Makefile` for them.

To build on Windows
```
$ cl.exe unity.c
```

### Running
Currently only the `build` and `run` commands are supported and only for a single source file.
```
$ ./codin build tests/main.odin -file
$ ./codin run tests/main.odin -file
```

#### Windows
On Windows you'll need to have Visual Studio installed. Just run `codin` from the __x64 Native Tools Command Prompt for VS__.

#### Linux
On Linux you'll need to have `gcc` installed and in your `PATH`.

### Example
<details>
  <summary>Click to see an example of how codin translates</summary>
  
  ### Odin
  ```odin
  main :: proc() -> i32 {
    x, y, z: i32 = 10, 20, 30;
    s: string = "world";
    fmt.printf("Hello %s %d\n", s, -x + y * z - 1);
  }
  ```

  ### Generated C0
  ```c
  #include <stdio.h>
  
  typedef int i32;
  typedef const char *string;
  
  #if defined(_MSC_VER)
  	#define FORCE_INLINE __forceinline
  #else
  	#define FORCE_INLINE __attribute__((always_inline)) inline
  #endif
  
  FORCE_INLINE i32 negi32(i32 value) {
    return -value;
  }
  
  FORCE_INLINE i32 addi32(i32 lhs, i32 rhs) {
    return lhs + rhs;
  }
  
  FORCE_INLINE i32 subi32(i32 lhs, i32 rhs) {
    return lhs - rhs;
  }
  
  FORCE_INLINE i32 muli32(i32 lhs, i32 rhs) {
    return lhs * rhs;
  }
  
  i32 main() {
    i32 x = 10;
    i32 y = 20;
    i32 z = 30;
    string s = "world";
    printf("Hello %s %d\n", s, subi32(addi32(negi32(x), muli32(y, z)), 1));
  }
  ```
</details>

### Documentation
Before contributing please read the [C0.md](doc/C0.md) and [STYLE.md](doc/STYLE.md) documents in the `doc` directory.

### License
Licensed under the MIT license. View copyright information [here](doc/LICENSE.md)