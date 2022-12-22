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
```
$ ./codin build tests/main.odin -file
```

### Documentation
Before contributing please read the [C0.md](doc/C0.md) and [STYLE.md](doc/STYLE.md) documents in the `doc` directory.

### License
Licensed under the MIT license. View copyright information [here](doc/LICENSE.md)