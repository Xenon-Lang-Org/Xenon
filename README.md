# Xenon Programming Language

Xenon is a new programming language designed for simplicity and efficiency. This repository contains the source code for the Xenon compiler and interpreter.

## Features

- **Simple Syntax**: Easy to read and write.
- **Strong Typing**: Ensures type safety.
- **Efficient Compilation**: Compiles to WebAssembly for high performance.
- **Interpreter**: Run Xenon programs without compilation.

## Getting Started

### Dependencies/Requirements
- GHC: 9.6.6
- HLS: 2.9.0.1
- Cabal: 3.10.3.0
- Stack: 3.1.1
- Make: 4.3 (Optional, Linux Only)

### Installation

**1. Clone the repository:**

```sh
git clone https://github.com/Xenon-Lang-Org/Xenon.git
```

**2. Build the project:**

*using Make*
```sh
make
```

*using Stack*
```sh
stack build
```

Alternatively, you may build the compiler, vm or interpreter separately *using Make*.

```sh
make xcc
make xrun
make xin
```

### Usage

**To compile a Xenon source file to WebAssembly:**
```sh
./xcc <source-file.xn>
```
**To compile a Xenon source file to a specified output file:**
```sh
./xcc <source-file.xn> -o <output-file.wasm>
```

**To run the interpreter:**
```sh
./xin
```
**To run the interpreter and load Xenon files beforehand:**
```sh
./xin <file1> <file2> ...
```

### Examples
You can find example Xenon programs in the [examples](examples) directory.

### Running Tests

**To run all tests:**

*using Make*
```sh
make tests
```

**To run the unit tests:**

*using Make*
```sh
make unit_tests
```

*using Stack*
```sh
stack test
```

**To run the functional tests:**

*using Make*
```sh
make func_tests
```

*using Bash*
```sh
./test/tester.sh
```

### Documentation
[Documentation Repository](https://github.com/Xenon-Lang-Org/docs)

### Authors
- [Léo Wehrle - Rentmeister](https://github.com/leoWherle)
- [Théodore Magna](https://github.com/TheodoreMagna)
- [Lucien Pineau](https://github.com/mathematisse)
- [Alexis Hachemi](https://github.com/alexishachemi)
- [Karim Mohamed](https://github.com/Kuawhrime)
