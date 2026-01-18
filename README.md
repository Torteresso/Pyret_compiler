A compiler for a subset of the [pyret language](https://pyret.org/).

Usage
=====
```bash
# To install dependencies :
make dependencies

# To build the compiler :
make

# Command explanations :
./pyretc --help

# Example : to parse a file.arr file :
./pyretc --parse-only main.arr

# To compile a file.arr file to assembly : 
./pyretc file.arr

# To execute the compiled program :

gcc -no-pie file.s -o file && ./file

``
