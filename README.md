# Turtle

This is a buildtool for the [turtle graphics programming language](https://computerix.info/comp-bau/turtle.pdf).
It can be used to run, debug and compile turtle programs.
To start using turtle, run `cargo install --git https://github.com/ba-lindner/turtle.git`.

## Usage

### Check syntax

To simply check whether a source file is a valid turtle graphics program, use `turtle check <file>`.

### Run program

To interpret a program, run `turtle run <file> [-- <args..>]`. Everything following `--` is passed as arguments to the turtle program.

### Debug program

You can start the debugger with `turtle debug <file> [-b <breakpoint>] [-- <args..>]`. Breakpoints must be given in the format of `line,column`. Once running, you can use different keys to start/stop execution, step through the program or pring debug information.

### Compile program

> ⛔ this doesn't work as of now

Depending on what will be implemented in the future, this will either simply produce a single `.c` file that needs to be linked with SDL2 or (if an SDL2 installation is found) also include the linking step.

Already implemented is the option to include `printf`-statements for debugging purposes.