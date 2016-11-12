# brainfuck-interpreter

A simple brainfuck interpreter written in Haskell.

Invoking the program without any flags will run the program in interpreting mode.

In this mode, you can enter a program on a single line and it will run immediately.

Programs cannot be spread across lines in this mode. Use a file for longer programs.

Hit enter to run the line you just entered.

Note: state is not saved across lines, so you can't enter a long program line by line.

## Flags:

#### -f / --file
Specify a file to run the interpreter on.

Usage: `./brainfuck-interpreter --file=helloworld.bf

This will run the interpreter on the file helloworld.bf, if it is present.
If it isn't, the program will run in interpreting mode.

#### -h / --help
Output a help message.

Usage: `./brainfuck-interpreter --help`

## Installation:

#### Clone the repository
`git clone https://github.com/ZedPea/brainfuck-interpreter.git`

#### Compile
Enter the directory which contains the brainfuck-interpreter.hs and run

`ghc brainfuck-interpreter.hs`

You will need ghc installed. I suggest downloading the haskell platform, which should be in your repositories.

Note that some intermediate compile files will be left around, brainfuck-interpreter.hi, brainfuck-interpreter.o, Utilities.hi, and Utilities.hi. You can delete these if you wish.

#### Running the program
Run

`./brainfuck-interpreter`

in the location where the executable got created.

See the flags section to customize how you use the program.

#### Windows installation

The setup is the same on windows, note that Ctrl+Z is used to exit the interpreter rather than Ctrl+D as is standard.
