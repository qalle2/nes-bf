# read a Brainfuck program from stdin, write an FCEUX FM2 movie file to stdout

import sys

# see https://fceux.com/web/FM2.html
FM2_HEADER = """\
version 3
emuVersion 20500
rerecordCount 0
palFlag 0
romFilename foo
romChecksum base64:AAAAAAAAAAAAAAAAAAAAAA==
guid 00000000-0000-0000-0000-000000000000
fourscore 0
microphone 0
port0 1
port1 0
port2 0
FDS 0
NewPPU 1
RAMInitOption 0
RAMInitSeed 569937536"""

# BF instructions to NES buttons
INSTRUCTIONS = {
    "<": "L",
    ">": "R",
    "-": "D",
    "+": "U",
    ",": "SB",
    ".": "SA",
    "[": "B",
    "]": "A",
}

def read_program():
    # read Brainfuck program from stdin
    program = ""
    for line in sys.stdin:
        program += "".join(c for c in line if c in INSTRUCTIONS)
    return program

def fm2_line(buttons=""):
    # format FM2 line (buttons pressed on controller 1)
    # none: "|0|........|||"
    # all:  "|0|RLDUTSBA|||" (T = start)
    buttons2 = "".join((b if b in buttons else ".") for b in "RLDUTSBA")
    return "|0|" + buttons2 + "|||"

def print_fm2(program):
    # print an FM2 file that inputs the Brainfuck program

    print(FM2_HEADER)
    for i in range(5):
        print(fm2_line())

    prevInstr = ""
    for instr in program:
        if instr == prevInstr:
            print(fm2_line())
        else:
            prevInstr = instr
        print(fm2_line(INSTRUCTIONS[instr]))

def main():
    program = read_program()
    if len(program) > 239:
        sys.exit("Program too long.")
    print_fm2(program)

main()
