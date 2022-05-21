# Qalle's Brainfuck

A Brainfuck interpreter for the [NES](https://en.wikipedia.org/wiki/Nintendo_Entertainment_System).

Table of contents:
* [List of files](#list-of-files)
* [How to use](#how-to-use)
* [Character set](#character-set)
* [Limitations](#limitations)
* [Misc notes](#misc-notes)
* [References](#references)

## List of files
* `assemble.sh`: a Linux script that assembles the program (warning: deletes files)
* `bf.asm`: source code (assembles with [ASM6](https://www.romhacking.net/utilities/674/))
* `bf.nes.gz`: assembled program (iNES format, gzip compressed)
* `examples.txt`: Brainfuck programs
* `snap*.png`: screenshots

## How to use
There are two modes:
* edit mode:
  * program starts in this mode
  * cursor (blinking square) is always on program input area
* run mode:
  * if cursor not visible: Brainfuck program finished or reached maximum output size
  * if cursor on output area: Brainfuck program running
  * if cursor on virtual keyboard: user expected to input character
  * press B at any time to return to edit mode

Edit mode and run mode:

![edit mode](snap1.png)
![run mode](snap2.png)

## Character set
* output behavior:
  * byte `0x0a`: moves cursor to start of next line
  * other bytes: advance cursor by one
* output appearance:
  * bytes `0x20` to `0x7e`: as in ASCII
  * some bytes from `0x7f` on: special characters
  * other bytes: blank
* input (virtual keyboard):
  * newline symbol (&#x21b5; at bottom right): byte `0x0a`
  * other symbols: bytes `0x20` to `0x7e`

## Limitations
* maximum program size: 255 instructions
* Brainfuck RAM size: 1 KiB
* maximum output size: 256 bytes
* output speed: 1 character/frame

## Misc notes
* Brainfuck program won't run if brackets don't match
* Brainfuck program stops when end is reached or 256 characters have been printed
* Uses CHR RAM. Actual PRG ROM size is 2 KiB.

## References
* [Wikipedia &ndash; Brainfuck](https://en.wikipedia.org/wiki/Brainfuck)
* [Esolang &ndash; Brainfuck](https://esolangs.org/wiki/Brainfuck)
* [NESDev Wiki](https://www.nesdev.org/wiki/)
