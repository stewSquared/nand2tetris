# Emulator Planning

## Wish List
- Test assembly translation
- source assembly test programs
- maybe a DSL for assembly programs
- clean up the API for ASM/ML

- run compiled hack binary
- machine language representation
- assembler from ASM to binary
- ASM as a scala DSL or representation?
- option to output text .asm files
- optional -- go directly from ASM rep to ML rep? (probably go to binary first)

## Architecture
- computer state
  - ROM
  - RAM
  - I/O
  - Registers
    - A
    - D
    - PC
    - M (derived)
    - current instruction (derived)

## Steps

1. ML Representation
2. CPU Architecture/State
3. Assembler
4. Visuals
5. Emulator features
