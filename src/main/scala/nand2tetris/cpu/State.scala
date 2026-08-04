package nand2tetris
package cpu

type Binary = Int

// represent data, instructions, and addresses with different type tags?

type Memory = Vector[Binary]

case class State(
  rom: Memory, // always instructions?
  ram: Memory, // always data
  a: Binary, // address or data
  d: Binary, // data
  pc: Binary // address
):
  def m: Binary = ram(a)
  def currentInstruction: Binary = rom(pc)
  def execute(inst: ml.Instruction): State = ???
  // TODO: symbolic register values?
  def r0: Binary = ram(0) // maybe optimize later?
  // SP, LCL, ARG, THIS, THAT
  // SCREEN 13384 0x4000 (RAM)
  // KEYBOARD 24576 0x6000
