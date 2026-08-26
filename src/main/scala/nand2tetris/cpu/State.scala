package nand2tetris
package cpu

type Binary = Int

// represent data, instructions, and addresses with different type tags?

type Memory = Vector[Binary]

case class State(
  // rom: Memory, // always instructions?
  rom: Vector[ml.Instruction], // always instructions?
  ram: Memory, // always data
  a: Binary, // address or data
  d: Binary, // data
  pc: Binary // address
):
  def m: Binary = ram(a)
  // def currentInstruction: Binary = rom(pc)
  // def currentInstruction: ml.Instruction = ml.Instruction.parse(rom(pc))
  def currentInstruction: ml.Instruction = rom(pc)

  private def tick: State = copy(pc = pc + 1)
  private def jump: State = copy(pc = a)
  private def setA(n: Binary): State = copy(a = n)
  private def setD(n: Binary): State = copy(d = n)
  private def setM(n: Binary): State = copy(ram = ram.updated(a, n))

  import util.chaining.*

  def calcComp(comp: ml.Comp): Binary = comp match
    case ml.Comp(fromMem, zx, nx, zy, ny, f, no) =>
      val x = d.pipe: x =>
        if zx then 0 else x
      .pipe: x =>
        if nx then ~x else x
      val y = (if fromMem then m else a).pipe: y =>
        if zy then 0 else y
      .pipe: y =>
        if ny then ~y else y
      val binop = if f then x + y else x & y
      if no then ~binop else binop

  def setDest(dest: ml.Dest, value: Binary): State = dest match
    case ml.Dest(a, d, m) =>
      this.pipe: state =>
        if a then state.setA(value) else state
      .pipe: state =>
        if d then state.setD(value) else state
      .pipe: state =>
        if m then state.setM(value) else state

  def movePC(jump: ml.Jump, result: Binary): State = jump match
    case ml.Jump.Null => this.tick
    case ml.Jump.JGT => if result > 0 then this.jump else this.tick
    case ml.Jump.JEQ => if result == 0 then this.jump else this.tick
    case ml.Jump.JGE => if result >= 0 then this.jump else this.tick
    case ml.Jump.JLT => if result < 0 then this.jump else this.tick
    case ml.Jump.JNE => if result != 0 then this.jump else this.tick
    case ml.Jump.JLE => if result <= 0 then this.jump else this.tick
    case ml.Jump.JMP => this.jump

  def step: State = currentInstruction match
    case ml.AInst(n) => setA(n).tick
    case ml.CInst(comp, dest, jump) =>
      val compValue = calcComp(comp)
      this.setDest(dest, compValue).movePC(jump, compValue)

  // virtual registers r0 - r15
  def r0 = ram(0)
  def r1 = ram(1)
  def r2 = ram(2)
  def r3 = ram(3)
  def r4 = ram(4)
  def r5 = ram(5)
  def r6 = ram(6)
  def r7 = ram(7)
  def r8 = ram(8)
  def r9 = ram(9)
  def r10 = ram(10)
  def r11 = ram(11)
  def r12 = ram(12)
  def r13 = ram(13)
  def r14 = ram(14)
  def r15 = ram(15)

  def setR0(n: Binary) = this.copy(ram = ram.updated(0, n))
  def setR1(n: Binary) = this.copy(ram = ram.updated(1, n))
  def setR2(n: Binary) = this.copy(ram = ram.updated(2, n))
  def setR3(n: Binary) = this.copy(ram = ram.updated(3, n))
  def setR4(n: Binary) = this.copy(ram = ram.updated(4, n))
  def setR5(n: Binary) = this.copy(ram = ram.updated(5, n))
  def setR6(n: Binary) = this.copy(ram = ram.updated(6, n))
  def setR7(n: Binary) = this.copy(ram = ram.updated(7, n))
  def setR8(n: Binary) = this.copy(ram = ram.updated(8, n))
  def setR9(n: Binary) = this.copy(ram = ram.updated(9, n))
  def setR10(n: Binary) = this.copy(ram = ram.updated(10, n))
  def setR11(n: Binary) = this.copy(ram = ram.updated(11, n))
  def setR12(n: Binary) = this.copy(ram = ram.updated(12, n))
  def setR13(n: Binary) = this.copy(ram = ram.updated(13, n))
  def setR14(n: Binary) = this.copy(ram = ram.updated(14, n))
  def setR15(n: Binary) = this.copy(ram = ram.updated(15, n))

  def sp = ram(0) // stack pointer
  def lcl = ram(1) // local segment base address
  def arg = ram(2) // argument segment base address
  def ths = ram(3) // this segment base address
  def tht = ram(4) // that segment base address
  // def screen = ram(0x4000)
  // def keyboard = ram(0x6000)

object State:
  // def init(rom: Memory): State =
  //   State(rom=rom, ram=Vector.fill(0x4000)(0), a=0, d=0, pc=0)

  def init(rom: Vector[ml.Instruction]): State =
    State(rom=rom, ram=Vector.fill(0x4000)(0), a=0, d=0, pc=0)

//
