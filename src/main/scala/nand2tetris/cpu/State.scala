package nand2tetris
package cpu

// type Word = Int
// TODO need an apply method and plus method that wraps around 16 bits

// represent data, instructions, and addresses with different type tags?

case class RAM(underlying: Vector[Word]):
  def apply(addr: U15): Word = underlying(addr.toInt)
  def updated(addr: U15, value: Word): RAM =
    RAM(underlying.updated(addr.toInt, value))

case class ROM(underlying: Vector[ml.Instruction]):
  def apply(addr: U15): ml.Instruction = underlying(addr.toInt)
  def updated(addr: U15, value: ml.Instruction): ROM =
    ROM(underlying.updated(addr.toInt, value))

case class State(
  // rom: Memory, // always instructions?
  rom: ROM, // always instructions?
  ram: RAM, // always data
  // TODO: document and encode/validate rom/ram size
  a: Word, // address or data
  d: Word, // data
  pc: U15 // address
):

  def m: Word = ram(a.asAddr)
  def currentInstruction: ml.Instruction = rom(pc)

  private def tick: State = copy(pc = pc.inc)
  private def jump: State = copy(pc = a.asAddr)
  private def setA(n: Word): State = copy(a = n) // note: A inst can only set U15 values
  private def setD(n: Word): State = copy(d = n)
  def setM(n: Word): State = copy(ram = ram.updated(a.asAddr, n))

  import util.chaining.*

  def calcComp(comp: ml.Comp): Word = comp match
    case ml.Comp(fromMem, zx, nx, zy, ny, f, no) =>
      val x = d.pipe[Word]: x =>
        if zx then Word(0) else x
      .pipe: x =>
        if nx then ~x else x

      val y = (if fromMem then m else a).pipe: y =>
        if zy then Word(0) else y
      .pipe: y =>
        if ny then ~y else y

      val binop = if f then x + y else x & y
      if no then ~binop else binop

  def setDest(dest: ml.Dest, value: Word): State = dest match
    case ml.Dest(a, d, m) =>
      this.pipe: state =>
        if a then state.setA(value) else state
      .pipe: state =>
        if d then state.setD(value) else state
      .pipe: state =>
        if m then state.setM(value) else state

  def movePC(jump: ml.Jump, result: Word): State = jump match
    // TODO think about comparing Word with 0? think about .toInt behavior
    case ml.Jump.Null => this.tick
    case ml.Jump.JGT => if result.toInt > 0 then this.jump else this.tick
    case ml.Jump.JEQ => if result.toInt == 0 then this.jump else this.tick
    case ml.Jump.JGE => if result.toInt >= 0 then this.jump else this.tick
    case ml.Jump.JLT => if result.toInt < 0 then this.jump else this.tick
    case ml.Jump.JNE => if result.toInt != 0 then this.jump else this.tick
    case ml.Jump.JLE => if result.toInt <= 0 then this.jump else this.tick
    case ml.Jump.JMP => this.jump

  def step: State = currentInstruction match
    case ml.AInst(u15) => setA(u15.toWord).tick
    case ml.CInst(comp, dest, jump) =>
      val compValue = calcComp(comp)
      this.setDest(dest, compValue).movePC(jump, compValue)

  // virtual registers r0 - r15
  def r0 = ram(U15(0))
  def r1 = ram(U15(1))
  def r2 = ram(U15(2))
  def r3 = ram(U15(3))
  def r4 = ram(U15(4))
  def r5 = ram(U15(5))
  def r6 = ram(U15(6))
  def r7 = ram(U15(7))
  def r8 = ram(U15(8))
  def r9 = ram(U15(9))
  def r10 = ram(U15(10))
  def r11 = ram(U15(11))
  def r12 = ram(U15(12))
  def r13 = ram(U15(13))
  def r14 = ram(U15(14))
  def r15 = ram(U15(15))

  def setR0(n: Word) = this.copy(ram = ram.updated(U15(0), n))
  def setR1(n: Word) = this.copy(ram = ram.updated(U15(1), n))
  def setR2(n: Word) = this.copy(ram = ram.updated(U15(2), n))
  def setR3(n: Word) = this.copy(ram = ram.updated(U15(3), n))
  def setR4(n: Word) = this.copy(ram = ram.updated(U15(4), n))
  def setR5(n: Word) = this.copy(ram = ram.updated(U15(5), n))
  def setR6(n: Word) = this.copy(ram = ram.updated(U15(6), n))
  def setR7(n: Word) = this.copy(ram = ram.updated(U15(7), n))
  def setR8(n: Word) = this.copy(ram = ram.updated(U15(8), n))
  def setR9(n: Word) = this.copy(ram = ram.updated(U15(9), n))
  def setR10(n: Word) = this.copy(ram = ram.updated(U15(10), n))
  def setR11(n: Word) = this.copy(ram = ram.updated(U15(11), n))
  def setR12(n: Word) = this.copy(ram = ram.updated(U15(12), n))
  def setR13(n: Word) = this.copy(ram = ram.updated(U15(13), n))
  def setR14(n: Word) = this.copy(ram = ram.updated(U15(14), n))
  def setR15(n: Word) = this.copy(ram = ram.updated(U15(15), n))

  def sp = ram(U15(0)) // stack pointer
  def lcl = ram(U15(1)) // local segment base address
  def arg = ram(U15(2)) // argument segment base address
  def ths = ram(U15(3)) // this segment base address
  def tht = ram(U15(4)) // that segment base address

  // screen memory starts at 0x4000 and is 256x512 bits (32x256 words)
  def screen: Vector[Vector[Word]] =
    ram.underlying.slice(0x4000, 0x6000).grouped(32).toVector

  def subscreenPixels(x: Int, y: Int, width: Int, height: Int): Vector[Vector[Boolean]] =
    screen.slice(y, y + height).map: row =>
      val firstWordIndex = x / 16
      // x=0 gets us the 0th word
      // width = 17 gets us the 1st word
      val lastWordIndex = (x + width) / 16
      println(s"x=$x, y=$y, width=$width, height=$height, firstWordIndex=$firstWordIndex, lastWordIndex=$lastWordIndex")
      val words = row.slice(firstWordIndex, lastWordIndex + 1)
      val pixels = words
        .flatMap: word =>
          (0 until 16).map(bitIndex => (word.toInt >> (15 - bitIndex)) & 1)
        .slice(x, x + width)
      println(pixels)
      pixels.toVector.map(_ == 1)
    .toVector

  def drawSubscreen(x: Int, y: Int, width: Int, height: Int): Unit =
    val pixels = subscreenPixels(x, y, width, height)
    for j <- 0 until height do
      for i <- 0 until width do
        val pixel = pixels(j)(i)
        print(if pixel then "█" else "░")
      println()

  // def keyboard = ram(0x6000)

object State:
  // def init(rom: Memory): State =
  //   State(rom=rom, ram=Vector.fill(0x4000)(0), a=0, d=0, pc=0)

  def init(
    rom: Vector[ml.Instruction],
    ram: RAM = RAM(Vector.fill(0x8000)(Word(0))),
    a: Word = Word(0),
    d: Word = Word(0),
    pc: U15 = U15(0)
  ): State =
    State(rom=ROM(underlying = rom), ram=ram, a=a, d=d, pc=pc)

  def initRom(rom: Vector[ml.Instruction]): State = init(rom=rom)

//
