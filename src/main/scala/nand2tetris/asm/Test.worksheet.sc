import nand2tetris.asm.*

val testProgram =
  """|// Computes R1=1+...+R0
     |  // i = 0
     |  @i
     |
     |""".stripMargin

def parse(program: String): List[Line] =
  val rawLines = program.linesIterator
  rawLines.map[Line]:
    line =>
      line.strip() match
        case s"//$comment" => comment
        case s"($label)" =>
          assert(label.forall(_.isUpper), "Uppercase labels by convention")
          Label(label)
        case s"@$constant" if constant.toIntOption.isDefined =>
          AInst(Constant.parse(constant.toInt))
        case s"@$symbol" =>
          AInst(Symbol.parse(symbol))
        case raw@s"$dest=$comp;$jump" =>
          CInst.parse(raw)
          // CInst(
          //   Reg.parseDest(dest),
          //   comp = Comp.parse(comp),
          //   jump = Jump.parse(jump)
          // )
        // case s"$comp;$jump" =>

          //

  .toList

//
