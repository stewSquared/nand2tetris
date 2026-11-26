package nand2tetris

import vm.{Source, Config}

case class Translator(sources: List[vm.Source], sysVm: vm.Source, config: vm.Config):

  private def translateSource(partial: vm.Translation[asm.Program], source: Source): vm.Translation[asm.Program] =
    source.code.foldLeft(partial.changeFile(source.name)): (acc, command) =>
      for
        lines <- acc
        comment = asm.Comment(s"translate $command")
        newLines <- command.translate
      yield lines ++ (comment +: newLines)

  def translate: asm.Program =
    val bootstrap: vm.Translation[asm.Program] =
      val setSP = asm.Program.parse:
        s"""|@${config.initSp}
            |D=A
            |@SP
            |M=D""".stripMargin
      vm.Call("Sys.init", 0).translate.map(setSP ++ _)

    val translation = (sysVm :: sources).foldLeft(bootstrap)(translateSource)
    val (assembly, context) = translation.run(vm.VMContext("", 0, ""))
    assembly

object Translator:
  def apply(sources: List[vm.Source]): Translator =
    Translator(sources, defaultSysInit, vm.Config(initSp = 0x100))

  // TODO: could pass in arg list here
  val defaultSysInit: Source =
    val rawCode =
      """|function Sys.init 0
         |  call Main.main 0
         |  label END_LOOP
         |  goto END_LOOP""".stripMargin
    val commands = rawCode.linesIterator.map(vm.VMCommand.parse)
    Source("Sys", commands.toVector)
