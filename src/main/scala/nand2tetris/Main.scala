package nand2tetris

@main def main(command: String, args: String*): Unit = command match
  case "translate" =>
    val directory = args.headOption.getOrElse(".")
    val vmFiles = new java.io.File(directory).listFiles.filter(_.getName.endsWith(".vm"))
    vmFiles.foreach(file => println(s"Found VM file: ${file.getName}"))
    if (vmFiles.isEmpty) println("No VM files found.")
    val sources: List[vm.Source] = vmFiles.toList.map: file =>
      val name = file.getName.stripSuffix(".vm")
      val raw = util.Using(scala.io.Source.fromFile(file))(_.mkString).get
      val code = raw.linesIterator.map(vm.VMCommand.parse).toVector
      vm.Source(name, code)

    println(s"Parsed ${sources.length} VM sources.")
    // save assembly to directory/directory.asm
    val outputFile = new java.io.File(s"$directory/$directory.asm")
    val translator = Translator(sources)
    val assembly = translator.translate
    val assemblySource = assembly.map:
      case inst: asm.Inst => s"  ${inst.toString}"
      case comment: asm.Comment => s"  // ${comment.value}"
      case label: asm.Label => s"(${label.sym.name})"
    .mkString("\n")

    java.nio.file.Files.writeString(outputFile.toPath, assemblySource)
    println(s"Assembly written to: ${outputFile.getPath}")
