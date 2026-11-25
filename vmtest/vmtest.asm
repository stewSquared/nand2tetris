  @256
  D=A
  @SP
  M=D
  //  push return address
  @UserSymbol(Sys.init$RETURN$0)
  D=A
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push Local address
  @LCL
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push Argument address
  @ARG
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push This address
  @THIS
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push That address
  @THAT
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  reposition arg
  @SP
  D=M
  @5
  D=D-A
  @ARG
  M=D
  //  repostion local
  @SP
  D=M
  @LCL
  M=D
  //  jump to Sys.init entrypoint
  @UserSymbol(Sys.init)
  0;JMP
  // return to  from Sys.init
(Sys.init$RETURN$0)
(Sys.init)
  //  push return address
  @UserSymbol(Main.main$RETURN$1)
  D=A
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push Local address
  @LCL
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push Argument address
  @ARG
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push This address
  @THIS
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push That address
  @THAT
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  reposition arg
  @SP
  D=M
  @5
  D=D-A
  @ARG
  M=D
  //  repostion local
  @SP
  D=M
  @LCL
  M=D
  //  jump to Main.main entrypoint
  @UserSymbol(Main.main)
  0;JMP
  // return to  from Main.main
(Main.main$RETURN$1)
(.END_LOOP)
  @UserSymbol(.END_LOOP)
  0;JMP
(Double.double)
  //  save Argument 0 to D
  @ARG
  D=M
  @0
  A=D+A
  D=M
  //  push D to stack
  @SP
  AM=M+1
  A=A-1
  M=D
  //  save Argument 0 to D
  @ARG
  D=M
  @0
  A=D+A
  D=M
  //  push D to stack
  @SP
  AM=M+1
  A=A-1
  M=D
  @SP
  AM=M-1
  D=M
  A=A-1
  M=D+M
  //  save frame
  @LCL
  D=M
  @13
  M=D
  //  save return address
  @13
  D=M
  @5
  A=D-A
  D=M
  @14
  M=D
  //  store return value at arg[0]
  @SP
  AM=M-1
  D=M
  @ARG
  A=M
  M=D
  //  reposition SP
  @ARG
  D=M
  @SP
  M=D+1
  //  pop That
  @13
  AM=M-1
  D=M
  @THAT
  M=D
  //  pop This
  @13
  AM=M-1
  D=M
  @THIS
  M=D
  //  pop Argument
  @13
  AM=M-1
  D=M
  @ARG
  M=D
  //  pop Local
  @13
  AM=M-1
  D=M
  @LCL
  M=D
  //  goto return
  @14
  A=M
  0;JMP
(Main.main)
  @3
  D=A
  //  push D to stack
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push return address
  @UserSymbol(Double.double$RETURN$2)
  D=A
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push Local address
  @LCL
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push Argument address
  @ARG
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push This address
  @THIS
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push That address
  @THAT
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  reposition arg
  @SP
  D=M
  @6
  D=D-A
  @ARG
  M=D
  //  repostion local
  @SP
  D=M
  @LCL
  M=D
  //  jump to Double.double entrypoint
  @UserSymbol(Double.double)
  0;JMP
  // return to  from Double.double
(Double.double$RETURN$2)
  //  push return address
  @UserSymbol(Double.double$RETURN$3)
  D=A
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push Local address
  @LCL
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push Argument address
  @ARG
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push This address
  @THIS
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  push That address
  @THAT
  D=M
  @SP
  AM=M+1
  A=A-1
  M=D
  //  reposition arg
  @SP
  D=M
  @6
  D=D-A
  @ARG
  M=D
  //  repostion local
  @SP
  D=M
  @LCL
  M=D
  //  jump to Double.double entrypoint
  @UserSymbol(Double.double)
  0;JMP
  // return to  from Double.double
(Double.double$RETURN$3)
  //  save frame
  @LCL
  D=M
  @13
  M=D
  //  save return address
  @13
  D=M
  @5
  A=D-A
  D=M
  @14
  M=D
  //  store return value at arg[0]
  @SP
  AM=M-1
  D=M
  @ARG
  A=M
  M=D
  //  reposition SP
  @ARG
  D=M
  @SP
  M=D+1
  //  pop That
  @13
  AM=M-1
  D=M
  @THAT
  M=D
  //  pop This
  @13
  AM=M-1
  D=M
  @THIS
  M=D
  //  pop Argument
  @13
  AM=M-1
  D=M
  @ARG
  M=D
  //  pop Local
  @13
  AM=M-1
  D=M
  @LCL
  M=D
  //  goto return
  @14
  A=M
  0;JMP