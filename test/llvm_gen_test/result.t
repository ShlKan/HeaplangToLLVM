  $ HeaplangToLLVM --llvm let1.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %z) {
  entry:
    %x = alloca i32
    store i32 1, i32* %x
    %tmp_2_0 = load i32, i32* %x
    %y = alloca i32
    store i32 %tmp_2_0, i32* %y
    %tmp_5_0 = load i32, i32* %y
    ret i32 %tmp_5_0
  }

  $ HeaplangToLLVM --llvm alloc.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f() {
  entry:
    %x = call i32* @malloc(i64 1)
    %tmp_0_0 = getelementptr i32, i32* %x, i32 0
    store i32 23, i32* %tmp_0_0
    %tmp_3_0 = load i32*, i32** %x
    ret i32* %tmp_3_0
  }

  $ HeaplangToLLVM --llvm if1.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %a) {
  entry:
    %x = alloca i32
    store i32 %a, i32* %x
    %tmp_2_0 = load i32, i32* %x
    br i1 %tmp_2_0, label %then, label %else
  then:
    ret i32 1
  else:
    ret i32 2
  }

  $ HeaplangToLLVM --llvm pair1.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %z) {
  entry:
    %x = alloca {i32, i32}
    %tmp_0_0 = insertvalue {i32, i32} undef, i32 1, 0
    %tmp_0_1 = insertvalue {i32, i32} %x, i32 %z, 1
    %x = call {i32, i32}* @malloc(i64 1)
    %tmp_3_0 = getelementptr {i32, i32}*, {i32, i32}** %tmp_0_1, i32 0
    store {i32, i32} %tmp_0_1, {i32, i32}* %tmp_3_0
    %tmp_6_0 = load {i32, i32}*, {i32, i32}** %x
    %y = alloca {i32, {i32, i32}*}
    %tmp_7_0 = insertvalue {i32, {i32, i32}*} undef, i32 12, 0
    %tmp_7_1 = insertvalue {i32, {i32, i32}*} %tmp_7_0, {i32, i32}* %tmp_6_0, 1
    store {i32, {i32, i32}*} %tmp_7_1, {i32, {i32, i32}*}* %y
    %tmp_11_0 = load {i32, {i32, i32}*}, {i32, {i32, i32}*}* %y
    ret {i32, {i32, i32}*} %tmp_11_0
  }
  $ HeaplangToLLVM --llvm pair2.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %a) {
  entry:
    %x = alloca {i32, i32}
    %tmp_0_0 = insertvalue {i32, i32} undef, i32 %a, 0
    %tmp_0_1 = insertvalue {i32, i32} %tmp_0_0, i32 22, 1
    store {i32, i32} %tmp_0_1, {i32, i32}* %x
    %tmp_4_0 = load {i32, i32}, {i32, i32}* %x
    %y = alloca i32*
    %tmp_5_0 = extractvalue {i32, i32} %tmp_4_0, 1
    store i32 %tmp_5_0, i32* %y
    %tmp_8_0 = load i32, i32* %y
    ret i32 %tmp_8_0
  }
  $ HeaplangToLLVM --llvm multiParam.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %a, i32 %b) {
  entry:
    %x = alloca i32
    store i32 %a, i32* %x
    %tmp_2_0 = load i32, i32* %b
    %y = alloca i32
    store i32 %tmp_2_0, i32* %y
    %tmp_5_0 = load i32, i32* %x
    ret i32 %tmp_5_0
  }
  $ HeaplangToLLVM --llvm call.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f() {
  entry:
    %x = call i32* @malloc(i64 1)
    %tmp_0_0 = getelementptr i32, i32* %x, i32 0
    store i32 23, i32* %tmp_0_0
    %tmp_3_0 = load i32*, i32** %x
    ret i32* %tmp_3_0
  }
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f()
    ret i32 %tmp_0_0
  }
  $ HeaplangToLLVM --llvm plus.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %z) {
  entry:
    %x = alloca i32
    %tmp_0_0 = add i32 1, %z
    store i32 %tmp_0_0, i32* %x
    ret i32 %z
  }
  $ HeaplangToLLVM --llvm print.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %z) {
  entry:
    %x = alloca i32
    store i32 1, i32* %x
    %tmp_2_0 = load i32, i32* %x
    %tmp_3_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_3_1 = call i32 (i8*, ...) @printf(i8* %tmp_3_0, i32 %tmp_2_0)
  }
  $ HeaplangToLLVM --llvm seq.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %z) {
  entry:
    %x = alloca i32
    store i32 1, i32* %x
    %tmp_2_0 = load i32, i32* %x
    %tmp_3_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_3_1 = call i32 (i8*, ...) @printf(i8* %tmp_3_0, i32 %tmp_2_0)
    %tmp_5_0 = load i32, i32* %x
    ret i32 %tmp_5_0
  }
