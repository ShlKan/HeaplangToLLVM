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
  
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f(i32 123)
    %x = alloca i32
    store i32 %tmp_0_0, i32* %x
    %tmp_3_0 = load i32, i32* %x
    %tmp_4_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_4_1 = call i32 (i8*, ...) @printf(i8* %tmp_4_0, i32 %tmp_3_0)
    ret i32 0
  }

  $ HeaplangToLLVM --llvm alloc.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f() {
  entry:
    %tmp_0_0 = getelementptr i32, i32* null, i32 1
    %size_tmp_0_0 = ptrtoint ptr %tmp_0_0 to i64
    %tmp_0_1 = call i32* @malloc(i64 1)
    %tmp_0_2 = getelementptr i32, i32* %tmp_0_1, i32 0
    store i32 23, i32* %tmp_0_2
    %x = alloca i32*
    store i32* %tmp_0_1, i32** %x
    %tmp_7_0 = load i32*, i32** %x
    %tmp_8_0 = load i32, i32* %tmp_7_0
    ret i32 %tmp_8_0
  }
  
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f()
    %a = alloca i32
    store i32 %tmp_0_0, i32* %a
    %tmp_3_0 = load i32, i32* %a
    %tmp_4_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_4_1 = call i32 (i8*, ...) @printf(i8* %tmp_4_0, i32 %tmp_3_0)
    ret i32 0
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
    %tmp_3_0 = icmp eq i32 %tmp_2_0, 2
    br i1 %tmp_3_0, label %thentmp_2_0, label %elsetmp_2_0
  thentmp_2_0:
    ret i32 1
  elsetmp_2_0:
    ret i32 2
  }
  
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f(i32 2)
    %a = alloca i32
    store i32 %tmp_0_0, i32* %a
    %tmp_3_0 = load i32, i32* %a
    %tmp_4_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_4_1 = call i32 (i8*, ...) @printf(i8* %tmp_4_0, i32 %tmp_3_0)
    ret i32 0
  }

  $ HeaplangToLLVM --llvm pair1.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %z) {
  entry:
    %tmp_0_0 = insertvalue {i32, i32} undef, i32 1, 0
    %tmp_0_1 = insertvalue {i32, i32} %tmp_0_0, i32 %z, 1
    %tmp_2_0 = getelementptr {i32, i32}, {i32, i32}* null, i32 1
    %size_tmp_2_0 = ptrtoint ptr %tmp_2_0 to i64
    %tmp_2_1 = call {i32, i32}* @malloc(i64 %size_tmp_2_0)
    %tmp_2_2 = getelementptr {i32, i32}, {i32, i32}* %tmp_2_1, i32 0
    store {i32, i32} %tmp_0_1, {i32, i32}* %tmp_2_2
    %x = alloca {i32, i32}*
    store {i32, i32}* %tmp_2_1, {i32, i32}** %x
    %tmp_9_0 = load {i32, i32}*, {i32, i32}** %x
    %tmp_10_0 = insertvalue {i32, {i32, i32}*} undef, i32 12, 0
    %tmp_10_1 = insertvalue {i32, {i32, i32}*} %tmp_10_0, {i32, i32}* %tmp_9_0, 1
    %y = alloca {i32, {i32, i32}*}
    store {i32, {i32, i32}*} %tmp_10_1, {i32, {i32, i32}*}* %y
    %tmp_14_0 = load {i32, {i32, i32}*}, {i32, {i32, i32}*}* %y
    %tmp_15_0 = extractvalue {i32, {i32, i32}*} %tmp_14_0, 1
    %z1 = alloca {i32, i32}*
    store {i32, i32}* %tmp_15_0, {i32, i32}** %z1
    %tmp_18_0 = load {i32, i32}*, {i32, i32}** %z1
    %tmp_19_0 = load {i32, i32}, {i32, i32}* %tmp_18_0
    %z2 = alloca {i32, i32}
    store {i32, i32} %tmp_19_0, {i32, i32}* %z2
    %tmp_22_0 = load {i32, i32}, {i32, i32}* %z2
    %tmp_23_0 = extractvalue {i32, i32} %tmp_22_0, 1
    %z3 = alloca i32
    store i32 %tmp_23_0, i32* %z3
    %tmp_26_0 = load i32, i32* %z3
    %tmp_27_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_27_1 = call i32 (i8*, ...) @printf(i8* %tmp_27_0, i32 %tmp_26_0)
    ret i32 0
  }
  
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f(i32 2)
    %x = alloca i32
    store i32 %tmp_0_0, i32* %x
    ret i32 0
  }
  $ HeaplangToLLVM --llvm pair2.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %a) {
  entry:
    %tmp_0_0 = insertvalue {i32, i32} undef, i32 %a, 0
    %tmp_0_1 = insertvalue {i32, i32} %tmp_0_0, i32 %a, 1
    %x = alloca {i32, i32}
    store {i32, i32} %tmp_0_1, {i32, i32}* %x
    %tmp_4_0 = load {i32, i32}, {i32, i32}* %x
    %tmp_5_0 = extractvalue {i32, i32} %tmp_4_0, 1
    %y = alloca i32
    store i32 %tmp_5_0, i32* %y
    %tmp_8_0 = load i32, i32* %y
    ret i32 %tmp_8_0
  }
  
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f(i32 12)
    %v = alloca i32
    store i32 %tmp_0_0, i32* %v
    %tmp_3_0 = load i32, i32* %v
    %tmp_4_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_4_1 = call i32 (i8*, ...) @printf(i8* %tmp_4_0, i32 %tmp_3_0)
    ret i32 0
  }
  $ HeaplangToLLVM --llvm multiParam.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %a, i32 %b) {
  entry:
    %x = alloca i32
    store i32 %a, i32* %x
    %y = alloca i32
    store i32 %b, i32* %y
    %tmp_4_0 = load i32, i32* %x
    ret i32 %tmp_4_0
  }
  
  define i32 @main() {
  entry:
    %x = alloca i32
    store i32 1, i32* %x
    %tmp_2_0 = load i32, i32* %x
    %tmp_3_0 = add i32 %tmp_2_0, 1
    %y = alloca i32
    store i32 %tmp_3_0, i32* %y
    %tmp_6_0 = load i32, i32* %y
    %tmp_7_0 = load i32, i32* %y
    %tmp_8_0 = call i32 @f(i32 %tmp_7_0, i32 %tmp_6_0)
    %z = alloca i32
    store i32 %tmp_8_0, i32* %z
    %tmp_11_0 = load i32, i32* %z
    %tmp_12_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_12_1 = call i32 (i8*, ...) @printf(i8* %tmp_12_0, i32 %tmp_11_0)
    ret i32 0
  }
  $ HeaplangToLLVM --llvm call.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %a) {
  entry:
    %tmp_0_0 = getelementptr i32, i32* null, i32 1
    %size_tmp_0_0 = ptrtoint ptr %tmp_0_0 to i64
    %tmp_0_1 = call i32* @malloc(i64 1)
    %tmp_0_2 = getelementptr i32, i32* %tmp_0_1, i32 0
    store i32 %a, i32* %tmp_0_2
    %x = alloca i32*
    store i32* %tmp_0_1, i32** %x
    %tmp_7_0 = load i32*, i32** %x
    %tmp_8_0 = load i32, i32* %tmp_7_0
    ret i32 %tmp_8_0
  }
  
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f(i32 45)
    %a = alloca i32
    store i32 %tmp_0_0, i32* %a
    %tmp_3_0 = load i32, i32* %a
    %tmp_4_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_4_1 = call i32 (i8*, ...) @printf(i8* %tmp_4_0, i32 %tmp_3_0)
    ret i32 0
  }
  $ HeaplangToLLVM --llvm plus.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %z) {
  entry:
    %tmp_0_0 = add i32 1, %z
    %x = alloca i32
    store i32 %tmp_0_0, i32* %x
    %tmp_3_0 = load i32, i32* %x
    ret i32 %tmp_3_0
  }
  
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f(i32 12)
    %x = alloca i32
    store i32 %tmp_0_0, i32* %x
    %tmp_3_0 = load i32, i32* %x
    %tmp_4_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_4_1 = call i32 (i8*, ...) @printf(i8* %tmp_4_0, i32 %tmp_3_0)
    ret i32 0
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
    ret i32 2
  }
  
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f(i32 22)
    %x = alloca i32
    store i32 %tmp_0_0, i32* %x
    ret i32 0
  }
  $ HeaplangToLLVM --llvm seq.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32 %z) {
  entry:
    %x = alloca i32
    store i32 %z, i32* %x
    %tmp_2_0 = load i32, i32* %x
    %tmp_3_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_3_1 = call i32 (i8*, ...) @printf(i8* %tmp_3_0, i32 %tmp_2_0)
    %tmp_5_0 = load i32, i32* %x
    ret i32 %tmp_5_0
  }
  
  define i32 @main(i32 %vd) {
  entry:
    %tmp_0_0 = call i32 @f(i32 32)
    %x = alloca i32
    store i32 %tmp_0_0, i32* %x
    ret i32 0
  }
  $ HeaplangToLLVM --llvm load.hl
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f() {
  entry:
    %tmp_0_0 = getelementptr i32, i32* null, i32 1
    %size_tmp_0_0 = ptrtoint ptr %tmp_0_0 to i64
    %tmp_0_1 = call i32* @malloc(i64 1)
    %tmp_0_2 = getelementptr i32, i32* %tmp_0_1, i32 0
    store i32 23, i32* %tmp_0_2
    %x = alloca i32*
    store i32* %tmp_0_1, i32** %x
    %tmp_7_0 = load i32*, i32** %x
    %tmp_8_0 = load i32, i32* %tmp_7_0
    ret i32 %tmp_8_0
  }
  
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f()
    %x = alloca i32
    store i32 %tmp_0_0, i32* %x
    %tmp_3_0 = load i32, i32* %x
    %tmp_4_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_4_1 = call i32 (i8*, ...) @printf(i8* %tmp_4_0, i32 %tmp_3_0)
    ret i32 0
  }
