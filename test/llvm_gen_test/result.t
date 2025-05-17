  $ HeaplangToLLVM --llvm let1.hl
  declare i8* @malloc(i64)
  define i32 @f(i32 %z) {
  entry:
    %x = alloca i32
    store i32 1, i32* %x
    %y = alloca i32
    store i32 %x, i32* %y
    ret i32 %y
  }

  $ HeaplangToLLVM --llvm alloc.hl
  declare i8* @malloc(i64)
  define i32 @f() {
  entry:
    %x = call i32* @malloc(i64 1)
    %tmp_0_0 = getelementptr i32, i32* %x, i32 0
    store i32 23, i32* %tmp_0_0
    ret i32* %x
  }

  $ HeaplangToLLVM --llvm if1.hl
  declare i8* @malloc(i64)
  define i32 @f(i32 %a) {
  entry:
    %x = alloca i32
    store i32 %a, i32* %x
    br i1 %x, label %then, label %else
  then:
    ret i32 1
  else:
    ret i32 2
  }

  $ HeaplangToLLVM --llvm pair1.hl
  declare i8* @malloc(i64)
  define i32 @f(i32 %z) {
  entry:
    %x = alloca {i32, i32}
    %tmp_0_0 = insertvalue {i32, i32} undef, 1, 0
    %tmp_0_1 = insertvalue {i32, i32} %x, %z, 1
    %x = call {i32, i32}* @malloc(i64 1)
    %tmp_3_0 = getelementptr {i32, i32}*, {i32, i32}** %tmp_0_1, i32 0
    store {i32, i32} %tmp_0_1, {i32, i32}* %tmp_3_0
    %y = alloca {i32, {i32, i32}*}
    %tmp_6_0 = insertvalue {i32, {i32, i32}*} undef, 12, 0
    %y = insertvalue {i32, {i32, i32}*} %y, %x, 1
    ret {i32, {i32, i32}*} %y
  }
  $ HeaplangToLLVM --llvm pair2.hl
  declare i8* @malloc(i64)
  define i32 @f(i32 %a) {
  entry:
    %x = alloca {i32, i32}
    %tmp_0_0 = insertvalue {i32, i32} undef, %a, 0
    %x = insertvalue {i32, i32} %x, 22, 1
    %y = alloca i32*
    %tmp_3_0 = extractvalue i32* %x, 0
    store i32* %y, i32* %tmp_3_0
    ret i32* %y
  }
  $ HeaplangToLLVM --llvm multiParam.hl
  declare i8* @malloc(i64)
  define i32 @f(i32 %a, i32 %b) {
  entry:
    %x = alloca i32
    store i32 %a, i32* %x
    %y = alloca i32
    store i32 %b, i32* %y
    ret i32 %x
  }
  $ HeaplangToLLVM --llvm call.hl
  declare i8* @malloc(i64)
  define i32 @f() {
  entry:
    %x = call i32* @malloc(i64 1)
    %tmp_0_0 = getelementptr i32, i32* %x, i32 0
    store i32 23, i32* %tmp_0_0
    ret i32* %x
  }
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f()
    ret i32 %tmp_0_0
  }
