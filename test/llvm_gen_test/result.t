  $ HeaplangToLLVM --llvm let1.hl
  LLVM:
  define i32 @f(i32 %z) {
  entry:
    %x = alloca i32
    store i32 1, i32* %x
    %y = alloca i32
    store i32 %x, i32* %y
    ret %y
  }

  $ HeaplangToLLVM --llvm alloc.hl
  LLVM:
  define i32 @f(void %a) {
  entry:
    %x = call i32* @malloc(i64 1)
    %x_index_1 = getelementptr i32*, %x, 0
    store i32 23, i32* %x_index_1
    ret %x
  }

  $ HeaplangToLLVM --llvm if1.hl
  LLVM:
  define i32 @f(i32 %a) {
  entry:
    %x = alloca i32
    store i32 %a, i32* %x
    br i1 %x, label %then, label %else
  then:
    ret 1
  else:
    ret 2
  }

  $ HeaplangToLLVM --llvm pair1.hl
  LLVM:
  define i32 @f(i32 %z) {
  entry:
    %x = alloca pair(i32, i32)
    %x_fst = insertvalue pair(i32, i32) undef, 1, 0
    %x_snd = insertvalue pair(i32, i32) %x, %z, 1
    %x = call pair(i32, i32)* @malloc(i64 1)
    %x_index_1 = getelementptr pair(i32, i32)*, %x_snd, 0
    store pair(i32, i32) %x_snd, pair(i32, i32)* %x_index_1
    y = alloca pair(i32, pair(i32, i32)*)
    y_fst = insertvalue pair(i32, pair(i32, i32)*) undef, 12, 0
    y = insertvalue pair(i32, pair(i32, i32)*) %y, %x, 1
    ret %y
  }
  $ HeaplangToLLVM --llvm pair2.hl
  LLVM:
  define i32 @f(i32 %a) {
  entry:
    x = alloca pair(i32, i32)
    x_fst = insertvalue pair(i32, i32) undef, %a, 0
    x = insertvalue pair(i32, i32) %x, 22, 1
    y = alloca i32*
    y_fst = extractvalue i32* %x, 0
    store i32* %y, i32* %y_fst
    ret %y
  }
  $ HeaplangToLLVM --llvm multiParam.hl
  LLVM:
  define i32 @f(i32 %a, i32 %b) {
  entry:
    %x = alloca i32
    store i32 %a, i32* %x
    %y = alloca i32
    store i32 %b, i32* %y
    ret %x
  }
