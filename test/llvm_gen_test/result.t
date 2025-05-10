  $ HeaplangToLLVM --llvm let1.hl
  LLVM:
  define i32 @f(i32 %f) {
  entry:
    %x = alloca i32
    store i32 1, i32* %x
    %y = alloca i32
    store i32 %x, i32* %y
    ret %y
  }

  $ HeaplangToLLVM --llvm alloc.hl
  LLVM:
  define i32 @f(i32 %f) {
  entry:
    %x = call i32* @malloc(i64 1)
    %x_index_1 = getelementptr i32*, %x, 0
    store i32 23, i32* %x_index_1
    ret %x
  }

  $ HeaplangToLLVM --llvm if1.hl
  LLVM:
  define i32 @f(i32 %f) {
  entry:
    %x = alloca i32
    store i32 1, i32* %x
    br i1 %x, label %then, label %else
  then:
    ret 1
  else:
    ret 2
  }
