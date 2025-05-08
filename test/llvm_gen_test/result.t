  $ HeaplangToLLVM --llvm let1.hl
  LLVM:
  %x = alloca i32
  store i32 %x, 1
  ret %x

  $ HeaplangToLLVM --llvm alloc.hl
  LLVM:
  %x = call i32* @malloc(i64 1)
  %x_index_1 = getelementptr i32*, %x, 0
  store i32* %x_index_1, 23
  ret %x
