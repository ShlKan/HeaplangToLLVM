  $ HeaplangToLLVM --llvm map.hl
  LLVM:
  define i32 @get_left({i32, i32} %v) {
  entry:
    extractvalue {i32, i32} %v, 0
  }
