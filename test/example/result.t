  $ ./run.sh
  lli: lli: ./heaplang_llvm_XXXXXX.ll:6:43: error: '%v' defined with type '{ i32, i32 }' but expected 'ptr'
    %tmp_0_0 = load {i32, i32}, {i32, i32}* %v
                                            ^
  
  Error running lli on ./heaplang_llvm_XXXXXX.ll
  
  declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @get_left({i32, i32} %v) {
  entry:
    %tmp_0_0 = load {i32, i32}, {i32, i32}* %v
    %tmp_1_0 = extractvalue {i32, i32} %tmp_0_0, 0
    ret i32 %tmp_1_0
  }
  define i32 @f() {
  entry:
    ret i32 2
  }
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f()
    %y = alloca i32
    store i32 %tmp_0_0, i32* %y
    ret i32 0
  }
  [1]
