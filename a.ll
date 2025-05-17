declare i8* @malloc(i64)
  declare i32 @printf(i8*, ...)
  @.str = private constant [4 x i8] c"%d\0A\00"
  define i32 @f(i32* %z) {
  entry:
    %x = alloca i32
    store i32 1, i32* %x
    %tmp_2_0 = load i32, i32* %x
    %tmp_3_0 = getelementptr [4 x i8], [4 x i8]* @.str, i32 0, i32 0
    %tmp_3_1 = call i32 @printf(i8* %tmp_3_0, i32 %tmp_2_0)
    ret i32 %tmp_3_1
  }
  define i32 @main() {
  entry:
    %tmp_0_0 = call i32 @f(i32 1)
    %x = alloca i32
    store i32 %tmp_0_0, i32* %x
    ret i32 1
  }