declare i8* @malloc(i64)            
declare i32 @printf(i8*, ...)
@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00"
define i32 @f(i32 %z) {
entry:
  %x = alloca i32
  store i32 1, i32* %x
  %tmp_2_0 = load i32, i32* %x
  %tmp_3_0 = getelementptr [4 x i8], [4 x i8]* @.str, i64 0, i64 0
  %tmp_3_1 = call i32 (i8*, ...) @printf(i8* %tmp_3_0, i32 %tmp_2_0)
  ret i32 0
}
define i32 @main() {
entry:
  %x = alloca i32
  store i32 1, i32* %x
  %tmp_2_0 = load i32, i32* %x
  %tmp_3_0 = call i32 @f(i32 %tmp_2_0)
  %y = alloca i32
  store i32 %tmp_3_0, i32* %y
  %tmp_6_0 = load i32, i32* %y
  ret i32 %tmp_6_0
}