; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @calculation(i32 %x, i32 %y) {
func:
  %z = alloca i32, align 4
  %a = alloca i1, align 1
  %y2 = alloca i32, align 4
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  store i32 %y, ptr %y2, align 4
  store i1 true, ptr %a, align 1
  %a3 = load i1, ptr %a, align 1
  %x4 = load i32, ptr %x1, align 4
  %0 = zext i1 %a3 to i32
  %addtmp = add i32 %0, %x4
  store i32 %addtmp, ptr %z, align 4
  %z5 = load i32, ptr %z, align 4
  ret i32 %z5
}
