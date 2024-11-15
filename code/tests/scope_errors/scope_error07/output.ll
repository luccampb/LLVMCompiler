; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @calculation(i32 %x) {
func:
  %y = alloca i32, align 4
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  store i32 2, ptr %y, align 4
  %y2 = load i32, ptr %y, align 4
  %addtmp = add i32 %y2, 2
  store i32 %addtmp, ptr %x1, align 4
  %x3 = load i32, ptr %x1, align 4
  ret i32 %x3
}
