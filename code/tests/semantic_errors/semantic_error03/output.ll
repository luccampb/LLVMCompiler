; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @function(i32 %x) {
func:
  %y = alloca i32, align 4
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  %x2 = load i32, ptr %x1, align 4
  %addtmp = add i32 2, %x2
  store i32 %addtmp, ptr %y, align 4
  %y3 = load i32, ptr %y, align 4
  ret i32 %y3
}

define i32 @main() {
func:
  %y = alloca i32, align 4
  %x = alloca i32, align 4
  %x1 = load i32, ptr %x, align 4
  %calltmp = call i32 @function(i32 %x1)
  store i32 %calltmp, ptr %y, align 4
  %y2 = load i32, ptr %y, align 4
  ret i32 %y2
}
