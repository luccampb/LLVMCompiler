; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @calculation(i32 %x) {
func:
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  %x2 = load i32, ptr %x1, align 4
  ret i32 %x2
}
