; ModuleID = 'mini-c'
source_filename = "mini-c"

define void @function(i32 %x) {
func:
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  ret void
}
