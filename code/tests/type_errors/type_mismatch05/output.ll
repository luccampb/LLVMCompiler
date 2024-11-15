; ModuleID = 'mini-c'
source_filename = "mini-c"

define float @calculation(i32 %x, i32 %y) {
func:
  %f = alloca float, align 4
  %y2 = alloca i32, align 4
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  store i32 %y, ptr %y2, align 4
  %x3 = load i32, ptr %x1, align 4
  %y4 = load i32, ptr %y2, align 4
  %addtmp = add i32 %x3, %y4
  %0 = sitofp i32 %addtmp to float
  store float %0, ptr %f, align 4
  %f5 = load float, ptr %f, align 4
  ret float %f5
}
