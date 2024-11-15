; ModuleID = 'mini-c'
source_filename = "mini-c"

define i1 @calculation(float %x, i1 %y) {
func:
  %truth = alloca i1, align 1
  %y2 = alloca i1, align 1
  %x1 = alloca float, align 4
  store float %x, ptr %x1, align 4
  store i1 %y, ptr %y2, align 1
  %x3 = load float, ptr %x1, align 4
  %y4 = load i1, ptr %y2, align 1
  %0 = sitofp i1 %y4 to float
  %eq = fcmp ueq float %x3, %0
  store i1 %eq, ptr %truth, align 1
  %truth5 = load i1, ptr %truth, align 1
  ret i1 %truth5
}
