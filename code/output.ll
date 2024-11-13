; ModuleID = 'mini-c'
source_filename = "mini-c"

declare float @print_float(float)

define float @cosine(float %x) {
func:
  %alt = alloca float, align 4
  %eps = alloca float, align 4
  %term = alloca float, align 4
  %n = alloca float, align 4
  %cos = alloca float, align 4
  %x1 = alloca float, align 4
  store float %x, ptr %x1, align 4
  store float 0x3EB0C6F7A0000000, ptr %eps, align 4
  store float 1.000000e+00, ptr %n, align 4
  store float 1.000000e+00, ptr %cos, align 4
  store float 1.000000e+00, ptr %term, align 4
  store float -1.000000e+00, ptr %alt, align 4
  br label %cond

cond:                                             ; preds = %iftrue, %func
  %term2 = load float, ptr %term, align 4
  %eps3 = load float, ptr %eps, align 4
  %gt = fcmp ugt float %term2, %eps3
  br i1 %gt, label %iftrue, label %end

iftrue:                                           ; preds = %cond
  %term4 = load float, ptr %term, align 4
  %x5 = load float, ptr %x1, align 4
  %x6 = load float, ptr %x1, align 4
  %n7 = load float, ptr %n, align 4
  %n8 = load float, ptr %n, align 4
  %addtmp = fadd float %n8, 1.000000e+00
  %divtmp = fdiv float %n7, %addtmp
  %divtmp9 = fdiv float %x6, %divtmp
  %multmp = fmul float %x5, %divtmp9
  %multmp10 = fmul float %term4, %multmp
  store float %multmp10, ptr %term, align 4
  %cos11 = load float, ptr %cos, align 4
  %alt12 = load float, ptr %alt, align 4
  %term13 = load float, ptr %term, align 4
  %multmp14 = fmul float %alt12, %term13
  %addtmp15 = fadd float %cos11, %multmp14
  store float %addtmp15, ptr %cos, align 4
  %alt16 = load float, ptr %alt, align 4
  %neg = fneg float %alt16
  store float %neg, ptr %alt, align 4
  %n17 = load float, ptr %n, align 4
  %addtmp18 = fadd float %n17, 2.000000e+00
  store float %addtmp18, ptr %n, align 4
  br label %cond

end:                                              ; preds = %cond
  %cos19 = load float, ptr %cos, align 4
  %calltmp = call float @print_float(float %cos19)
  %cos20 = load float, ptr %cos, align 4
  ret float %cos20
}
