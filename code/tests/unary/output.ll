; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

declare float @print_float(float)

define float @unary(i32 %n, float %m) {
func:
  %sum = alloca float, align 4
  %result = alloca float, align 4
  %m2 = alloca float, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store float %m, ptr %m2, align 4
  store float 0.000000e+00, ptr %sum, align 4
  %n3 = load i32, ptr %n1, align 4
  %m4 = load float, ptr %m2, align 4
  %0 = sitofp i32 %n3 to float
  %addtmp = fadd float %0, %m4
  store float %addtmp, ptr %result, align 4
  %result5 = load float, ptr %result, align 4
  %calltmp = call float @print_float(float %result5)
  %sum6 = load float, ptr %sum, align 4
  %result7 = load float, ptr %result, align 4
  %addtmp8 = fadd float %sum6, %result7
  store float %addtmp8, ptr %sum, align 4
  %n9 = load i32, ptr %n1, align 4
  %m10 = load float, ptr %m2, align 4
  %neg = fneg float %m10
  %1 = sitofp i32 %n9 to float
  %addtmp11 = fadd float %1, %neg
  store float %addtmp11, ptr %result, align 4
  %result12 = load float, ptr %result, align 4
  %calltmp13 = call float @print_float(float %result12)
  %sum14 = load float, ptr %sum, align 4
  %result15 = load float, ptr %result, align 4
  %addtmp16 = fadd float %sum14, %result15
  store float %addtmp16, ptr %sum, align 4
  %n17 = load i32, ptr %n1, align 4
  %m18 = load float, ptr %m2, align 4
  %neg19 = fneg float %m18
  %neg20 = fneg float %neg19
  %2 = sitofp i32 %n17 to float
  %addtmp21 = fadd float %2, %neg20
  store float %addtmp21, ptr %result, align 4
  %result22 = load float, ptr %result, align 4
  %calltmp23 = call float @print_float(float %result22)
  %sum24 = load float, ptr %sum, align 4
  %result25 = load float, ptr %result, align 4
  %addtmp26 = fadd float %sum24, %result25
  store float %addtmp26, ptr %sum, align 4
  %n27 = load i32, ptr %n1, align 4
  %neg28 = sub i32 0, %n27
  %m29 = load float, ptr %m2, align 4
  %neg30 = fneg float %m29
  %3 = sitofp i32 %neg28 to float
  %addtmp31 = fadd float %3, %neg30
  store float %addtmp31, ptr %result, align 4
  %result32 = load float, ptr %result, align 4
  %calltmp33 = call float @print_float(float %result32)
  %sum34 = load float, ptr %sum, align 4
  %result35 = load float, ptr %result, align 4
  %addtmp36 = fadd float %sum34, %result35
  store float %addtmp36, ptr %sum, align 4
  %sum37 = load float, ptr %sum, align 4
  ret float %sum37
}
