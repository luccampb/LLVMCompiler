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
  %m3 = load float, ptr %m2, align 4
  %n4 = load i32, ptr %n1, align 4
  %0 = sitofp i32 %n4 to float
  %addtmp = fadd float %m3, %0
  store float %addtmp, ptr %result, align 4
  %result5 = load float, ptr %result, align 4
  %calltmp = call float @print_float(float %result5)
  %result6 = load float, ptr %result, align 4
  %sum7 = load float, ptr %sum, align 4
  %addtmp8 = fadd float %result6, %sum7
  store float %addtmp8, ptr %sum, align 4
  %m9 = load float, ptr %m2, align 4
  %neg = fneg float %m9
  %n10 = load i32, ptr %n1, align 4
  %1 = sitofp i32 %n10 to float
  %addtmp11 = fadd float %neg, %1
  store float %addtmp11, ptr %result, align 4
  %result12 = load float, ptr %result, align 4
  %calltmp13 = call float @print_float(float %result12)
  %result14 = load float, ptr %result, align 4
  %sum15 = load float, ptr %sum, align 4
  %addtmp16 = fadd float %result14, %sum15
  store float %addtmp16, ptr %sum, align 4
  %m17 = load float, ptr %m2, align 4
  %neg18 = fneg float %m17
  %neg19 = fneg float %neg18
  %n20 = load i32, ptr %n1, align 4
  %2 = sitofp i32 %n20 to float
  %addtmp21 = fadd float %neg19, %2
  store float %addtmp21, ptr %result, align 4
  %result22 = load float, ptr %result, align 4
  %calltmp23 = call float @print_float(float %result22)
  %result24 = load float, ptr %result, align 4
  %sum25 = load float, ptr %sum, align 4
  %addtmp26 = fadd float %result24, %sum25
  store float %addtmp26, ptr %sum, align 4
  %m27 = load float, ptr %m2, align 4
  %neg28 = fneg float %m27
  %n29 = load i32, ptr %n1, align 4
  %neg30 = sub i32 0, %n29
  %3 = sitofp i32 %neg30 to float
  %addtmp31 = fadd float %neg28, %3
  store float %addtmp31, ptr %result, align 4
  %result32 = load float, ptr %result, align 4
  %calltmp33 = call float @print_float(float %result32)
  %result34 = load float, ptr %result, align 4
  %sum35 = load float, ptr %sum, align 4
  %addtmp36 = fadd float %result34, %sum35
  store float %addtmp36, ptr %sum, align 4
  %sum37 = load float, ptr %sum, align 4
  ret float %sum37
}
