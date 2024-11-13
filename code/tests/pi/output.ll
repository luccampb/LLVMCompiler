; ModuleID = 'mini-c'
source_filename = "mini-c"

define float @pi() {
func:
  %i = alloca i32, align 4
  %PI = alloca float, align 4
  %flag = alloca i1, align 1
  store i1 true, ptr %flag, align 1
  store float 3.000000e+00, ptr %PI, align 4
  store i32 2, ptr %i, align 4
  br label %cond

cond:                                             ; preds = %end, %func
  %i1 = load i32, ptr %i, align 4
  %lt = icmp slt i32 %i1, 100
  br i1 %lt, label %iftrue, label %end23

iftrue:                                           ; preds = %cond
  %flag2 = load i1, ptr %flag, align 1
  br i1 %flag2, label %iftrue3, label %else

iftrue3:                                          ; preds = %iftrue
  %i4 = load i32, ptr %i, align 4
  %addtmp = add i32 2, %i4
  %i5 = load i32, ptr %i, align 4
  %addtmp6 = add i32 1, %i5
  %i7 = load i32, ptr %i, align 4
  %multmp = mul i32 %addtmp6, %i7
  %multmp8 = mul i32 %addtmp, %multmp
  %0 = sitofp i32 %multmp8 to float
  %divtmp = fdiv float %0, 4.000000e+00
  %PI9 = load float, ptr %PI, align 4
  %addtmp10 = fadd float %divtmp, %PI9
  store float %addtmp10, ptr %PI, align 4
  br label %end

else:                                             ; preds = %iftrue
  %i11 = load i32, ptr %i, align 4
  %addtmp12 = add i32 2, %i11
  %i13 = load i32, ptr %i, align 4
  %addtmp14 = add i32 1, %i13
  %i15 = load i32, ptr %i, align 4
  %multmp16 = mul i32 %addtmp14, %i15
  %multmp17 = mul i32 %addtmp12, %multmp16
  %1 = sitofp i32 %multmp17 to float
  %divtmp18 = fdiv float %1, 4.000000e+00
  %PI19 = load float, ptr %PI, align 4
  %subtmp = fsub float %divtmp18, %PI19
  store float %subtmp, ptr %PI, align 4
  br label %end

end:                                              ; preds = %else, %iftrue3
  %flag20 = load i1, ptr %flag, align 1
  %not = xor i1 %flag20, true
  store i1 %not, ptr %flag, align 1
  %i21 = load i32, ptr %i, align 4
  %addtmp22 = add i32 2, %i21
  store i32 %addtmp22, ptr %i, align 4
  br label %cond

end23:                                            ; preds = %cond
  %PI24 = load float, ptr %PI, align 4
  ret float %PI24
}
