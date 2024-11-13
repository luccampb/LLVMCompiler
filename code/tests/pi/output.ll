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
  %PI4 = load float, ptr %PI, align 4
  %i5 = load i32, ptr %i, align 4
  %i6 = load i32, ptr %i, align 4
  %addtmp = add i32 %i6, 1
  %i7 = load i32, ptr %i, align 4
  %addtmp8 = add i32 %i7, 2
  %multmp = mul i32 %addtmp, %addtmp8
  %multmp9 = mul i32 %i5, %multmp
  %0 = sitofp i32 %multmp9 to float
  %divtmp = fdiv float 4.000000e+00, %0
  %addtmp10 = fadd float %PI4, %divtmp
  store float %addtmp10, ptr %PI, align 4
  br label %end

else:                                             ; preds = %iftrue
  %PI11 = load float, ptr %PI, align 4
  %i12 = load i32, ptr %i, align 4
  %i13 = load i32, ptr %i, align 4
  %addtmp14 = add i32 %i13, 1
  %i15 = load i32, ptr %i, align 4
  %addtmp16 = add i32 %i15, 2
  %multmp17 = mul i32 %addtmp14, %addtmp16
  %multmp18 = mul i32 %i12, %multmp17
  %1 = sitofp i32 %multmp18 to float
  %divtmp19 = fdiv float 4.000000e+00, %1
  %subtmp = fsub float %PI11, %divtmp19
  store float %subtmp, ptr %PI, align 4
  br label %end

end:                                              ; preds = %else, %iftrue3
  %flag20 = load i1, ptr %flag, align 1
  %not = xor i1 %flag20, true
  store i1 %not, ptr %flag, align 1
  %i21 = load i32, ptr %i, align 4
  %addtmp22 = add i32 %i21, 2
  store i32 %addtmp22, ptr %i, align 4
  br label %cond

end23:                                            ; preds = %cond
  %PI24 = load float, ptr %PI, align 4
  ret float %PI24
}
