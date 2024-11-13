; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @factorial(i32 %n) {
func:
  %factorial = alloca i32, align 4
  %i = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 1, ptr %factorial, align 4
  store i32 1, ptr %i, align 4
  br label %cond

cond:                                             ; preds = %iftrue, %func
  %i2 = load i32, ptr %i, align 4
  %n3 = load i32, ptr %n1, align 4
  %le = icmp sle i32 %i2, %n3
  br i1 %le, label %iftrue, label %end

iftrue:                                           ; preds = %cond
  %i4 = load i32, ptr %i, align 4
  %factorial5 = load i32, ptr %factorial, align 4
  %multmp = mul i32 %i4, %factorial5
  store i32 %multmp, ptr %factorial, align 4
  %i6 = load i32, ptr %i, align 4
  %addtmp = add i32 1, %i6
  store i32 %addtmp, ptr %i, align 4
  br label %cond

end:                                              ; preds = %cond
  %factorial7 = load i32, ptr %factorial, align 4
  ret i32 %factorial7
}
