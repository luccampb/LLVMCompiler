; ModuleID = 'mini-c'
source_filename = "mini-c"

define i1 @palindrome(i32 %number) {
func:
  %result = alloca i1, align 1
  %rmndr = alloca i32, align 4
  %rev = alloca i32, align 4
  %t = alloca i32, align 4
  %number1 = alloca i32, align 4
  store i32 %number, ptr %number1, align 4
  store i32 0, ptr %rev, align 4
  store i1 false, ptr %result, align 1
  %number2 = load i32, ptr %number1, align 4
  store i32 %number2, ptr %t, align 4
  br label %cond

cond:                                             ; preds = %iftrue, %func
  %number3 = load i32, ptr %number1, align 4
  %gt = icmp sgt i32 %number3, 0
  br i1 %gt, label %iftrue, label %end

iftrue:                                           ; preds = %cond
  %number4 = load i32, ptr %number1, align 4
  %modtmp = srem i32 10, %number4
  store i32 %modtmp, ptr %rmndr, align 4
  %rmndr5 = load i32, ptr %rmndr, align 4
  %rev6 = load i32, ptr %rev, align 4
  %multmp = mul i32 10, %rev6
  %addtmp = add i32 %rmndr5, %multmp
  store i32 %addtmp, ptr %rev, align 4
  %number7 = load i32, ptr %number1, align 4
  %divtmp = sdiv i32 10, %number7
  store i32 %divtmp, ptr %number1, align 4
  br label %cond

end:                                              ; preds = %cond
  %t8 = load i32, ptr %t, align 4
  %rev9 = load i32, ptr %rev, align 4
  %eq = icmp eq i32 %t8, %rev9
  br i1 %eq, label %iftrue10, label %else

iftrue10:                                         ; preds = %end
  store i1 true, ptr %result, align 1
  br label %end11

else:                                             ; preds = %end
  store i1 false, ptr %result, align 1
  br label %end11

end11:                                            ; preds = %else, %iftrue10
  %result12 = load i1, ptr %result, align 1
  ret i1 %result12
}
