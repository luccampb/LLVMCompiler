; ModuleID = 'mini-c'
source_filename = "mini-c"

define i32 @multiplyNumbers(i32 %n) {
func:
  %result = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 0, ptr %result, align 4
  %n2 = load i32, ptr %n1, align 4
  %ge = icmp sge i32 %n2, 1
  br i1 %ge, label %iftrue, label %else

iftrue:                                           ; preds = %func
  %n3 = load i32, ptr %n1, align 4
  %n4 = load i32, ptr %n1, align 4
  %subtmp = sub i32 %n4, 1
  %call = call i32 @multiplyNumbers(i32 %subtmp)
  %multmp = mul i32 %n3, %call
  store i32 %multmp, ptr %result, align 4
  br label %end

else:                                             ; preds = %func
  store i32 1, ptr %result, align 4
  br label %end

end:                                              ; preds = %else, %iftrue
  %result5 = load i32, ptr %result, align 4
  ret i32 %result5
}

define i32 @rfact(i32 %n) {
func:
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  %n2 = load i32, ptr %n1, align 4
  %call = call i32 @multiplyNumbers(i32 %n2)
  ret i32 %call
}
