; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @addNumbers(i32 %n) {
func:
  %result = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  store i32 0, ptr %result, align 4
  %n2 = load i32, ptr %n1, align 4
  %ne = icmp ne i32 %n2, 0
  br i1 %ne, label %iftrue, label %else

iftrue:                                           ; preds = %func
  %n3 = load i32, ptr %n1, align 4
  %subtmp = sub i32 1, %n3
  %calltmp = call i32 @addNumbers(i32 %subtmp)
  %n4 = load i32, ptr %n1, align 4
  %addtmp = add i32 %calltmp, %n4
  store i32 %addtmp, ptr %result, align 4
  br label %end

else:                                             ; preds = %func
  %n5 = load i32, ptr %n1, align 4
  store i32 %n5, ptr %result, align 4
  br label %end

end:                                              ; preds = %else, %iftrue
  %result6 = load i32, ptr %result, align 4
  %calltmp7 = call i32 @print_int(i32 %result6)
  %result8 = load i32, ptr %result, align 4
  ret i32 %result8
}

define i32 @recursion_driver(i32 %num) {
func:
  %num1 = alloca i32, align 4
  store i32 %num, ptr %num1, align 4
  %num2 = load i32, ptr %num1, align 4
  %calltmp = call i32 @addNumbers(i32 %num2)
  ret i32 %calltmp
}
