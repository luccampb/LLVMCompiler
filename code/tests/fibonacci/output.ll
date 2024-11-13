; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

define i32 @fibonacci(i32 %n) {
func:
  %total = alloca i32, align 4
  %c = alloca i32, align 4
  %next = alloca i32, align 4
  %second = alloca i32, align 4
  %first = alloca i32, align 4
  %n1 = alloca i32, align 4
  store i32 %n, ptr %n1, align 4
  %n2 = load i32, ptr %n1, align 4
  %calltmp = call i32 @print_int(i32 %n2)
  store i32 0, ptr %first, align 4
  store i32 1, ptr %second, align 4
  store i32 1, ptr %c, align 4
  store i32 0, ptr %total, align 4
  br label %cond

cond:                                             ; preds = %end, %func
  %c3 = load i32, ptr %c, align 4
  %n4 = load i32, ptr %n1, align 4
  %lt = icmp slt i32 %c3, %n4
  br i1 %lt, label %iftrue, label %end19

iftrue:                                           ; preds = %cond
  %c5 = load i32, ptr %c, align 4
  %le = icmp sle i32 %c5, 1
  br i1 %le, label %iftrue6, label %else

iftrue6:                                          ; preds = %iftrue
  %c7 = load i32, ptr %c, align 4
  store i32 %c7, ptr %next, align 4
  br label %end

else:                                             ; preds = %iftrue
  %second8 = load i32, ptr %second, align 4
  %first9 = load i32, ptr %first, align 4
  %addtmp = add i32 %second8, %first9
  store i32 %addtmp, ptr %next, align 4
  %second10 = load i32, ptr %second, align 4
  store i32 %second10, ptr %first, align 4
  %next11 = load i32, ptr %next, align 4
  store i32 %next11, ptr %second, align 4
  br label %end

end:                                              ; preds = %else, %iftrue6
  %next12 = load i32, ptr %next, align 4
  %calltmp13 = call i32 @print_int(i32 %next12)
  %c14 = load i32, ptr %c, align 4
  %addtmp15 = add i32 1, %c14
  store i32 %addtmp15, ptr %c, align 4
  %next16 = load i32, ptr %next, align 4
  %total17 = load i32, ptr %total, align 4
  %addtmp18 = add i32 %next16, %total17
  store i32 %addtmp18, ptr %total, align 4
  br label %cond

end19:                                            ; preds = %cond
  %total20 = load i32, ptr %total, align 4
  %calltmp21 = call i32 @print_int(i32 %total20)
  %total22 = load i32, ptr %total, align 4
  ret i32 %total22
}
