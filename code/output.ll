; ModuleID = 'mini-c'
source_filename = "mini-c"

declare i32 @print_int(i32)

declare float @print_float(float)

define i32 @nested_blocks(i32 %x) {
func:
  %x1 = alloca i32, align 4
  store i32 %x, ptr %x1, align 4
  %x2 = load i32, ptr %x1, align 4
  ret i32 %x2
}

define i1 @strange_assoc() {
func:
  %rhs = alloca float, align 4
  %rhs_1 = alloca float, align 4
  %lhs = alloca float, align 4
  store float 0x3FD8618620000000, ptr %lhs, align 4
  store float 1.500000e+00, ptr %rhs_1, align 4
  %rhs_11 = load float, ptr %rhs_1, align 4
  %divtmp = fdiv float 4.000000e+00, %rhs_11
  %divtmp2 = fdiv float %divtmp, 7.000000e+00
  store float %divtmp2, ptr %rhs, align 4
  %lhs3 = load float, ptr %lhs, align 4
  %rhs4 = load float, ptr %rhs, align 4
  %eq = fcmp ueq float %lhs3, %rhs4
  ret i1 %eq
}

define i32 @void_param() {
func:
  ret i32 0
}

define i32 @example_scope() {
func:
  %cond3 = alloca i1, align 1
  %x2 = alloca i32, align 4
  %y = alloca i32, align 4
  %x = alloca i32, align 4
  store i32 5, ptr %x, align 4
  store i32 2, ptr %y, align 4
  br label %cond

cond:                                             ; preds = %end, %func
  %y1 = load i32, ptr %y, align 4
  %gt = icmp sgt i32 %y1, 0
  br i1 %gt, label %iftrue, label %end9

iftrue:                                           ; preds = %cond
  store i1 true, ptr %cond3, align 1
  br label %cond4

cond4:                                            ; preds = %iftrue5, %iftrue
  %cond6 = load i1, ptr %cond3, align 1
  br i1 %cond6, label %iftrue5, label %end

iftrue5:                                          ; preds = %cond4
  store i32 17, ptr %x2, align 4
  store i1 false, ptr %cond3, align 1
  br label %cond4

end:                                              ; preds = %cond4
  store i32 2, ptr %x2, align 4
  %y7 = load i32, ptr %y, align 4
  %x8 = load i32, ptr %x2, align 4
  %subtmp = sub i32 %y7, %x8
  store i32 %subtmp, ptr %y, align 4
  br label %cond

end9:                                             ; preds = %cond
  %x10 = load i32, ptr %x2, align 4
  ret i32 %x10
}

define i32 @expr_stmt() {
func:
  %x = alloca i32, align 4
  store i32 5, ptr %x, align 4
  %x1 = load i32, ptr %x, align 4
  ret i32 0
}

define i32 @shadowing() {
func:
  %nested_blocks = alloca i32, align 4
  store i32 5, ptr %nested_blocks, align 4
  %nested_blocks1 = load i32, ptr %nested_blocks, align 4
  %calltmp = call i32 @nested_blocks(i32 %nested_blocks1)
  ret i32 %calltmp
}

define i1 @drive() {
func:
  %calltmp = call i32 @nested_blocks(i32 5)
  %ne = icmp ne i32 %calltmp, 5
  br i1 %ne, label %end_, label %rhs_

rhs_:                                             ; preds = %func
  %calltmp1 = call i32 @shadowing()
  %ne2 = icmp ne i32 %calltmp1, 5
  br i1 %ne2, label %end_4, label %rhs_3

end_:                                             ; preds = %end_4, %func
  %ortmp21 = phi i1 [ true, %end_ ], [ %ortmp20, %rhs_ ]
  ret i1 %ortmp21

rhs_3:                                            ; preds = %rhs_
  %calltmp5 = call i32 @example_scope()
  %ne6 = icmp ne i32 %calltmp5, 5
  br i1 %ne6, label %end_8, label %rhs_7

end_4:                                            ; preds = %end_8, %rhs_
  %ortmp20 = phi i1 [ true, %end_4 ], [ %ortmp19, %rhs_3 ]
  br label %end_

rhs_7:                                            ; preds = %rhs_3
  %calltmp9 = call i1 @strange_assoc()
  %not = xor i1 %calltmp9, true
  br i1 %not, label %end_11, label %rhs_10

end_8:                                            ; preds = %end_11, %rhs_3
  %ortmp19 = phi i1 [ true, %end_8 ], [ %ortmp18, %rhs_7 ]
  br label %end_4

rhs_10:                                           ; preds = %rhs_7
  %calltmp12 = call i32 @expr_stmt()
  %ne13 = icmp ne i32 %calltmp12, 0
  br i1 %ne13, label %end_15, label %rhs_14

end_11:                                           ; preds = %end_15, %rhs_7
  %ortmp18 = phi i1 [ true, %end_11 ], [ %ortmp, %rhs_10 ]
  br label %end_8

rhs_14:                                           ; preds = %rhs_10
  %calltmp16 = call i32 @void_param()
  %ne17 = icmp ne i32 %calltmp16, 0
  br label %end_15

end_15:                                           ; preds = %rhs_14, %rhs_10
  %ortmp = phi i1 [ true, %end_15 ], [ %ne17, %rhs_14 ]
  br label %end_11
}
