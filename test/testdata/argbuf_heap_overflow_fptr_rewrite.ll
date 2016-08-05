; ModuleID = 'argbuf_heap_overflow_fptr_rewrite.bc'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }
%struct.MsgPrinter = type { i32 (i8*)* }

@.str = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str1 = private unnamed_addr constant [25 x i8] c"location of deja_vu: %p\0A\00", align 1
@.str2 = private unnamed_addr constant [22 x i8] c"location of door: %p\0A\00", align 1
@.str3 = private unnamed_addr constant [48 x i8] c"location of msg_printer->fptr before fread: %p\0A\00", align 1
@stdin = external global %struct._IO_FILE*
@.str4 = private unnamed_addr constant [47 x i8] c"location of msg_printer->fptr after fread: %p\0A\00", align 1
@.str5 = private unnamed_addr constant [13 x i8] c"pwn me here\0A\00", align 1
@.str6 = private unnamed_addr constant [4 x i8] c"%c\0A\00", align 1

; Function Attrs: nounwind uwtable
define i32 @printstr(i8* %msg) #0 {
  %1 = alloca i8*, align 8
  store i8* %msg, i8** %1, align 8
  %2 = load i8** %1, align 8
  %3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i8* %2)
  ret i32 %3
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: nounwind uwtable
define signext i8 @deja_vu(i64 %len) #0 {
  %1 = alloca i64, align 8
  %door = alloca i8*, align 8
  %msg_printer = alloca %struct.MsgPrinter*, align 8
  %door_0 = alloca i8, align 1
  store i64 %len, i64* %1, align 8
  %2 = call noalias i8* @malloc(i64 8) #3
  store i8* %2, i8** %door, align 8
  %3 = call noalias i8* @malloc(i64 8) #3
  %4 = bitcast i8* %3 to %struct.MsgPrinter*
  store %struct.MsgPrinter* %4, %struct.MsgPrinter** %msg_printer, align 8
  %5 = load %struct.MsgPrinter** %msg_printer, align 8
  %6 = getelementptr inbounds %struct.MsgPrinter* %5, i32 0, i32 0
  store i32 (i8*)* @printstr, i32 (i8*)** %6, align 8
  %7 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([25 x i8]* @.str1, i32 0, i32 0), i8 (i64)* @deja_vu)
  %8 = load i8** %door, align 8
  %9 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([22 x i8]* @.str2, i32 0, i32 0), i8* %8)
  %10 = load %struct.MsgPrinter** %msg_printer, align 8
  %11 = getelementptr inbounds %struct.MsgPrinter* %10, i32 0, i32 0
  %12 = load i32 (i8*)** %11, align 8
  %13 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([48 x i8]* @.str3, i32 0, i32 0), i32 (i8*)* %12)
  %14 = load i8** %door, align 8
  %15 = load i64* %1, align 8
  %16 = load %struct._IO_FILE** @stdin, align 8
  %17 = call i64 @fread(i8* %14, i64 %15, i64 1, %struct._IO_FILE* %16)
  %18 = load %struct.MsgPrinter** %msg_printer, align 8
  %19 = getelementptr inbounds %struct.MsgPrinter* %18, i32 0, i32 0
  %20 = load i32 (i8*)** %19, align 8
  %21 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([47 x i8]* @.str4, i32 0, i32 0), i32 (i8*)* %20)
  %22 = load %struct.MsgPrinter** %msg_printer, align 8
  %23 = getelementptr inbounds %struct.MsgPrinter* %22, i32 0, i32 0
  %24 = load i32 (i8*)** %23, align 8
  %25 = call i32 %24(i8* getelementptr inbounds ([13 x i8]* @.str5, i32 0, i32 0))
  %26 = load i8** %door, align 8
  %27 = getelementptr inbounds i8* %26, i64 0
  %28 = load i8* %27, align 1
  store i8 %28, i8* %door_0, align 1
  %29 = load i8** %door, align 8
  call void @free(i8* %29) #3
  %30 = load i8* %door_0, align 1
  ret i8 %30
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #2

declare i64 @fread(i8*, i64, i64, %struct._IO_FILE*) #1

; Function Attrs: nounwind
declare void @free(i8*) #2

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  %4 = load i32* %2, align 4
  %5 = icmp sle i32 %4, 1
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  store i32 -1, i32* %1
  br label %15

; <label>:7                                       ; preds = %0
  %8 = load i8*** %3, align 8
  %9 = getelementptr inbounds i8** %8, i64 1
  %10 = load i8** %9, align 8
  %11 = call i64 @strtoull(i8* %10, i8** null, i32 10) #3
  %12 = call signext i8 @deja_vu(i64 %11)
  %13 = sext i8 %12 to i32
  %14 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str6, i32 0, i32 0), i32 %13)
  store i32 0, i32* %1
  br label %15

; <label>:15                                      ; preds = %7, %6
  %16 = load i32* %1
  ret i32 %16
}

; Function Attrs: nounwind
declare i64 @strtoull(i8*, i8**, i32) #2

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"Ubuntu clang version 3.6.2-1 (tags/RELEASE_362/final) (based on LLVM 3.6.2)"}
