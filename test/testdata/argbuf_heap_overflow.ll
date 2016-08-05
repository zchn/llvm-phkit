; ModuleID = 'argbuf_heap_overflow.bc'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, i8*, i8*, i8*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type { %struct._IO_marker*, %struct._IO_FILE*, i32 }

@.str = private unnamed_addr constant [25 x i8] c"location of deja_vu: %p\0A\00", align 1
@.str1 = private unnamed_addr constant [22 x i8] c"location of door: %p\0A\00", align 1
@stdin = external global %struct._IO_FILE*
@.str2 = private unnamed_addr constant [4 x i8] c"%c\0A\00", align 1

; Function Attrs: nounwind uwtable
define signext i8 @deja_vu(i64 %len) #0 {
  %1 = alloca i64, align 8
  %door = alloca i8*, align 8
  %door_0 = alloca i8, align 1
  store i64 %len, i64* %1, align 8
  %2 = call noalias i8* @malloc(i64 8) #3
  store i8* %2, i8** %door, align 8
  %3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([25 x i8]* @.str, i32 0, i32 0), i8 (i64)* @deja_vu)
  %4 = load i8** %door, align 8
  %5 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([22 x i8]* @.str1, i32 0, i32 0), i8* %4)
  %6 = load i8** %door, align 8
  %7 = load i64* %1, align 8
  %8 = load %struct._IO_FILE** @stdin, align 8
  %9 = call i64 @fread(i8* %6, i64 %7, i64 1, %struct._IO_FILE* %8)
  %10 = load i8** %door, align 8
  %11 = getelementptr inbounds i8* %10, i64 0
  %12 = load i8* %11, align 1
  store i8 %12, i8* %door_0, align 1
  %13 = load i8** %door, align 8
  call void @free(i8* %13) #3
  %14 = load i8* %door_0, align 1
  ret i8 %14
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #1

declare i32 @printf(i8*, ...) #2

declare i64 @fread(i8*, i64, i64, %struct._IO_FILE*) #2

; Function Attrs: nounwind
declare void @free(i8*) #1

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  %4 = load i8*** %3, align 8
  %5 = getelementptr inbounds i8** %4, i64 1
  %6 = load i8** %5, align 8
  %7 = call i64 @strtoull(i8* %6, i8** null, i32 10) #3
  %8 = call signext i8 @deja_vu(i64 %7)
  %9 = sext i8 %8 to i32
  %10 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str2, i32 0, i32 0), i32 %9)
  ret i32 0
}

; Function Attrs: nounwind
declare i64 @strtoull(i8*, i8**, i32) #1

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.ident = !{!0}

!0 = !{!"Ubuntu clang version 3.6.2-1 (tags/RELEASE_362/final) (based on LLVM 3.6.2)"}
