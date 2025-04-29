; Copied directly from the documentation
; Declare the string constant as a global constant.
@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00"

@.str2 = private unnamed_addr constant [6 x i8] c"None\0A\00"

; External declaration of the puts function
declare i32 @puts(i8* nocapture) nounwind; Module declaration
declare i32 @printf(i8*, ...)

; Declare the malloc function
declare i8* @malloc(i64)

; Define the triple structure type
%triple = type { %triple*, i32, %triple* }

; Define the pair structure type
%pair = type {i32, i32}

; Define the function type
%fn_type = type %pair (i32)

; Define optional pair type
%opt_pair = type {i1, i32}

; Function declaration for map_get
define %opt_pair @map_get(%triple* %l, i32 %key, %fn_type* %f) {
    %cmp = icmp eq %triple* %l, null    
    br i1 %cmp, label %if_null, label %if_not_null

if_null:
    ret %opt_pair {i1 0, i32 0}

if_not_null:

    %key_i1 = icmp eq i32 %key, 0
    br i1 %key_i1, label %key_zero, label %key_not_zero

key_zero:
    %data = getelementptr %triple, %triple* %l, i32 0, i32 1
    %data_i32 = load i32, i32* %data
    %pair_val = insertvalue %opt_pair undef, i1 1, 0  
    %pair_val1 = insertvalue %opt_pair %pair_val, i32 %data_i32, 1
    ret %opt_pair %pair_val1

key_not_zero:

    %l_key = call %pair %f(i32 %key)
    %rest = extractvalue %pair %l_key, 0
    %br = extractvalue %pair %l_key, 1
    %br_i1 = icmp eq i32 %br, 1
    br i1 %br_i1, label %br_right, label %br_left

br_right:
    %l_right = getelementptr %triple, %triple* %l, i32 0, i32 2
    %l_right_v = load %triple*, %triple** %l_right
    %res_right = call %opt_pair @map_get(%triple* %l_right_v, i32 %rest, %fn_type* %f)
    ret %opt_pair %res_right

br_left:
    %l_left = getelementptr %triple, %triple* %l, i32 0, i32 0
    %l_left_v = load %triple*, %triple** %l_left
    %res_left = call %opt_pair @map_get(%triple* %l_left_v, i32 %rest, %fn_type* %f)
    ret %opt_pair %res_left
}

;Definition of f
define %pair @f(i32 %key) {
    ; Calculate %key / 2
    %div = sdiv i32 %key, 2

    ; Calculate %key % 2
    %mod = srem i32 %key, 2

    %pair_val = insertvalue %pair undef, i32 %div, 0
    %pair_val1 = insertvalue %pair %pair_val, i32 %mod, 1

    ret %pair %pair_val1
}



; Definition of map_set
define %triple* @map_set(%triple* %l, i32 %key, i32 %value, %fn_type* %f) {
    ; a register to store the pointer to the triple
    %triple_ptr = alloca %triple*
    store %triple* %l, %triple** %triple_ptr
    ; compare the pointer to the triple with null
    %cmp = icmp eq %triple* %l, null

    ; if the pointer to the triple is null, then jump to the if_null label
    br i1 %cmp, label %if_null, label %if_not_null

if_null:
    ; allocate memory for the new triple
    %size_ptr = getelementptr %triple, %triple* null, i32 1
    %size_i64 = ptrtoint ptr %size_ptr to i64
    %new_triple = call %triple* @malloc(i64 %size_i64)

    %first_element_ptr = getelementptr %triple, %triple* %new_triple, i32 0, i32 0
    %second_element_ptr = getelementptr %triple, %triple* %new_triple, i32 0, i32 1
    %third_element_ptr = getelementptr %triple, %triple* %new_triple, i32 0, i32 2

    ; Initialize the first element to null
    store %triple* null, %triple** %first_element_ptr

    ; Initialize the second element to 0
    store i32 0, i32* %second_element_ptr

    ; Initialize the third element to null
    store %triple* null, %triple** %third_element_ptr

    store %triple* %new_triple, %triple** %triple_ptr

    ; jump to the if_not_null label
    br label %if_not_null

if_not_null:
    %triple_ptr_v = load %triple*, %triple** %triple_ptr
    %key_i1 = icmp eq i32 %key, 0
    br i1 %key_i1, label %key_zero, label %key_not_zero

key_zero:

    %data = getelementptr %triple, %triple* %triple_ptr_v, i32 0, i32 1
    store i32 %value, i32* %data
    ret %triple* %triple_ptr_v

key_not_zero:

    %l_key = call %pair %f(i32 %key)
    %rest = extractvalue %pair %l_key, 0
    %br = extractvalue %pair %l_key, 1
    %br_i1 = icmp eq i32 %br, 1
    br i1 %br_i1, label %br_right, label %br_left

br_right:
    %old_triple_right = getelementptr %triple, %triple* %triple_ptr_v, i32 0, i32 2
    %old_triple_right_v = load %triple*, %triple** %old_triple_right
    %new_triple_right = call %triple* @map_set(%triple* %old_triple_right_v, i32 %rest, i32 %value, %fn_type* %f)
    ; store the new triple in the first element of the new triple
    %first_right = getelementptr %triple, %triple* %triple_ptr_v, i32 0, i32 2
    store %triple* %new_triple_right, %triple** %first_right
    ret %triple* %triple_ptr_v


br_left:
    %old_triple_left = getelementptr %triple, %triple* %triple_ptr_v, i32 0, i32 0
    %old_triple_left_v = load %triple*, %triple** %old_triple_left
    %new_triple_left = call %triple* @map_set(%triple* %old_triple_left_v, i32 %rest, i32 %value, %fn_type* %f)
    %first_left = getelementptr %triple, %triple* %triple_ptr_v, i32 0, i32 0
    store %triple* %new_triple_left, %triple** %first_left
    ret %triple* %triple_ptr_v
}

; Definition of main function
define i32 @main() { ; i32()*
    ; Convert [13 x i8]* to i8  *...
    %cast210 = getelementptr [4 x i8],[4 x i8]* @.str, i64 0, i64 0
    %cast211 = getelementptr [6 x i8],[6 x i8]* @.str2, i64 0, i64 0

    %triple_ptr1 = call %triple* @map_set(%triple* null, i32 121, i32 92, %fn_type* @f)

    %triple_ptr2 = call %triple* @map_set(%triple* %triple_ptr1, i32 120, i32 93, %fn_type* @f)

    %triple_ptr3 = call %triple* @map_set(%triple* %triple_ptr2, i32 121, i32 94, %fn_type* @f)

    %res = call %opt_pair @map_get(%triple* %triple_ptr2, i32 121, %fn_type* @f)
    %res_i1 = extractvalue %opt_pair %res, 0
    br i1 %res_i1, label %if_not_null, label %if_null

if_null:
    call i32 (i8*, ...) @printf(i8* %cast211)
    ret i32 0

if_not_null:
    %v = extractvalue %opt_pair %res, 1
    call i32 (i8*, ...) @printf(i8* %cast210,  i32 %v)
    ret i32 0   
}
