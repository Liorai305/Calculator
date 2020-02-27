section	.rodata			; we define (global) read-only variables in .rodata section
	format_string: db "%s", 0	; format string
	format_string_n: db "%s", 10, 0	; format string
    format_hexa_base: db "%X", 0 ;format string hexa base output
    format_hexa_base_n: db "%X",10, 0 ;format string hexa base output
    format_decimal: db "%d",0 ;format decimal base output
    format_with_zero: db "%02X", 0
	calc: db "calc: ",0 ; output string
	new_line:db "",0 ; for moving one line down 
	Stack_Overflow: db "Error: Operand Stack Overflow",0 ; Error string in case calculation attempts to push operands onto the operand stack and there is no free space.
	Stack_Insufficient: db "Error: Insufficient Number of Arguments on Stack",0 ;Error string in case of attemption to pop an empty stack.
	Over_C8: db "wrong Y value",0 ;Error string in case y>200

section .bss			; we define (global) uninitialized variables in .bss section
    operand_stack_size equ 20
	operand_stack: resb operand_stack_size	; initialize the operand stack.
    buffer:         resb 80             ;store my input
    address_buffer:         resb 160             ;store adreeses of linkim
    
section .data			; we define (global) initialized variables in .data section
    operation_counter: dd 0		; count how much operation (both successful and unsuccessful) performed.
	sum: dd 0  
	data:dd 0  ;help with creating link
	counter:dd 0
	pointer: dd 0 ; help moving throw the buffer
    pointer2: dd 0 ; help moving throw the operand stack
    pointer3: dd 0 ; help moving throw the operand stack
    pointer4: dd 0 ; help moving throw the operand stack
    save_ebx:dd 0
    save_ecx:dd 0
    save_edx:dd 0
    carry_f: dd 0 ; the carry flag of unsigned addition
    special_case_f: dd 0 ;indicates we are in special_case operation
	next_link: dd 0; stores the adrees of the next link
	first: dd 0; stores the adrees of the first link in number
	debug_f:dd 0 ;flag for debug mode 

section .text
  align 16
     global main
     extern printf
     extern fflush
     extern malloc
     extern calloc
     extern free
     extern gets
     extern fgets

main: 
    mov ecx, 0
	mov edx, [esp+4] ; put num of arguments in edx
    push ebp 
	mov ebp, esp	
	pushad
	cmp edx, 2 ; when debug mode edx = 2 , then change debug flag
	jne start
	mov dword [debug_f], 1
	start:
	mov ecx,0
	call my_calc
    popad			
	mov esp, ebp	
	pop ebp
	ret

	
my_calc:
    push calc			;print calc to screen
	push format_string	
	call printf
	add esp, 8                     ;remove pushed arguments
    push dword 80                   ;max lenght
    push dword buffer               ;input buffer
    call gets                       ;read from screen into buffer
    add esp, 8					;remove pushed arguments
    mov dword [special_case_f],0
    mov ecx, [pointer2]
    mov dword [pointer], 0
    mov dword [counter], 0
    mov dword [carry_f],0
    inc dword [operation_counter]  ; count another operation
    cmp byte [buffer],'+'
    jz unsigned_addition
    cmp byte [buffer],'d'
    jz duplicate
    cmp byte [buffer],'p'
    jz pop_and_print
    cmp byte [buffer],'^'   
    jz special_case
    cmp byte [buffer],'v'
    jz handle_v
    cmp byte [buffer],'n'
    jz one_bits
    dec dword [operation_counter]  ; not an operation
    cmp byte [buffer],'q'            ;find the requested operation 
    jz quit
    mov edx, 0                      ; edx will store the length of the buffer
	cmp ecx, operand_stack_size 					;no place in the operand_stack for pushing number
	jz handle_overflow_error 		;prints an error to the user
	mov eax, 0
	skip: ; skip  over the leading zeroes in the input
        cmp byte [buffer+edx], 48
        je incindex
        jmp count
        
    incindex: ;increase eax by one
        mov eax, 1
        inc edx 
        jmp skip
      
      ; this function creates link
    %macro create_link 4
        cmp %1, 0
        %2 %3
        mov ecx, [next_link] ; now ecx stores the adress of the next link
        mov dword [save_ebx], ebx
        mov ebx, [data]
		mov [ecx], ebx		;stores the data in the first place in the allocated memory
		inc ecx
		mov dword [save_edx], edx
		mov dword [save_ecx], ecx
        cmp dword [counter], 0 ; check if we finished going over the number
        jz operand_stack_push
		push dword 5       ; size of memory
		push dword 1 		; size of each item in memory ( 1 byte)
		call calloc			; allocate 5 bytes in memory, adrees goes to eax
		add esp, 8                     ;remove pushed arguments
		mov edx,[save_edx]
		mov ecx, [save_ecx]
		mov dword [ecx], eax		;stores the next_link adress in the next 4 bytes (pointer to the next link)
		mov dword [next_link], eax		; stores the adress of the next link 
		mov ebx, [save_ebx]
		mov dword [data], 0

		jmp %4				
    %endmacro
    
    ;this function pops the link on the top of the stack and removes it
    %macro pop_operand_stack 1
        cmp ecx, 0                      ;check if the operand stack is empty, if true error
        jz handle_stack_Insufficient
        sub ecx, 4 
        mov edx, [operand_stack+ecx]    ; moves the argument in the top of the stack to edx
        mov dword [operand_stack+ecx], 0 ; clear the place
        mov dword [pointer2], ecx       ; saves ecx from changes
        jmp %1
    %endmacro
        
    ; this function allocates memory for the first link in a new linklist
    %macro before_creating_link 1
        mov dword [pointer2], ecx ; the call to calloc changes ecx
        mov [pointer], edx ;when using calloc edx is changing
        push dword 5       ; size of memory
		push dword 1 		; size of each item in memory ( 1 byte)
		call calloc			; allocate 5 bytes in memory, adrees goes to eax
		add esp, 8  
		mov dword [next_link], eax
		mov dword edx, [pointer]
		mov dword [first], eax    ;saving the adress of the first link
		jmp %1
	%endmacro
     
            
    count: ; calculate how much digits in the input string (number)
        sub edx, eax
        mov eax, 0
        mov ebx , 0
        mov bl , [buffer+edx]
        cmp byte bl , 0 ; check if the next character (character = byte) is zero (i.e. null string termination)
        je call_macro
        inc edx
        inc dword [counter]
        jmp count
        
    call_macro:
        before_creating_link handle_number

        
    inc_counter1:
        inc dword [counter]
        jmp c3
    
    inc_counter2:
        inc dword [counter]
        mov edx,ebx ;when only one of the lists is over
        jmp c4
    
    unsigned_addition:
        mov ecx, [pointer2]
        cmp ecx, 4  ; there is only one number in the stack, not enough to make an operation
        je handle_stack_Insufficient
        mov dword [counter] ,1 ;for create link func
        pop_operand_stack return1 ; pops link from the stack
        return1:
        mov ebx, edx
        pop_operand_stack return2 ; pops link from the stack
        return2:
        before_creating_link return3
        return3:
            mov eax,0
            mov al, [edx] ; moves the data of the link 
            mov ecx, 0
            mov cl, [ebx]  ; moves the data of the link 
            mov dword [pointer], edx
            mov edx, [carry_f]
            mov dword [carry_f], 0
            clc
            add al, dl ; make the addition 
            jc change_flag1
            continue1:
                clc
                add al, cl
                jc change_flag2
                continue2:
                    mov dword [data], eax
                    mov eax, 0
                    create_link eax ,jnz, c1, c2
                    c2:
                    mov edx, [pointer]
                    inc edx
                    inc ebx
                    mov eax, [edx]          ;the adrees of the next link goes into eax
                    mov dword [pointer4], eax ; eax changes when we use free
                    dec edx 
                    push edx ;free to the privious adress
                    call free
                    add esp, 4
                    mov eax, [pointer4]
                    mov edx, eax ; next adress returns to edx
                    mov ecx, [ebx]
                    mov dword [pointer], edx
                    mov dword [pointer4], ecx ; ecx changes when we use free
                    dec ebx 
                    push ebx
                    call free ;free to the privious adress
                    add esp, 4
                    mov edx, [pointer]
                    mov ecx, [pointer4]
                    mov ebx, ecx ; next adress returns to ecx 
                    cmp ebx, 0
                    jz inc_counter1
                    c3:
                    cmp edx, 0
                    jz inc_counter2
                    c4:
                    cmp dword [counter], 3
                    je done_adding_two_lists
                    cmp dword [counter], 2
                    je add_only_one_list
                    jmp return3


    
    add_only_one_list:
        ;edx has the adress to the next link
        cmp dword [carry_f], 0
        jne with_carry
        mov ebx, [save_ecx] ;save_ecx stores the adrees of the last link in the new link list
        mov dword [pointer4], eax 
        mov dword [pointer], edx 
        mov eax, [ebx]
        mov dword [save_ecx], ecx 
        push eax 
        call free
        add esp, 4
        mov eax, [pointer4]
        mov edx, [pointer]
        mov ecx, [save_ecx]
        mov [ebx], edx ;the last link we created gets the rest of the existing link list
        jmp push_without_putting_zero
    with_carry:
        cmp edx,0
        je done_adding_two_lists
        mov eax,0
        mov al, [edx]
        mov dword [pointer], edx
        mov edx, [carry_f]
        mov dword [carry_f], 0
        add al, dl
        jc change_flag3
        c6:
        mov dword [data], eax
        mov eax, 0
        create_link eax ,jnz, c1, c7
        c7:
        mov edx,[pointer]
        inc edx;get the adress to the next link
        mov eax, [edx]
        mov dword [pointer4], eax ; eax changes when we use free
        dec edx 
        push edx ;free to the privious adress
        call free
        add esp, 4
        mov eax, [pointer4]
        mov edx, eax
        jmp add_only_one_list
        
    done_adding_two_lists:
        cmp dword [carry_f], 1
        jnz c5 ;carry is 0 
        mov dword [data], 1
        mov dword [counter], 0
        create_link dword [counter],jnz , c1, c5
        c5:
            jmp operand_stack_push ;done creating link list, pushing it to the stack
        
    change_flag1:
        mov dword [carry_f], 1
        jmp continue1
        
    change_flag2:
        mov dword [carry_f], 1
        jmp continue2
    
    change_flag3:
        mov dword [carry_f], 1
        jmp c6
    

    pop_and_print:
        mov ebx, 0 ;index of address_buffer
        pop_operand_stack loop_address
        loop_address: ; creates an array of link adresses
            mov dword [address_buffer+ebx], edx
            add ebx, 4
            inc edx
            mov eax, [edx]
            mov edx, eax
            cmp edx, 0
            jnz loop_address
        sub ebx, 4
        mov edx, [address_buffer+ebx]
        mov dword [first], edx
 
        cmp ebx, 0
        jne skip_zero
        add_ebx:    ;for the loop
        add ebx, 4
        loop_print: 
            sub ebx, 4
            mov edx, [address_buffer+ebx]
            mov eax, 0
            mov al, [edx]
            mov dword [pointer4], edx ;because edx change in printf
            cmp edx, [first]
            jne continue
            ;cmp eax, 0
            ;je continue
            push dword eax; call printf with 2 arguments -  
            push format_hexa_base	; pointer to str and pointer to format string
            call printf
            add esp, 8		; clean up stack after call
            jmp continuee
            continue:
                push dword eax; call printf with 2 arguments -  
                push format_with_zero	; pointer to str and pointer to format string
                call printf
                add esp, 8		; clean up stack after call
                continuee:
                mov edx, [pointer4]
                push edx
                call free
                add esp, 4
                cmp ebx,0
                jne loop_print
        push new_line
        push format_string_n
        call printf
        add esp, 8
        mov ecx, [pointer2]
        mov dword [first], 0
        jmp my_calc

    skip_zero:
        mov eax, 0
        mov al, [edx]
        cmp eax, 0
        jne add_ebx
        cmp ebx, 0
        je add_ebx
        sub ebx, 4
        push edx
        call free
        add esp, 4
        mov edx, [address_buffer+ebx]
        mov dword [first], edx
        jmp skip_zero
    
    duplicate:
        mov ecx, [pointer2]
        cmp ecx, 0
        je handle_stack_Insufficient
        cmp ecx, operand_stack_size
        je handle_overflow_error
        sub ecx, 4
        mov edx, [operand_stack+ecx]    ; moves the argument in the top of the stack to eax
        add ecx, 4
        mov dword [pointer2], ecx       ; saves ecx from changes
        before_creating_link dup_loop
        dup_loop:
            mov eax, [edx]
            mov dword [data], eax 
            inc edx 
            mov eax, [edx] ; moves the address of the next link to eax
            mov edx, eax
            mov dword [counter], eax ;indicate when the next address is 0, last link
            mov eax, 0
            create_link eax, jnz, c1, dup_loop
    
    special_case:
        mov ecx, [pointer2]
        cmp ecx, 4
        je handle_stack_Insufficient
        pop_operand_stack c1_special_case ;get X
        c1_special_case:
            mov ebx, edx ;ebx now point to the address of X link list
            pop_operand_stack c2_special_case ;edx now point to the address of Y link list
            c2_special_case:
                mov eax, [edx] ;eax stores the data of Y
                cmp eax, 0XC8
                ja handle_over_C8 ; Y>200
                inc edx 
                mov eax, [edx]
                cmp eax, 0
                jne handle_over_C8 ; Y is made of more then one link >200
                dec edx 
                mov eax, [edx]
                mov dword [pointer4], eax 
                mov dword [pointer2], ecx 
                push edx 
                call free
                add esp, 4
                mov eax, [pointer4]
                mov ecx, [pointer2]
                inc dword [special_case_f] ; change the special case flag 
                mov dword [operand_stack+ecx], ebx ; return X to the stack
                add ecx, 4 ;point to the next avilable place in the stack
                mov dword [pointer2], ecx
                special_case_loop:
                    cmp eax, 0
                    je my_calc
                    mov dword [pointer3], eax
                    jmp duplicate
                    c3_special_case:
                    inc dword [special_case_f]
                    jmp unsigned_addition
                    c4_special_case:
                    mov eax, [pointer3]
                    dec dword [special_case_f]
                    dec eax
                    jmp special_case_loop
                    
        
    handle_v:
        mov ecx, [pointer2]
        cmp ecx, 4
        je handle_stack_Insufficient
        pop_operand_stack c1_handle_v ;get X
        c1_handle_v:
            mov ebx, edx ;ebx now point to the address of X link list
            pop_operand_stack c2_handle_v ;edx now point to the address of Y link list
            c2_handle_v:
                mov eax, [edx] ;eax stores the data of Y
                cmp eax, 0XC8
                ja handle_over_C8 ; Y>200
                inc edx 
                mov eax, [edx]
                cmp eax, 0
                jne handle_over_C8 ; Y is made of more then one link >200
                dec edx 
                mov eax, [edx] 
                mov dword [pointer4], eax 
                mov dword [pointer2], ecx 
                push edx 
                call free
                add esp, 4
                mov eax, [pointer4]
                mov ecx, [pointer2]
                mov dword [operand_stack+ecx], ebx ; return X to the stack
                add ecx, 4
                mov dword [pointer2], ecx
                cmp eax, 0
                je my_calc
                c4_handle_v:
                mov dword [pointer2], ecx
                mov ecx,0
                mov cl, [ebx] ; ecx stores the data of the first link in X
                shr ecx, 1 ; divide the data by 2
                mov [ebx], cl
                inc ebx 
                mov ecx, [ebx] ;set the adress after mov
                dec ebx 
                mov edx, ebx ; edx is the prev link
                mov ebx, ecx ; ebx is the curr link
                v_loop:
                    cmp ebx, 0
                    je reduce_eax
                    clc
                    mov ecx, 0
                    mov cl, [ebx] ; ecx stores the data of the first link in X
                    shr ecx, 1 ; divide the data by 2
                    jc add_carry
                    c3_handle_v:
                    mov [ebx], cl ; the new value after the shift
                    mov edx, ebx  
                    inc ebx 
                    mov ecx, [ebx]
                    dec ebx 
                    mov ebx, ecx ; ebx is the curr link
                    jmp v_loop                                            
    reduce_eax:
        dec eax 
        cmp eax, 0
        je my_calc
        mov ecx, [pointer2]
        sub ecx, 4
        mov ebx, [operand_stack+ecx]
        add ecx, 4
        jmp c4_handle_v
        
    add_carry:
        mov dword [pointer4], eax ;save eax
        mov eax, 0
        mov al, [edx] ; eax points on the data of the prev link
        add eax, 128 
        mov [edx], al
        mov eax, [pointer4] ;return eax
        jmp c3_handle_v
                    
    
    one_bits:
        mov ecx, [pointer2]
        pop_operand_stack c1_one_bits ; get one number in edx 
        c1_one_bits:
        before_creating_link c0_one_bits
        c0_one_bits:
        mov ebx, 8
        mov eax, 0
        mov al, [edx]
        mov dword [data], 0
        c2_one_bits:
            cmp edx, 0
            je finish_one_bits
            cmp ebx, 0
            je renew_ebx
            dec ebx 
            shr eax, 1
            jc inc_one_bits
            c3_one_bits:
            clc
            jmp c2_one_bits
            
    inc_one_bits:
        inc dword [data] ; summerize the 1 bits in the number 
        jmp c3_one_bits
        
    renew_ebx:  
            mov ebx, 8
            inc edx
            mov eax, 0
            mov eax, [edx]
            mov dword [pointer4], eax
            dec edx
            push edx 
            call free
            add esp, 4 
            mov eax, [pointer4]
            mov edx, eax
            mov eax,0
            cmp edx, 0
            je c2_one_bits
            mov al, [edx]
            jmp c2_one_bits
    
    finish_one_bits:
        cmp dword [data], 255
        ja add_two_links
        add_one_link:
            mov dword [counter], 0
            create_link dword [counter], jnz, c1, c2 ;always returns to my_calc lable
        add_two_links:
            mov eax, 0
            mov eax, [data]
            mov ebx, 256
            div ebx
            mov dword [data], edx 
            mov eax, 0
            mov dword [counter], 1
            create_link eax, jnz, c1, c0_add_two_links
            c0_add_two_links:
            mov dword [counter], 0
            mov dword [data], 1
            create_link dword [counter], jnz, c1, c2 ;always returns to my_calc lable
            
	
     handle_number: ; the input is a number
        dec edx ;we need to reduce edx by one, because edx point to the nullterminator
        mov ebx,16                    ; handaling a number 
        cmp dword [counter], 0 ; check if we finished going over the number
        jz operand_stack_push
        dec dword [counter]
        mov eax, 0
        mov al, [buffer+edx]  ; move the char in buffer+edx place to eax
        cmp eax, 57
        jg sub_char_first
        sub eax, 0x30               ; convert the char to integer in hexa representation
		mov dword [data], eax       ; mov the sum into data
        handle_second_char:
			dec edx
           ; cmp dword [counter], 0 ; check if we finished going over the number
            create_link dword [counter], jnz, c1, handle_number
            ;jz create_link
            c1:
            dec dword [counter]
            mov dword [pointer], edx ;when using mul edx is changing
            mov eax, 0
            mov al, [buffer+edx]
            ;mov eax ,[buffer+edx]       ; move the char in buffer+edx place to eax
            cmp eax, 57
            jg sub_char_second
            sub eax, 0x30
            continue_second_char:; convert the char to integer in hexa representation
                mul ebx ; eax<- eax * ebx
                mov edx, [data]
                add eax, edx
                mov dword [data], eax       ; mov the sum into data
                mov eax,0
                mov edx, [pointer]
                create_link eax, jnz, c1, handle_number
                
    
            
            
   sub_char_first:
        sub eax,55                   ; convert the char to integer in hexa representation
        mov dword [data], eax       ; mov the sum into data
        jmp handle_second_char
        
    sub_char_second:
        sub eax,55                   ; convert the char to integer in hexa representation
        jmp continue_second_char
        

;in our linked list we insert links to the end of the list			

		
     operand_stack_push:
        mov ecx, [save_ecx] ;pointer3 stores the address of the last link
        mov dword [pointer4], eax 
        mov dword [pointer], edx 
        mov eax, [ecx]
        mov dword [save_ecx], ecx 
        push eax 
        call free
        add esp, 4
        mov eax, [pointer4]
        mov edx, [pointer]
        mov ecx, [save_ecx]
        mov dword [ecx], 0
        push_without_putting_zero:
        mov dword [data], 0
		mov ebx, [first]
        mov ecx, [pointer2]
        cmp ecx, operand_stack_size
        je handle_overflow_error
		mov dword [operand_stack+ecx], ebx		;the ecx is now pointing on the number we created
		add ecx, 4 		;not sure of syntax, but we need to inc ecx in 4 (its a pointer to int)
		mov dword [pointer2], ecx
		mov dword [next_link], 0 				; clear the next link for getting new number   ;we also need th free the buffer but im not sure how
		mov dword [first], 0 
		mov dword [carry_f], 0
		cmp dword [special_case_f], 1
		je c3_special_case
        cmp dword [special_case_f], 2
		je c4_special_case
		cmp dword [debug_f], 1
		je debug_print
		jmp my_calc		; get another request from the user
	
	debug_print:
        mov ebx, 0 ;index of address_buffer
        sub ecx, 4
        mov edx, [operand_stack+ecx]
        add ecx, 4
        debug_loop_address: ; creates an array of link adresses
            mov dword [address_buffer+ebx], edx
            add ebx, 4
            inc edx
            mov eax, [edx]
            mov edx, eax
            cmp edx, 0
            jnz debug_loop_address
        sub ebx, 4
        mov edx, [address_buffer+ebx]
        mov dword [first], edx
        cmp ebx, 0
        jne debug_skip_zero
        debug_add_ebx:    ;for the loop
        add ebx, 4
        debug_loop_print: 
            sub ebx, 4
            mov edx, [address_buffer+ebx]
            mov eax, 0
            mov al, [edx]
            mov dword [pointer4], edx ;because edx change in printf
            cmp edx, [first]
            jne debug_continue
            push dword eax; call printf with 2 arguments -  
            push format_hexa_base	; pointer to str and pointer to format string
            call printf
            add esp, 8		; clean up stack after call
            jmp debug_continuee
            debug_continue:
                push dword eax; call printf with 2 arguments -  
                push format_with_zero	; pointer to str and pointer to format string
                call printf
                add esp, 8		; clean up stack after call
                debug_continuee:
                mov edx, [pointer4]
                cmp ebx,0
                jne debug_loop_print
        push new_line
        push format_string_n
        call printf
        add esp, 8
        mov ecx, [pointer2]
        mov dword [first], 0
        jmp my_calc
        
        
        debug_skip_zero:
        mov eax, 0
        mov al, [edx]
        cmp eax, 0
        jne debug_add_ebx
        cmp ebx, 0
        je debug_add_ebx
        sub ebx, 4
        mov edx, [address_buffer+ebx]
        mov dword [first], edx
        jmp debug_skip_zero

	
    handle_overflow_error:
		push Stack_Overflow			;print error to screen
		push format_string_n	
		call printf
		add esp, 8                      ;remove pushed arguments
		jmp my_calc						; get another request from the user
		
    handle_stack_Insufficient:
        push Stack_Insufficient			;print error to screen
		push format_string_n	
		call printf
		add esp, 8                      ;remove pushed arguments
		jmp my_calc						; get another request from the user 
		
    handle_over_C8:
        mov dword [pointer4], edx ;because edx changes after printf
        push Over_C8			;print error to screen
		push format_string_n	
		call printf
		add esp, 8                      ;remove pushed arguments
		mov ecx, [pointer2] ;return the value of ecx 
		mov edx, [pointer4]
		mov dword [operand_stack+ecx], edx ;stack uneffected
		add ecx, 4
		mov dword [operand_stack+ecx], ebx ;stack uneffected
		add ecx,4
		mov dword [pointer2], ecx
		jmp my_calc						; get another request from the user 
            
    
    quit:
        mov eax, 0
        mov eax, [operation_counter] ;print the number of operations
        push eax
        push format_hexa_base_n
		call printf ; print the number of operation
		add esp, 8 
		mov ecx, [pointer2]
        cmp ecx, 0
        je end
        sub ecx, 4
		delete_loop:  ; free the allocated memory in the operand stack
        mov eax, 0
        mov eax, [operand_stack+ecx]
        link_list_delete_loop:
            inc eax 
            mov ebx, [eax]
            dec eax
            mov dword [pointer2], ecx
            push eax
            call free
            add esp, 4
            mov ecx, [pointer2]
            mov eax, ebx 
            cmp eax, 0
            jne link_list_delete_loop ;going over the link list and freeing the memory
            cmp ecx, 0
            je end
            sub ecx, 4
            jmp delete_loop
            end:
                ret
            
        
