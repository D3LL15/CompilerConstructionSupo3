

	.global main

	.text
main:
	add $8, %rsi
	mov (%rsi), %rcx
	mov (%rcx), %dl
	movzx %dl, %rbx
	sub $48, %rbx
	mov %rbx, %rax
	mov $1, %rdx

	cmp $0, %rbx
	je finish

	sub %rdx, %rax
cnt:
	cmp %rax, %rbx
	je dec

	push %rbx
	push %rax
	push %rdx
	mov star, %rdi
	call putchar
	pop %rdx
	pop %rax
	pop %rbx

	add $1, %rax
	jmp cnt

dec:
	cmp %rbx, %rdx
	je finish

	add $1, %rdx
	mov %rbx, %rax
	sub %rdx, %rax

	push %rbx
	push %rax
	push %rdx
	mov newline, %rdi
	call putchar
	pop %rdx
	pop %rax
	pop %rbx

	jmp cnt

finish:
	ret


star: 	.byte '*'
newline: .byte 10
