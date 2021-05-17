	.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	jmp	start
start:
	movq	$32, -8(%rbp)
	movq	$10, 0(%rbp)
	movq	-8(%rbp), %rax
	addq	0(%rbp), %rax
	jmp	conclusion
conclusion:
	addq	$16, %rsp
	popq	%rbp
	retq
