	.globl _append
	.align	4
_append:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rsi,%rax
	cmpq $0,%rdi
	je then5
	movq 0(%rdi),%rbx
	movq 8(%rdi),%rdi
	movq %rax,%rsi
	callq _append
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
	jmp next6
then5:
next6:
	addq $8,%rsp
	popq %rbx
	popq %rbp
	retq

	.globl _ltList
	.align	4
_ltList:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rsi,%rax
	cmpq $0,%rax
	je then18
	movq 0(%rax),%rsi
	cmpq %rdi,%rsi
	jl then16
	movq 8(%rax),%rax
	movq %rax,%rsi
	callq _ltList
	jmp next17
then16:
	movq 0(%rax),%rbx
	movq 8(%rax),%rax
	movq %rax,%rsi
	callq _ltList
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
next17:
	jmp next19
then18:
	movq $0,%rax
next19:
	addq $8,%rsp
	popq %rbx
	popq %rbp
	retq

	.globl _geList
	.align	4
_geList:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rdi,%rax
	movq %rsi,%rdi
	cmpq $0,%rdi
	je then39
	movq 0(%rdi),%rsi
	cmpq %rsi,%rax
	je then37
	movq 0(%rdi),%rsi
	cmpq %rsi,%rax
	jl then35
	movq 8(%rdi),%rsi
	movq %rax,%rdi
	callq _geList
	jmp next36
then35:
	movq 0(%rdi),%rbx
	movq 8(%rdi),%rsi
	movq %rax,%rdi
	callq _geList
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
next36:
	jmp next38
then37:
	movq 0(%rdi),%rbx
	movq 8(%rdi),%rsi
	movq %rax,%rdi
	callq _geList
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
next38:
	jmp next40
then39:
	movq $0,%rax
next40:
	addq $8,%rsp
	popq %rbx
	popq %rbp
	retq

	.globl _qsort
	.align	4
_qsort:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	pushq %r12
	pushq %r13
	subq $8,%rsp
	movq %rdi,%r12
	movq %r12,%rax
	cmpq $0,%rax
	je then53
	movq 0(%r12),%rdi
	movq 8(%r12),%rax
	movq %rax,%rsi
	callq _ltList
	movq %rax,%rdi
	callq _qsort
	movq %rax,%r13
	movq 0(%r12),%rbx
	movq 0(%r12),%rdi
	movq 8(%r12),%rax
	movq %rax,%rsi
	callq _geList
	movq %rax,%rdi
	callq _qsort
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
	movq %r13,%rdi
	movq %rax,%rsi
	callq _append
	jmp next54
then53:
	movq $0,%rax
next54:
	addq $8,%rsp
	popq %r13
	popq %r12
	popq %rbx
	popq %rbp
	retq

	.globl _testList
	.align	4
_testList:
	pushq %rbp
	movq %rsp,%rbp
	pushq %rbx
	subq $8,%rsp
	movq %rdi,%rbx
	movq %rbx,%rax
	cmpq $0,%rax
	je then59
	movq %rbx,%rax
	subq $1,%rax
	movq %rax,%rdi
	callq _testList
	movq %rbx,%rdi
	movq %rax,%rsi
	callq _cons
	jmp next60
then59:
	movq $0,%rax
next60:
	addq $8,%rsp
	popq %rbx
	popq %rbp
	retq

	.globl _test
	.align	4
_test:
	pushq %rbp
	movq %rsp,%rbp
	subq $0,%rsp
	movq %rdi,%rax
	movq %rax,%rdi
	callq _testList
	movq %rax,%rdi
	callq _qsort
	addq $0,%rsp
	popq %rbp
	retq

