# Calculator
Calculator implemented with a stack, the calculation is made on the 2 top numbers on the stack. The project implemented entirely in Assembly language. The calculator supports : unsigned +, ^, dup, print and pop, etc.

 The calculator support the next operators:

-  Quit.

-  Unsigned addition

-  Pop and print – pop two operands from operand stace, and push one result, their sum.

-  Duplicate – push a copy of the top of the operand stack onto the top of the operand stack.

-  x*2^(-y) - with X being the top of operand stack and Y the element next to x in the operand stack. If Y>200 this is considered an error, in which case you should print out an error message and leave the operand stack unaffected.

- Number of '1' bits - pop one operand from the operand stack, and push one result.

- Debug mode – print out the Debugging messages to stderr

How does it work?

- Operations are performed as is standard for an RPN (Reverse Polish notation) calculator: any input number is pushed onto an operand stack. Each operation is performed on operands which are popped from the operand stack. The result, if any, is pushed onto the operand stack. The output should contain no leading zeroes, but the input may have some leading zeroes.

- The program should also count the number of operations (both successful and unsuccessful) performed. This is the return valued which returned to function main. The size of the operands is unbounded, except by the size of available heap space on your virtual memory.

- Number is hold in bytes per node in the linkedlist from right to left (MSB in the right node), this implementation make the calculate and the carry handle easier
