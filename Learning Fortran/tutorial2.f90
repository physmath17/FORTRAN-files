! Program to find the perimeter and area of a triangle using Heron's formula

program triangle

	implicit none
	
	real :: a, b, c, s, p, Area
	
	! 	+ - * / -----> Add, Subtract, Multiply, Divide
	! 	** -----> Exponent
	!	mod(5,3) = 2			! for both real and integer numbers
	!	Operator Precedence: a-b*c+g**5	-----> PEMDAS - Parenthesis, Exponent, Multiplication, Division, 	Addition, Subtraction
	!	Please Excuse My Dear Aunt Sally	
	
	
	a = 3
	b = 4
	c = 5
	
	p = a + b + c
	s = p/2
	Area = (s*(s-a)*(s-b)*(s-c))**0.5
	
	print *, "The area of the triangle is:" , Area
	print *, "The perimeter of the triangle is: ", p

end program triangle
