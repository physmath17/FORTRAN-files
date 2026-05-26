! This program is about certain syntaxes, use of implicit none, and data types

program numbers

	implicit none  	! to compiler, do not assume datatype for any variable on its own
	! l, m, n ... The compiler assumes them to be integers
	! a, b, c, x, y, z ... reals/floating
	
	integer a, b, c						! do not have a fractional part
	real ar, br, cr
	double ad, bd						! real but can store larger no. of significant digits
	complex ac, bc					! can store complex numbers
	character ach, bch				! stores single symbol or number or alphabet, can make strings
	logical al, bl						! used to store or work with binary answers - True/False
	

end program numbers
