! Program to find the perimeter and area of a triangle using Heron's formula

program triangle

	implicit none
	
	real :: a, b, c, s, p, Area	
! 	integer :: a, b, c, s, p, Area	---> "typecasting" problem, Area comes out to be 0
!	integer :: d, e
!	real :: g, f
	
	
!	a = 8
!	b = 4
!	c = 5

!	user-defined input
	
	print *, "Enter the sides of the triangle:"
	read *, a, b, c
	print *, "The sides of the triangle are:"
	print *, "a = ", a, "b = ", b, "c = ", c
	
	p = a + b + c
	s = p/2
	Area = (s*(s-a)*(s-b)*(s-c))**0.5
	
	print *, "The area of the triangle is:" , Area
	print *, "The perimeter of the triangle is: ", p
	
!	d = 4
!	f = 1.7
!	g = d
!	e = f
	
!	print *, "d =  ", d, "f =  ", f
!	print *, "g =  ", g, "e =  ", e		!	---->  integer becomes a float (d to g), real in truncated to intger (f to e)
	

end program triangle

! Fortran is case insensitive language
! typecasting can be done explicitly as well using int() or real() functions
