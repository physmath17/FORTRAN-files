program eigen
    implicit none
    
    integer:: m, i, j
    real, dimension(2,2):: K
    real:: eigenvalue
    real, dimension(2):: v, temp = 0
    
	print*,"Enter the matrix:"
	do i = 1,2
			read*, (K(i,j), j = 1, 2)
	enddo
	
	v(1) = 1
	v(2) = 0
	
	do j = 1, 10
		temp = 0
		do i = 1, 2
			do m = 1, 2
				temp(i) = temp(i) + K(i,m)*v(m)
			enddo
		enddo
		print*, temp
		eigenvalue = temp(1)
		if(temp(2)>eigenvalue) eigenvalue = temp(2)
		print*, eigenvalue
		do i= 1,2	
			temp(i) = temp(i)/eigenvalue
			v(i) = temp(i)
		enddo
		print*, v
	enddo
	
	print*, eigenvalue
	
end program

!subroutine findeigenvalue(A, x, n, eigen, itr)		!finding the eigenvalue
!	implicit none
	
!	integer,intent(in)::n, itr	!order of matrix, no. of iterations
!	real,intent(in),dimension(n,n)::A		!input matrix
!	real,intent(inout),dimension(n)::x		! eigenvector
!	real,intent(inout)::eigen		!eigenvalue
!	integer::i,j
	
!		x = 1
	
!	do i = 1, itr
!		call matrixmult(A, x, n)			! multiplying input matrix with the initialised vector
!		call get_largest(x, n, eigen)		! finding eigenvalue
		
!		if(eigen == 0) exit
		
!		do j = 1, n
!			x(j) = x(j)/eigen
!		enddo
!	enddo
!end subroutine findeigenvalue
	
!subroutine matrixmult(M, b, n)      !multiplying matrices with vectors
!    implicit none

!    integer, intent(in)::n      !components of vector
!    real,intent(in),dimension(n,n)::M   !input matrix
!    real,intent(inout),dimension(n)::b  !output matrix multiplied vector from initial vector
!    integer::i,j
!    real,dimension(n)::tmp

!    tmp = 0

!    do i = 1, n
!        do j = 1, n
!            tmp(i) = tmp(i) + M(i,j)*b(j)
!        enddo
!    enddo
!end subroutine matrixmult

!subroutine get_largest(x, n, l)     !getting the largest value in a vector
!    implicit none

!    integer, intent(in)::n			! components of vector
!    real,intent(in),dimension(n)::x	!input vector 
!    real,intent(inout)::l
!    integer::i
    
!    l = abs(x(1))
    
!    do i = 2, n
!		if(abs(x(i))>l) l = abs(x(i))
!	enddo
!end subroutine get_largest


	
	
	
	
	
	
	
