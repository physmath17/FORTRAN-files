program eigen
    implicit none
    
    integer:: m, i, j
    real, dimension(:,:), allocatable:: K
    real:: eigenvalue
    real, dimension(:), allocatable:: v
    
    open(unit=1, file="../data/input_matrix.dat")
    read*, m
    allocate(K(1:m,1:m))
    allocate(v(1:m))
	
	do i = 1,m
		read*, (K(i,j), j = 1, m)
	enddo
	close(1)
	
	call findeigenvalue(K, v, m, eigenvalue, 10)
	
	print*, eigenvalue
	
end program

subroutine findeigenvalue(A, x, n, eigen, itr)		!finding the eigenvalue
	implicit none
	
	integer,intent(in)::n, itr	!order of matrix, no. of iterations
	real,intent(in),dimension(n,n)::A		!input matrix
	real,intent(inout),dimension(n)::x		! eigenvector
	real,intent(inout)::eigen		!eigenvalue
	integer::i,j
	
		x = 1
	
	do i = 1, itr
		call matrixmult(A, x, n)			! multiplying input matrix with the initialised vector
		call get_largest(x, n, eigen)		! finding eigenvalue
		
		if(eigen == 0) exit
		
		do j = 1, n
			x(j) = x(j)/eigen
		enddo
	enddo
end subroutine findeigenvalue
	
subroutine matrixmult(M, b, n)      !multiplying matrices with vectors
    implicit none

    integer, intent(in)::n      !components of vector
    real,intent(in),dimension(n,n)::M   !input matrix
    real,intent(inout),dimension(n)::b  !output matrix multiplied vector from initial vector
    integer::i,j
    real,dimension(n)::tmp

    tmp = 0

    do i = 1, n
        do j = 1, n
            tmp(i) = tmp(i) + M(i,j)*b(j)
        enddo
    enddo
end subroutine matrixmult

subroutine get_largest(x, n, l)     !getting the largest value in a vector
    implicit none

    integer, intent(in)::n			! components of vector
    real,intent(in),dimension(n)::x	!input vector 
    real,intent(inout)::l
    integer::i
    
    l = abs(x(1))
    
    do i = 2, n
		if(abs(x(i))>l) l = abs(x(i))
	enddo
end subroutine get_largest


	
	
	
	
	
	
	
