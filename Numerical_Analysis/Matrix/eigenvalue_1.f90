program eigen
    implicit none
    
    integer:: m, i, j, l
    real, dimension(:,:), allocatable:: K
    real:: eigenvalue
    real, dimension(:), allocatable:: v, temp 
    
    open(unit=1, file="../data/input_matrix.dat")
    read*, m
    allocate(K(1:m,1:m))
    allocate(v(1:m))
	
	do i = 1,m
		read*, (K(i,j), j = 1, m)
	enddo
	close(1)
	
	do l = 1, 10
		temp = 0
		do i = 1, m
			do j = 1, m
				temp(i) = temp(i) + K(i,j)*v(j)
			enddo
		enddo
		
		
		eigenvalue = temp(1)
		do i = i, m
			if(temp(i)>eigenvalue) eigenvalue = temp(i)
			print*, eigenvalue
			temp(i) = temp(i)/eigenvalue
			v(i) = temp(i)
		enddo
	enddo
	
	print*, eigenvalue
	
end program
