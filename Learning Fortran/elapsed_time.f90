program time

	implicit none

	real :: t1, t2
	integer :: i

	call cpu_time(t1)
	do i = 1, 5
		print*, "FORTRAN IS NOT BAD!!"
	enddo
	call cpu_time(t2)

	print*, t2 - t1

end program

