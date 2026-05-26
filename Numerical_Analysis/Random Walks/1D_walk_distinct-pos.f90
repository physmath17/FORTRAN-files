program distinct_positions

	implicit none
	
	real(kind=16) :: x, sum = 0, p, distinct_pos, avg
	integer :: count, tr, N, i, c = 0, min = 0, max = 0, j
	
	print*, "Enter the probability:"
	read*, p
	print*,"Enter the number of trials:"
	read*, tr
	
	do N = 100, 2000, 10
		sum = 0
!		c = 0
		do j = 1, tr
			count = 0
			max = 0
			min = 0
			do i = 1, N									! this loop will define the position at the end of one trial, after taking N steps
				x = rand()
				if(x<p) then
				count = count - 1
				elseif(x>p) then
				count = count + 1
				endif
				if(min>count) then
					min = count
				elseif(count>max) then
					max = count
				endif
			enddo
			distinct_pos = real(max - min)
			sum = sum + distinct_pos
		enddo	
	avg = sum/tr
	!print*, "The number of steps is:", N, "The average of distinct positions is:", avg
	open(unit=17, file="rw_distinct_positions_0.3.txt")
	write(17,*), N, avg
	enddo
	
end program distinct_positions
