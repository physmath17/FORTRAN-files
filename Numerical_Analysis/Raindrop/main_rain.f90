program raindrop
	implicit none
		
		integer :: h, i, j, t
		real :: x = 0, y = 0, pdown, pup, pleft, pright, avgx, avgt, sumt, sumx, a
!		real, dimension(1:1000) :: T, xpos
		
		print*, "Enter the probabilities: "
		read*, pdown, pup, pleft, pright
		
		open(unit=1, file="trials_reqd.txt")
		open(unit=2, file="rain_h_t.txt")
		open(unit=3, file="x(h).txt")
		do h = 10, 50, 5				! loop for varying the heights
			sumt = 0
			sumx = 0
			do i = 1, 1000				! loop determining the no. of times the drop is observed i.e, no. of times the expt is repeated
				x = 0
				y = h
				do t = 1, 10000		! loop for the determing the time (equivalent) for the drop to fall
					a = rand()
					if(a<pdown) then
						y = y - 1
					elseif(a>pdown .and. a<pup) then 
						y = y + 1
					elseif(a>pup .and. a<pleft) then
						x = x - 1
					elseif(a>pleft .and. a<pright) then
						x = x + 1
					endif
					if(y == 0)  exit		! breaks the loop once the drop reaches the ground
				enddo
				write(1,*), h, t, x, y
				write(3,*), x
				sumt = sumt + t			! sum of times for every trial/repetition
				sumx = sumx + x		! sum of the lateral displacement along the x-direction
			enddo
			avgt = sumt/1000			! average time taken by the drop to fall for a given height
			avgx = sumx/1000			! average lateral displacemnet in x-direction for a given height
			write(2,*), h, avgt, avgx
		enddo
		
end program
