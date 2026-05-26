program open_chain
	implicit none
	
	integer :: i, j, itr, x0, L, x, t, t1
	real :: p, tavg, D, v, tavg1
	
	open(unit = 1, file = "position_diffusion.txt")
	open(unit = 2, file = "time_length.txt")
	do x0 = 1, 99					! loop for variation of x0 positions
		t = 0
		do itr = 1, 100000				! loop that determines the number of times the experiment is repeated for a fixed x0
			x = x0
			do i = 1, 100000					! loop that will determine the time it takes to reach the trapping site
				p = rand()
				if(p<0.5) then
					x = x - 1
				elseif(p>0.5) then
					x = x + 1
				endif
				
				if(x == 0 .or. x == 100)  exit
			enddo
			t = t + i
		enddo
		tavg = real(t)/100000
		D = (0.5*x0*(100-x0))/(tavg)
	write(1,*), x0, tavg, D 
	enddo
	
	do L = 100, 900, 100
		x0 = L/2
		t1 = 0
		do j = 1, 10000
			x = x0
			do while(x>0 .and. x<L)
				p = rand()
				if(p<0.5) then
					x = x - 1
				elseif(p>0.5) then
					x = x + 1
				endif
				t1 = t1 + 1
			enddo
		enddo
	tavg1 = real(t1)/10000
	D = (0.5*x0*(L-x0))/(tavg1)
	write(2,*), L, tavg1, D
	enddo		
	
end program open_chain
	
	
