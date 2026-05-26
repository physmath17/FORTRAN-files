program closed_chain
	implicit none
	
	integer :: i, j, x0, L, x, t
	real :: p, tavg, D
	
	L = 100
	
	open(unit = 1, file = "pos_time_100.txt")
	do x0 = 1, 99					! loop for variation of x0 positions
		t = 0
		do i = 1, 100000			! loop for number of iterations
			x = x0
			do while(x/=L)		! until the particle falls in the trap
				p = rand()
				if(p<0.5) then
					if(x==1) then		! implementing the periodic boundary condition
						x = L
					else
						x = x - 1
					endif
				elseif(p>0.5) then
					x = x + 1
				endif
				t = t + 1
			enddo
		enddo
	tavg = real(t)/100000
	D = (0.5*x0*(L-x0))/(tavg)
	write(1,*), x0, tavg, D
	enddo		
		
end program closed_chain
	
	
