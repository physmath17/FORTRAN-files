program eqbrm2

	implicit none
	
	real,dimension(1000000)::X
!	real,dimension(1:1000000)::N
	real(kind=16)::a, SD = 0.0, Av = 0.0, Sq = 0.0, Q = 0
	integer::i,j,k,t,count = 0, m,  S = 0, c = 0
	Call Random_Number(X)
	X = X*0.5
	open(unit=1, file='N_2.txt')
	do m = 10, 1000, 10
	c = 0
	S = 0
	Q = 0
	Av = 0.0
	SD = 0.0 
	Sq = 0.0
	do t = 1, m*1000
		count = 0
		do i = 1, m
			if(X(i)<0.5) then
			count = count + 1
			endif
		enddo
			
		a = rand()
		if(X(int(a*(m)))<0.5) then
		X(int(a*(m))) = X(int((a*m))) + 0.5
		else
		X(int(a*(m))) = X(int(a*(m))) - 0.5
		endif
		
		if(t>50) then
		S = S + count
		Q = Q + count*count
		c = c+1
		endif
	enddo
	Av = real(S)/c
	Sq = Q/c
	SD = sqrt(real(Sq - Av**2))
	write(1,*), m, Av, SD, SD/Av
!	print*, m, Av, Sq, SD, SD/Av
	enddo
	close(1)
	
	call system('gnuplot -p N_l_2_plot.txt')
	call system('gnuplot -p N_l_3_plot.txt')
	
end program
	
