program eqbrm1

	implicit none
	
	real,dimension(64,2)::X
	real,dimension(1:1000)::N
	real::a, SD1 = 0, SD2 = 0, SD3 = 0 
	integer::i,j,k,t,count = 0, m, N1 = 0, N2 = 0, N3 = 0, S1 = 0, S2 = 0, S3 = 0
	
	open(unit=1, file='N_1.txt')
	Call Random_Number(X)
	X = X*0.5
	do t = 1, 1000
		count = 0
		do i = 1, 64
			if(X(i,1)<0.5) then
			count = count + 1
			endif
		enddo
		
		write(1,*), t, count
		
		a = rand()
		if(X(int(a*64),1)<0.5) then
		X(int(a*64),1) = X(int(a*64),1) + 0.5
		else
		X(int(a*64),1) = X(int(a*64),1) - 0.5
		endif
	enddo
	close(1)
	
	open(unit=1, file='N_1.txt')
	do i = 1, 1000
		read(1,*), m,N(i)
	enddo
	do j = 501, 1000
		N1 = N1 + N(j)
		S1 = S1 + N(j)*N(j)
	enddo
	do j = 401, 800
		N2 = N2 + N(j)
		S2 = S2 + N(j)*N(j)
	enddo
	do j = 201, 500
		N3 = N3+ N(j)
		S3 = S3 + N(j)*N(j)
	enddo
	print*, real(N1)/500, real(N2)/400, real(N3)/300, real(S1)/500, real(S2)/400, real(S3)/300
	
	SD1 = sqrt(real(S1)/500 - (real(N1)/500)**2)
	SD2 = sqrt(real(S2)/400 - (real(N2)/400)**2)
	SD3 = sqrt(real(S3)/300 - (real(N3)/300)**2)
	print*, SD1, SD2, SD3
	
	call system('gnuplot -p N_l_1_plot.txt')
	
end program
		
		
			
			
!			open(unit=i, file='../build/Equilibrium_1/1.txt')
!			do j = 1, 8
!				write(i,*), (X(j,k), k = 1,2)
!			enddo
!			close(i)
		
				
