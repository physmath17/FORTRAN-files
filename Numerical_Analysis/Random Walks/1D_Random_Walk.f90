program walk_1D

	implicit none
	real, dimension(1:2) :: prob 												! stores the probability 
!	real, dimension(1:1000000) :: X											! random number array
	real :: p, avg, sum1 = 0, sum2 = 0	, sd, cavg, X						! probability to go right, average
	integer :: i, j, k, N, count, t													! N---->no. of steps
	
! ask the probability values from the user	
	print*, "Enter the probability:"
	read*, p
	prob(1) = p
	prob(2) = 1 - p
	
! ask the number of steps from the user
	print*, "Enter the number of steps:"
	read*, N
	
! ask the number of trials from the user
	print*,"Enter the number of trials :"
	read*, t
	
	do i = 1, t								! this loop defines the number of trials
		count = 0
		do j = 1, N									! this loop will define the position at the end of one trial, after taking N steps
			X = rand()
			if(X<p) then
			count = count - 1
			elseif(X>p) then
			count = count + 1
			endif
		enddo
			sum1 = sum1 + count
			sum2 = sum2 + count*count
			open(unit=1, file="rw_100_0.3.txt")
			cavg = real(sum1)/i
			write(1,*), N, count, sum1, cavg
		enddo
		
		avg = cavg
		sd = sqrt(real(sum2)/t - avg*avg)
		print*, "The average value of position is:", avg
		print*, "The standard deviation is: ", sd
			
	
end program
