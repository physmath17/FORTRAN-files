program twoD_walk

	implicit none
	
	real :: pp, pm, qp, qm, x, y, a
	integer ::  i, n , t, j
	
	print*, "Enter the probability:"
	read*, pm, pp, qm, qp
	
	print*, "Enter the number of steps:"
	read*, n
	
	open(unit=1, file="random_walk2D.txt")
	do j = 1, n
		a = rand()
		if(a<pm) then
			x = x - 1
		elseif(a>pm .and. a<pp) then
			x = x + 1
		elseif(a>pp .and. a<qm) then
			y  = y - 1
		elseif(a>qp) then
			y = y + 1
		endif
		write(1,*), a, x, y
	enddo

end program
