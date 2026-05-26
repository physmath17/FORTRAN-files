program walk_2D

	implicit none
	
	real :: pp, pm, qp, qm, avgx, avgy, sx = 0, sy = 0, a, sxx = 0, syy = 0, r, avgxx, avgyy, sdx, sdy, x, y, r2
	integer ::  i, n, t, j
	
	print*, "Enter the probability:"
	read*, pm, pp, qm, qp
	
	print*, "Enter the number of trials:"
	read*, t
	
	do n = 100, 2000, 100
		x = 0
		y = 0
		sx = 0
		sy = 0
		sxx = 0
		syy = 0
		r = 0
		r2 = 0
		do i = 1, t
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
			enddo
		sx = sx + x
		sxx = sxx + x*x
		sy = sy + y
		syy = syy + y*y
		enddo
		avgx = sx/t
		avgxx = sxx/t
		avgy = sy/t
		avgyy = syy/t 
		sdx = sqrt(avgxx - avgx*avgx)
		sdy = sqrt(avgyy - avgy*avgy)
		r = sdx + sdy
		r2 = avgxx + avgyy - avgx*avgx - avgy*avgy
		open(unit = 12, file="rw_2D.txt")
		write(12,*), n, r, r2, log(real(n)), log(r), log(r2)
	enddo
	
end program
