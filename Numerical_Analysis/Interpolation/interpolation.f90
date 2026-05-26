program lagrange_interpolation
	implicit none
	
	real, dimension(1:5) :: x1, y1
	real, dimension(1:2) :: a, val_a
	
	interface
		subroutine interpol(x, xp, yp, y)
			real, intent(in) :: x
			real, intent(out) :: y
			real, dimension(1:5), intent(in) :: xp, yp
			integer :: j, k
			real :: p, L = 0
		end subroutine interpol
	end interface
			
	x1 = (/1, 2, 3, 4, 5/)
	y1 = (/0.5, 1.5, 2.5, 3.5, 4.5/)
	a = (/1.5, 2.5/)
	call interpol(a(1), x1, y1, val_a(1))
	call interpol(a(2), x1, y1, val_a(2))
	
	print*, a
	print*, val_a

end program lagrange_interpolation
	
subroutine interpol(x, xp, yp, y)
	implicit none
	
	real, intent(in) :: x		                ! point at which we need to interpolate
	real, intent(out) :: y		                ! the interpolated value
	real, dimension(1:5), intent(in) :: xp, yp	! given data points
	integer :: arsize, j, k
	real :: p, L = 0
	
	arsize = size(xp)
! this loops execute the Lagrange interpolation algorithm
	do j = 1, arsize
		p = 1.0
		do k = 1, arsize
			if(j /= k) then
				p = p * ((x - xp(k))/(xp(j) - xp(k)))
			endif
		enddo
		L = L + p * yp(j)
	enddo
	y = L
end subroutine interpol
	
