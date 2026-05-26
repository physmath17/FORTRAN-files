program Ising_2D
	implicit none
		
		integer :: i, j, n, k, l, a, b, c, d = 1
		integer, dimension(1000,1000) :: lattice, tmp
		real :: mavg, m, Ei, Ef, p, E, temp = 5.0				! temp = k_B * T/ J
		
		interface
		subroutine ising(q, A, E1, mag)
			implicit none
			integer, intent(in) :: q
			integer, dimension(:,:), intent(in) :: A
			real, intent(out) :: E1, mag
			integer :: r, c, up, down, left, right
			real :: p1
		end subroutine ising
	end interface
		
		print*, "Enter the lattice size:"
		read*, n
			
		open(unit=1, file="lattice_E10.txt")
		
			do c = 1, 10
! a,b loops generate a random initial lattice
				d = 1
				do a = 1, n
					do b = 1, n
						p = rand()
						if(p<0.5) then
							lattice(a,b) = -1
						else
							lattice(a,b) = 1
						endif
					enddo
				enddo
	
				Ei = 0		
				m = 0	
				call ising(n, lattice, Ei, m)
				write(2,*), d, Ei
				
! k loop gives the number of times the flippings in the whole lattice is carried out 
! i, j loops carry out the spin flippings at different lattice sites
				do k = 1, 100											
					do i = 1, n											
						do j = 1, n
							m = 0
							tmp = lattice
							tmp(i,j) = tmp(i,j)*(-1)			! spin flipping
! calculating the new energy and magnetisation for the new lattice arrangement						
							call ising(n, tmp, Ef, m)								
							
							if(Ef < Ei .or. Ef == Ei) then						! if Ef is lower accept the new lattice configuration
								lattice = tmp
							else
								p = rand()									
								if(p .le. exp(-(Ef - Ei)/temp(l))) then		! if Ef is higher do not directly reject it, but accept with some probability given by the Maxwell-Boltzmann distribution
									lattice = tmp
									Ei = Ef
								endif
							endif
						enddo
					enddo
					
					if(c==1) then
						d = d + 1										
						write(1,*), d, Ei							! seeing the equilibrium for the case of highest temperature 
					endif
				enddo
				
			mavg = mavg + m
			E = E + Ei
			enddo
			write(3,*), temp(l), mavg/50, E/50
		enddo
							
end program Ising_2D

subroutine ising(q, A, E1, mag)
	implicit none
		integer, intent(in) :: q
		integer, dimension(:,:), intent(in) :: A
		real, intent(out) :: E1, mag
		integer :: r, c, up, down, left, right
		real :: p1
		
		do r = 1, q
			do c = 1, q
				up = c - 1 
				down = c + 1
				right = r + 1
				left = r - 1
				if(r==1) then
					up = q
				endif
				if(r==q) then
					down = 1
				endif
				if(c==1) then 
					left = q
				endif
				if(c==q) then
					right = 1
				endif
				E1 = E1 + A(r,c)*(A(r,right) + A(r,left) + A(up,c) + A(down,c))
			enddo
		enddo
		
		do r = 1, q
			do c = 1, q
				mag = mag + A(r,c)
			enddo
		enddo
		mag = mag/(q*q)
	
end subroutine ising
		
