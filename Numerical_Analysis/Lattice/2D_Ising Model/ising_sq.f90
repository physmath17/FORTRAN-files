program Ising_2D
	implicit none
		
		integer :: i, j, n, k, a, b, c
		integer, dimension(40,40) :: lattice, tmp
		real :: mavg, m, Ei, Ef, p, E, l,  temp					! temp = k_B * T/ J
		
		print*, "Enter the lattice size:"
		read*, n
			
		open(unit=1, file="temp,m,E_4.txt")
		 
		do l = 5, 1, -0.05
!			do c = 1, 5
! a,b loops generate a random initial lattice
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
			
			call energy(n, lattice, Ei)
			call magnetisation(n, lattice, m)
			mavg = m
					
! k loop gives the number of times the flippings in the whole lattice is carried out 
! i, j loops carry out the spin flippings at different lattice sites
				do k = 1, n*20000										
					do i = 1, n											
						do j = 1, n
							tmp = lattice
							tmp(i,j) = tmp(i,j)*(-1)			! spin flipping
! calculating the new energy and magnetisation for the new lattice arrangement	
							call energy(n, tmp, Ef)								
							tmp = lattice
							if(Ef .le. Ei) then						! if Ef is lower accept the new lattice configuration
								lattice = tmp
							else
								p = rand()									
								if(p .le. exp(-(Ef - Ei)/l)) then		! if Ef is higher do not directly reject it, but accept with some probability given by the Maxwell-Boltzmann distribution
									lattice = tmp
									Ei = Ef
								endif
							endif
						enddo
					enddo	
				call magnetisation(n, lattice, m)
				mavg = mavg + m					
				E = E + Ei
				enddo
!		enddo
		write(1,*), l, mavg/(n*20000), E/(n*20000)
	enddo
							
end program Ising_2D

subroutine energy(q, A, E1)
	implicit none
		integer, intent(in) :: q
		integer, dimension(40,40) :: A
		real, intent(out) :: E1
		integer :: r, c, up, down, left, right		
					
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
				A(r,c) = 0
				E1 = -E1/(q*q)
			enddo
		enddo
end subroutine energy

subroutine magnetisation(q1,B,mag)
			implicit none
			integer, intent(in) :: q1
			integer, dimension(40,40) ::B
			real, intent(out) :: mag
			integer :: r1, c1
			
			do r1 = 1, q1
				do c1 = 1, q1
					mag = mag + B(r1,c1)
				enddo
		enddo
		mag = mag/real(q1*q1)
		end subroutine magnetisation
		
		
		
