program Ising_2D
	implicit none
		
		integer :: i, j, n, k, l, a, b, c, d = 1
		integer, dimension(1000,1000) :: lattice, tmp
		real :: mavg, m, Ei, Ef, p, E
		real, dimension(1:26) :: temp					! temp = k_B * T/ J
		
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
			
		open(unit=1, file="lattice_temp5.txt")
		open(unit=5, file="eqbrm_temp5_1.txt")
		open(unit=15, file="eqbrm_temp5_11.txt")
		open(unit=25, file="eqbrm_temp5_21.txt")
		open(unit=35, file="eqbrm_temp5_31.txt")
		open(unit=45, file="eqbrm_temp5_41.txt")
		open(unit=3, file="temp, m, E.txt")
		open(unit=4, file="tempearure.txt")
		
		read(4,*), temp
		 
		do l = 1, 26
			do c = 1, 50
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
	
				if(l==1) then										! initial lattice for the highest temperature
					do i = 1, n
						write(1,*), (lattice(i,j), j = 1, n)
					enddo
				endif
				write(1,*), "######################################"

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
								if(p .ge. exp(-(Ef - Ei)/temp(l))) then		! if Ef is higher do not directly reject it, but accept with some probability given by the Maxwell-Boltzmann distribution
									lattice = tmp
									Ei = Ef
								endif
							endif
						enddo
					enddo
					
					if(l==1 .and. c==1) then
						d = d + 1										
						write(5,*), d, Ei							! seeing the equilibrium for the case of highest temperature
						if(mod(k,5)==1) then				! seeing the formation of domains for the case of highest temperature
							do i = 1, n
								write(1,*), (lattice(i,j), j = 1, n)			! printing the lattice configuration
							enddo
							write(1,*), "######################################"
						endif
					endif
					
					if(l==1 .and. c==11) then
						d = d + 1	
						write(15,*), d, Ei
					endif

					if(l==1 .and. c==21) then
						d = d + 1	
						write(25,*), d, Ei
					endif
					
					if(l==1 .and. c==31) then
						d = d + 1	
						write(35,*), d, Ei
					endif
					
					if(l==1 .and. c==41) then
						d = d + 1	
						write(45,*), d, Ei
					endif
					
					if(l==1 .and. c==51) then
						d = d + 1	
						write(55,*), d, Ei
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
		
!		do r = 1, q
!			do c = 1, q
!				if(r==1 .and. c==1) then
!					E1 = E1 + A(1,1)*(A(q,1) + A(1,q) + A(1,2) + A(2,1))
!				elseif(r==q .and. c==1) then
!					E1 = E1 + A(q,1)*(A(1,1) + A(q,2) + A(q-1,1) + A(q,q))
!				elseif(r==1 .and. c==q) then
!					E1  = E1 + A(1,q)*(A(1,1) + A(q,q) + A(1,q-1) + A(2,q))
!				elseif(r==q .and. c==q) then
!					E1 = E1 + A(q,q)*(A(q,1) + A(1,q) + A(q,q-1) + A(q-1,q))
!				elseif((r==1 .and. c/=1) .or. (r==1 .and. c/=q)) then
!					E1 = E1 + A(r,c)*(A(q,c) + A(1,c-1) + A(1,c+1) + A(2,c))
!				elseif((c==1 .and. r/=1) .or. (c==1 .and. r/=q)) then
!					E1 = E1 + A(r,c)*(A(r,q) + A(r-1,c) + A(r+1,c) + A(r,2))
!				elseif((r==q .and. c/=1) .or. (r==q .and. c/=q)) then
!					E1 = E1 + A(r,c)*(A(1,c) + A(q,c-1) + A(q,c+1) + A(q-1,c))
!				elseif((c==q .and. r/=1) .or. (c==q .and. r/=q)) then
!					E1 = E1 + A(r,c)*(A(r,1) + A(r-1,q) + A(r+1,q) + A(r,q-1))
!				elseif((r/=1 .and. c/=1) .or. (r/=q .and. c/=1) .or. (r/=1 .and. c/=q) .or. (r/=q .and. c/=q)) then
!					E1 = E1 + A(r,c)*(A(r,c+1) + A(r,c-1) + A(r+1,c) + A(r-1,c))
!				endif
!			enddo
!		enddo
		
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
	
end subroutine Ising
		
