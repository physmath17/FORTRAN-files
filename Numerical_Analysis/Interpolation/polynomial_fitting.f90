program poly
    implicit none

    real::f, sx = 0, sy = 0, ssqx = 0, sxy = 0, x, y, scbx = 0, ssqxy = 0, sqnx = 0
    integer::n, m, i, j, k
    real, dimension(3,4):: C, temp
    real, dimension(1:3):: a

    open(1, file='../data/input_poly.dat')
    read(1,*), n
    do m=1, n
        read(1,*), x, y
        sx = sx + x
        sy = sy + y
        ssqx = ssqx + x*x
        scbx = scbx + x*x*x
        sqnx = sqnx + x*x*x*x
        sxy = sxy + x*y
        ssqxy = ssqxy + x*x*y
    enddo
    close(1)

    C(1,1) = n
    C(1,2) = sx
    C(2,1) = sx
    C(1,3) = ssqx
    C(2,2) = ssqx
    C(3,1) = ssqx
    C(2,3) = scbx
    C(3,2) = scbx
    C(3,3) = sqnx
    C(1,4) = sy
    C(2,4) = sxy
    C(3,4) = ssqxy

    do i = 1,2
        if(C(i,i) == 0) then
            print*,'Error. Swap the rows for non-zero first coefficient.'
            stop
        else
            do j = i+1,3
                f = C(j,i)/C(i,i)
                do k = 1,4
                    C(j,k) = C(j,k) - f*C(i,k)
                enddo
            enddo
        endif
    enddo

    do i = 1,3
        a(i) = C(i,4)/C(i,i)
    enddo

    do i = 2,1,-1
        do j = 3,i+1,-1
            a(i) = a(i) - C(i,j)*a(j)/C(i,i)
        enddo
    enddo

    open(2, file='output_poly.txt')
    do i=1,3
        write(2,*), a(i)
    enddo

end program poly
