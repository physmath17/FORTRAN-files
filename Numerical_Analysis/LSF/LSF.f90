program lsf

    implicit none

    real::sx = 0, sy = 0, ssqx = 0, sxy = 0, a, b, x, y
    integer::n,j

    open(1, file='input.dat')
    read(1,*), n
    do j=1, n
        read(1,*), x, y
        sx = sx + x
        sy = sy + y
        ssqx = ssqx + x*x
        sxy = sxy + x*y
    enddo
    close(1)

    b = (n*sxy - sx*sy)/(n*ssqx - sx*sx)
    a = (ssqx*sy - sx*sxy)/(n*ssqx - sx*sx)

    open(2, file='output.dat')
    write(2,60), b, a
    60 format(2x, 'slope = ', f8.5, 2x, 'intercept = ', f8.5)

end program lsf
