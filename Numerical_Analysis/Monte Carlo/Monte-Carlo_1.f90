program PI

    implicit none

    integer:: count = 0, i, j, n
    real:: p, x, y

    print*, "Enter the ensemble size: "
    read*, n
    open(1,file='../data/sample_MC_sq.txt')
    do i = 1, n
        x = rand()
        y = rand()
    write(1,*), x, y
    open(2,file='../data/sample_MC_c.txt')
        if ((x*x + y*y) < 1) then
            count = count + 1
            write(2,*), x, y
        endif
    enddo

    p = 4*real(count)/n
    print*, p, count, atan(1.00)*4

end program PI
