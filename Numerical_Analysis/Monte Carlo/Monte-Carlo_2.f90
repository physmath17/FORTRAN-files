program Integration

    implicit none

    integer:: count = 0, j, n
    real:: I = 0.0, x, a, b, f

    print*, "Enter the ensemble size: "
    read*, n
    print*, "Enter the integration limits:"
    read*, a, b
    open(1,file='../data/sample_MC_int_1.txt')
    do j = 1, n
        x = a + (b-a)*rand()
        I = I + (b-a)*x/n
    write(1,*), I
    enddo

    Print*, I

end program Integration
