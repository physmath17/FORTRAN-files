program Lagr_interpol
    implicit none

    integer :: i, j, k, n
    real :: a, L = 0.0, p = 1.0
    real,dimension(:), allocatable :: x

    print*, "Enter the no. of input values: "
    read*, n
    
    allocate(x(n))
    
    print*, "Enter the input data points: "
    do i = 1, n
        read*, x(i)
    enddo
    print*,"Enter the value at which the function is to be approximated: "
    read*, a

    do j = 1, n
        p = 1.0
        do k = 1, n
            if(j /= k) then
                p = p * ((a - x(k))/(x(j) - x(k)))
            endif
            enddo
        L = L + p * log(real(x(j)))			! estimating the value of log at 'a'
    enddo

    print*, "The required value is : ", L
    print*, abs(real(log(a) - L))/log(a)

end program Lagr_interpol
