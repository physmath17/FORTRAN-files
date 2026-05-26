program Newton_interpol
    implicit none

    integer::i,j,n
    real::a,b0, b1, b2, V
    real,dimension(3)::x, y

    print*, "Enter the no. of input values: "
    read*, n
    print*, "Enter the input data points: "
    do i = 1, 3
        read*, x(i)
    enddo
    print*,"Enter the no. value at which the function is to be approximated: "
    read*, a

    do i = 1, 3
        y(i) = log(real(x(i)))
    enddo

    b0 = y(1)
    b1 = (y(2) - b0)/(x(2) - x(1))
    b2 = ((y(3) - b0)/(x(3) - x(1)) - b1)/(x(3) - x(2))

    V = b0 + b1*(a-x(1)) + b2*(a - x(1))*(a - x(2))
    print*, "The required value is :", V
    print*, abs(real(log(2.0) - V))/log(2.0)

end program Newton_interpol
