program trap_int
    implicit none

    integer::i,n
    real::a,b,h, J, f, V=1.4172
    real,dimension(5)::x

    print*, "Enter the limits and no. of intervals: "
    read*, a,b, n

    h = (b-a)/real(n)
    J = h*(f(a) + f(b))/2

    do i = 0, n
        x(i) = a + i*h
    enddo

    do i = 1, n-1
        J = J + h*f(x(i))
    enddo

    print*, "The required value of the integral is: ", J
    print*, "Error", abs(real((V-J)))*100/V

end program

real function f(t)
    real::t
    f = 0.2 + 25*t - 200*t*t + 675*t**3 - 900*t**4 + 400*t**5
end function
