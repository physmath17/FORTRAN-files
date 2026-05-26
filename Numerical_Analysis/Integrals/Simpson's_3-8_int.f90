program simpson38
    implicit none

    integer::i,n
    real::a,b,h, J, f, V=1.4172
    real,dimension(5)::x

    print*, "Enter the limits and no. of intervals: "
    read*, a,b, n

    h = (b-a)/real(n)
    J = 3*h*(f(a) + f(b))/8

    do i = 0, n
        x(i) = a + i*h
    enddo

    do i = 1, n-1
        if(mod(i,3) == 0) then
            J = J + 6*h*f(x(i))/8
        else
            J = J + 9*h*f(x(i))/8
        endif
    enddo

    print*, "The required value of the integral is: ", J
    print*, "Error", abs(real((V-J)))*100/V
end program

real function f(t)
    real::t
    f = 0.2 + 25*t - 200*t*t + 675*t**3 - 900*t**4 + 400*t**5
end function
