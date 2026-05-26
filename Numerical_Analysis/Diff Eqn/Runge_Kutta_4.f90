program RK4
    implicit none

    real::x,y,f,h,k1,k2,k3,k4,v,p,g
    integer::i

    print*,"Enter the initial values and point of evaluation: "
    read*, x,y,v
    print*,"Enter the step-size: "
    read*,h

    do i=1, int(abs((x-v))/h)
        k1 = f(x,y)
        k2 = f(x+h/2,y+h*k1/2)
        k3 = f(x+h/2,y+h*k2/2)
        k4 = f(x+h,y+h*k3)
        y = y + h*(k1 + 2*k2 + 2*k3 + k4)/6
        x = x + h
        print*, x,y
    enddo

    print*,"Enter the initial values and point of evaluation: "
    read*, x,y,v
    print*,"Enter the step-size: "
    read*,h

    do i=1, int(abs((x-v))/h)
        k1 = g(x,y)
        k2 = g(x+h/2,y+h*k1/2)
        k3 = g(x+h/2,y+h*k2/2)
        k4 = g(x+h,y+h*k3)
        y = y + h*(k1 + 2*k2 + 2*k3 + k4)/6
        x = x + h
        print*, x,y
    enddo

    print*,"Enter the initial values and point of evaluation: "
    read*, x,y,v
    print*,"Enter the step-size: "
    read*,h

    do i=1, int(abs((x-v))/h)
        k1 = p(x,y)
        k2 = p(x+h/2,y+h*k1/2)
        k3 = p(x+h/2,y+h*k2/2)
        k4 = p(x+h,y+h*k3)
        y = y + h*(k1 + 2*k2 + 2*k3 + k4)/6
        x = x + h
        print*, x,y
    enddo

end program

real function f(t,z)
    implicit none
    real::t,z
    f = (z**2 - t**2)/(t**2 + z**2)
end function

real function g(t,z)
    implicit none
    real::t,z
    g = (5*t**2 - z)/exp(t+z)
end function

real function p(t,z)
    implicit none
    real::t,z
    p = (t**3 + z/2)
end function
