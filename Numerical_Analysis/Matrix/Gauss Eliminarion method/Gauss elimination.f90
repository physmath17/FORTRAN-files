program Gauss_Elimination

    implicit none

    real,dimension(10,11)::A
    real,dimension(10)::x
    integer::i,j,k,n
    real::f,c

    open(1, file='input.dat')
    read(1,*), n
    do i = 1,n
        read(1,*), (A(i,j), j= 1,n+1)
    enddo
    close(1)

    do i = 1,n-1
        if(A(i,i) == 0) then
            print*,'Error. Swap the rows for non-zero first coefficient.'
            stop
        else
            do j = i+1,n
                f = A(j,i)/A(i,i)
                do k = 1,n+1
                    A(j,k) = A(j,k) - f*A(i,k)
                enddo
            enddo
        endif
    enddo

    do i = 1,n
        write(*,*), (A(i,j), j= 1,n+1)
    enddo

    do i = 1,n
        x(i) = 0.0
    enddo

    x(n) = A(n,n+1)/A(n,n)

    do i = n-1,1,-1
        do j = n,i+1,-1
            x(i) = x(i) + (A(i,n+1) - A(i,j)*x(j))/A(i,i)
        enddo
    enddo

    open(2, file='output.dat')
    do i=1,n
        write(2,*), x(i)
    enddo

end program Gauss_Elimination
