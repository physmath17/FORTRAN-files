program Gauss_Jordan

    implicit none

    real,dimension(10,10)::A, B, C, P
    integer::i,j,k,n

    open(1, file='input.dat')
    read(1,*), n
    do i = 1,n
        read(1,*), (A(i,j), j= 1,n)
    enddo
    close(1)

    C = A

    do i=1, n
        do j=1,n
            if(i==j) then
                B(i,j) = 1
            else
                B(i,j) = 0
            endif
        enddo
    enddo

    do i = 1,n
        write(*,*), (B(i,j), j= 1,n)
    enddo

    do i = 1,n
        if(A(i,i) == 0) then
            print*,'Error. Swap the rows for non-zero first coefficient.'
            stop
        else
            do k = n, i, -1
                A(i,k) = A(i,k)/A(i,i)
                B(i,k) = B(i,k)/A(i,i)
                do j = i+1, n
                    A(j,k) = A(j,k) - A(i,k)*A(j,i)
                    B(j,k) = B(j,k) - A(i,k)*B(j,i)
                enddo
            enddo
        endif
    enddo

    do i = n, 2, -1
        do j = i-1, 1, -1
            A(j,i) = A(j,i) - A(j,i)*A(i,i)
            B(j,i) = B(j,i) - A(j,i)*B(i,i)
        enddo
    enddo

    do i = 1,n
        write(*,*), (A(i,j), j= 1,n)
    enddo

    open(2, file='output.dat')
    do i = 1,n
        write(2,*), (B(i,j), j= 1,n)
    enddo

end program Gauss_Jordan
