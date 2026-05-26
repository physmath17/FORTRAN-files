Program Factorial

Implicit None

Integer :: n
Double precision :: f
Double precision , External :: fact

Print *, 'Enter the number '
Read *, n

f=fact(n)
Print *,'The factorial value is ',f

End Program Factorial

Double precision Function fact(m)
Integer, Intent(in) :: m
Double precision :: x=0.0,y
Double precision :: z=1,u
!Integer :: i

Do i=1,m
x=x+log(real(i))
Enddo
y=exp(x)


Do i=1,m
z=i*z
Enddo
fact=z
Print *,y
Print *,z
End Function fact
