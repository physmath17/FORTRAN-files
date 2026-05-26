Program GCD

Implicit none

Integer :: m,n,q

Print *,'Enter the numbers whose G.C.D. is to be found '
Read *, m,n

if(m<=n)then
q=m
else
q=n
endif

Do while((mod(m,q)/=0) .or. (mod(n,q)/=0))
q=q-1
Enddo

Print *,'The required G.C.D. is', q

End Program GCD
