Program HCF

Implicit None

Integer :: m,n,g
Integer, External :: gcd

Print *, 'Enter the numbers '
Read *, m,n

g=gcd(m,n)

Print *, 'The G.C.D. is ',g

End Program HCF

Integer Function  gcd(s,t)
Implicit none
Integer,Intent(in) :: s,t
Integer :: u

if(s<=t) then
u=s
else
u=t
endif
Do while((mod(s,u)/=0) .or. (mod(t,u)/=0))
u=u-1
Enddo
gcd=u 

End Function gcd
