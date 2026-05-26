Program PI

Implicit None
Real,Dimension(2,170000000) :: X
Real,Dimension(1:170000000) :: R
Integer :: j,k,l,count=0
Real :: p,i

Do k = 1,2
        Call Random_Seed(int(k))
        Call Random_Number(X)
Enddo

Do j=1,170000000
        R(j)=sqrt(X(1,j)*X(1,j)+X(2,j)*X(2,j))
Enddo

Do l=1,170000000
       if(R(l)<=1) then
           count=count+1
       Endif
Enddo

p=4*Real(count)/170000000

i=4*atan(Real(1))

Print *,count,p,i

End Program PI
