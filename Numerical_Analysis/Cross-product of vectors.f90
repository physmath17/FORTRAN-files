Program CrossProduct
Implicit None
Integer::i,j,k,s
Real,Dimension(1:3)::A,B,C
Integer::E

Print *,'Enter the components of first vector a '
Do i=1,3
        Read *, A(i)
Enddo

Print *,'Enter the components of second vector b '
Do j=1,3
        Read *,B(j)
Enddo

Do k=1,3
        C(k)=0
Enddo

Do i=1,3
        Do j=1,3
                Do k=1,3
                       C(i)=C(i)+E(i,j,k)*A(j)*B(k)
                Enddo
        Enddo
Enddo

Print *,'The vector obtained from cross-product of a and b is given componentwise as '
Do i=1,3
        Print *,C(i)
Enddo
Print *,'Here,vectors are such that 1st component is along x-axis,2nd component is along y-axis and 3rd component is along z-axis '
End Program CrossProduct


Integer Function E(l,m,n)
Implicit None
Integer,Intent(in)::l,m,n
Integer::p

if(l==m .or. m==n .or. n==l) then
p=0
elseif((l==1 .and. m==2 .and. n==3) .or. (l==2 .and. m==3 .and. n==1) .or. (l==3 .and. m==1 .and. n==2)) then
p=1
else
p=-1
endif
E=p
End Function E




