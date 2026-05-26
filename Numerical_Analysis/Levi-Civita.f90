Program Levi_Civita
Implicit None
Integer::l,m,n
Integer::E

Do l=1,3
        Do m=1,3
                Do n=1,3
                        Print *,E(l,m,n)
                Enddo
        Enddo
Enddo

End Program Levi_Civita

Integer Function E(i,j,k)
Implicit None
Integer,Intent(In)::i,j,k
Integer::p

if(i==j .or. j==k .or. k==i) then
p=0
elseif((i==1 .and. j==2 .and. k==3) .or. (i==2 .and. j==3 .and. k==1) .or. (i==3 .and. j==1 .and. k==2)) then
p=1
else
p=-1
endif
E=p
End Function E


