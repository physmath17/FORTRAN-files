Program RN

Implicit None
Integer :: i,j,k
Real,Dimension(5,6)::X

Do k=1,6
        Call Random_Seed(int(k))
        Call Random_Number(X)
Enddo

Do i=1,5
        Print *,(X(i,j),j=1,6)
Enddo

End Program RN
