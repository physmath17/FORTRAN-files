Program ONE_D_RW

Implicit None
Integer::i,j,k,d,count=0,sum=0
Real,Dimension(1:2**5)::X
Real,Dimension(1:5)::A

Do i=1,2**5
        Call Random_Seed(int(i))
        Call Random_Number(X)
Enddo

Do j=1,5
        Do k=1,2**j
                if(X(k)<0.5) then
                count=count-1
                elseif(X(k)>0.5) then
                count=count+1
                Endif
        Enddo
                d=count
                count=0
                sum=sum+d*d
        A(j)=sum/(2**j)
        Print *,A(j)
Enddo

End Program ONE_D_RW
