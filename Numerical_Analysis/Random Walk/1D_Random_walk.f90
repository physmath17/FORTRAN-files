Program One_RW

Implicit None
Integer::i,j,l,k,m,n=2**5,p=5,count=0
Real,Dimension(1:2**5)::R
Real,Dimension(5,2**5)::X
Real::sum=0,avg=0,s=0
Real,Dimension(1:100)::A

Do l=1,100

        Do k=1,n
                R(k)=0
        Enddo   
 
        Do j=1,n
               Call Random_Seed(j)
               Call Random_Number(X)
       
                 Do i=1,p
                        if(X(i,j)<0.5) then
                        count=count-1
                        elseif(X(i,j)>0.5) then
                        count=count+1
                        Endif
                Enddo
                R(j)=count
                count=0
        Enddo

        Do m=1,n
                sum=sum+R(m)*R(m)
        Enddo

        A(l)=sum/n
        sum=0
        s=s+A(l)
Enddo

avg=avg+s/100
Print *,avg


End Program One_RW


