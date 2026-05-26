Program Two_RW

Implicit None
Integer::i,j,l,k,m,t,n=20*2**4,count=0
Real,Dimension(2,2**4)::R
Real,Dimension(20,2**4)::Num
Real::sum=0,s=0
Real,Dimension(1:10)::Avg
Real,Dimension(1:100)::A
Open(1,File="Data.out")

Do t=1,10

Do l=1,100

        Do m=1,n
               Call Random_Seed(int(m))
               Call Random_Number(Num)
        Enddo
         
        Do j=1,2**4

                Do i=1,10,1
                        if(Num(i,j)<0.5) then
                        count=count-1
                        elseif(Num(i,j)>0.5) then
                        count=count+1
                        Endif
                Enddo
                R(1,j)=count
                count=0
                
                Do k=11,20,1
                        if(Num(k,j)<0.5) then
                        count=count-1
                        elseif(Num(k,j)>0.5) then
                        count=count+1
                        Endif
                Enddo
                R(2,j)=count
                count=0
        
                sum=sum+(R(1,j)*R(1,j))+(R(2,j)*R(2,j))
        Enddo
        A(l)=sum/(2**4)
        sum=0
        s=s+A(l)
Enddo

        Avg(t)=Avg(t)+s/100
        Print *,Avg(t)
Enddo

!Do t=1,10
 !       Write(1,*) t,Avg(t)
!Enddo

s=0

End Program Two_RW
