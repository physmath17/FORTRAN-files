Program Central_Limit_Theorem

Implicit None

Real,Dimension(100000,100):: X
Real,Dimension(1:100000):: Avg
Integer :: i,j,k,l
Real :: sum=0
Integer :: p0=0,p1=0,p2=0,p3=0,p4=0,p5=0,p6=0,p7=0,p8=0,p9=0

Do k=1,100000
        Call Random_Seed(int(k))
        Call Random_Number(X)
Enddo

Do l=1,100000
        Do j=1,100
                sum=sum+X(l,j)
                Avg(l)=sum/100
        Enddo
        sum=0        
        Print *, Avg(l)
Enddo        

Do l=1,100000
        if((0<Avg(l)) .and. (Avg(l)<=0.1)) then
        p0=p0+1
        elseif((0.1<Avg(l)) .and. (Avg(l)<=0.2)) then
        p1=p1+1
        elseif((0.2<Avg(l)) .and. (Avg(l)<=0.3)) then
        p2=p2+1
        elseif((0.3<Avg(l)) .and. (Avg(l)<=0.4)) then
        p3=p3+1
        elseif((0.4<Avg(l)) .and. (Avg(l)<=0.5)) then
        p4=p4+1
        elseif((0.5<Avg(l)) .and. (Avg(l)<=0.6)) then 
        p5=p5+1
        elseif((0.6<Avg(l)) .and. (Avg(l)<=0.7)) then
        p6=p6+1
        elseif((0.7<Avg(l)).and. (Avg(l)<=0.8)) then
        p7=p7+1
        elseif((0.8<Avg(l)) .and. (Avg(l)<=0.9)) then
        p8=p8+1
        elseif((0.9<Avg(l)) .and. (Avg(l)<=1)) then
        p9=p9+1
        else 
        Print *,'Invalid data'
        Endif
Enddo

Print *,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9

End Program Central_Limit_Theorem

