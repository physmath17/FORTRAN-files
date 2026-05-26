Program Calculator
Implicit None
Real :: m,n
Character :: c

Print *,'Enter the numbers '
Read *,m,n
Print *,'Enter the operation '
Read '(A)',c

Select case(c)
        Case('+')
        Print *,m+n
        Case('-')
        Print *,m-n
        Case('*')
        Print *,m*n
        Case('/')
        Print *,m/n
        Case Default
        Print *,'Invalid input'
End Select

End Program Calculator
