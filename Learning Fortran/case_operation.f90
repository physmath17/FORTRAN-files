Program Control_Structure
Implicit None
Integer ::i,V=0,C=0,D=0,B=0,Others=0
Character :: Letter
Character(Len=50) :: Line
Print *,'Enter the sentence '
Read '(A)', Line

Do i=1,50
        Letter=Line(i:i)
        Select Case(Letter)
                Case('A','E','I','O','U','a','e','i','o','u')
                V=V+1
                Case('b','c','d','f','g','h','j','k','l','m','n','p','q',&
                     'r','s','t','v','w','x','y','z')
                C=C+1
                Case('0','1','2','3','4','5','6','7','8','9')
                D=D+1
                Case(' ')
                B=B+1
                Case Default
                Others=Others+1
        End Select
Enddo
Print *,'Vowels= ',V
Print *,'Consonants= ',C
Print *,'Digits= ',D
Print *,'Blank Space= ',B
Print *,'Others= ',Others

End Program Control_Structure
