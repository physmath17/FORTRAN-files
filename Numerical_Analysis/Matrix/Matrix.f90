PROGRAM MATRIX_INVERSION

! THIS PROGRAM INVERTS A MATRIX 

IMPLICIT NONE

REAL, DIMENSION(2,2) :: A
REAL, DIMENSION(2,2) :: B
INTEGER :: i,j
REAL :: K

PRINT *, 'ENTER THE MATRIX '

        DO i=1,2
                READ *, (A(i,j), j=1,2)       
        ENDDO
        DO i=1,2
                PRINT *, (A(i,j), j=1,2)
        ENDDO

        IF(A(1,1)*A(2,2)-A(1,2)*A(2,1)==0) THEN
                PRINT *, 'A IS NOT INVERTIBLE'
        ELSE
                K=A(1,1)*A(2,2)-A(1,2)*A(2,1)
                B(1,1)=A(2,2)/K
                B(2,2)=A(1,1)/K
                B(1,2)=-A(1,2)/K
                B(2,1)=-A(2,1)/K   
                PRINT *, 'THE INVERSE OF MATRIX A IS '
                DO i=1,2
                        PRINT *, (B(i,j), j=1,2)
                ENDDO
        ENDIF


END PROGRAM MATRIX_INVERSION
        
