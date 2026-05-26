PROGRAM Dynamical_array

IMPLICIT NONE

INTEGER :: i,j,k
INTEGER,PARAMETER :: n=50

PRINT *, 'Enter the order of the matrix '
READ *, k

REAL,DIMENSION(1:n,1:n):: Matrix

PRINT *, 'Enter the matrix '
DO i=1,k
        READ *, (Matrix(i,j), j=1,k)
ENDDO

PRINT *, 'The matrix is '
DO i=1,k
        PRINT *, (Matrix(i,j), j=1,k)
ENDDO

END PROGRAM Dynamical_array 
