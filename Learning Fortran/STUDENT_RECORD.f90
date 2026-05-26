PROGRAM Student_Record

IMPLICIT NONE

REAL,DIMENSION(5,6):: Record
REAL,DIMENSION(1:6) :: Sub_avg
REAL,DIMENSION(1:5) :: Student_avg
INTEGER :: i,j,k

PRINT *,'Enter the student record '

DO i=1,5
        READ *,(Record(i,j), j=1,6)
ENDDO

DO i=1,5
        PRINT *,(Record(i,j), j=1,6)
ENDDO

DO k=1,5
        Record(k,3)=Record(k,3)*2.5
ENDDO

Sub_avg=SUM(Record,DIM=1)/5
Student_avg=SUM(Record,DIM=2)/6

PRINT *,'THE STUDENT AVERAGES ARE'
DO k=1,5
        PRINT *,Student_avg(k)
ENDDO

PRINT *,'THE SUBJECT AVERAGES ARE'
DO  k=1,6
       PRINT *,Sub_avg(k)
ENDDO

END PROGRAM Student_Record    
