!PLOT SOME LINES
MODULE MOD_LINES
IMPLICIT NONE
CONTAINS

SUBROUTINE WRITE_LINES(OUTNAME,NUMLINE,LINENAME,LINECOLOR,LINEWIDTH)
CHARACTER(LEN=100)                       ::   OUTNAME
INTEGER,INTENT(IN)                      ::   NUMLINE
CHARACTER(LEN=100),DIMENSION(NUMLINE)   ::  LINENAME(:),LINECOLOR(:)
REAL,DIMENSION(NUMLINE)                 ::  LINEWIDTH(:)
INTEGER                                  ::  II,JJ
INTEGER                                  ::   NUMRECORD
REAL                                     ::   XX0,YY0
REAL,ALLOCATABLE                        ::   XX(:),YY(:)

OPEN(UNIT=10,FILE=TRIM(OUTNAME),STATUS='OLD',ENCODING='utf-8',ACCESS='APPEND')
DO JJ=1,NUMLINE
    WRITE(10,100) "#画直线"
    WRITE(10,100) "$!ATTACHGEOM "
    WRITE(10,100) "ANCHORPOS "
    WRITE(10,100) "    {"
    NUMRECORD=0
    ! TO GET NUMBER OF RECORD
    OPEN(UNIT=20,FILE=TRIM(LINENAME(JJ)),STATUS='OLD')
    DO WHILE (.NOT.EOF(20))
        READ(20,*) XX0,YY0
        NUMRECORD=NUMRECORD+1
    ENDDO
    CLOSE(20)
    ALLOCATE(XX(NUMRECORD))
    ALLOCATE(YY(NUMRECORD))
    OPEN(UNIT=20,FILE=TRIM(LINENAME(JJ)),STATUS='OLD')
    DO II=1,NUMRECORD
        READ(20,*) XX(II),YY(II)
        IF(II.EQ.1) THEN
           XX0=XX(II)
           YY0=YY(II)
           WRITE(10,400) "X =",XX0
           WRITE(10,400) "Y =",YY0
           WRITE(10,100) "    }"
           WRITE(10,400)  " LINETHICKNESS = ",LINEWIDTH(JJ)
           WRITE(10,"(A,A)")  "  COLOR =  ",TRIM(LINECOLOR(JJ))
           WRITE(10,100)  " RAWDATA "
           WRITE(10,100)  " 1 "
           WRITE(10,"(5I)")  NUMRECORD 
           WRITE(10,100)  "0 0"
        ELSE
           WRITE(10,"(F15.5,F15.5)") XX(II)-XX0,YY(II)-YY0
        ENDIF
    ENDDO
    CLOSE(20)
ENDDO
     100 FORMAT (A) 
     200 FORMAT (F20.5)
     400 FORMAT (A,F20.5)
     CLOSE(10)
     DEALLOCATE(XX,YY)
     !WRITE(10,100)  "0 0"
     ENDSUBROUTINE WRITE_LINES

     character(len=20) function str(k)
     !   "Convert an integer to string."
        integer, intent(in) :: k
        write (str, *) k
        str = adjustl(str)
        end function str

END MODULE MOD_LINES