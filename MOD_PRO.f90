﻿!RECLA AND PROTECT ZONE
MODULE MOD_PRO
IMPLICIT NONE
CONTAINS

SUBROUTINE WRITE_PRO(OUTNAME,NUMPRO,PROTECTNAME)
INTEGER                                  :: NUMPRO
CHARACTER(LEN=100),DIMENSION(NUMPRO)    :: PROTECTNAME(:)
CHARACTER(LEN=100)                       :: OUTNAME
INTEGER                                  :: II,JJ
INTEGER                                  :: NUMRECORD
REAL                                      :: XX,XX0,YY,YY0
LOGICAL                                  :: CLOSEOPT

CLOSEOPT=.FALSE.

OPEN(UNIT=10,FILE=TRIM(OUTNAME),STATUS='OLD',ENCODING='utf-8',ACCESS='APPEND')
DO II=1,NUMPRO
    WRITE(10,100) "#画保护区"
    WRITE(10,100) "$!ATTACHGEOM "
    WRITE(10,100) "ANCHORPOS "
    WRITE(10,100) "    {"
    NUMRECORD=0
    ! TO GET NUMBER OF RECORD
    OPEN(UNIT=20,FILE=TRIM(PROTECTNAME(II)),STATUS='OLD')
    DO WHILE (.NOT.EOF(20))
        READ(20,*) XX,YY
        NUMRECORD=NUMRECORD+1
        IF(NUMRECORD.EQ.1) THEN
            XX0=XX
            YY0=YY
        ENDIF
    ENDDO    
    CLOSE(20)
    IF(XX.NE.XX0) THEN
        CLOSEOPT= .TRUE.
    ELSE
        CLOSEOPT= .FALSE.
    ENDIF
    OPEN(UNIT=20,FILE=TRIM(PROTECTNAME(II)),STATUS='OLD')
    DO JJ=1,NUMRECORD
        READ(20,*) XX,YY
        IF(JJ.EQ.1) THEN
           XX0=XX
           YY0=YY
           WRITE(10,400) "X =",XX0
           WRITE(10,400) "Y =",YY0
           WRITE(10,100) "    }"
           WRITE(10,100)  " COLOR = RED"
           WRITE(10,100)  " RAWDATA "
           WRITE(10,100)  " 1 "
           IF(CLOSEOPT) THEN
                WRITE(10,"(5I)")  NUMRECORD+1
           ELSE
                WRITE(10,"(5I)")  NUMRECORD
           ENDIF
           WRITE(10,"(2I,2I)")  XX-XX0,YY-YY0
        ELSE
           WRITE(10,"(F15.5,F15.5)") XX-XX0,YY-YY0
        ENDIF
        IF(CLOSEOPT .AND. (JJ.EQ.NUMRECORD) ) THEN
            WRITE(10,100)  " 0  0 "
        ENDIF
     ENDDO
     !WRITE(10,100)  "0 0"
ENDDO
CLOSE(10)
100 FORMAT (A)    
400 FORMAT (A,F20.5) 
ENDSUBROUTINE WRITE_PRO
END MODULE MOD_PRO

