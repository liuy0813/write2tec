!PLOT PARTICAL TRAJ
MODULE MOD_TRAJ
IMPLICIT NONE
CONTAINS

SUBROUTINE WRITE_TRAJ(OUTNAME,TRAJNAME,TIME0,NORTHS)
CHARACTER(LEN=100)                       :: OUTNAME,TRAJNAME
INTEGER                                  :: II,JJ
INTEGER                                  :: NUMRECORD,LL
REAL                                     :: XX0,YY0,TMPVAR
REAL,DIMENSION(:)                       :: TIME0
INTEGER,INTENT(IN)                         :: NORTHS
REAL                                     ::TIME2
REAL,ALLOCATABLE                        :: TIME1(:),XX(:),YY(:)

LL=SIZE(TIME0)
OPEN(UNIT=10,FILE=TRIM(OUTNAME),STATUS='OLD',ENCODING='utf-8',ACCESS='APPEND')
    WRITE(10,100) "#画轨迹"
    WRITE(10,100) "$!ATTACHGEOM "
    WRITE(10,100) "ANCHORPOS "
    WRITE(10,100) "    {"
    NUMRECORD=0
    ! TO GET NUMBER OF RECORD
    OPEN(UNIT=20,FILE=TRIM(TRAJNAME),STATUS='OLD')
    DO WHILE (.NOT.EOF(20))
        READ(20,*) XX0,YY0,TMPVAR
        NUMRECORD=NUMRECORD+1
    ENDDO
    CLOSE(20)
    ALLOCATE(TIME1(NUMRECORD))
    ALLOCATE(XX(NUMRECORD))
    ALLOCATE(YY(NUMRECORD))
    OPEN(UNIT=20,FILE=TRIM(TRAJNAME),STATUS='OLD')
    DO II=1,NUMRECORD
        READ(20,*) XX(II),YY(II),TIME1(II)
        IF(II.EQ.1) THEN
           XX0=XX(II)
           YY0=YY(II)
           WRITE(10,400) "X =",XX0
           WRITE(10,400) "Y =",YY0
           WRITE(10,100) "    }"
           WRITE(10,100)  " LINETHICKNESS = 0.4"
           WRITE(10,100)  "  COLOR = RED  "
           WRITE(10,100)  " RAWDATA "
           WRITE(10,100)  " 1 "
           WRITE(10,"(5I)")  NUMRECORD 
           WRITE(10,100)  "0 0"
        ELSE
           WRITE(10,"(F15.5,F15.5)") XX(II)-XX0,YY(II)-YY0
        ENDIF
    ENDDO
    CLOSE(20)
    WRITE(10,100) "#画溢油点"
    WRITE(10,100) "$!ATTACHGEOM "
    WRITE(10,100) "     GEOMTYPE = CIRCLE "
    WRITE(10,100) "ANCHORPOS "
    WRITE(10,100) "    {"
    WRITE(10,400) "X =",XX0
    WRITE(10,400) "Y =",YY0
    WRITE(10,100) "    }"
    WRITE(10,100) " NUMELLIPSEPTS = 4 "
    WRITE(10,100) "  COLOR = BLUE"
    WRITE(10,100) "  ISFILLED = YES"
    WRITE(10,100) " FILLCOLOR = CUSTOM6"
    WRITE(10,100) " RAWDATA "
    WRITE(10,200) NORTHS*1./25
    ! label text
    DO II=1,LL
        TIME2=TIME0(II)
        DO JJ=1,NUMRECORD
            IF(TIME1(JJ).EQ.TIME2) THEN
                !txt label
                WRITE(10,100) "$!ATTACHTEXT "
                WRITE(10,100) "POSITIONCOORDSYS = GRID"
                WRITE(10,100) "  ANCHORPOS"
                WRITE(10,100) "  {"
                WRITE(10,400) "X =",XX(JJ)
                WRITE(10,400) "Y =",YY(JJ)
                WRITE(10,100) "    }"
                WRITE(10,100) " TEXTSHAPE"
                WRITE(10,100) "  {"
                WRITE(10,100) "  FONTFAMILY = "//"'Times New Roman'"
                WRITE(10,100) "    }"
                WRITE(10,"(A)") "TEXT='"//TRIM(STR(int(TIME1(JJ))))//"h'"
                ! circle
                WRITE(10,100) "$!ATTACHGEOM "
                WRITE(10,100) "GEOMTYPE = CIRCLE"
                WRITE(10,100) "  ANCHORPOS"
                WRITE(10,100) "  {"
                WRITE(10,400) "X =",XX(JJ)
                WRITE(10,400) "Y =",YY(JJ)
                WRITE(10,100) "    }"
                WRITE(10,100) " COLOR = WHITE"
                WRITE(10,100) "  ISFILLED = YES"
                WRITE(10,100) "  FILLCOLOR = BLUE"
                WRITE(10,100) "    RAWDATA"
                WRITE(10,200) NORTHS*1./25
            ENDIF
        ENDDO
     ENDDO
     WRITE(10,100)"$!REDRAWALL"
     WRITE(10,100)""
     WRITE(10,100)" $!EXPORTSETUP EXPORTFORMAT = JPEG"
     WRITE(10,100)"$!EXPORTSETUP IMAGEWIDTH = 1500"
     WRITE(10,100)"$!EXPORTSETUP QUALITY = 100"
     100 FORMAT (A) 
     200 FORMAT (F20.5)
     400 FORMAT (A,F20.5)
     CLOSE(10)
     DEALLOCATE(TIME1,XX,YY)
     !WRITE(10,100)  "0 0"
     ENDSUBROUTINE WRITE_TRAJ

     character(len=20) function str(k)
     !   "Convert an integer to string."
        integer, intent(in) :: k
        write (str, *) k
        str = adjustl(str)
        end function str

END MODULE MOD_TRAJ