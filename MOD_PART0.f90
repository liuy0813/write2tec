﻿! TO PRINT SOME SOLID LINE TO MCR AND SAVE LINES IN MAIN PROGRAM
MODULE MOD_PART0
IMPLICIT NONE
CONTAINS
    SUBROUTINE WRITE_PART0(OUTNAME)
    CHARACTER(LEN=100)                  :: OUTNAME
    OPEN(UNIT=10,FILE=TRIM(OUTNAME),STATUS='OLD',ENCODING='utf-8',ACCESS='APPEND')
    WRITE(10,100)""
    WRITE(10,100)""
    WRITE(10,100)""
    WRITE(10,100)"  READDATAOPTION = NEW"
    WRITE(10,100)"  RESETSTYLE = YES"
    WRITE(10,100)"  INCLUDETEXT = NO"
    WRITE(10,100)"  INCLUDEGEOM = NO"
    WRITE(10,100)"  INCLUDECUSTOMLABELS = NO"
    WRITE(10,100)"  INITIALPLOTFIRSTZONEONLY = YES"
    WRITE(10,100)"  VARLOADMODE = BYNAME"
    WRITE(10,100)"  ASSIGNSTRANDIDS = YES"
    WRITE(10,100)"  INITIALPLOTTYPE = CARTESIAN2D"
    WRITE(10,100)"  VARNAMELIST = '"//'"X" '//'"Y" '//'"thickness"'//"'"
    WRITE(10,100)"$!PICK ADDATPOSITION"
    WRITE(10,100)"  X = 1.50664451827"
    WRITE(10,100)"  Y = 4.23671096346"
    WRITE(10,100)"  CONSIDERSTYLE = YES"
    WRITE(10,100)"$!TWODAXIS YDETAIL{SHOWAXIS = NO}"
    WRITE(10,100)"$!TWODAXIS XDETAIL{SHOWAXIS = NO}"
    WRITE(10,100)"$!PICK ADDATPOSITION"
    WRITE(10,100)"  X = 0.310631229236"
    WRITE(10,100)"  Y = 4.39617940199"
    WRITE(10,100)"  CONSIDERSTYLE = YES"
    WRITE(10,100)"$!REDRAWALL"
    WRITE(10,100)"$!PICK ADDATPOSITION"
    WRITE(10,100)"  X = 2.13122923588"
    WRITE(10,100)"  Y = 4.64867109635"
    WRITE(10,100)"  CONSIDERSTYLE = YES"
    WRITE(10,100)"$!PICK SHIFT"
    WRITE(10,100)"  X = -1.1561461794"
    WRITE(10,100)"  Y = 0"
    WRITE(10,100)"  PICKSUBPOSITION = LEFT"
    WRITE(10,100)"$!PICK SHIFT"
    WRITE(10,100)"  X = 0"
    WRITE(10,100)"  Y = -0.93023255814"
    WRITE(10,100)"  PICKSUBPOSITION = TOP"
    WRITE(10,100)"$!PICK SHIFT"
    WRITE(10,100)"  X = 1.06312292359"
    WRITE(10,100)"  Y = 0"
    WRITE(10,100)"  PICKSUBPOSITION = RIGHT"
    WRITE(10,100)"$!PICK SHIFT"
    WRITE(10,100)"  X = 0"
    WRITE(10,100)"  Y = 0.85049833887"
    WRITE(10,100)"  PICKSUBPOSITION = BOTTOM"
    WRITE(10,100)"$!REDRAWALL"
    WRITE(10,100)"$!VIEW ZOOM"
    WRITE(10,100)"  X1 = |XZX|"
    WRITE(10,100)"  Y1 = |YZX|"
    WRITE(10,100)"  X2 = |XYS|"
    WRITE(10,100)"  Y2 = |YYS|"
    WRITE(10,100)"$!REDRAWALL"
    WRITE(10,100)"$!TWODAXIS GRIDAREA{ISFILLED = YES}"
    WRITE(10,100)"$!TWODAXIS GRIDAREA{FILLCOLOR = CUSTOM18}"
    WRITE(10,100)"$!REDRAWALL"
    WRITE(10,100)"#$!RemoveVar |MFBD|"
    WRITE(10,100)"## 为防止图形位置变化，使用了FRAME坐标"
    WRITE(10,100)"## 做指北针的多边形，用红色填充"
    WRITE(10,100)"$!ATTACHGEOM"
    WRITE(10,100)"POSITIONCOORDSYS = FRAME"
    WRITE(10,100)"ANCHORPOS"
    WRITE(10,100)"    {"
    WRITE(10,100)"    X = (|X_GEO|)"
    WRITE(10,100)"    Y = (|Y_GEO|)"
    WRITE(10,100)"    }"
    WRITE(10,100)"COLOR = RED"
    WRITE(10,100)"ISFILLED = YES"
    WRITE(10,100)"FILLCOLOR = RED"
    WRITE(10,100)"RAWDATA"
    WRITE(10,100)"1"
    WRITE(10,100)"4"
    WRITE(10,100)"0 0"
    WRITE(10,100)"1 -6"
    WRITE(10,100)"0 -3"
    WRITE(10,100)"-1 -6"
    WRITE(10,100)""
    WRITE(10,100)"## 做文字N"
    WRITE(10,100)"$!ATTACHTEXT"
    WRITE(10,100)"POSITIONCOORDSYS = FRAME"
    WRITE(10,100)"ANCHORPOS"
    WRITE(10,100)"    {"
    WRITE(10,100)"    X = |X_TXT|"
    WRITE(10,100)"    Y = |Y_TXT|"
    WRITE(10,100)"    }"
    WRITE(10,100)"TEXTSHAPE"
    WRITE(10,100)"    {"
    WRITE(10,100)"    HEIGHT = 17"
    WRITE(10,100)"    }"
    WRITE(10,100)"TEXT = 'N'"
    WRITE(10,100)"## 画比例尺"
    WRITE(10,100)"$!ATTACHGEOM"
    WRITE(10,100)"  GEOMTYPE = RECTANGLE"
    WRITE(10,100)"  ANCHORPOS"
    WRITE(10,100)"    {"
    WRITE(10,100)"    X = |X_BLC|"
    WRITE(10,100)"    Y = |Y_BLC|"
    WRITE(10,100)"    }"
    WRITE(10,100)"  ISFILLED = YES"
    WRITE(10,100)"  FILLCOLOR = WHITE"
    WRITE(10,100)"  RAWDATA"
    WRITE(10,100)"  |LEN_BLC2| |HIGH_BLC|"
    WRITE(10,100)"  $!ATTACHGEOM"
    WRITE(10,100)"  GEOMTYPE = RECTANGLE"
    WRITE(10,100)"  ANCHORPOS"
    WRITE(10,100)"    {"
    WRITE(10,100)"    X = |X_BLC2|"
    WRITE(10,100)"    Y = |Y_BLC|"
    WRITE(10,100)"    }"
    WRITE(10,100)"  ISFILLED = YES"
    WRITE(10,100)"  FILLCOLOR = BLACK"
    WRITE(10,100)"  RAWDATA"
    WRITE(10,100)"  |LEN_BLC2| |HIGH_BLC|"
    WRITE(10,100)"## 写比例尺上的标识"
    WRITE(10,100)""
    WRITE(10,100)"$!ATTACHTEXT"
    WRITE(10,100)"POSITIONCOORDSYS = GRID"
    WRITE(10,100)"ANCHORPOS"
    WRITE(10,100)"    {"
    WRITE(10,100)"    X = |TXT_BLC_X|"
    WRITE(10,100)"    Y = |TXT_BLC_Y|"
    WRITE(10,100)"    }"
    WRITE(10,100)"TEXTSHAPE"
    WRITE(10,100)"    {"
    WRITE(10,100)"    HEIGHT = 17"
    WRITE(10,100)"    }"
    WRITE(10,100)"TEXT = '|LEN_BLC|m'"
    CLOSE(10)
    100 FORMAT (A)
    ENDSUBROUTINE WRITE_PART0
    END MODULE MOD_PART0