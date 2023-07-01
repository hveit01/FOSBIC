      SUBROUTINE ZINSNO                                                 ZIS00001
C*****SUBROUTINE TO INSERT NUMBER FROM ACCUMULATOR INTO NEXT PRINT POSITZIS00002
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZIS00003
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST, INEXT,ZIS00004
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZIS00005
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZIS00006
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZIS00007
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZIS00008
     1NIFMAX,INTZEI                                                     ZIS00009
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZIS00010
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZIS00011
     1IRET(20),XXX(4),NFILE(25,3)                                       ZIS00012
      COMMON// ISTLST(340),LISTST(340)                                  ZIS00013
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZIS00015
      DIMENSION IPROG(3700)                                             ZIS00016
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZIS00017
      DIMENSION CHAR(6)                                                 ZIS00018
      IST=INEXT-IZONE                                                   ZIS00019
      ISGN=0                                                            ZIS00020
      IF(ACC.GT.0.) GOTO 110                                            ZIS00021
      ACC=-ACC                                                          ZIS00022
      ISGN=1                                                            ZIS00023
C*****TEST FOR ZERO                                                     ZIS00024
 110  IF(ACC.GT.SMALL) GOTO 200                                         ZIS00025
C*****NUMBER IS TO BE PRINTED AS 0.0                                    ZIS00026
 150  CARP(IST+8)=DIGIT(1)                                              ZIS00027
      CARP(IST+9)=DECMAL                                                ZIS00028
      CARP(IST+10)=DIGIT(1)                                             ZIS00029
      GOTO 1000                                                         ZIS00030
C                                                                       ZIS00031
C*****FIND FRACTIONAL PART AND EXPONENT                                 ZIS00032
 200  ZLOG=ALOG10(ACC)                                                  ZIS00033
      IEXPN=ZLOG                                                        ZIS00034
      IF(ZLOG.LT.0.) IEXPN=IEXPN-1                                      ZIS00035
      FEXPN=IEXPN                                                       ZIS00036
      FRAC=10.**(ZLOG-FEXPN)                                            ZIS00037
 250  IFRAC=(ACC*(10.**(5-IEXPN)))+.5                                   ZIS00038
      IF(IFRAC.LE.999999) GOTO 280                                      ZIS00039
      IEXPN=IEXPN+1                                                     ZIS00040
      GOTO 250                                                          ZIS00041
C******LOAD CHARACTERS                                                  ZIS00042
 280  DO 300 I=1,6                                                      ZIS00043
      IDIV=6-I                                                          ZIS00044
      IX=IFRAC/(10**IDIV)                                               ZIS00045
      CHAR(I)=DIGIT(IX+1)                                               ZIS00046
 300  IFRAC=IFRAC-(IX*(10**IDIV))                                       ZIS00047
C                                                                       ZIS00048
C*****FIND LENGTH                                                       ZIS00049
      LNGTH=6                                                           ZIS00050
      DO 350 I=1,6                                                      ZIS00051
      IPOS=7-I                                                          ZIS00052
      IF(CHAR(IPOS).NE.DIGIT(1)) GOTO 380                               ZIS00053
 350  LNGTH=LNGTH-1                                                     ZIS00054
 380  IF(LNGTH.EQ.0) GOTO 150                                           ZIS00055
C                                                                       ZIS00056
C*****CHECK FOR A NUMBER GREATER THAN OR EQUAL TO 1                     ZIS00057
      IF(IEXPN.GE.0) GOTO 600                                           ZIS00058
C*****NUMBER IS FRACTIONAL                                              ZIS00059
      LNGTOT=LNGTH-IEXPN                                                ZIS00060
      IF(LNGTOT.GT.12) GOTO 900                                         ZIS00061
      LOCD=9                                                            ZIS00062
      IF(LNGTOT.GT.6) LOCD=15-LNGTOT                                    ZIS00063
C*****INSERT SIGN IF NECESSARY                                          ZIS00064
      LOCSGN=IST+LOCD-1                                                 ZIS00065
      CARP(LOCSGN)=DIGIT(1)                                             ZIS00066
      IF(ISGN.EQ.0) GOTO 450                                            ZIS00067
      CARP(LOCSGN)=CMINUS                                               ZIS00068
 450  LOCDEC=IST+LOCD                                                   ZIS00069
      CARP(LOCDEC)=DECMAL                                               ZIS00070
      IFSTL=IST+LOCD-IEXPN                                              ZIS00071
      IFSTLM=IFSTL-1                                                    ZIS00072
      IF(IFSTL.EQ.(IST+LOCD+1)) GOTO 500                                ZIS00073
      IBEG=IST+LOCD+1                                                   ZIS00074
      DO 475 I=IBEG,IFSTLM                                              ZIS00075
 475  CARP(I)=DIGIT(1)                                                  ZIS00076
 500  DO 510 I=1,LNGTH                                                  ZIS00077
      CARP(IFSTL)=CHAR(I)                                               ZIS00078
 510  IFSTL=IFSTL+1                                                     ZIS00079
C*****COMPLETE                                                          ZIS00080
      GOTO 1000                                                         ZIS00081
C                                                                       ZIS00082
C*****NUMBER IS NOT FRACTIONAL                                          ZIS00083
C*****NUMBER IS GREATER THAN OR EQUAL TO 1                              ZIS00084
C*****SEE IF NUMBER IS TOO BIG TO PRINT IN NORMAL FORM                  ZIS00085
 600  IF(IEXPN.GT.12) GOTO 900                                          ZIS00086
      LOCD=9                                                            ZIS00087
      IF(IEXPN.GT.6) LOCD=IEXPN+3                                       ZIS00088
      IFSTL=LOCD-1+IST-IEXPN                                            ZIS00089
C*****INSERT SIGN IF NECESSARY                                          ZIS00090
      IF(ISGN.EQ.0) GOTO 650                                            ZIS00091
      CARP(IFSTL-1)=CMINUS                                              ZIS00092
C*****INSERT CHARACTERS                                                 ZIS00093
 650  DO 675 I=1,LNGTH                                                  ZIS00094
      CARP(IFSTL)=CHAR(I)                                               ZIS00095
 670  IFSTL=IFSTL+1                                                     ZIS00096
      IF(IFSTL.EQ.(IST+LOCD)) GOTO 670                                  ZIS00097
 675  CONTINUE                                                          ZIS00098
C                                                                       ZIS00099
C*****CHECK FOR INTEGER                                                 ZIS00100
      IF(IFSTL.EQ.(IST+LOCD+1)) GOTO 1000                               ZIS00101
      IF(IFSTL.LE.(IST+LOCD-1)) GOTO 700                                ZIS00102
C*****INSERT DECIMAL                                                    ZIS00103
      LOCDEC=IST+LOCD                                                   ZIS00104
      CARP(LOCDEC)=DECMAL                                               ZIS00105
      GOTO 1000                                                         ZIS00106
C*****INSERT ZEROES                                                     ZIS00107
 700  IEND=IST+LOCD-1                                                   ZIS00108
      IBEG=IFSTL                                                        ZIS00109
      DO 725 I=IBEG,IEND                                                ZIS00110
 725  CARP(I)=DIGIT(1)                                                  ZIS00111
      GOTO 1000                                                         ZIS00112
C                                                                       ZIS00113
C*****INSERT NUMBER IN EXPONENTIAL FORM                                 ZIS00114
 900  IF(ISGN.EQ.1) CARP(IST+1)=CMINUS                                  ZIS00115
      CARP(IST+2)=CHAR(1)                                               ZIS00116
      CARP(IST+3)=DECMAL                                                ZIS00117
      DO 925 I=2,6                                                      ZIS00118
      LOC=IST+2+I                                                       ZIS00119
 925  CARP(LOC)=CHAR(I)                                                 ZIS00120
      CARP(IST+9)=ALPH(5)                                               ZIS00121
      CARP(IST+10)=PLUS                                                 ZIS00122
      IF(IEXPN.GT.0) GOTO 950                                           ZIS00123
      CARP(IST+10)=CMINUS                                               ZIS00124
      IEXPN=-IEXPN                                                      ZIS00125
C                                                                       ZIS00126
C*****INSERT EXPONENT                                                   ZIS00127
 950  IF(IEXPN.GT.9) GOTO 970                                           ZIS00128
      CARP(IST+11)=DIGIT(IEXPN+1)                                       ZIS00129
      GOTO 1000                                                         ZIS00130
 970  IF(IEXPN.GT.99) GOTO 980                                          ZIS00131
      IX=IEXPN/10                                                       ZIS00132
      CARP(IST+11)=DIGIT(IX+1)                                          ZIS00133
      IZ=IEXPN-(10*IX)                                                  ZIS00134
      CARP(IST+12)=DIGIT(IZ+1)                                          ZIS00135
      GOTO 1000                                                         ZIS00136
 980  IF(IEXPN.GT.IEXPO) GOTO 990                                       ZIS00137
      IX=IEXPN/100                                                      ZIS00138
      CARP(IST+11)=DIGIT(IX+1)                                          ZIS00139
      IZ=IEXPN-(100*IX)                                                 ZIS00140
      CARP(IST+11)=DIGIT(IZ+1)                                          ZIS00141
      LOC=IEXPN-(100*IX)-(10*IZ)                                        ZIS00142
      CARP(IST+13)=DIGIT(LOC+1)                                         ZIS00143
      GOTO 1000                                                         ZIS00144
 990  CARP(IST+11)=ASTRSK                                               ZIS00145
      CARP(IST+12)=ASTRSK                                               ZIS00146
      CARP(IST+13)=ASTRSK                                               ZIS00147
 1000 IF(NPRI.EQ.1) RETURN                                              ZIS00148
      IX=INEXT                                                          ZIS00149
      DO 1010 I=IST,IX                                                  ZIS00150
      IF(CARP(I).NE.BLANK) GOTO 1020                                    ZIS00151
 1010 CONTINUE                                                          ZIS00152
1020  IBEG=I                                                            ZIS00153
      DO 1030 LOC=IBEG,IX                                               ZIS00154
      IF(CARP(LOC).EQ.BLANK) GOTO 1040                                  ZIS00155
 1030 CONTINUE                                                          ZIS00156
 1040 IF((IST+LOC-IBEG+1).LT.IWRIT) GOTO 1050                           ZIS00157
      WRITE(IWC,1060) (CARP(I),I=1,IST)                                 ZIS00158
 1060 FORMAT(5X,125A1)                                                  ZIS00159
      IZ=LOC-IBEG                                                       ZIS00160
      DO 1070 I=1,IZ                                                    ZIS00161
      IX=IBEG+I-1                                                       ZIS00162
      CARP(I)=CARP(IX)                                                  ZIS00163
 1070 CONTINUE                                                          ZIS00164
      CALL CLEAR(IZ+1,140)                                              ZIS00165
      INEXT=IZ+2                                                        ZIS00166
      RETURN                                                            ZIS00167
 1050 LOC=LOC-1                                                         ZIS00168
      IF(IST.EQ.1) GOTO 1051                                            ZIS00169
      IF(IBEG.LT.IST+2) RETURN                                          ZIS00170
      IF(CARP(IST-1).NE.BLANK) IST=IST+1                                ZIS00171
 1051 DO 1090 I=IBEG,LOC                                                ZIS00172
      CARP(IST)=CARP(I)                                                 ZIS00173
      CARP(I)=BLANK                                                     ZIS00174
      IST=IST+1                                                         ZIS00175
 1090 CONTINUE                                                          ZIS00176
      INEXT=IST+1                                                       ZIS00177
      RETURN                                                            ZIS00178
      END                                                               ZIS00179

