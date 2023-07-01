      SUBROUTINE ZEVAL (NSTOP)                                          ZEV00001
C                                                                       ZEV00002
C*****SUBROUTINE TO EVALUATE EXPRESSIONS IN REVERSE POLISH NOTATION     ZEV00003
C                                                                       ZEV00004
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZEV00005
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST, INEXT,ZEV00006
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZEV00007
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZEV00008
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZEV00009
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZEV00010
     1NIFMAX,INTZEI                                                     ZEV00011
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZEV00012
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZEV00013
     1IRET(20),XXX(4),NFILE(25,3)                                       ZEV00014
      COMMON// ISTLST(340),LISTST(340)                                  ZEV00015
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZEV00017
      DIMENSION IPROG(3700)                                             ZEV00018
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZEV00019
      DIMENSION FEXP(100),IEXP(100)                                     ZEV00020
      EQUIVALENCE(IEXP(1),FEXP(1))                                      ZEV00021
C                                                                       ZEV00022
C*****INSERT STRING IN IEXP                                             ZEV00023
      INREG=INREG-1                                                     ZEV00024
      LNGTH=IPROG(INREG)                                                ZEV00025
      DO 5 I=1,LNGTH                                                    ZEV00026
      ILOC=INREG-I                                                      ZEV00027
 5    IEXP(I)=IPROG(ILOC)                                               ZEV00028
C*****RESET INREG                                                       ZEV00029
      INREG=INREG-1-LNGTH                                               ZEV00030
C                                                                       ZEV00031
C*****EXPRESSION IS IN IEXP(1) THROUGH IEXP(LNGTH)                      ZEV00032
C*****OPERANDS ARE INDICATED BY THEIR POSITIONS IN VECTOR DATA          ZEV00033
C                                                                       ZEV00034
C*****OPERATIONS ARE --                                                 ZEV00035
C*****-1 ADDITION                                                       ZEV00036
C*****-2 SUBTRACTION                                                    ZEV00037
C*****-3 MULTIPLICATION                                                 ZEV00038
C*****-4 DIVISION                                                       ZEV00039
C*****-5 EXPONENTIATION                                                 ZEV00040
C*****-6 UNARY MINUS                                                    ZEV00041
C*****-7 INDIRECT FOR SUBSCRIPTED VARIABLE WITHOUT A SUBSCRIPT          ZEV00042
C*****-8 INDIRECT FOR SUBSCRIPTED VARIABLE WITH A SUBSCRIPT             ZEV00043
C*****-9 SIN                                                            ZEV00044
C*****-10 COS                                                           ZEV00045
C*****-11 TAN                                                           ZEV00046
C*****-12 ATN                                                           ZEV00047
C*****-13 EXP                                                           ZEV00048
C*****-14 ABS                                                           ZEV00049
C*****-15 LOG                                                           ZEV00050
C*****-16 SQR                                                           ZEV00051
C*****-17 INT                                                           ZEV00052
C*****-18 RND                                                           ZEV00053
C*****-19 STORE ACCUMULATOR IN SUBSCRIPTED POSITION                     ZEV00054
C*****-20 STORE ALPHANUMERIC CODE INTO ACCUMULATOR                      ZEV00055
C                                                                       ZEV00056
C*****SET CURRENT LAST POSITION                                         ZEV00057
      ILAST=0                                                           ZEV00058
      DO 1000 I=1,LNGTH                                                 ZEV00059
      IENTRY=IEXP(I)                                                    ZEV00060
      IF(IENTRY.GT.0) GOTO 900                                          ZEV00061
C*****ENTRY IS AN OPERAND -- PROCESS IT                                 ZEV00062
      IENTRY=-IENTRY                                                    ZEV00063
      IF(IENTRY.EQ.20) GOTO 200                                         ZEV00064
      GOTO (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,     ZEV00065
     1 170,180,190), IENTRY                                             ZEV00066
C                                                                       ZEV00067
 10   ILAST=ILAST-1                                                     ZEV00068
      FEXP(ILAST)=FEXP(ILAST)+FEXP(ILAST+1)                             ZEV00069
      GOTO 1000                                                         ZEV00070
C                                                                       ZEV00071
 20   ILAST=ILAST-1                                                     ZEV00072
      FEXP(ILAST)=FEXP(ILAST)-FEXP(ILAST+1)                             ZEV00073
      GOTO 1000                                                         ZEV00074
C                                                                       ZEV00075
 30   ILAST=ILAST-1                                                     ZEV00076
      IF((FEXP(ILAST).EQ.0.).OR.(FEXP(ILAST+1).EQ.0.)) GOTO 36          ZEV00077
      IF(ALOG10(ABS(FEXP(ILAST)))+ALOG10(ABS(FEXP(ILAST+1))).GT.FLOAT(  ZEV00078
     1IEXPO)) GOTO 35                                                   ZEV00079
 36   FEXP(ILAST)=FEXP(ILAST)*FEXP(ILAST+1)                             ZEV00080
      GOTO 1000                                                         ZEV00081
 35   NERROR=40                                                         ZEV00082
      X1=FEXP(ILAST)                                                    ZEV00083
      X2=FEXP(ILAST+1)                                                  ZEV00084
      GOTO 44                                                           ZEV00085
C                                                                       ZEV00086
 40   ILAST=ILAST-1                                                     ZEV00087
      IF(FEXP(ILAST+1).NE.0.) GOTO 42                                   ZEV00088
      NERROR=14                                                         ZEV00089
      GOTO 44                                                           ZEV00090
 42   FEXP(ILAST)=FEXP(ILAST) /FEXP(ILAST+1)                            ZEV00091
      GOTO 1000                                                         ZEV00092
C                                                                       ZEV00093
 50   ILAST=ILAST-1                                                     ZEV00094
      IF(FEXP(ILAST).EQ.0.) GOTO 58                                     ZEV00095
      IF(FEXP(ILAST+1).EQ.0.) GOTO 51                                   ZEV00096
      IF(ABS(ALOG10(ABS(FEXP(ILAST)))*FEXP(ILAST+1)).GT.FLOAT(IEXPO))   ZEV00097
     1 GOTO 57                                                          ZEV00098
      IF(FEXP(ILAST+1).GT.0.) GOTO 56                                   ZEV00099
      FEXP(ILAST+1)=-FEXP(ILAST+1)                                      ZEV00100
      FEXP(ILAST)=1./FEXP(ILAST)                                        ZEV00101
 56   ILOC=FEXP(ILAST+1)                                                ZEV00102
      XLOC=ILOC                                                         ZEV00103
      IF(XLOC.EQ.FEXP(ILAST+1)) GOTO 54                                 ZEV00104
      IF(FEXP(ILAST).GE.0.) GOTO 53                                     ZEV00105
      XLOC=ABS(FEXP(ILAST))**FEXP(ILAST+1)                              ZEV00106
      ACC=-1.*XLOC**(1./FEXP(ILAST+1))                                  ZEV00107
      IF(ACC.NE.FEXP(ILAST)) GOTO 55                                    ZEV00108
      FEXP(ILAST)=-XLOC                                                 ZEV00109
      GOTO 1000                                                         ZEV00110
 55   NERROR=15                                                         ZEV00111
      X1=FEXP(ILAST)                                                    ZEV00112
      X2=FEXP(ILAST+1)                                                  ZEV00113
      GOTO 44                                                           ZEV00114
 57   NERROR=16                                                         ZEV00115
      GOTO 44                                                           ZEV00116
 53   FEXP(ILAST)=FEXP(ILAST)**FEXP(ILAST+1)                            ZEV00117
      GOTO 1000                                                         ZEV00118
 54   FEXP(ILAST)=FEXP(ILAST)**ILOC                                     ZEV00119
      GOTO 1000                                                         ZEV00120
 58   IF(FEXP(ILAST+1).EQ.0.) GOTO 51                                   ZEV00121
      FEXP(ILAST)=0.                                                    ZEV00122
      GOTO 1000                                                         ZEV00123
 51   FEXP(ILAST)=1.                                                    ZEV00124
      GOTO 1000                                                         ZEV00125
C                                                                       ZEV00126
 60   FEXP(ILAST)=-FEXP(ILAST)                                          ZEV00127
      GOTO 1000                                                         ZEV00128
C                                                                       ZEV00129
 70   ILOC=FEXP(ILAST)+1.5                                              ZEV00130
      FEXP(ILAST)=DATA(ILOC)                                            ZEV00131
      GOTO 1000                                                         ZEV00132
C                                                                       ZEV00133
 80   ILAST=ILAST-1                                                     ZEV00134
      IBEG=FEXP(ILAST)+.5                                               ZEV00135
      ITEM=FEXP(ILAST+1)+.5                                             ZEV00136
      LOC=IBEG+ITEM+1                                                   ZEV00137
      IUP=DATA(IBEG)+.5                                                 ZEV00138
      IF((LOC.GT.IBEG).AND.(LOC.LT.IUP)) GOTO 85                        ZEV00139
 801  DO 81 K=1,26                                                      ZEV00140
      LOCN=DATA(K)+.5                                                   ZEV00141
      IF(LOCN.EQ.IBEG) GOTO 82                                          ZEV00142
 81   CONTINUE                                                          ZEV00143
      K=37                                                              ZEV00144
 82   NERROR=17                                                         ZEV00145
      I1=ITEM                                                           ZEV00146
      X1=ALPH(K)                                                        ZEV00147
 44   NSTOP=1                                                           ZEV00148
      CALL EXERR(NERROR,I1,I2,X1,X2)                                    ZEV00149
      RETURN                                                            ZEV00150
C                                                                       ZEV00151
 85   FEXP(ILAST)=DATA(LOC)                                             ZEV00152
      GOTO 1000                                                         ZEV00153
C                                                                       ZEV00154
 90   FEXP(ILAST)=SIN(FEXP(ILAST))                                      ZEV00155
      GOTO 1000                                                         ZEV00156
C                                                                       ZEV00157
 100  FEXP(ILAST)=COS(FEXP(ILAST))                                      ZEV00158
      GOTO 1000                                                         ZEV00159
C                                                                       ZEV00160
 110  IF(COS(FEXP(ILAST)).EQ.0.) GOTO 115                               ZEV00161
      FEXP(ILAST)=SIN(FEXP(ILAST))/COS(FEXP(ILAST))                     ZEV00162
      GOTO 1000                                                         ZEV00163
 115  NERROR=38                                                         ZEV00164
      GOTO 44                                                           ZEV00165
C                                                                       ZEV00166
 120  FEXP(ILAST)=ATAN(FEXP(ILAST))                                     ZEV00167
      GOTO 1000                                                         ZEV00168
C                                                                       ZEV00169
 130  FEXP(ILAST)=EXP(FEXP(ILAST))                                      ZEV00170
      GOTO 1000                                                         ZEV00171
C                                                                       ZEV00172
 140  FEXP(ILAST)=ABS(FEXP(ILAST))                                      ZEV00173
      GOTO 1000                                                         ZEV00174
C                                                                       ZEV00175
 150  IF(FEXP(ILAST).GT.0.) GOTO 151                                    ZEV00176
      NERROR=18                                                         ZEV00177
      IF(FEXP(ILAST).LT.0.) NERROR=19                                   ZEV00178
      GOTO 44                                                           ZEV00179
 151  FEXP(ILAST)=ALOG(FEXP(ILAST))                                     ZEV00180
      GOTO 1000                                                         ZEV00181
C                                                                       ZEV00182
 160  IF(FEXP(ILAST).GE.0.) GOTO 161                                    ZEV00183
      NERROR=20                                                         ZEV00184
      GOTO 44                                                           ZEV00185
 161  FEXP(ILAST)=SQRT(FEXP(ILAST))                                     ZEV00186
      GOTO 1000                                                         ZEV00187
C                                                                       ZEV00188
 170  IF(FEXP(ILAST).LT.0.) FEXP(ILAST)=FEXP(ILAST)-1.                  ZEV00189
      FEXP(ILAST)=AINT(FEXP(ILAST))                                     ZEV00190
      GOTO 1000                                                         ZEV00191
C                                                                       ZEV00192
C*****RANDOM NUMBER CALL                                                ZEV00193
 180  CALL RANDOM_NUMBER(FEXP(ILAST))                                   ZEV00194
      GOTO 1000                                                         ZEV00195
C                                                                       ZEV00196
C******INSERT ACCUMULATOR IN SUBSCRIPTED VARIABLE                       ZEV00197
 190  ILAST=ILAST-1                                                     ZEV00198
      IBEG=FEXP(ILAST)+.5                                               ZEV00199
      ITEM=FEXP(ILAST+1)+.5                                             ZEV00200
      LOC=IBEG+ITEM+1                                                   ZEV00201
      IUP=DATA(IBEG)+.5                                                 ZEV00202
      IF((LOC.GT.IBEG).AND.(LOC.LT.IUP)) GOTO 195                       ZEV00203
      GOTO 801                                                          ZEV00204
 195  DATA(LOC)=ACC                                                     ZEV00205
      GOTO 1000                                                         ZEV00206
C                                                                       ZEV00207
C*****IEXP(I) CONTAINS THE ADDRESS OF AN OPERAND                        ZEV00208
C*****INSERT VALUE IN NEXT AVAILABLE POSITION                           ZEV00209
 900  ILAST=ILAST+1                                                     ZEV00210
      FEXP(ILAST)=DATA(IENTRY)                                          ZEV00211
C                                                                       ZEV00212
C*****STORE ALPHANUMERIC CODE OF CONSTANT INTO ACCUMULATOR              ZEV00213
      GOTO 1000                                                         ZEV00214
 200  FEXP(1)=IEXP(I+1)                                                 ZEV00215
      GOTO 1001                                                         ZEV00216
 1000 CONTINUE                                                          ZEV00217
C                                                                       ZEV00218
C*****INSERT RESULT IN ACCUMULATOR                                      ZEV00219
 1001 ACC=FEXP(1)                                                       ZEV00220
C                                                                       ZEV00221
      NSTOP=0                                                           ZEV00222
      RETURN                                                            ZEV00223
      END                                                               ZEV00224

 