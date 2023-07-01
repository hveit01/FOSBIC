      SUBROUTINE ZIMAGE(ICODE,NSTOP)                                    ZIM00001
C**** SUBROUTINE TO TRANSLATE IMAGE INTO CARP                           ZIM00002
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZIM00003
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, ZIM00004
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZIM00005
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZIM00006
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZIM00007
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZIM00008
     1NIFMAX,INTZEI                                                     ZIM00009
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZIM00010
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZIM00011
     1IRET(20),XXX(4),NFILE(25,3)                                       ZIM00012
      COMMON// ISTLST(340),LISTST(340)                                  ZIM00013
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZIM00015
      DIMENSION IPROG(3700)                                             ZIM00016
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZIM00017
C**** ICODE=1 NUMERIC VARIABLE OR EXPRESSION                            ZIM00018
C**** ICODE=2 ALPHANUMERIC CONSTANT                                     ZIM00019
C**** ICODE=3 ALPHANUMERIC VARIABLE                                     ZIM00020
      NSTEU=0                                                           ZIM00021
      INEXT=1                                                           ZIM00022
      NSTOP=0                                                           ZIM00023
      IDEC=0                                                            ZIM00024
      IBEG1=0                                                           ZIM00025
      IBEG2=0                                                           ZIM00026
      IEND1=0                                                           ZIM00027
      IEND2=0                                                           ZIM00028
      IF((NCARD.NE.1).OR.(NPRUS.NE.1)) GOTO 10                          ZIM00029
C**** CHECK TO SKIP LINES                                               ZIM00030
      IF((CARD(2).NE.DOPU).OR.(CARD(3).NE.DOPU)) GOTO 10                ZIM00031
      CALL ZDIGIT(CARD(1),IA)                                           ZIM00032
      NSKIP=IA-1                                                        ZIM00033
      NCARD=4                                                           ZIM00034
      IF(IA.LE.10) GOTO 15                                              ZIM00035
 20   NSKIP=1                                                           ZIM00036
 15   IF(NSKIP.LE.0) GOTO 20                                            ZIM00037
      DO 16 I=1,NSKIP                                                   ZIM00038
      WRITE(IWC,17)                                                     ZIM00039
 17   FORMAT(1H )                                                       ZIM00040
 16   CONTINUE                                                          ZIM00041
 10   IF(ICODE.EQ.9) GOTO 810                                           ZIM00042
      DO 30 I=NCARD,80                                                  ZIM00043
      IF(CARD(I).EQ.QUOTE) GOTO 100                                     ZIM00044
      IF(CARD(I).EQ.DOPU) GOTO 40                                       ZIM00045
      CARP(NPRUS)=CARD(I)                                               ZIM00046
      NPRUS=NPRUS+1                                                     ZIM00047
      IF(NPRUS.GT.IIMAGE) GOTO 2556                                     ZIM00048
 30   CONTINUE                                                          ZIM00049
 40   IF(NPRI.EQ.-1) GOTO 45                                            ZIM00050
      NX=2                                                              ZIM00051
      CALL PRILIN(NX)                                                   ZIM00052
 45   NCARD=1                                                           ZIM00053
      IF((CARD(2).EQ.DOPU).AND.(CARD(3).EQ.DOPU)) NCARD=4               ZIM00054
      GOTO 10                                                           ZIM00055
 100  IBEG1=I                                                           ZIM00056
      IF(I.EQ.1) GOTO 101                                               ZIM00057
      IF(CARD(I-1).EQ.DECMAL) IDEC=I-1                                  ZIM00058
 101  DO 110 I=IBEG1,80                                                 ZIM00059
      IF(CARD(I).EQ.DECMAL) GOTO 115                                    ZIM00060
      IF(CARD(I).EQ.EXSIGN) GOTO 120                                    ZIM00061
      IF(CARD(I).NE.QUOTE) GOTO 130                                     ZIM00062
      GOTO 110                                                          ZIM00063
 115  IDEC=I                                                            ZIM00064
 110  CONTINUE                                                          ZIM00065
      I=81                                                              ZIM00066
 130  IEND1=I-1                                                         ZIM00067
      IF(IDEC.EQ.0) GOTO 200                                            ZIM00068
      GOTO 300                                                          ZIM00069
 120  IEND1=I-1                                                         ZIM00070
      IBEG2=I                                                           ZIM00071
      DO 135 I=IBEG2,80                                                 ZIM00072
      IF(CARD(I).NE.EXSIGN) GOTO 140                                    ZIM00073
 135  CONTINUE                                                          ZIM00074
      I=81                                                              ZIM00075
 140  IEND2=I-1                                                         ZIM00076
      GOTO 400                                                          ZIM00077
C**** I-FORMAT FOUND (INTEGER)                                          ZIM00078
 200  IF(ICODE.GE.2) GOTO 2000                                          ZIM00079
      IFIR=0                                                            ZIM00080
 201  IF(ACC.LT.FLOAT(INTMAX)) GOTO 2011                                ZIM00081
      IF(NSTEU.EQ.1) GOTO 218                                           ZIM00082
      IF(IEND1-4.LE.IBEG1) GOTO 350                                     ZIM00083
      IEND2=IEND1                                                       ZIM00084
      IEND1=IEND1-5                                                     ZIM00085
      IBEG2=IEND1+1                                                     ZIM00086
 218  NSTEU=2                                                           ZIM00087
      NEXPO=0                                                           ZIM00088
 217  IF(ACC.LT.FLOAT(INTMAX)) GOTO 2011                                ZIM00089
      NEXPO=NEXPO+1                                                     ZIM00090
      ACC=ACC/10.                                                       ZIM00091
      GOTO 217                                                          ZIM00092
 2011 INPRI=ACC                                                         ZIM00093
      ISGN=0                                                            ZIM00094
      IF(INPRI.GE.0) GOTO 208                                           ZIM00095
      ISGN=1                                                            ZIM00096
      INPRI=-INPRI                                                      ZIM00097
 208  IF(NSTEU.GE.1) GOTO 205                                           ZIM00098
      CALL ZNUMB(IBEG1,IEND1,INPRI,ISGN,IFIR)                           ZIM00099
      GOTO 500                                                          ZIM00100
C**** F-FORMAT FOUND (REAL)                                             ZIM00101
 300  IF(ICODE.GE.2) GOTO 2000                                          ZIM00102
      IF(IEND1.NE.IDEC) GOTO 301                                        ZIM00103
      IEND1=IDEC-1                                                      ZIM00104
      GOTO 200                                                          ZIM00105
 301  ISGN=0                                                            ZIM00106
      IF(ACC.GE.0.) GOTO 310                                            ZIM00107
      ISGN=1                                                            ZIM00108
      ACC=-ACC                                                          ZIM00109
 310  INPRI=ACC                                                         ZIM00110
      IF(INPRI.EQ.0) GOTO 330                                           ZIM00111
      IF(IDEC.LT.IBEG1) GOTO 350                                        ZIM00112
 340  IFIR=0                                                            ZIM00113
      CALL ZNUMB(IBEG1,IDEC-1,INPRI,ISGN,IFIR)                          ZIM00114
      IF(IBEG1.EQ.-1) GOTO 2555                                         ZIM00115
      GOTO 320                                                          ZIM00116
 330  IF((ISGN.EQ.1).AND.(IDEC.LE.IBEG1)) GOTO 360                      ZIM00117
      IF((IDEC.LE.IBEG1).AND.(ISGN.EQ.0)) GOTO 345                      ZIM00118
      GOTO 340                                                          ZIM00119
 320  CARP(NPRUS)=DECMAL                                                ZIM00120
      NPRUS=NPRUS+1                                                     ZIM00121
 345  ISGN=0                                                            ZIM00122
      INPRI=(ACC-INT(ACC))*10**(IEND1-IDEC)                             ZIM00123
      IFIR=1                                                            ZIM00124
      CALL ZNUMB(IDEC+1,IEND1,INPRI,ISGN,IFIR)                          ZIM00125
      GOTO 500                                                          ZIM00126
 350  ISGN=2                                                            ZIM00127
      CALL ZNUMB(IBEG1,IEND1,INPRI,ISGN,IFIR)                           ZIM00128
      GOTO 500                                                          ZIM00129
 360  CARP(NPRUS-1)=CMINUS                                              ZIM00130
      CARP(NPRUS)=DECMAL                                                ZIM00131
      NPRUS=NPRUS+1                                                     ZIM00132
      IDEC=IDEC+1                                                       ZIM00133
      IF(NSTEU.EQ.1) GOTO 1005                                          ZIM00134
      GOTO 345                                                          ZIM00135
C**** TRANSLATE E-FORMAT                                                ZIM00136
400   IF(ICODE.GE.2) GOTO 2000                                          ZIM00137
      IFIR=1                                                            ZIM00138
      NSTEU=1                                                           ZIM00139
      NEXPO=0                                                           ZIM00140
      ISGN=0                                                            ZIM00141
      IF(IDEC.NE.0) GOTO 1000                                           ZIM00142
C**** INTEGER WITH EXPONENT                                             ZIM00143
      GOTO 201                                                          ZIM00144
 205  MEM=IEND1-IBEG1+1                                                 ZIM00145
      IF(ISGN.EQ.1) MEM=MEM-1                                           ZIM00146
      IF(MEM.LE.INTNUM) GOTO 405                                        ZIM00147
      MEM=INTNUM                                                        ZIM00148
 405  IF(INPRI.GE.10**MEM) GOTO 410                                     ZIM00149
      IF(NSTEU.EQ.2) GOTO 4051                                          ZIM00150
      IF(INPRI.LT.10**(MEM-1)) GOTO 420                                 ZIM00151
 4051 CALL ZNUMB(IBEG1,IEND1,INPRI,ISGN,IFIR)                           ZIM00152
      GOTO 550                                                          ZIM00153
 410  NEXPO=NEXPO+1                                                     ZIM00154
      INPRI=INPRI/10                                                    ZIM00155
      GOTO 405                                                          ZIM00156
 420  NEXPO=NEXPO-1                                                     ZIM00157
      INPRI=INPRI*10                                                    ZIM00158
      GOTO 405                                                          ZIM00159
C**** F-FORMAT WITH EXPONENT                                            ZIM00160
 1000 IF(IDEC.GT.IBEG1) GOTO 1100                                       ZIM00161
      IF(ACC.GT.0.) GOTO 1005                                           ZIM00162
      GOTO 360                                                          ZIM00163
 1005 MEM=0                                                             ZIM00164
      GOTO 1205                                                         ZIM00165
 1100 IF(ACC.GE.0.) GOTO 1200                                           ZIM00166
      ISGN=1                                                            ZIM00167
      IFIR=0                                                            ZIM00168
      ACC=-ACC                                                          ZIM00169
 1200 MEM=IDEC-IBEG1                                                    ZIM00170
      IF(ISGN.EQ.1) MEM=MEM-1                                           ZIM00171
      IF(MEM.GT.0) GOTO 1205                                            ZIM00172
      CARP(NPRUS)=CMINUS                                                ZIM00173
      CARP(NPRUS+1)=DECMAL                                              ZIM00174
      NPRUS=NPRUS+2                                                     ZIM00175
 1205 IF(ACC.GE.FLOAT(10**MEM)) GOTO 1210                               ZIM00176
      IF(ACC.LT.FLOAT(10**(MEM-1))) GOTO 1220                           ZIM00177
      IF(MEM.EQ.0) GOTO 451                                             ZIM00178
      INPRI=ACC                                                         ZIM00179
      CALL ZNUMB(IBEG1,IDEC-1,INPRI,ISGN,IFIR)                          ZIM00180
      IF(IBEG1.EQ.-1) GOTO 2555                                         ZIM00181
      GOTO 450                                                          ZIM00182
 1210 NEXPO=NEXPO+1                                                     ZIM00183
      ACC=ACC/10.                                                       ZIM00184
      GOTO 1205                                                         ZIM00185
 1220 NEXPO=NEXPO-1                                                     ZIM00186
      ACC=ACC*10.                                                       ZIM00187
      GOTO 1205                                                         ZIM00188
 450  CARP(NPRUS)=DECMAL                                                ZIM00189
      NPRUS=NPRUS+1                                                     ZIM00190
 451  INPRI=(ACC-INT(ACC))*10**(IEND1-IDEC)                             ZIM00191
      ISGN=0                                                            ZIM00192
      IBEG1=IDEC+1                                                      ZIM00193
      CALL ZNUMB(IBEG1,IEND1,INPRI,ISGN,IFIR)                           ZIM00194
C**** TRANSLATE EXPONENT                                                ZIM00195
 550  CARP(NPRUS)=ALPH(5)                                               ZIM00196
      CARP(NPRUS+1)=PLUS                                                ZIM00197
      IF(NEXPO.GE.0) GOTO 555                                           ZIM00198
      CARP(NPRUS+1)=CMINUS                                              ZIM00199
      NEXPO=-NEXPO                                                      ZIM00200
 555  NPRUS=NPRUS+2                                                     ZIM00201
      ISGN=0                                                            ZIM00202
      IFIR=1                                                            ZIM00203
      IBEG1=IBEG2+2                                                     ZIM00204
      CALL ZNUMB(IBEG1,IEND2,NEXPO,ISGN,IFIR)                           ZIM00205
      IEND1=IEND2                                                       ZIM00206
      GOTO 500                                                          ZIM00207
C**** PRINT ALPHANUMERIC VARIABLE                                       ZIM00208
 2000 IF((IDEC.NE.0).OR.(IBEG2.NE.0)) GOTO 2010                         ZIM00209
      IF(ICODE.EQ.2) GOTO 3000                                          ZIM00210
      IX=ACC                                                            ZIM00211
 3020 MEM=IEND1-IBEG1+1                                                 ZIM00212
      DO 16400 I=1,5                                                    ZIM00213
      IZ=IX-((IX/48)*48)                                                ZIM00214
      IF(I.GT.MEM) GOTO 16401                                           ZIM00215
      IF(NPRUS.GT.IIMAGE) GOTO 2556                                     ZIM00216
      CARP(NPRUS)=ALPH(IZ+1)                                            ZIM00217
      NPRUS=NPRUS+1                                                     ZIM00218
      IX=IX/48                                                          ZIM00219
16400 CONTINUE                                                          ZIM00220
16401  IF(IDEC.GT.0) GOTO 3010                                          ZIM00221
      GOTO 500                                                          ZIM00222
 2010 IF(IEND2.NE.0) IEND1=IEND2                                        ZIM00223
      GOTO 350                                                          ZIM00224
C**** ALPHANUMERIC CONSTANT                                             ZIM00225
 3000 IDEC=IPROG(INREG)                                                 ZIM00226
      INREG=INREG-1                                                     ZIM00227
 3010 IX=IPROG(INREG)                                                   ZIM00228
      INREG=INREG-1                                                     ZIM00229
      IDEC=IDEC-1                                                       ZIM00230
      GOTO 3020                                                         ZIM00231
 500  NCARD=IEND1+1                                                     ZIM00232
      IF(IBEG1.EQ.-1) NSTOP=-1                                          ZIM00233
 810  DO 510 I=NCARD,80                                                 ZIM00234
      IF(CARD(I).EQ.QUOTE) GOTO 520                                     ZIM00235
      IF(CARD(I).EQ.DOPU) GOTO 525                                      ZIM00236
      IF(NPRUS.GT.IIMAGE) GOTO 2556                                     ZIM00237
      CARP(NPRUS)=CARD(I)                                               ZIM00238
      NPRUS=NPRUS+1                                                     ZIM00239
 510  CONTINUE                                                          ZIM00240
 520  NCARD=I                                                           ZIM00241
      IF(IPROG(INREG).EQ.-67) NPRI=1                                    ZIM00242
      RETURN                                                            ZIM00243
 525  NCARD=1                                                           ZIM00244
      IF((NPRI.EQ.-1).AND.(IPROG(INREG).EQ.-67)) GOTO 526               ZIM00245
      IF((NPRI.EQ.1).AND.(IPROG(INREG).EQ.-69)) RETURN                  ZIM00246
      IF(NPRI.EQ.-1) RETURN                                             ZIM00247
 526  NX=2                                                              ZIM00248
      CALL PRILIN(NX)                                                   ZIM00249
      RETURN                                                            ZIM00250
 2555 NSTOP=-1                                                          ZIM00251
      RETURN                                                            ZIM00252
 2556 NERROR=37                                                         ZIM00253
      CALL EXERR(NERROR,I1,I2,X1,X2)                                    ZIM00254
      NSTOP=-1                                                          ZIM00255
      RETURN                                                            ZIM00256
      END                                                               ZIM00257
