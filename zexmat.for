      SUBROUTINE ZEXMAT(IOP)                                            EXM00001
C**** SUBROUTINE TO EXECUTE MAT COMMANDS                                EXM00002
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      EXM00003
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, EXM00004
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  EXM00005
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, EXM00006
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     EXM00007
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     EXM00008
     1NIFMAX,INTZEI                                                     EXM00009
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        EXM00010
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      EXM00011
     1IRET(20),XXX(4),NFILE(25,3)                                       EXM00012
      COMMON// ISTLST(340),LISTST(340)                                  EXM00013
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               EXM00015
      COMMON/A/ DINV(12,12)                                             EXM00016
      DIMENSION IPROG(3700)                                             EXM00017
      EQUIVALENCE (DATA(1),IPROG(1))                                    EXM00018
      MOP=0                                                             EXM00019
      MIC=0                                                             EXM00020
      IF(IOP.EQ.49) GOTO 49000                                          EXM00021
      IF(IOP.GT.25) GOTO 10                                             EXM00022
      GOTO(23000,24000,25000,26000,10,28000,29000,10,31000,32000,33000, EXM00023
     134000,35000,36000,37000,38000,39000,40000,41000,42000,43000,10,10 EXM00024
     2,46000,47000),IOP                                                 EXM00025
 10   NERROR=11                                                         EXM00026
      GOTO 29008                                                        EXM00027
C***  COMMAND IS MAT A=B+C                                              EXM00028
23000 MOP=23                                                            EXM00029
      VALUE=1                                                           EXM00030
      GOTO 24001                                                        EXM00031
C**** COMMAND IS MAT A=B-C                                              EXM00032
25000  MOP=25                                                           EXM00033
      VALUE=-1                                                          EXM00034
      GOTO 24001                                                        EXM00035
C**** COMMAND IS MATRIX MULTIPLICATION                                  EXM00036
24000 MOP=24                                                            EXM00037
24001 IA=IPROG(INREG-1)                                                 EXM00038
      IB=IPROG(INREG-2)                                                 EXM00039
      IC=IPROG(INREG-3)                                                 EXM00040
      LOCS=DATA(IB)+0.1                                                 EXM00041
      LOCU=DATA(LOCS)+0.1                                               EXM00042
      JB=DATA(IB+27)-1.                                                 EXM00043
      IF(MERKER(IB,2).EQ.0) GOTO 24110                                  EXM00044
      IZ=(LOCU-LOCS-1)/(JB+1)-1                                         EXM00045
      GOTO 24010                                                        EXM00046
24110 IZ=1                                                              EXM00047
      JB=LOCU-LOCS-2                                                    EXM00048
24010 IF(IA.GT.26) GOTO 24017                                           EXM00049
      IF(MERKER(IA,1).NE.0) GOTO 24015                                  EXM00050
24017 JA=1                                                              EXM00051
      GOTO 24016                                                        EXM00052
24015 LOCS=DATA(IA)+0.1                                                 EXM00053
      LOCU=DATA(LOCS)+0.1                                               EXM00054
      JA=DATA(IA+27)-1.                                                 EXM00055
      IF(MERKER(IA,2).EQ.0) GOTO 24111                                  EXM00056
      MIV=(LOCU-LOCS-1)/(JA+1)-1                                        EXM00057
      GOTO 24011                                                        EXM00058
24111 JA=LOCU-LOCS-2                                                    EXM00059
24016 MIV=1                                                             EXM00060
24011 IS= DATA(IC+27)-1.                                                EXM00061
      LOCS=DATA(IC)+0.1                                                 EXM00062
      LOCU=DATA(LOCS)+0.1                                               EXM00063
      JC=IS                                                             EXM00064
      IF(MERKER(IC,2).EQ.0) GOTO 24211                                  EXM00065
      ISS=(LOCU-LOCS-1)/(JC+1)-1                                        EXM00066
      GOTO 170                                                          EXM00067
24211 JC=LOCU-LOCS-2                                                    EXM00068
      IS=JC                                                             EXM00069
      ISS=1                                                             EXM00070
 170  IAPLUS=1                                                          EXM00071
      IBPLUS=1                                                          EXM00072
      ICPLUS=1                                                          EXM00073
      IF(MERKER(IA,2).EQ.0) IAPLUS=-JA                                  EXM00074
      IF(MERKER(IB,2).EQ.0) IBPLUS=-JB                                  EXM00075
      IF(MERKER(IC,2).EQ.0) ICPLUS=-JC                                  EXM00076
      IF(MOP.EQ.24) GOTO 24315                                          EXM00077
      IF((JA.NE.JB).OR.(JA.NE.JC)) GOTO 43300                           EXM00078
      IF((MIV.NE.IZ).OR.(MIV.NE.ISS)) GOTO 43300                        EXM00079
      GOTO 24325                                                        EXM00080
24315 IF(JB.NE.ISS) GOTO 24260                                          EXM00081
      IF(JA.NE.JC) GOTO 24260                                           EXM00082
      IF(MIV.NE.IZ) GOTO 24260                                          EXM00083
24325 LOCS=JA+1                                                         EXM00084
      LOCU=JB+1                                                         EXM00085
      MIV=JC+1                                                          EXM00086
      IF((MOP.EQ.24).AND.(IA.EQ.IC)) GOTO 24800                         EXM00087
      DO 24100 IN=1,IZ                                                  EXM00088
      DO 24200 IM=1,IS                                                  EXM00089
      MIC=IN                                                            EXM00090
      IF(IA.NE.IB) GOTO 24317                                           EXM00091
      IF(MOP.NE.24) GOTO 24317                                          ***
      IF(MERKER(IA,2).EQ.0) GOTO 24347                                  ***
      MIC=MIC-1                                                         ***
24317 JA=INT(DATA(IA))+IAPLUS+LOCS*MIC+IM                               EXM00093
      IF(MOP.EQ.24) GOTO 24305                                          EXM00094
      JB=INT(DATA(IB))+IBPLUS+LOCU*IN+IM                                EXM00095
      JC=INT(DATA(IC))+ICPLUS+MIV*IN+IM                                 EXM00096
      DATA(JA)=DATA(JB)+VALUE*DATA(JC)                                  EXM00097
      GOTO 24200                                                        EXM00098
24305 IF(IA.GT.26) GOTO 24307                                           EXM00099
      IF(MERKER(IA,1).NE.0) GOTO 24308                                  ***
      JA=DATA(IA)+1.5                                                   EXM00101
      GOTO 24308                                                        EXM00102
24307 JA=IA                                                             EXM00103
24308 ACC=0.                                                            EXM00106
      DO 24300 IO=1,ISS                                                 EXM00107
      JB=INT(DATA(IB))+IBPLUS+LOCU*IN+IO                                EXM00108
      JC=INT(DATA(IC))+ICPLUS+MIV*IO+IM                                 EXM00109
      ACC=ACC+DATA(JB)*DATA(JC)                                         EXM00110
24300 CONTINUE                                                          EXM00111
      DATA(JA)=ACC                                                      EXM00112
24200 CONTINUE                                                          EXM00113
24100 CONTINUE                                                          EXM00114
      INREG=INREG-4                                                     EXM00115
      IF(MOP.NE.24) GOTO 900                                            EXM00116
      IF(IA.NE.IB) GOTO 900                                             EXM00117
      IF(IA.GT.26) GOTO 900                                             EXM00118
      IF(MERKER(IA,1).EQ.0) GOTO 900                                    EXM00119
      DO 24410 IN=1,IZ                                                  EXM00120
      DO 24420 IM=1,IS                                                  EXM00121
      MIC=IZ-IN+1                                                       EXM00122
      JA=INT(DATA(IA))+IAPLUS+LOCS*MIC+IM                               EXM00123
      JB=INT(DATA(IA))+IAPLUS+LOCS*(MIC-1)+IM                           EXM00124
      DATA(JA)=DATA(JB)                                                 EXM00126
24420 CONTINUE                                                          EXM00127
24410 CONTINUE                                                          EXM00128
      GOTO 900                                                          EXM00129
24800 IF(MERKER(IA,2).EQ.0) GOTO 24260                                  EXM00130
      DO 24801 IM=1,IS                                                  EXM00131
      DO 24802 IN=1,IZ                                                  EXM00132
      JA=INT(DATA(IA))+IAPLUS+LOCS*IN+IM-1                              EXM00133
      ACC=0.                                                            ***
      DO 24803 IO=1,ISS                                                 EXM00134
      JB=INT(DATA(IB))+IBPLUS+LOCU*IN+IO                                EXM00135
      JC=INT(DATA(IC))+ICPLUS+MIV*IO+IM                                 EXM00136
      ACC=ACC+DATA(JB)*DATA(JC)                                         EXM00137
24803 CONTINUE                                                          EXM00138
      DATA(JA)=ACC                                                      EXM00139
24802 CONTINUE                                                          EXM00140
24801 CONTINUE                                                          EXM00141
      DO 24804 IM=1,IS                                                  EXM00142
      DO 24805 IN=1,IZ                                                  EXM00143
      MIV=IS-IM+1                                                       EXM00144
      JA=INT(DATA(IA))+IAPLUS+LOCS*IN+MIV                               ***
      JB=JA-1                                                           EXM00146
      DATA(JA)=DATA(JB)                                                 EXM00147
24805 CONTINUE                                                          EXM00148
24804 CONTINUE                                                          EXM00149
      INREG=INREG-4                                                     EXM00150
      GOTO 900                                                          EXM00151
24260 NX=3                                                              EXM00152
      CALL PRILIN(NX)                                                   EXM00153
      WRITE(IWC,24290) ALPH(IA),MIV,JA,ALPH(IB),IZ,JB,ALPH(IC),ISS,JC   EXM00154
24290 FORMAT(1H0,79HPROGRAM STOPPED -- SIZE ERROR IN SUBSCRIPTS OF THE VEXM00155
     1ARIABLES *  ROWS  * COLUMNS/57X,A1,2I10/57X,A1,2I10/57X,A1,2I10)  EXM00156
      IF(MOP.NE.24) GOTO 29008                                          EXM00157
      NERROR=30                                                         EXM00158
      GOTO 29008                                                        EXM00159
24347 NERROR=51                                                         ***
      I1=IA                                                             ***
      I2=IC                                                             ***
      GOTO 29008                                                        ***
C**** TRANSPONATION AF A MATRIX                                         EXM00160
26000 IA=IPROG(INREG-1)                                                 EXM00161
      IB=IPROG(INREG-2)                                                 EXM00162
      IZ=DATA(IA+27)-1.                                                 EXM00163
      IAPLUS=1                                                          EXM00164
      LOCS=DATA(IA)+0.1                                                 EXM00165
      LOCU=DATA(LOCS)+0.1                                               EXM00166
      IF(MERKER(IA,2).EQ.0) GOTO 26111                                  EXM00167
      IS=(LOCU-LOCS-1)/(IZ+1)-1                                         EXM00168
      MIC=IZ                                                            EXM00169
      GOTO 26011                                                        EXM00170
26111 IS=1                                                              EXM00171
      MIC=1                                                             EXM00172
      IAPLUS=-1                                                         EXM00173
      IZ=LOCU-LOCS-2                                                    EXM00174
26011 ISS=DATA(IB+27)-1.                                                EXM00175
      IBPLUS=1                                                          EXM00176
      LOCS=DATA(IB)+0.1                                                 EXM00177
      LOCU=DATA(LOCS)+0.1                                               EXM00178
      IF(MERKER(IB,2).EQ.0) GOTO 26112                                  EXM00179
      IC=(LOCU-LOCS-1)/(ISS+1)-1                                        EXM00180
      MIV=ISS                                                           EXM00181
      GOTO 26012                                                        EXM00182
26112 IBPLUS=-1                                                         EXM00183
      IC=1                                                              EXM00184
      MIV=1                                                             EXM00185
      ISS=LOCU-LOCS-2                                                   EXM00186
26012 IF(IZ.NE.IC) GOTO 26003                                           EXM00187
      IF(IS.NE.ISS) GOTO 26003                                          EXM00188
      DO 26002 J=1,IS                                                   EXM00189
      DO 26001 I=1,IZ                                                   EXM00190
      JA=INT(DATA(IA))+I+J*(MIC+1)+IAPLUS                               EXM00191
      JB=INT(DATA(IB))+J+I*(MIV+1)+IBPLUS                               EXM00192
      IF(IA.NE.IB) GOTO 26005                                           EXM00193
      IF(I.LE.J) GOTO 26001                                             EXM00194
      ACC=DATA(JA)                                                      EXM00195
      DATA(JA)=DATA(JB)                                                 EXM00196
      DATA(JB)=ACC                                                      EXM00197
      GOTO 26001                                                        EXM00198
26005 DATA(JA)=DATA(JB)                                                 EXM00199
26001 CONTINUE                                                          EXM00200
26002 CONTINUE                                                          EXM00201
      INREG=INREG-3                                                     EXM00202
      GOTO 900                                                          EXM00203
C                                                                       EXM00204
26003 WRITE(IWC,24290)ALPH(IA),IS,IZ,ALPH(IB),IC,ISS                    EXM00205
      NERROR=31                                                         EXM00206
      GOTO 29008                                                        EXM00207
C                                                                       EXM00208
C**** MAT READ NO REDIMENSION                                           EXM00209
28000 NSW=1                                                             EXM00210
28100 IA=IPROG(INREG-1)                                                 EXM00211
29003 IZ=DATA(IA+27)-1.                                                 EXM00212
      LOCS=DATA(IA)+0.1                                                 EXM00213
      LOCU=DATA(LOCS)+0.1                                               EXM00214
      ISS=IZ                                                            EXM00215
      IAPLUS=1                                                          EXM00216
      IF(MERKER(IA,2).EQ.0) GOTO 28104                                  EXM00217
      IS=(LOCU-LOCS-1)/(IZ+1)-1                                         EXM00218
      GOTO 28004                                                        EXM00219
28104 IZ=LOCU-LOCS-2                                                    EXM00220
28006 ISS=1                                                             EXM00221
      IS=1                                                              EXM00222
      IAPLUS=-1                                                         EXM00223
28004 DO 28002 IN=1,IS                                                  EXM00224
      DO 28003 IM=1,IZ                                                  EXM00225
      IF((MOP.LE.-35).AND.(MOP.GE.-41)) GOTO 28005                      EXM00226
      IFR=1                                                             EXM00227
      CALL ZHOPPR(VALUE,NSTOP,IFR,NSW)                                  EXM00228
      IF(NSTOP.NE.0) GOTO 22400                                         EXM00229
28005 JA=INT(DATA(IA))+IAPLUS+(ISS+1)*IN+IM                             EXM00230
      DATA(JA)=VALUE                                                    EXM00231
28003 CONTINUE                                                          EXM00232
28002 CONTINUE                                                          EXM00233
      INREG=INREG-2                                                     EXM00234
      IF(MOP.EQ.-35) GOTO 35010                                         EXM00235
      GOTO 900                                                          EXM00236
C**** REDIMENSION                                                       ***
29100 IA=IPROG(INREG-1)                                                 EXM00238
      LOCS=DATA(IA)+0.1                                                 EXM00239
      IF(XXX(3).LT.1.) GOTO 29005                                       ***
      IF(MERKER(IA,2).EQ.0) GOTO 29001                                  EXM00240
      IF(XXX(4).LT.1.) GOTO 29004                                       EXM00241
      DATA(IA+27)=XXX(4)+1.01                                           EXM00243
      DATA(LOCS)=(INT(XXX(3))+1)*(INT(XXX(4))+1)+LOCS+1                 EXM00244
      GOTO 29002                                                        EXM00245
29001 DATA(LOCS)=INT(XXX(3))+LOCS+2                                     ***
29002 NSIZE=(MERKER(IA,1)+1)*(MERKER(IA,2)+1)+1                         EXM00248
      LOCU=INT(DATA(LOCS))-LOCS                                         EXM00249
      IF(LOCU.GT.NSIZE) GOTO 29010                                      EXM00250
      XXX(3)=0.                                                         EXM00251
      XXX(4)=0.                                                         EXM00252
      GOTO 29003                                                        EXM00253
29004 NERROR=32                                                         EXM00254
      X2=XXX(4)                                                         EXM00255
      GOTO 29009                                                        EXM00256
29005 NERROR=33                                                         EXM00257
      IF(MERKER(IA,2).EQ.0) NERROR=32                                   EXM00258
      X2=XXX(3)                                                         EXM00259
      GOTO 29009                                                        EXM00260
29010 X2=(MERKER(IA,1)+1)*(MERKER(IA,2)+1)                              EXM00261
      IF(MERKER(IA,2).NE.0) GOTO 29110                                  EXM00262
      I1=1                                                              EXM00263
      I2=XXX(3)                                                         EXM00264
      X2=MERKER(IA,1)+1                                                 EXM00265
      GOTO 29210                                                        EXM00266
29110 I1=XXX(3)                                                         EXM00267
      I2=XXX(4)                                                         EXM00268
29210 NERROR=34                                                         EXM00269
29009 X1=ALPH(IA)                                                       EXM00270
      GOTO 29008                                                        EXM00271
C*** MAT READ WITH ACTUAL SIZE                                          EXM00272
29000 NSW=1                                                             EXM00273
29150 IA=IPROG(INREG-1)                                                 EXM00274
      LOCS=DATA(IA)+0.1                                                 EXM00275
      LOCU=DATA(LOCS)+0.1                                               EXM00276
      IF(XXX(3).LT.1.) GOTO 29005                                       ***
      IF(MERKER(IA,2).EQ.0) GOTO 29200                                  EXM00277
      IF(XXX(4).LT.1.) GOTO 29004                                       EXM00278
      IZ=DATA(IA+27)-1.                                                 EXM00280
      IS=(LOCU-LOCS-1)/(IZ+1)-1                                         EXM00281
      IF(XXX(4).GT.FLOAT(IZ)) GOTO 29100                                EXM00282
      IF(XXX(3).GT.FLOAT(IS)) GOTO 29100                                EXM00283
      IS=XXX(3)                                                         EXM00284
      IAPLUS=1                                                          EXM00285
      ISS=IZ                                                            EXM00286
      IZ=XXX(4)                                                         EXM00287
      GOTO 28004                                                        EXM00288
29200 IZ=XXX(3)                                                         ***
      IF(IZ.GT.(LOCU-LOCS-2)) GOTO 29100                                EXM00291
      GOTO 28006                                                        EXM00292
C                                                                       EXM00293
C**** PRINT WITHOUT REDIMENSION  -- NUMERIC                             EXM00294
31000 IA=IPROG(INREG-1)                                                 EXM00295
      NX=2                                                              EXM00296
      CALL PRILIN(NX)                                                   EXM00297
      IS=DATA(IA+27)-1.                                                 EXM00298
      LOCS=DATA(IA)+0.1                                                 EXM00299
      LOCU=DATA(LOCS)+0.1                                               EXM00300
      IF(MERKER(IA,2).EQ.0) GOTO 31101                                  EXM00301
      IZ=(LOCU-LOCS-1)/(IS+1)-1                                         EXM00302
      ISS=IS                                                            EXM00303
      IAPLUS=1                                                          EXM00304
      IF(MIC.NE.32) GOTO 31004                                          EXM00305
      IF(XXX(3).LT.1.) GOTO 29005                                       ***
      IF(XXX(4).LT.1.) GOTO 29004                                       ***
      IF(XXX(4).GT.FLOAT(IS)) GOTO 29100                                ***
      IF(XXX(3).GT.FLOAT(IZ)) GOTO 29100                                ***
      IZ=XXX(3)                                                         EXM00306
      IS=XXX(4)                                                         EXM00307
      GOTO 31004                                                        EXM00308
31101 IS=LOCU-LOCS-2                                                    EXM00309
      ISS=1                                                             EXM00310
      IZ=1                                                              EXM00311
      IAPLUS=-1                                                         EXM00312
      IF(MIC.NE.32) GOTO 31004                                          EXM00313
      IF(XXX(3).LT.1.) GOTO 29005                                       ***
      IF(XXX(3).GT.FLOAT(IS)) GOTO 29100                                ***
      IS=XXX(3)                                                         EXM00314
31004 DO 31002 IN=1,IZ                                                  EXM00315
      IF(NSTZEI.EQ.3) GOTO 31027                                        EXM00316
      IF(MERKER(IA,2).EQ.0) WRITE(IWC,31020) ALPH(IA)                   EXM00317
      IF(MERKER(IA,2).NE.0) WRITE(IWC,31007)ALPH(IA),IN                 EXM00318
31020 FORMAT(5X,8HVECTOR *,A1,1H*)                                      EXM00319
31007 FORMAT(5X,8HMATRIX *,A1,9H* ZEILE *,I4,1H*)                       EXM00320
31027 DO 31003 IM=1,IS                                                  EXM00321
      JA=INT(DATA(IA))+IAPLUS+(ISS+1)*IN+IM                             EXM00322
      IF(MOP.EQ.-33) GOTO 31008                                         EXM00323
      ACC=DATA(JA)                                                      EXM00324
      INEXT=INEXT+IZONE                                                 EXM00325
      CALL ZINSNO                                                       EXM00326
      IF(IM.EQ.IS) GOTO 31006                                           EXM00327
      IF(INEXT.LT.IWRIT) GOTO 31003                                     EXM00328
      GOTO 31006                                                        EXM00329
31008 IX=DATA(JA)                                                       EXM00330
      IPOS=INEXT                                                        EXM00331
      DO 31015 K=1,5                                                    EXM00332
      JB=IX-((IX/INTZEI)*INTZEI)                                        EXM00333
      CARP(IPOS)=ALPH(JB+1)                                             ***
      IPOS=IPOS+1                                                       ***
31015 IX=IX/INTZEI                                                      EXM00336
      IF(NPRI.EQ.1) INEXT=INEXT+IZONE                                   EXM00337
      IF(NPRI.EQ.-1) INEXT=INEXT+5                                      EXM00338
      IF(INEXT.GE.IWRIT) GOTO 31006                                     EXM00339
      GOTO 31003                                                        EXM00340
31006 NX=1                                                              EXM00341
      CALL PRILIN(NX)                                                   EXM00342
      INEXT=1                                                           EXM00343
      NPRUS=1                                                           EXM00344
      IPOS=0                                                            EXM00345
31003 CONTINUE                                                          EXM00346
31002 CONTINUE                                                          EXM00347
      INREG=INREG-2                                                     EXM00348
      NPRI=1                                                            ***
      IF((INEXT.EQ.1).AND.(NPRUS.EQ.1)) GOTO 900                        ***
      NX=1                                                              EXM00349
      CALL PRILIN(NX)                                                   EXM00350
      GOTO 900                                                          EXM00353
C**** PRINT WITH REDIMENSION -- NUMERIC                                 EXM00354
32000 MOP=-32                                                           EXM00355
      MIC=32                                                            EXM00356
      GOTO 31000                                                        EXM00357
C**** PRINT WITHOUT REDIMENSION --ALPHANUMERIC                          EXM00358
33000 MOP=-33                                                           EXM00359
      GOTO 31000                                                        EXM00360
C**** PRINT WITH REDIMENSION -- ALPHANUMERIC                            EXM00361
34000 MIC=32                                                            EXM00362
      MOP=-33                                                           EXM00363
      GOTO 31000                                                        EXM00364
C                                                                       EXM00365
C**** IDENTITY MATRIX                                                   EXM00366
C**** NO REDIMENSION                                                    EXM00367
35000 IA=IPROG(INREG-1)                                                 EXM00368
      MOP=-35                                                           EXM00369
      VALUE=0.                                                          EXM00370
      GOTO 29003                                                        EXM00371
35010 IF(IS.EQ.IZ) GOTO 35012                                           EXM00372
      NERROR=35                                                         EXM00373
      X1=ALPH(IA)                                                       EXM00374
      I1=IS                                                             EXM00375
      I2=IZ                                                             EXM00376
      GOTO 29009                                                        EXM00377
35012 DO 35011 IN=1,IS                                                  EXM00378
      JA=INT(DATA(IA))+1+IS*IN+2*IN                                     EXM00379
      DATA(JA)=1.                                                       EXM00380
35011 CONTINUE                                                          EXM00381
      GOTO 900                                                          EXM00382
C**** IDENTITY MATRIX WITH REDIMENSION                                  EXM00383
36000 MOP=-35                                                           EXM00384
      VALUE=0.                                                          EXM00385
      GOTO 29100                                                        EXM00386
C**** MATRIX=1 NO REDIMENSION                                           EXM00387
37000 IA=IPROG(INREG-1)                                                 EXM00388
      VALUE=1.                                                          EXM00389
      MOP=-37                                                           EXM00390
      GOTO 29003                                                        EXM00391
C**** MATRIX=1 WITH REDIMENSION                                         EXM00392
38000 MOP=-38                                                           EXM00393
      VALUE=1.                                                          EXM00394
      GOTO 29100                                                        EXM00395
C**** MATRIX=0 NO REDIMENSION                                           EXM00396
39000 IA=IPROG(INREG-1)                                                 EXM00397
      VALUE=0.                                                          EXM00398
      MOP=-39                                                           EXM00399
      GOTO 29003                                                        EXM00400
C**** MATRIX=0 WITH REDIMENSION                                         EXM00401
40000 VALUE=0.                                                          EXM00402
      MOP=-40                                                           EXM00403
      GOTO 29100                                                        EXM00404
C**** A=EXPRESSION -- MAT LET COMMAND                                   EXM00405
41000 VALUE=ACC                                                         EXM00406
      IA=IPROG(INREG-1)                                                 EXM00407
      MOP=-41                                                           EXM00408
      GOTO 29003                                                        EXM00409
C**** A(I,J)=B(I,J) -- MAT LET COMMAND                                  EXM00410
42000 IB=IPROG(INREG-1)                                                 EXM00411
      IA=IPROG(INREG-2)                                                 EXM00412
      MOP=4                                                             EXM00413
      VALUE=1.                                                          EXM00414
      INREG=INREG-3                                                     EXM00415
      GOTO 42500                                                        EXM00416
C**** A(I,J)=B(I,J) -BINARY OPERATOR)- (EXPRESSION) *MAT LET COMMAND    EXM00417
43000 MOP=IPROG(INREG-1)                                                EXM00418
      IOP=1                                                             EXM00419
      IF(IPROG(INREG-2).NE.-43) GOTO 43001                              EXM00420
      IOP=-1                                                            EXM00421
      INREG=INREG-1                                                     EXM00422
43001 IA=IPROG(INREG-2)                                                 EXM00423
      IB=IPROG(INREG-3)                                                 EXM00424
      INREG=INREG-4                                                     EXM00425
      VALUE=ACC                                                         EXM00426
42500 IZ=DATA(IA+27)-1.                                                 EXM00427
      LOCS=DATA(IA)                                                     EXM00428
      LOCU=DATA(LOCS)                                                   EXM00429
      IAPLUS=1                                                          EXM00430
      ISS=IZ                                                            EXM00431
      IF(MERKER(IA,2).EQ.0) GOTO 42501                                  EXM00432
      IS=(LOCU-LOCS-1)/(IZ+1)-1                                         EXM00433
      GOTO 42502                                                        EXM00434
42501 IZ=LOCU-LOCS-2                                                    EXM00435
      IAPLUS=-1                                                         EXM00436
      ISS=1                                                             EXM00437
      IS=ISS                                                            EXM00438
42502 IC=DATA(IB+27)-1.                                                 EXM00439
      IBPLUS=1                                                          EXM00440
      LOCS=DATA(IB)                                                     EXM00441
      LOCU=DATA(LOCS)                                                   EXM00442
      IF(MERKER(IB,2).EQ.0) GOTO 42503                                  EXM00443
      JC=(LOCU-LOCS-1)/(IC+1)-1                                         EXM00444
      IF(IZ.GT.IC) IZ=IC                                                EXM00445
      IF(IS.GT.JC) IS=JC                                                EXM00446
      GOTO 43200                                                        EXM00447
42503 JC=LOCU-LOCS-2                                                    EXM00448
      IBPLUS=-1                                                         EXM00449
      IC=1                                                              EXM00450
      IF(IZ.GT.JC) IZ=JC                                                EXM00451
43200 DO 43110 IN=1,IS                                                  EXM00452
      DO 43120 IM=1,IZ                                                  EXM00453
      JA=INT(DATA(IA))+IAPLUS+(ISS+1)*IN+IM                             EXM00454
      JB=INT(DATA(IB))+IBPLUS+(IC+1)*IN+IM                              EXM00455
      IF(MOP.NE.1) GOTO 43130                                           EXM00456
      DATA(JA)=DATA(JB)+VALUE                                           EXM00457
      GOTO 43120                                                        EXM00458
43130 IF(MOP.NE.2) GOTO 43140                                           EXM00459
      IF(IOP.EQ.1) DATA(JA)=DATA(JB)-VALUE                              EXM00460
      IF(IOP.EQ.-1) DATA(JA)=VALUE-DATA(JB)                             EXM00461
      GOTO 43120                                                        EXM00462
43140 IF(MOP.NE.3) GOTO 43150                                           EXM00463
      IF(IOP.EQ.-1) GOTO 43143                                          EXM00464
      IF(VALUE.NE.0.) GOTO 43141                                        EXM00465
43144 NERROR=42                                                         EXM00466
      GOTO 29008                                                        EXM00467
43141 DATA(JA)=DATA(JB)/VALUE                                           EXM00468
      GOTO 43120                                                        EXM00469
43143 IF(DATA(JB).EQ.0.) GOTO 43144                                     EXM00470
      DATA(JA)=VALUE/DATA(JB)                                           EXM00471
      GOTO 43120                                                        EXM00472
43150 IF(MOP.NE.4) GOTO 43160                                           EXM00473
      IF((DATA(JA).EQ.0.).OR.(DATA(JB).EQ.0.)) GOTO 43151               EXM00474
      IF(ALOG10(ABS(DATA(JB)))+ALOG10(ABS(VALUE)).GT.FLOAT(IEXPO))      EXM00475
     1 GOTO 35                                                          EXM00476
43151 DATA(JA)=DATA(JB)*VALUE                                           EXM00477
      GOTO 43120                                                        EXM00478
43160 IF(MOP.NE.5) GOTO 43170                                           EXM00479
      IF(IOP.EQ.-1) GOTO 43180                                          EXM00480
      ACC=DATA(JB)                                                      EXM00481
      VAL=VALUE                                                         EXM00482
      GOTO 43190                                                        EXM00483
43180 ACC=VALUE                                                         EXM00484
      VAL=DATA(JB)                                                      EXM00485
43190 IF(ACC.EQ.0.) GOTO 58                                             EXM00486
      IF(VAL.EQ.0.) GOTO 51                                             EXM00487
      IF(ABS(ALOG10(ABS(ACC))*VAL).GT.FLOAT(IEXPO)) GOTO 57             EXM00488
      IF(VAL.GT.0.) GOTO 56                                             EXM00489
      VAL=-VAL                                                          EXM00490
      ACC=1./ACC                                                        EXM00491
 56   MIV=VAL                                                           EXM00492
      XS=MIV                                                            EXM00493
      IF(XS.EQ.VAL) GOTO 54                                             EXM00494
      IF(ACC.GE.0.) GOTO 53                                             EXM00495
      XS=ABS(ACC)**VAL                                                  EXM00496
      XV=-1.*XS**(1./VAL)                                               EXM00497
      IF(XV.NE.ACC) GOTO 55                                             EXM00498
      DATA(JA)=-XS                                                      EXM00499
      GOTO 43120                                                        EXM00500
 55   NERROR=43                                                         EXM00501
      X1=ACC                                                            EXM00502
      X2=VAL                                                            EXM00503
      GOTO 29008                                                        EXM00504
 57   NERROR=44                                                         EXM00505
      GOTO 29008                                                        EXM00506
 53   DATA(JA)=ACC**VAL                                                 EXM00507
      GOTO 43120                                                        EXM00508
 54   DATA(JA)=ACC**MIV                                                 EXM00509
      GOTO 43120                                                        EXM00510
 58   IF(VAL.EQ.0.) GOTO 51                                             EXM00511
      DATA(JA)=0.                                                       EXM00512
      GOTO 43120                                                        EXM00513
 51   DATA(JA)=1.                                                       EXM00514
43120 CONTINUE                                                          EXM00515
43110 CONTINUE                                                          EXM00516
      IOP=1                                                             EXM00517
      GOTO 900                                                          EXM00518
43300 WRITE(IWC,24290) ALPH(IA),MIV,JA,ALPH(IB),IZ,JB,ALPH(IC),ISS,JC   EXM00519
      NERROR=36                                                         EXM00520
      GOTO 29008                                                        EXM00521
C**** EXECUTION OF MAT INPUT                                            EXM00522
46000 NSW=3                                                             EXM00523
      GOTO 28100                                                        EXM00524
C**** MAT INPUT WITH ACTUAL SIZE IN INDEX                               EXM00525
47000 NSW=3                                                             EXM00526
      GOTO 29150                                                        EXM00527
29008 CALL EXERR(NERROR,I1,I2,X1,X2)                                    EXM00528
      IOP=-1                                                            EXM00529
 900  RETURN                                                            EXM00530
22400 NERROR=NSTOP+3                                                    EXM00531
      I1=VALUE                                                          EXM00532
      CALL EXERR(NERROR,I1,I2,X1,X2)                                    EXM00533
      IOP=-1                                                            EXM00534
      RETURN                                                            EXM00535
43170 NERROR=11                                                         EXM00536
      GOTO 29008                                                        EXM00537
 35   NERROR=45                                                         EXM00538
      GOTO 29008                                                        EXM00539
C**** INVERSION                                                         EXM00540
49000 MOP=1                                                             EXM00541
C**** JB=SIZE LIMIT TO INVERT A MATRIX                                  EXM00542
C**** DINV MUST BE DIMENSIONED TO EQUAL JB                              EXM00543
      JB=12                                                             ***
      IA=IPROG(INREG-2)                                                 EXM00545
49009 IZ=DATA(IA+27)-1.                                                 EXM00546
      LOCS=DATA(IA)                                                     EXM00547
      LOCU=DATA(LOCS)                                                   EXM00548
      IS=(LOCU-LOCS-1)/(IZ+1)-1                                         EXM00549
      IF(MOP.EQ.2) GOTO 49003                                           EXM00550
      IB=IZ                                                             EXM00551
      IC=IS                                                             EXM00552
49003 IF((IB.NE.IZ).OR.(IC.NE.IS)) GOTO 49010                           EXM00553
      IF(IS.NE.IZ) GOTO 49007                                           EXM00554
      IF(IS.GT.JB) GOTO 49017                                           EXM00555
      DO 49001 IN=1,IS                                                  EXM00556
      DO 49002 IM=1,IZ                                                  EXM00557
      JA=INT(DATA(IA))+1+(IZ+1)*IN+IM                                   EXM00558
      IF(MOP.EQ.1) GOTO 49005                                           EXM00559
      DATA(JA)=DINV(IN,IM)                                              EXM00560
      GOTO 49002                                                        EXM00561
49005 DINV(IN,IM)=DATA(JA)                                              EXM00562
49002 CONTINUE                                                          EXM00563
49001 CONTINUE                                                          EXM00564
      IF(MOP.EQ.2) GOTO 49008                                           EXM00565
      CALL SUBINV(IS,MOP)                                               EXM00566
      IF(MOP.EQ.-1) GOTO 49011                                          EXM00567
      MOP=2                                                             EXM00568
      IA=IPROG(INREG-1)                                                 EXM00569
      GOTO 49009                                                        EXM00570
49007 NERROR=35                                                         EXM00571
      X1=ALPH(IA)                                                       EXM00572
      I1=IS                                                             EXM00573
      I2=IZ                                                             EXM00574
      GOTO 29008                                                        EXM00575
49008 INREG=INREG-3                                                     EXM00576
      GOTO 900                                                          EXM00577
49010 NERROR=48                                                         EXM00578
      GOTO 29008                                                        EXM00579
49011 NERROR=49                                                         EXM00580
      I1=IA                                                             EXM00581
      GOTO 29008                                                        EXM00582
49017 NERROR=50                                                         EXM00583
      I1=10                                                             EXM00584
      GOTO 29008                                                        EXM00585
      END                                                               EXM00586
