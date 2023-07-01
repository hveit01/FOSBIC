      SUBROUTINE MATTRA                                                 MAT00001
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      MAT00002
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST, INEXT,MAT00003
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  MAT00004
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, MAT00005
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     MAT00006
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     MAT00007
     1NIFMAX,INTZEI                                                     MAT00008
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        MAT00009
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      MAT00010
     1IRET(20),XXX(4),NFILE(25,3)                                       MAT00011
      COMMON// ISTLST(340),LISTST(340)                                  MAT00012
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               MAT00014
      DIMENSION IPROG(3700)                                             MAT00015
      EQUIVALENCE (DATA(1),IPROG(1))                                    MAT00016
C**** MAT A=B+C     CODE=-23                                            MAT00017
C**** MAT A=B*C     CODE=-24                                            MAT00018
C**** MAT A=B-C     CODE=-25                                            MAT00019
C**** MAT A=TRN(X)  CODE=-26                                            MAT00020
C**** MAT READ NO REDIMENSION CODE=-28                                  MAT00021
C**** MAT READ DIMENSION CODE=-28                                       MAT00022
C**** MAT READ REDIMENSION CODE=-29                                     MAT00023
C**** MAT INPUT NO REDIMENSION CODE=-46                                 MAT00024
C**** MAT INPUT DIMENSION CODE=-46                                      MAT00025
C**** MAT INPUT REDIMENSION CODE=-47                                    MAT00026
C**** MAT PRINT NUMERIC CODE=-31                                        MAT00027
C**** MAT PRINT NUMERIC REDIMENSION CODE=-32                            MAT00028
C**** MAT PRINT ALPHANUMERIC CODE=-33                                   MAT00029
C**** MAT PRINT ALPHANUMERIC REDIMENSION CODE=-34                       MAT00030
C**** MAT A=IDN      CODE=-35                                           MAT00031
C**** MAT A=IDN      CODE=-36 REDIMENSION                               MAT00032
C**** MAT A=CON      CODE=-37                                           MAT00033
C**** MAT A=CON      CODE=-38 REDIMENSION                               MAT00034
C**** MAT A=ZER      CODE=-39                                           MAT00035
C**** MAT A=ZER      CODE=-40 REDIMENSION                               MAT00036
C**** MAT A=(EXPRESSION)     CODE=-41                                   MAT00037
C**** MAT A=B  CODE=-42                                                 MAT00038
C**** MAT A=B-BINEARY OPERATOR-(EXPRESSION   CODE=-43                   MAT00039
C**** MAT A=(EXPRESSION)-BINEARY OPERATOR-B CODE=-43/-43                MAT00040
C**** MAT A=INV(B)     CODE=-71                                         MAT00041
      IBEGST=IBEGST+3                                                   MAT00042
C*****MOVE STATEMENT FROM CARDP TO CARDT                                MAT00043
      DO 9 IUM=IBEGST,LNGCRP                                            MAT00044
 9    CARDT(IUM)=CARDP(IUM)                                             MAT00045
      MCOM=0                                                            MAT00046
      IF((CARDP(IBEGST).EQ.ALPH(7)).AND.(CARDP(IBEGST+1).EQ.ALPH(5)))   MAT00047
     1 MCOM=7                                                           MAT00048
      IF((CARDP(IBEGST).EQ.ALPH(16)).AND.(CARDP(IBEGST+1).EQ.ALPH(21))) MAT00049
     1 MCOM=8                                                           MAT00050
      IF(MCOM.EQ.0) GOTO 6                                              MAT00051
C----AUF DIE FOLGENDEN ZEILEN LN=0053-0054 WIRD IM TEXT BEZUG GENOMMENvv***
      CALL ZFILE(MCOM)                                                  MAT00052
      RETURN                                                            MAT00053
 6    IF((CARDP(IBEGST).EQ.ALPH(18)).AND.(CARDP(IBEGST+1).EQ.ALPH(5)).  MAT00054
     1AND.(CARDP(IBEGST+3).EQ.ALPH(4))) GOTO 11                         MAT00055
      IF((CARDT(IBEGST).EQ.ALPH(9)).AND.(CARDT(IBEGST+1).EQ.ALPH(14)))  MAT00056
     1 GOTO 13                                                          MAT00057
      IF((CARDT(IBEGST).EQ.ALPH(12)).AND.(CARDT(IBEGST+1).EQ.ALPH(5)).  MAT00058
     1AND.(CARDT(IBEGST+2).EQ.ALPH(20))) GOTO 111                       MAT00059
      IF((CARDT(IBEGST).EQ.ALPH(16)).AND.(CARDT(IBEGST+1).EQ.ALPH(18))  MAT00060
     1.AND.(CARDT(IBEGST+2).EQ.ALPH(9))) GOTO 1111                      MAT00061
      IBEGST=IBEGST-3                                                   MAT00062
      GOTO 111                                                          MAT00063
 1000 NERRS=NERRS+1                                                     MAT00064
      NN=0                                                              MAT00065
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                MAT00066
      RETURN                                                            MAT00067
C     COMMAND IS READ                                                   MAT00068
 11   NDOL=4                                                            MAT00069
      MOP=-28                                                           MAT00070
      GOTO 12                                                           MAT00071
C     COMMAND IS INPUT   CODE IS -46                                    MAT00072
 13   NDOL=5                                                            MAT00073
      MOP=-46                                                           MAT00074
 12   ITANF=IBEGST+NDOL                                                 MAT00075
      IF(LNGCRP.LT.ITANF) GOTO 1000                                     MAT00076
 706  ITOT=0                                                            MAT00077
      DO 699 J=ITANF,LNGCRP                                             MAT00078
      IF(CARDT(J).EQ.DOLSGN) GOTO 698                                   MAT00079
      IF(CARDT(J).EQ.PARLFT) ITOT=ITOT+1                                MAT00080
      IF(CARDT(J).EQ.PARRT) ITOT=ITOT-1                                 MAT00081
      IF((CARDT(J).EQ.COMMA).AND.(ITOT.EQ.0)) GOTO 707                  MAT00082
 699  CONTINUE                                                          MAT00083
      J=LNGCRP+1                                                        MAT00084
      GOTO 707                                                          MAT00085
 698  CARDT(J)=CARDT(J-1)                                               MAT00086
      ITANF=J                                                           MAT00087
      GOTO 706                                                          MAT00088
 707  CALL ZALPH(CARDT(ITANF),I)                                        MAT00089
      IF(ITOT.NE.0) GOTO 318                                            MAT00090
      IF(I.LE.26) GOTO 704                                              MAT00091
      LOCN=ITANF                                                        MAT00092
      GOTO 2100                                                         MAT00093
 657  NERROR=32                                                         MAT00094
      I1=ITANF                                                          MAT00095
      I2=IC                                                             MAT00096
      GOTO 1000                                                         MAT00097
 704  IF(J-2.GT.ITANF) GOTO 702                                         MAT00098
C**** MAT READ OR INPUT -- NO REDIMENSION AND NO DIMENSION              MAT00099
 740  IPROG(INREG)=MOP                                                  MAT00100
      IPROG(INREG-1)=I                                                  MAT00101
      CALL ZKLAM(ITANF,ITANF,I)                                         MAT00102
 701  INREG=INREG-2                                                     MAT00103
      ITANF=J+1                                                         MAT00104
      IF(J.LT.LNGCRP) GOTO 706                                          MAT00105
      RETURN                                                            MAT00106
C**** MAT READ OR INPUT WITH REDIMENSION                                MAT00107
 702  IF(MERKER(I,1).EQ.0) GOTO 720                                     MAT00108
      CALL ZKLAM(ITANF+1,J-1,I)                                         MAT00109
      IPROG(INREG)=MOP-1                                                MAT00110
      IPROG(INREG-1)=I                                                  MAT00111
      GOTO 701                                                          MAT00112
C**** MAT READ OR INPUT WITH DIMENSION                                  MAT00113
 720  MERCRP=LNGCRP                                                     MAT00114
      LOC=ITANF+1                                                       MAT00115
      LNGCRP=J-1                                                        MAT00116
      GOTO 658                                                          MAT00117
 721  LNGCRP=MERCRP                                                     MAT00118
      GOTO 740                                                          MAT00119
C                                                                       MAT00120
C**** COMMAND IS MAT PRINT                                              MAT00121
1111  IF(LNGCRP.LT.IBEGST+5) GOTO 1000                                  MAT00122
      ITANF=IBEGST+5                                                    MAT00123
 310  NDOL=0                                                            MAT00124
 312  ITOT=0                                                            MAT00125
      DO 311 J=ITANF,LNGCRP                                             MAT00126
      IF(CARDP(J).EQ.DOLSGN) GOTO 314                                   MAT00127
      IF(CARDP(J).EQ.PARRT) ITOT=ITOT+1                                 MAT00128
      IF(CARDP(J).EQ.PARLFT) ITOT=ITOT-1                                MAT00129
      IF((CARDP(J).EQ.COMMA).AND.(ITOT.EQ.0)) GOTO 3135                 MAT00130
      IF((CARDP(J).EQ.PUCO).AND.(ITOT.EQ.0)) GOTO 317                   MAT00131
 311  CONTINUE                                                          MAT00132
      J=LNGCRP+1                                                        MAT00133
      GOTO 3135                                                         MAT00134
 313  CALL ZALPH(CARDP(ITANF),IA)                                       MAT00135
      MOP=-31                                                           MAT00136
      IF(IA.GT.26) GOTO 319                                             MAT00137
      IF(NDOL.EQ.1) MOP=-33                                             MAT00138
C*****NUMERIC MAT PRINT =-31                                            MAT00139
C*****ALPHANUMERIC MAT PRINT =-33                                       MAT00140
      IF(J.EQ.ITANF+1) GOTO 315                                         MAT00141
      IF(J.EQ.ITANF+3) GOTO 316                                         MAT00142
C*****MAT PRINT WITH REDIMENSION                                        MAT00143
      MOP=MOP-1                                                         MAT00144
      CALL ZKLAM(ITANF+1,J-1,IA)                                        MAT00145
 315  IPROG(INREG)=MOP                                                  MAT00146
      IPROG(INREG-1)=IA                                                 MAT00147
      INREG=INREG-2                                                     MAT00148
      ITANF=J+1                                                         MAT00149
      CALL ZKLAM(ITANF,ITANF,IA)                                        MAT00150
      IF(ITANF.GT.LNGCRP) RETURN                                        MAT00151
      GOTO 310                                                          MAT00152
 317  IPROG(INREG)=-69                                                  MAT00153
 3136 INREG=INREG-1                                                     MAT00154
      IF(ITOT.NE.0) GOTO 318                                            MAT00155
      GOTO 313                                                          MAT00156
 3135 IPROG(INREG)=-67                                                  MAT00157
      GOTO 3136                                                         MAT00158
 316  IC=J-1                                                            MAT00159
      GOTO 657                                                          MAT00160
 314  CARDP(J)=CARDP(J-1)                                               MAT00161
      ITANF=J                                                           MAT00162
      NDOL=1                                                            MAT00163
      GOTO 312                                                          MAT00164
 318  NERROR=53                                                         MAT00165
      I1=ITANF                                                          MAT00166
      I2=J                                                              MAT00167
      GOTO 1000                                                         MAT00168
 319  LOCN=ITANF                                                        MAT00169
      GOTO 2100                                                         MAT00170
C                                                                       MAT00171
C*****COMMAND IS MAT LET                                                MAT00172
 111  IBEGST=IBEGST+3                                                   MAT00173
      DO 2000 LOC=IBEGST,LNGCRP                                         MAT00174
      IF(CARDT(LOC).EQ.EQUALS) GOTO 2001                                MAT00175
 2000 CONTINUE                                                          MAT00176
      NERROR=33                                                         MAT00177
      GOTO 1000                                                         MAT00178
 2001 IF((CARDT(LOC+1).EQ.ALPH(9)).AND.(CARDT(LOC+2).EQ.ALPH(4)))       MAT00179
     1 GOTO 500                                                         MAT00180
      IF((CARDP(LOC+1).EQ.ALPH(9)).AND.(CARDP(LOC+3).EQ.ALPH(22)))      MAT00181
     1 GOTO 8000                                                        MAT00182
      IF((CARDT(LOC+1).EQ.ALPH(20)).AND.(CARDT(LOC+2).EQ.ALPH(18)))     MAT00183
     1 GOTO 600                                                         MAT00184
      IF((CARDT(LOC+1).EQ.ALPH(3)).AND.(CARDT(LOC+2).EQ.ALPH(15)))      MAT00185
     1 GOTO 610                                                         MAT00186
      IF((CARDT(LOC+1).EQ.ALPH(26)).AND.(CARDT(LOC+2).EQ.ALPH(5)))      MAT00187
     1 GOTO 650                                                         MAT00188
      IF(CARDT(LOC+1).EQ.PARLFT) GOTO 2003                              MAT00189
      IF(CARDT(LNGCRP).NE.PARRT) GOTO 2030                              MAT00190
C**** COMMAND IS A=B+(EXPRESSION)                                       MAT00191
      IANF=1                                                            MAT00192
      I=LOC+2                                                           MAT00193
      LOC1=LOC+3                                                        MAT00194
      LOC2=LNGCRP                                                       MAT00195
      IAS=LOC+1                                                         MAT00196
      GOTO 2025                                                         MAT00197
 2030 IF(CARDT(LOC+1).NE.QUOTE) GOTO 2040                               MAT00198
C**** COMMAND IS MAT A=STRING                                           MAT00199
      IF(CARDT(LOC-1).EQ.DOLSGN) GOTO 2041                              MAT00200
      NERROR=4                                                          MAT00201
      NN=1                                                              MAT00202
      CALL COMERR(NERROR,I1,I2,CARDT(LOC-1),X2,NN)                      MAT00203
 2041 CALL STRING(LOC+2,LNGCRP-1,IX)                                    MAT00204
      IPROG(INREG)=-1                                                   MAT00205
      IPROG(INREG-1)=2                                                  MAT00206
      IPROG(INREG-2)=-20                                                MAT00207
      IPROG(INREG-3)=IX                                                 MAT00208
      IPROG(INREG-4)=-41                                                MAT00209
      IB=INREG-5                                                        MAT00210
 5555 LOCN=LOC-1                                                        MAT00211
      IF(CARDT(LOCN).EQ.DOLSGN) LOCN=LOC-2                              MAT00212
      IF((LOCN-IBEGST+1).EQ.1) GOTO 5455                                MAT00213
      NERROR=51                                                         MAT00214
      I1=IBEGST                                                         MAT00215
      I2=LOC-1                                                          MAT00216
      GOTO 1000                                                         MAT00217
 5455 CALL ZALPH(CARDT(LOCN),IA)                                        MAT00218
      IF(IA.GT.26) GOTO 2100                                            MAT00219
      CALL ZKLAM(LOC,LOC,IA)                                            MAT00220
      IPROG(IB)=IA                                                      MAT00221
      INREG=IB-1                                                        MAT00222
      RETURN                                                            MAT00223
C**** NO EXPRESSION ON RIGHT                                            MAT00224
 2040 IF(CARDT(LOC+2).EQ.ASTRSK) GOTO 700                               MAT00225
      IF(LNGCRP.LE.LOC+2) GOTO 2102                                     MAT00226
      IF(LNGCRP.GT.LOC+3) GOTO 2112                                     MAT00227
C**** COMMAND IS MAT A=B+C OR MAT A=B-C                                 MAT00228
      IF(CARDP(LOC+2).EQ.PLUS) MOP=-23                                  MAT00229
      IF(CARDP(LOC+2).EQ.CMINUS) MOP=-25                                MAT00230
      IPROG(INREG)=MOP                                                  MAT00231
      IANF=1                                                            MAT00232
      IB=1                                                              MAT00233
      LOCN=LOC-1                                                        MAT00234
      GOTO 2221                                                         MAT00235
 2112 NERROR=34                                                         MAT00236
      GOTO 1000                                                         MAT00237
 2101 CALL ZTRANX(LOC+1,LNGCRP)                                         MAT00238
      IPROG(INREG)=-41                                                  MAT00239
      IB=INREG-1                                                        MAT00240
      GOTO 5555                                                         MAT00241
 2003 IF(CARDT(LNGCRP).EQ.PARRT) GOTO 2101                              MAT00242
      IANF=-1                                                           MAT00243
      I=LNGCRP-1                                                        MAT00244
      LOC1=LOC+1                                                        MAT00245
      LOC2=LNGCRP-2                                                     MAT00246
      IAS=LNGCRP                                                        MAT00247
 2025 IF(CARDT(I).NE.PLUS) GOTO 2012                                    MAT00248
      MOP=1                                                             MAT00249
      GOTO 2020                                                         MAT00250
 2012 IF(CARDT(I).NE.CMINUS) GOTO 2013                                  MAT00251
      MOP=2                                                             MAT00252
      GOTO 2020                                                         MAT00253
 2013 IF(CARDT(I).NE.SLASH) GOTO 2014                                   MAT00254
      MOP=3                                                             MAT00255
      GOTO 2020                                                         MAT00256
 2014 IF(CARDT(I).NE.ASTRSK) GOTO 2040                                  MAT00257
      IF((I.EQ.LNGCRP-1).AND.(CARDT(I-1).EQ.ASTRSK)) GOTO 2015          MAT00258
      IF((I.EQ.LOC+2).AND.(CARDT(I+1).EQ.ASTRSK)) GOTO 2017             MAT00259
      MOP=4                                                             MAT00260
      GOTO 2020                                                         MAT00261
 2015 LOC2=LOC2-1                                                       MAT00262
      GOTO 2018                                                         MAT00263
 2017 LOC1=LOC1+1                                                       MAT00264
 2018 MOP=5                                                             MAT00265
      GOTO 2020                                                         MAT00266
C*****IDENTITY MATRIX WITH REDIMENSION CODE=-36                         MAT00267
 500  MOP=-35                                                           MAT00268
      GOTO 660                                                          MAT00269
C**** MATRIX=1 (CON) REDIMENSION=-38                                    MAT00270 
 610  MOP=-37                                                           MAT00271
      GOTO 660                                                          MAT00272
C**** MATRIX=0 (ZERO) REDIMENSION=-40                                   MAT00273
 650  MOP=-39                                                           MAT00274
      GOTO 660                                                          MAT00275
C*****MAT A=B                                                           MAT00276
 2102 IF((CARDT(LOC-1).NE.DOLSGN).AND.(CARDT(LOC+2).NE.DOLSGN))GOTO 2136MAT00277
      IF((CARDT(LOC-1).EQ.DOLSGN).AND.(CARDT(LOC+2).EQ.DOLSGN))GOTO 2136MAT00278
      IF(CARDT(LOC-1).EQ.DOLSGN) NERROR=5                               MAT00279
      IF(CARDT(LOC+2).EQ.DOLSGN) NERROR=4                               MAT00280
      NN=1                                                              MAT00281
      CALL COMERR(NERROR,I1,I2,CARDT(LOC-2),X2,NN)                      MAT00282
 2136 CALL ZALPH(CARDP(LOC+1),IA)                                       MAT00283
      IF(IA.GT.26) GOTO 2100                                            MAT00284
      CALL ZKLAM(LOC,LOC,IA)                                            MAT00285
      IPROG(INREG)=-42                                                  MAT00286
      IPROG(INREG-1)=IA                                                 MAT00287
      IB=INREG-2                                                        MAT00288
      GOTO 5555                                                         MAT00289
C**** NO ACCEPTABLE MAT LET COMMAND                                     MAT00290
 2100 X1=CARDT(LOCN)                                                    MAT00291
      NERROR=43                                                         MAT00292
      GOTO 1000                                                         MAT00293
C*****WITH EXPRESSION                                                   MAT00294
 2020 CALL ZTRANX(LOC1,LOC2)                                            MAT00295
      IB=2                                                              MAT00296
      IPROG(INREG)=-43                                                  MAT00297
      IPROG(INREG-1)=MOP                                                MAT00298
      IF(IANF.EQ.1) GOTO 2072                                           MAT00299
      IPROG(INREG-2)=-43                                                MAT00300
      INREG=INREG-1                                                     MAT00301
 2072 LOCN=LOC-1                                                        MAT00302
 2221 IF(CARDT(LOCN).NE.DOLSGN) GOTO 2021                               MAT00303
      LOCN=LOCN-1                                                       MAT00304
      NERROR=5                                                          MAT00305
      NN=1                                                              MAT00306
      CALL COMERR(NERROR,I1,I2,CARDT(LOCN),X2,NN)                       MAT00307
 2021 CALL ZALPH(CARDT(LOCN),IA)                                        MAT00308
      IF(IA.GT.26) GOTO 2100                                            MAT00309
      CALL ZKLAM(LOCN,LOCN,IA)                                          MAT00310
      LOC1=INREG-IB                                                     MAT00311
      IPROG(LOC1)=IA                                                    MAT00312
      IF(IB.EQ.3) GOTO 2022                                             MAT00313
      IB=IB+1                                                           MAT00314
      IC=IA                                                             MAT00315
      IF(IANF.EQ.-1) GOTO 2076                                          MAT00316
      LOCN=LOCN+2                                                       MAT00317
      GOTO 2021                                                         MAT00318
 2076 LOCN=IAS                                                          MAT00319
      GOTO 2221                                                         MAT00320
 2022 INREG=INREG-4                                                     MAT00321
      IF((MERKER(IA,2).NE.0).AND.(MERKER(IC,2).EQ.0)) GOTO 2023         MAT00322
      IF((MERKER(IA,2).EQ.0).AND.(MERKER(IC,2).NE.0)) GOTO 2026         MAT00323
      RETURN                                                            MAT00324
 2026 IB=IC                                                             MAT00325
      IC=IA                                                             MAT00326
      IA=IB                                                             MAT00327
 2023 I1=IC                                                             MAT00328
      I2=IA                                                             MAT00329
      NERROR=36                                                         MAT00330
      GOTO 1000                                                         MAT00331
C**** MAT LET WITH CON OR IDN OR ZER                                    MAT00332
 660  LOCN=LOC-1                                                        MAT00333
      IF(CARDT(LOCN).NE.DOLSGN) GOTO 669                                MAT00334
      LOCN=LOC-2                                                        MAT00335
      NERROR=5                                                          MAT00336
      NN=1                                                              MAT00337
      CALL COMERR(NERROR,I1,I2,CARDT(LOCN),X2,NN)                       MAT00338
 669  CALL ZALPH(CARDT(LOCN),I)                                         MAT00339
      IF(I.GT.26) GOTO 2100                                             MAT00340
      IF(LNGCRP.GT.LOC+3) GOTO 652                                      MAT00341
C**** NO REDIMENSION                                                    MAT00342
 656  IPROG(INREG)=MOP                                                  MAT00343
      IPROG(INREG-1)=I                                                  MAT00344
      CALL ZKLAM(LOC-1,LOC-1,I)                                         MAT00345
 659  INREG=INREG-2                                                     MAT00346
      RETURN                                                            MAT00347
C**** DIMENSION OR REDIMENSION                                          MAT00348
 652  IF(CARDT(LNGCRP).EQ.PARRT) GOTO 658                               MAT00349
      ITANF=LOC+1                                                       MAT00350
      IC=LNGCRP                                                         MAT00351
      GOTO 657                                                          MAT00352
C**** REDIMENSION                                                       MAT00353
 655  CALL ZKLAM(LOC+4,LNGCRP,I)                                        MAT00354
      IPROG(INREG)=MOP-1                                                MAT00355
      IPROG(INREG-1)=I                                                  MAT00356
      GOTO 659                                                          MAT00357
C**** DIMENSION                                                         MAT00358
 658  IF(MERKER(I,1).NE.0) GOTO 655                                     MAT00359
      DO 661 IA=LOC,LNGCRP                                              MAT00360
      IF(CARDT(IA).EQ.COMMA) GOTO 662                                   MAT00361
      IF(CARDT(IA).EQ.PARLFT) LOCN=IA+1                                 MAT00362
 661  CONTINUE                                                          MAT00363
      IA=LNGCRP                                                         MAT00364
C**** SINGLE SUBSCRIPT                                                  MAT00365
 662  CALL ZCONVN(LOCN,IA-1,FNUM)                                       MAT00366
      IF(FNUM.GE.1.) GOTO 663                                           MAT00367
      I1=LOCN                                                           MAT00368
      I2=IA-1                                                           MAT00369
 6651 NERROR=54                                                         MAT00370
      GOTO 1000                                                         MAT00371
 663  MERKER(I,1)=FNUM                                                  MAT00372
      IF(IA.LT.LNGCRP) GOTO 664                                         MAT00373
      MERKER(I,2)=0                                                     MAT00374
      IPROG(I)=FNUM+2.                                                  MAT00375
      GOTO 6561                                                         MAT00376
C**** DOUBLE SUBSCRIPTED                                                MAT00377
 664  CALL ZCONVN(IA+1,LNGCRP-1,FNUM)                                   MAT00378
      IF(FNUM.GE.1.) GOTO 665                                           MAT00379
      I1=IA+1                                                           MAT00380
      I2=LNGCRP-1                                                       MAT00381
      GOTO 6651                                                         MAT00382
 665  MERKER(I,2)=FNUM                                                  MAT00383
      IPROG(I)=(MERKER(I,1)+1)*(MERKER(I,2)+1)+1                        MAT00384
      DATA(I+27)=FNUM+1.                                                MAT00385
 6561 IF((MOP.EQ.-28).OR.(MOP.EQ.-46)) GOTO 721                         MAT00386
      IF((MOP.EQ.-35).AND.(MERKER(I,1).NE.MERKER(I,2))) GOTO 666        MAT00387
      GOTO 656                                                          MAT00388
 666  X1=ALPH(I)                                                        MAT00389
      NERROR=37                                                         MAT00390
      GOTO 1000                                                         MAT00391
C*****MATRIX MULTIPLICATION                                             MAT00392
C*****CODE=-24                                                          MAT00393
C*****FIRST ADDRESS IS IN INREG-1                                       MAT00394
C*****SECOND ADDRESS IS IN INREG-2                                      MAT00395
C*****THIRD ADDRESS IS IN INREG-3                                       MAT00396
C*****A(I)=ROW VECTOR                                                   MAT00397
C*****A(1,I)=ROW VECTOR                                                 MAT00398
C*****A(I,1)=COLUMN VECTOR                                              MAT00399
 700  IF(LNGCRP.GT.LOC+3) GOTO 7777                                     MAT00400
      IANF=IBEGST                                                       MAT00401
 300  CALL ZALPH(CARDT(IANF),I)                                         MAT00402
      LOCN=IANF                                                         MAT00403
      IF(I.GT.26) GOTO 2100                                             MAT00404
      IF(IANF.EQ.LNGCRP) GOTO 173                                       MAT00405
      IF(IANF.EQ.IBEGST) GOTO 172                                       MAT00406
      IF(IANF.EQ.LOC+1) GOTO 102                                        MAT00407
      GOTO 7777                                                         MAT00408
 172  CALL ZDIGIT(CARDT(IANF+1),J)                                      MAT00409
      IF(J.GT.10) GOTO 178                                              MAT00410
      IPROG(INREG-1)=I+(26*(J-1))+53                                    MAT00411
      IANF=LOC+1                                                        MAT00412
      GOTO 300                                                          MAT00413
 178  IPROG(INREG-1)=I                                                  MAT00414
      IANF=LOC+1                                                        MAT00415
      IA=I                                                              MAT00416
      GOTO 300                                                          MAT00417
 102  IANF=LNGCRP                                                       MAT00418
      IPROG(INREG-2)=I                                                  MAT00419
      IB=I                                                              MAT00420
      GOTO 300                                                          MAT00421
 173  IPROG(INREG-3)=I                                                  MAT00422
      IC=I                                                              MAT00423
      IF((IA.EQ.IB).AND.(IA.EQ.IC)) GOTO 7777                           MAT00424
      IPROG(INREG)=-24                                                  MAT00425
      INREG=INREG-4                                                     MAT00426
      RETURN                                                            MAT00427
 7777 NERROR=38                                                         MAT00428
      GOTO 1000                                                         MAT00429
C*****TRANSPONATION CODE IS -26                                         MAT00430
 600  MOP=-26                                                           MAT00431
 601  CALL ZALPH(CARDT(LNGCRP-1),I)                                     MAT00432
      IF(I.GT.26) GOTO 2100                                             MAT00433
      CALL ZKLAM(LOC,LOC,I)                                             MAT00434
      IPROG(INREG-2)=I                                                  MAT00435
      CALL ZALPH(CARDT(LOC-1),I)                                        MAT00436
      IF(I.GT.26) GOTO 2100                                             MAT00437
      CALL ZKLAM(LOC,LOC,I)                                             MAT00438
      IPROG(INREG-1)=I                                                  MAT00439
      IPROG(INREG)=MOP                                                  MAT00440
      INREG=INREG-3                                                     MAT00441
      RETURN                                                            MAT00442
C**** INVERSION                                                         MAT00443
 8000 MOP=-71                                                           MAT00444
      GOTO 601                                                          MAT00445
      END                                                               MAT00446
