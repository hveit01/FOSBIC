      SUBROUTINE ZEXEC                                                  ZEX00001
C---- DAS FOLGENDE STATEMENT IST ZU ENTFERNEN ODER DZRCH EIN C IN DER   ZEX00002
C---- 1.SPALTE UNWIRKSAM ZU MACHEN,WENN NICHT IN *OVERLAY-TECHNIK*      ZEX00003
C---- GEARBEITET WIRD.IN DEM VORAUSGEHENDEN STATEMENT IST DAS C IN DER  ZEX00004
C---- 1.SPALTE ZU ENTFERNEN.                                            ZEX00005
C     PROGRAM EXECUT                                                    ZEX00006
C---- (SIEHE BEMERKUNGEN IM HAUPTELEMENT DER *OVERLAY-TECHNIK*)         ZEX00007
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZEX00008
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST, INEXT,ZEX00009
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZEX00010
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZEX00011
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZEX00012
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZEX00013
     1NIFMAX,INTZEI                                                     ZEX00014
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZEX00015
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZEX00016
     1IRET(20),XXX(4),NFILE(25,3)                                       ZEX00017
      COMMON// ISTLST(340),LISTST(340)                                  ZEX00018
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZEX00020
      DIMENSION IPROG(3700)                                             ZEX00021
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZEX00022
      DIMENSION IPRT(10)                                                ZEX00023
C                                                                       ZEX00024
C---- DAS FOLGENDE STATEMENT IST ZU ENTFERNEN ODER DURCH EIN C IN DER   ZEX00025
C---- 1.SPALTE UNWIRKSAM ZU MACHEN,WENN NICHT IN *OVERLAY-TECHNIK*      ZEX00026
C---- GEARBEITET WIRD.                                                  ZEX00027
C      OVERLAY(2)                                                       ZEX00028
      INEXT=1                                                           ZEX00029
      NPRUS=1                                                           ZEX00030
      IF(NSTZEI.NE.2) GOTO 1                                            ZEX00031
C**** NUMBER OF FOSBIC-CODE ERRORS = 51                                 ***
      IA=51                                                             ***
      I1=12                                                             ***
      I2=25                                                             ***
      X1=ALPH(1)                                                        ***
      X2=12345                                                          ***
      DO 2 I=1,IA                                                       ZEX00034
      NERROR=I                                                          ZEX00035
      CALL EXERR(NERROR,I1,I2,X1,X2)                                    ZEX00036
 2    CONTINUE                                                          ZEX00037
C**** MAXIMAL INTERNAL FOSBIC CODE NUMBER UNTIL APRIL 1976              ZEX00038
 1    ICODE=71                                                          ZEX00039
C**** INITIALIZE RANDOM NUMBER GENERATOR                                ZEX00040
C     XRN=RNG(2.)                                                       ZEX00041
      NOLNP=NCELLP-INREG+1                                              ZEX00042
C**** SET INTERNAL ADDRESS IN COMPUTED GOTO AND GOSUB                   ZEX00043
      DO 200 I=1,NOLNP                                                  ZEX00044
      INREG=NCELLP-I                                                    ZEX00045
      IF(IPROG(INREG).NE.-27) GOTO 200                                  ZEX00046
      IF(NSTLST.EQ.0) GOTO 204                                          ZEX00047
      MIC=IPROG(INREG-1)                                                ZEX00048
      DO 201 J=1,MIC                                                    ZEX00049
      DO 202 K=1,NSTLST                                                 ZEX00050
      IA=INREG-J-1                                                      ZEX00051
      IF(IPROG(IA).EQ.LISTST(K)) GOTO 203                               ZEX00052
 202  CONTINUE                                                          ZEX00053
 204  NERROR=2                                                          ZEX00054
      CALL EXERR(NERROR,IPROG(IA),I2,X1,X2)                             ZEX00055
      NERRS=NERRS+1                                                     ZEX00056
      GOTO 201                                                          ZEX00057
 203  IPROG(IA)=ISTLST(K)                                               ZEX00058
 201  CONTINUE                                                          ZEX00059
 200  CONTINUE                                                          ZEX00060
      IF(NERRS.NE.0) GOTO 21000                                         ZEX00061
      INREG=NCELLP                                                      ZEX00062
      MOP=0                                                             ZEX00063
      NIRET=0                                                           ZEX00064
      NPRI=1                                                            ZEX00065
      CALL CLEAR(1,140)                                                 ZEX00066
      DO 110 I=1,4                                                      ZEX00067
 110  XXX(I)=0.                                                         ZEX00068
      NIT=0                                                             ZEX00069
      WRITE(IWC,603)                                                    ZEX00070
 603  FORMAT(1H1)                                                       ZEX00071
C**** RESET COMMON-FILES                                                ZEX00072
      NPR=0                                                             ZEX00073
C----AUF DIE FOLGENDE ZEILE LN=0079 WIRD IM TEXT BEZUG GENOMMENvv       ***
      CALL ZEXFIL(NPR)                                                  ZEX00074
C                                                                       ZEX00075
 900  IOP=IPROG(INREG)                                                  ZEX00076
      NPR=0                                                             ZEX00077
      NSTOP=0                                                           ZEX00078
      MIC=0                                                             ZEX00079
      IF(IOP.LT.0) GOTO 950                                             ZEX00080
C*****ILLEGAL COMMAND                                                   ZEX00081
 909  NERROR=9                                                          ZEX00082
      IF(IOP.EQ.27) GOTO 29008                                          ***
      IF(IOP.EQ.30) GOTO 29008                                          ***
      IF(IOP.EQ.44) GOTO 29008                                          ***
      IF(IOP.EQ.45) GOTO 29008                                          ***
      IF(IOP.EQ.71) GOTO 906                                            ***
      IF(IOP.GE.48) GOTO 907                                            ***
 906  NERROR=11                                                         ***
      GOTO 29008                                                        ***
 907  IF(IOP.GT.61) GOTO 908                                            ***
      NERROR=12                                                         ***
      GOTO 29008                                                        ***
 908  NERROR=9                                                          ***
      GOTO 29008                                                        ZEX00083
C                                                                       ZEX00084
 950  IOP=-IOP                                                          ZEX00085
      IF((IOP.LT.1).OR.(IOP.GT.ICODE)) GOTO 19001                       ZEX00086
      IF(IOP.GE.23) GOTO 960                                            ZEX00087
      GOTO(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,          ZEX00088
     1 11000,12000,13000,14000,15000,16000,17000,18000,19000,20000,21000ZEX00089
     2,22000),IOP                                                       ZEX00090
 960  IF(IOP.EQ.27) GOTO 27000                                          ZEX00091
      IF(IOP.EQ.30) GOTO 30000                                          ZEX00092
      IF(IOP.EQ.44) GOTO 44000                                          ZEX00093
      IF(IOP.EQ.45) GOTO 45000                                          ZEX00094
      IF(IOP.EQ.71) GOTO 44400                                          ZEX00095
      IF(IOP.GE.48) GOTO 970                                            ZEX00096
44400 IOP=IOP-22                                                        ZEX00097
C**** MAT CODE EXECUTION IN ZEXMAT                                      ZEX00098
C----AUF DIE FOLGENDE ZEILE LN=0117 WIRD IM TEXT BEZUG GENOMMENvv       ***
      CALL ZEXMAT(IOP)                                                  ZEX00099
      IF(IOP.GT.0) GOTO 900                                             ZEX00100
      GOTO 21000                                                        ZEX00101
C**** FILE CODE EXECUTION IN ZEXFIL                                     ZEX00102
 970  IF(IOP.GT.61) GOTO 990                                            ZEX00103
      IOP=IOP-47                                                        ZEX00104
C----AUF DIE FOLGENDE ZEILE LN=0124 WIRD IM TEXT BEZUG GENOMMENvv       ***
      CALL ZEXFIL(IOP)                                                  ZEX00105
      IF(IOP.GT.0) GOTO 900                                             ZEX00106
      GOTO 21000                                                        ZEX00107
 990  IOP=IOP-61                                                        ZEX00108
      GOTO(62000,63000,64000,65000,66000,67000,68000,69000,70000),IOP   ZEX00109
C**** EVALUATE EXPRESSION                                               ZEX00110
 1000 CALL ZEVAL(NSTOP)                                                 ZEX00111
      IF(NSTOP.EQ.1) GOTO 21000                                         ZEX00112
      GOTO 900                                                          ZEX00113
C                                                                       ZEX00114
C**** STORE FOR RETURN                                                  ZEX00115
 2000 NIRET=NIRET+1                                                     ZEX00116
      IF(NIRET.LE.NIRMAX) GOTO 2100                                     ZEX00117
      NERROR=1                                                          ZEX00118
      GOTO 29008                                                        ZEX00119
 2100 IRET(NIRET)=INREG-3                                               ZEX00120
      INREG=INREG-1                                                     ZEX00121
      GOTO 900                                                          ZEX00122
C                                                                       ZEX00123
C*****GO TO EXTERNAL                                                    ZEX00124
 3000 IF(NSTLST.EQ.0) GOTO 3005                                         ZEX00125
      DO 3004 I=1,NSTLST                                                ZEX00126
      IF(IPROG(INREG-1).EQ.LISTST(I)) GOTO 3010                         ZEX00127
 3004 CONTINUE                                                          ZEX00128
 3005 NERROR=2                                                          ZEX00129
      I1=IPROG(INREG-1)                                                 ZEX00130
      GOTO 29008                                                        ZEX00131
C                                                                       ZEX00132
 3010 IPROG(INREG-1)=ISTLST(I)                                          ZEX00133
      IPROG(INREG)=-4                                                   ZEX00134
      INREG=IPROG(INREG-1)                                              ZEX00135
      GOTO 900                                                          ZEX00136
C                                                                       ZEX00137
C*****GO TO INTERNAL                                                    ZEX00138
 4000 INREG=IPROG(INREG-1)                                              ZEX00139
      GOTO 900                                                          ZEX00140
C                                                                       ZEX00141
C*****RETURN                                                            ZEX00142
 5000 IF(NIRET.GT.0) GOTO 5010                                          ZEX00143
      NERROR=3                                                          ZEX00144
      GOTO 29008                                                        ZEX00145
C                                                                       ZEX00146
 5010 INREG=IRET(NIRET)                                                 ZEX00147
      NIRET=NIRET-1                                                     ZEX00148
      GOTO 900                                                          ZEX00149
C                                                                       ZEX00150
C*****CONDITIONAL TRANSFER                                              ZEX00151
 6000 ICOMP=IPROG(INREG-1)                                              ZEX00152
      ITF=0                                                             ZEX00153
      GOTO(6010,6020,6030,6040,6050,6060),ICOMP                         ZEX00154
 6010 IF(XXX(1).GT.XXX(2)) ITF=1                                        ZEX00155
      GOTO 6100                                                         ZEX00156
 6020 IF(XXX(1).GE.XXX(2)) ITF=1                                        ZEX00157
      GOTO 6100                                                         ZEX00158
 6030 IF(XXX(1).LT.XXX(2)) ITF=1                                        ZEX00159
      GOTO 6100                                                         ZEX00160
 6040 IF(XXX(1).LE.XXX(2)) ITF=1                                        ZEX00161
      GOTO 6100                                                         ZEX00162
 6050 IF(XXX(1).EQ.XXX(2)) ITF=1                                        ZEX00163
      GOTO 6100                                                         ZEX00164
 6060 IF(XXX(1).NE.XXX(2)) ITF=1                                        ZEX00165
 6100 IF(ITF.EQ.0) GOTO 6200                                            ZEX00166
      INREG=INREG-2                                                     ZEX00167
      GOTO 900                                                          ZEX00168
 6200 INREG=INREG-4                                                     ZEX00169
      GOTO 900                                                          ZEX00170
C                                                                       ZEX00171
C*****READ INTO ACCUMULATOR                                             ZEX00172
 7000 NSW=1                                                             ZEX00173
 7001 IFR=1                                                             ZEX00174
      CALL ZHOPPR(VALUE,NSTOP,IFR,NSW)                                  ZEX00175
      IF(NSTOP.EQ.0) GOTO 7010                                          ZEX00176
22400 NERROR=NSTOP+3                                                    ZEX00177
      I1=VALUE                                                          ZEX00178
29008 CALL EXERR(NERROR,I1,I2,X1,X2)                                    ZEX00179
      GOTO 21000                                                        ZEX00180
 7010 ACC=VALUE                                                         ZEX00181
      INREG=INREG-1                                                     ZEX00182
      GOTO 900                                                          ZEX00183
C                                                                       ZEX00184
C*****STORE FROM ACCUMULATOR                                            ZEX00185
 8000 IPOS=IPROG(INREG-1)                                               ZEX00186
      DATA(IPOS)=ACC                                                    ZEX00187
      INREG=INREG-2                                                     ZEX00188
      GOTO 900                                                          ZEX00189
C                                                                       ZEX00190
C*****STORE FROM ACCUMULATOR INDIRECT                                   ZEX00191
 9000 IPOS=IPROG(INREG-1)                                               ZEX00192
      LOC=DATA(IPOS)+1.5                                                ZEX00193
      DATA(LOC)=ACC                                                     ZEX00194
      INREG=INREG-2                                                     ZEX00195
      GOTO 900                                                          ZEX00196
C                                                                       ZEX00197
C*****SKIP A LINE                                                       ZEX00198
10000 INREG=INREG-1                                                     ZEX00199
      IF((NPRUS.EQ.1).AND.(INEXT.EQ.1)) GOTO 10200                      ZEX00200
      NX=3                                                              ***
      CALL PRILIN(NX)                                                   ZEX00202
      GOTO 10300                                                        ZEX00203
10200 WRITE(IWC,10100)                                                  ZEX00204
10100 FORMAT(1H )                                                       ZEX00205
10300 NPRI=1                                                            ZEX00206
      GOTO 900                                                          ZEX00207
C                                                                       ZEX00208
C*****STOP                                                              ZEX00209
11000 NX=3                                                              ZEX00210
      CALL PRILIN(NX)                                                   ZEX00211
      GOTO 21000                                                        ZEX00212
C                                                                       ZEX00213
C*****NO OPERATION                                                      ZEX00214
12000 INREG=INREG-1                                                     ZEX00215
      GOTO 900                                                          ZEX00216
C                                                                       ZEX00217
C*****PAGE                                                              ZEX00218
13000 NX=3                                                              ZEX00219
      CALL PRILIN(NX)                                                   ZEX00220
      WRITE(IWC,13005)                                                  ZEX00221
13005 FORMAT(1H1)                                                       ZEX00222
      INREG=INREG-1                                                     ZEX00223
      GOTO 900                                                          ZEX00224
C                                                                       ZEX00225
C*****LOAD ACC INTO EXP                                                 ZEX00226
14000 LOC=IPROG(INREG-1)                                                ZEX00227
      XXX(LOC)=ACC                                                      ZEX00228
      INREG=INREG-2                                                     ZEX00229
      GOTO 900                                                          ZEX00230
C                                                                       ZEX00231
C*****FOR STEP AND TEST                                                 ZEX00232
C*****FIND LOCATION OF VARIABLE                                         ZEX00233
15000 LOCV=IPROG(INREG-1)                                               ZEX00234
      IF(LOCV.GT.26) GOTO 15100                                         ZEX00235
C*****ONE-CHARACTER NAME                                                ZEX00236
      LOCV=DATA(LOCV)+1.5                                               ZEX00237
15100 LOCE1=IPROG(INREG-2)                                              ZEX00238
C*****INCREMENT                                                         ZEX00239
      DATA(LOCV)=DATA(LOCV)+DATA(LOCE1+2)                               ZEX00240
C*****CHECK STEP                                                        ZEX00241
      IF(DATA(LOCE1+2).GT.0.) GOTO 15200                                ZEX00242
      IF(DATA(LOCE1+2).LT.0.) GOTO 15300                                ZEX00243
C*****ZERO STEP                                                         ZEX00244
      NERROR=10                                                         ZEX00245
      GOTO 29008                                                        ZEX00246
C*****TEST FOR TRANSFER OUT OF LOOP -- E3 GT 0                          ZEX00247
15200 IF(DATA(LOCV).GT.DATA(LOCE1+1)) GOTO 15500                        ZEX00248
C*****CONTINUE LOOP                                                     ZEX00249
      INREG=INREG-4                                                     ZEX00250
      GOTO 900                                                          ZEX00251
C*****TEST FOR TRANSFER OUT OF LOOP -- E3 LT 0                          ZEX00252
15300 IF(DATA(LOCV).LT.DATA(LOCE1+1)) GOTO 15500                        ZEX00253
C*****CONTINUE LOOP                                                     ZEX00254
      INREG=INREG-4                                                     ZEX00255
      GOTO 900                                                          ZEX00256
C*****TRANSFER OUT OF LOOP                                              ZEX00257
15500 INREG=IPROG(INREG-3)                                              ZEX00258
      GOTO 900                                                          ZEX00259
C                                                                       ZEX00260
C*****INSERT ALPHA                                                      ZEX00261
16000 IF(NPRI.EQ.1) GOTO 16012                                          ZEX00262
      IF(NPRUS.LE.INEXT) GOTO 16013                                     ZEX00263
      INEXT=NPRUS                                                       ZEX00264
      GOTO 16013                                                        ZEX00265
16012 I1=INEXT                                                          ZEX00266
      I2=NPRUS                                                          ZEX00267
      CALL CHECK(I1,I2)                                                 ZEX00268
16013 IF(INEXT.LT.IWRIT) GOTO 16014                                     ZEX00269
      NX=1                                                              ZEX00270
      CALL PRILIN(NX)                                                   ZEX00271
16014 IF(NPR.EQ.1) GOTO 30100                                           ZEX00272
      MOP=IPROG(INREG-1)                                                ZEX00273
      INREG=INREG-1                                                     ZEX00274
30100 IPOS=INEXT                                                        ZEX00275
      DO 16500 I=1,MOP                                                  ZEX00276
      IF(NPR.EQ.1) GOTO 16002                                           ZEX00277
      LOC=INREG-I                                                       ZEX00278
      IX=IPROG(LOC)                                                     ZEX00279
16002 DO 16400 K=1,5                                                    ZEX00280
      IZ=IX-((IX/INTZEI)*INTZEI)                                        ZEX00281
      CARP(IPOS)=ALPH(IZ+1)                                             ZEX00282
      IPOS=IPOS+1                                                       ZEX00283
16400 IX=IX/INTZEI                                                      ZEX00284
16500 CONTINUE                                                          ZEX00285
      INREG=INREG-MOP-1                                                 ZEX00286
      IF(NPRI.EQ.1) INEXT=INEXT+IZONE                                   ZEX00287
      IF(NPRI.EQ.-1) INEXT=INEXT+5*MOP                                  ZEX00288
      IF(INEXT.GE.IWRIT) GOTO 18100                                     ZEX00289
      GOTO 900                                                          ZEX00290
C                                                                       ZEX00291
C*****INSERT ACC                                                        ZEX00292
17000 INREG=INREG-1                                                     ZEX00293
      IF(NPRI.EQ.1) GOTO 17010                                          ZEX00294
      IF(NPRUS.LE.INEXT) GOTO 17020                                     ZEX00295
      INEXT=NPRUS                                                       ZEX00296
      GOTO 17020                                                        ZEX00297
17010 I1=INEXT                                                          ZEX00298
      I2=NPRUS                                                          ZEX00299
      CALL CHECK(I1,I2)                                                 ZEX00300
17020 IF(INEXT.LT.IWRIT) GOTO 17205                                     ZEX00301
      NX=1                                                              ZEX00302
      CALL PRILIN(NX)                                                   ZEX00303
17205 INEXT=INEXT+IZONE                                                 ZEX00304
      CALL ZINSNO                                                       ZEX00305
      IF(INEXT.GE.IWRIT) GOTO 18100                                     ZEX00306
      GOTO 900                                                          ZEX00307
C                                                                       ZEX00308
C*****PRINT AND RESTORE                                                 ZEX00309
18000 INREG=INREG-1                                                     ZEX00310
      NPRI=1                                                            ZEX00311
18100 NX=3                                                              ZEX00312
      CALL PRILIN(NX)                                                   ZEX00313
      GOTO 900                                                          ZEX00314
C                                                                       ZEX00315
C*****DUMP PROGRAM AND DATA                                             ZEX00316
19000 INREG=INREG-1                                                     ZEX00317
19001 WRITE(IWC,19100)                                                  ZEX00318
19100 FORMAT(15H1 PROGRAM DUMP )                                        ZEX00319
      IF(NSTLST.EQ.0) GOTO 19600                                        ZEX00320
      DO 19500 I=1,NSTLST                                               ZEX00321
      WRITE(IWC,19200) LISTST(I),ISTLST(I)                              ZEX00322
19200 FORMAT(18H0 STATEMENT NUMBER,I8,21H  INTERNAL LOCATION =,I8)      ZEX00323
      IF(ISTLST(I).GT.NCELLP) GOTO 19500                                ZEX00324
      ITOP=ISTLST(I)                                                    ZEX00325
      IF(I.EQ.NSTLST) GOTO 19300                                        ZEX00326
      NX=I                                                              ZEX00327
19355 NX=NX+1                                                           ZEX00328
      IBOT=ISTLST(NX)+1                                                 ZEX00329
      IF(ISTLST(NX).GT.NCELLP) GOTO 19355                               ZEX00330
      GOTO 19350                                                        ZEX00331
19300 IBOT=NCELLP+1-NOLNP                                               ZEX00332
19350 IF(ITOP.GT.(IBOT+9)) GOTO 19400                                   ZEX00333
      ITEMS=ITOP-IBOT+1                                                 ZEX00334
      DO 19360 K=1,ITEMS                                                ZEX00335
      IXYZ=ITOP-K+1                                                     ZEX00336
19360 IPRT(K)=IPROG(IXYZ)                                               ZEX00337
      WRITE(IWC,19370) (IPRT(K),K=1,ITEMS)                              ZEX00338
19370 FORMAT(1H ,5X,10I11)                                              ZEX00339
      GOTO 19500                                                        ZEX00340
19400 DO 19450 K=1,10                                                   ZEX00341
      IXYZ=ITOP-K+1                                                     ZEX00342
19450 IPRT(K)=IPROG(IXYZ)                                               ZEX00343
      WRITE(IWC,19370) (IPRT(K),K=1,10)                                 ZEX00344
      ITOP=ITOP-10                                                      ZEX00345
      GOTO 19350                                                        ZEX00346
19500 CONTINUE                                                          ZEX00347
19600 ITOP=DATA(1)+.1-1.                                                ZEX00348
      WRITE(IWC,19650)                                                  ZEX00349
19650 FORMAT(12H0 CONSTANTS   )                                         ZEX00350
      WRITE(IWC,19655)                                                  ZEX00351
19655 FORMAT(24H0LOCATION        VALUE        )                         ZEX00352
      DO 19700 I=314,ITOP                                               ZEX00353
19700 WRITE(IWC,19750) I,DATA(I)                                        ZEX00354
19750 FORMAT(1H ,I8,F20.5)                                              ZEX00355
      WRITE(IWC,603)                                                    ZEX00356
      IF((IOP.LT.1).OR.(IOP.GT.ICODE)) GOTO 20001                       ZEX00357
      GOTO 900                                                          ZEX00358
C                                                                       ZEX00359
C*****PRINT ALL                                                         ZEX00360
C*****DUMP OUTPUT BUFFER                                                ZEX00361
20000 INREG=INREG-1                                                     ZEX00362
20001 NX=3                                                              ZEX00363
      CALL PRILIN(NX)                                                   ZEX00364
      WRITE(IWC,10100)                                                  ZEX00365
C*****UNSUBSCRIPTED SINGLE-LETTER VARIABLES                             ZEX00366
      DO 20100 I=1,26                                                   ZEX00367
      LOCN=DATA(I)+1.1                                                  ZEX00368
      VAL=DATA(LOCN)                                                    ZEX00369
      IF((VAL.LT.SMALL).AND.(VAL.GT.-SMALL)) GOTO 20100                 ZEX00370
      INEXT=16                                                          ZEX00371
      ACC=VAL                                                           ZEX00372
      CALL CLEAR(1,45)                                                  ZEX00373
      CALL ZINSNO                                                       ZEX00374
      WRITE(IWC,20050) ALPH(I),(CARP(KZ),KZ=1,15)                       ZEX00375
20050 FORMAT(1H , 41X, A1, 4H  =  , 15A1)                               ZEX00376
20100 CONTINUE                                                          ZEX00377
C                                                                       ZEX00378
C*****UNSUBSCRIPTED DOUBLE-LETTER VARIABLES                             ZEX00379
      DO 20300 I=1,26                                                   ZEX00380
      DO 20200 J=1,10                                                   ZEX00381
      JM=J-1                                                            ZEX00382
      LOCN=I+(26*JM)+53                                                 ZEX00383
      VAL=DATA(LOCN)                                                    ZEX00384
      IF((VAL.LT.SMALL).AND.(VAL.GT.-SMALL)) GOTO 20200                 ZEX00385
      INEXT=16                                                          ZEX00386
      ACC=VAL                                                           ZEX00387
      CALL CLEAR(1,45)                                                  ZEX00388
      CALL ZINSNO                                                       ZEX00389
      WRITE(IWC,20150) ALPH(I),JM,(CARP(KZ),KZ=1,15)                    ZEX00390
20150 FORMAT(1H ,41X,A1,I1,3H = ,15A1)                                  ZEX00391
20200 CONTINUE                                                          ZEX00392
20300 CONTINUE                                                          ZEX00393
C                                                                       ZEX00394
C*****SUBSCRIPTED VARIABLES                                             ZEX00395
      DO 20500 I=1,26                                                   ZEX00396
      LOCS=DATA(I)+.1                                                   ZEX00397
      LOCU=DATA(LOCS)+.1                                                ZEX00398
      LOCUM=0                                                           ZEX00399
      IF(DATA(I+27).NE.0.) LOCUM=(LOCU-LOCS-1)/INT(DATA(I+27))          ZEX00400
      WRITE(IWC,350) LOCS,LOCU,LOCUM,DATA(I+27),ALPH(I)                 ZEX00401
 350  FORMAT(6H LOCS=,I6,7H  LOCU=,I6,7H  ROWS=,I6,10H  COLUMNS=,F6.0,11ZEX00402
     1H  VARIABLE ,A1)                                                  ZEX00403
      IF(LOCU.EQ.(LOCS+2)) GOTO 20500                                   ZEX00404
      NUMCOL=DATA(I+27)+.1                                              ZEX00405
      LOCUM=LOCU-1                                                      ZEX00406
      LOCSP=LOCS+1                                                      ZEX00407
      DO 20400 K=LOCSP,LOCUM                                            ZEX00408
      VAL=DATA(K)                                                       ZEX00409
      IF((VAL.LT.SMALL).AND.(VAL.GT.-SMALL)) GOTO 20400                 ZEX00410
      ITEM=K-LOCSP                                                      ZEX00411
      NR=ITEM/NUMCOL                                                    ZEX00412
      NC=ITEM-(NR*NUMCOL)                                               ZEX00413
      INEXT=16                                                          ZEX00414
      ACC=VAL                                                           ZEX00415
      CALL CLEAR(1,45)                                                  ZEX00416
      CALL ZINSNO                                                       ZEX00417
      WRITE(IWC,20350) ALPH(I),ITEM,ALPH(I),NR,NC,(CARP(KZ),KZ=1,15),K  ZEX00418
20350 FORMAT(1H ,20X,A1,1H(,I4,5H) OR ,A1,1H(,I4,1H,,I4,3H) =,15A1,5X,5HZEX00419
     1DATA(,I5,1H))                                                     ZEX00420
20400 CONTINUE                                                          ZEX00421
20500 CONTINUE                                                          ZEX00422
      WRITE(IWC,10100)                                                  ZEX00423
      INEXT=1                                                           ZEX00424
      WRITE(IWC,20701)                                                  ZEX00425
20701 FORMAT(20X,31HALLOCATED FILES UNTIL PRINT ALL//20X,9HFILE-NAME,5X,ZEX00426
     120HINTERNAL FILE NUMBER,5X,5HBEGIN,5X,3HEND,10X,9HSENTENCES/)     ZEX00427
      DO 20702 I=1,MAXFIL                                               ZEX00428
      IF(NFILE(I,1).EQ.-1) GOTO 20702                                   ZEX00429
      IPOS=0                                                            ZEX00430
      IX=NFILE(I,1)                                                     ZEX00431
      DO 20703 K=1,5                                                    ZEX00432
      IZ=IX-((IX/INTZEI)*INTZEI)                                        ZEX00433
      IPOS=IPOS+1                                                       ZEX00434
      CARP(IPOS)=ALPH(IZ+1)                                             ZEX00435
      IX=IX/INTZEI                                                      ZEX00436
20703 CONTINUE                                                          ZEX00437
      LOCS=NFILE(I,2)+NFILE(I,3)-1                                      ZEX00438
      WRITE(IWC,20704) I,(CARP(K),K=1,5),NFILE(I,1),NFILE(I,2),LOCS,NFILZEX00439
     1E(I,3)                                                            ZEX00440
20704 FORMAT(15X,I2,5X,5A1,8X,I12,11X,I5,5X,I5,5X,I5)                   ZEX00441
20702 CONTINUE                                                          ZEX00442
      CALL CLEAR(1,140)                                                 ZEX00443
      IF((IOP.LT.1).OR.(IOP.GT.ICODE)) GOTO 909                         ZEX00444
      GOTO 900                                                          ZEX00445
C                                                                       ZEX00446
C*****EXECUTION OF RESTORE COMMAND                                      ZEX00447
22000 IF(IPROG(INREG-1).EQ.-22) GOTO 22100                              ZEX00448
      NEWPOS=ACC                                                        ZEX00449
      INREG=INREG-1                                                     ZEX00450
      GOTO 22300                                                        ZEX00451
22100 NEWPOS=IPROG(INREG-2)                                             ZEX00452
      INREG=INREG-3                                                     ZEX00453
22300 NSW=2                                                             ZEX00454
      CALL ZHOPPR(VALUE,NSTOP,NEWPOS,NSW)                               ZEX00455
      IF(NSTOP.NE.0) GOTO 22400                                         ZEX00456
      GOTO 900                                                          ZEX00457
C**** EXECUTION --COMPUTED GOTO                                         ZEX00458
27000 MIC=IPROG(INREG-1)                                                ZEX00459
      MIV=ACC                                                           ZEX00460
      IF((MIV.LT.1).OR.(MIV.GT.MIC)) GOTO 44001                         ZEX00461
      INREG=INREG-MIV                                                   ZEX00462
      GOTO 4000                                                         ZEX00463
C                                                                       ZEX00464
C*****LOAD ALPHANUMERIC CODE INTO PRINT POSITION BY NPR                 ZEX00465
30000 IX=ACC                                                            ZEX00466
      NPR=1                                                             ZEX00467
      MOP=1                                                             ZEX00468
      INREG=INREG+1                                                     ZEX00469
      GOTO 16000                                                        ZEX00470
C                                                                       ZEX00471
C**** EXECUTION OF COMPUTED GOSUB                                       ZEX00472
44000 MIC=IPROG(INREG-2)                                                ZEX00473
      MIV=ACC                                                           ZEX00474
      IF((MIV.LT.1).OR.(MIV.GT.MIC)) GOTO 44001                         ZEX00475
      NIRET=NIRET+1                                                     ZEX00476
      IF(NIRET.GT.20) GOTO 2000                                         ZEX00477
      IRET(NIRET)=INREG-3-MIC                                           ZEX00478
      INREG=INREG-MIV-1                                                 ZEX00479
      GOTO 4000                                                         ZEX00480
44001 NERROR=41                                                         ZEX00481
      I1=MIV                                                            ZEX00482
      GOTO 29008                                                        ZEX00483
C**** EXECUTION OF INPUT                                                ZEX00484
45000 NSW=3                                                             ZEX00485
      GOTO 7001                                                         ZEX00486
C**** GET IMAGE STATEMENT NUMBER                                        ZEX00487
62000 MOP=ACC                                                           ZEX00488
      IF(INEXT.LE.NPRUS) GOTO 62030                                     ZEX00489
      NPRUS=INEXT                                                       ZEX00490
62030 IF(MOP.EQ.MAXIMA) GOTO 62035                                      ZEX00491
      REWIND NIMAGE                                                     ZEX00492
      DO 62010 I=1,NSTLST                                               ZEX00493
      IF(MOP.EQ.LISTST(I)) GOTO 62020                                   ZEX00494
62010 CONTINUE                                                          ZEX00495
62050 I1=MOP                                                            ZEX00496
      NERROR=13                                                         ZEX00497
      GOTO 29008                                                        ZEX00498
62020 MIV=ISTLST(I)-NCELLP                                              ZEX00499
      DO 62025 I=1,MIV                                                  ZEX00500
      READ(NIMAGE,62040,END=62050) MAXIMA,CARD                          ZEX00501
C-*-*                                                                   ZEX00502
C*-*- CALL FOR A POSIBLE NON-COMPATIBLE ROUTINE -IFEOF(NIMAGE)-         ZEX00503
C*-*-     ----CHECK IT----       ----CHECK IT----                       ZEX00504
C-*-*                                                                   ZEX00505
C     IF(IFEOF(NIMAGE).EQ.-1) GOTO 62050                                ZEX00506
62040 FORMAT(I4,80A1)                                                   ZEX00507
62025 CONTINUE                                                          ZEX00508
      IF(MAXIMA.NE.MOP) GOTO 62050                                      ZEX00509
62035 NCARD=1                                                           ZEX00510
      INREG=INREG-1                                                     ZEX00511
      GOTO 900                                                          ZEX00512
C**** ALPHANUMERIC CONSTANT WITHIN PRINT USING-STATEMENT                ZEX00513
63000 INREG=INREG-1                                                     ZEX00514
      NCODE=2                                                           ZEX00515
      CALL ZIMAGE(NCODE,NSTOP)                                          ZEX00516
      IF(NSTOP.EQ.-1) GOTO 21000                                        ZEX00517
      GOTO 900                                                          ZEX00518
C**** NUMERIC VARIABLE OR EXPRESSION WITHIN PRINT USING                 ZEX00519
64000 INREG=INREG-1                                                     ZEX00520
      NCODE=1                                                           ZEX00521
      CALL ZIMAGE(NCODE,NSTOP)                                          ZEX00522
      IF(NSTOP.EQ.-1) GOTO 21000                                        ZEX00523
      GOTO 900                                                          ZEX00524
C**** END OF LINE WITHIN PRINT USING COMMAND                            ZEX00525
65000 INREG=INREG-1                                                     ZEX00526
      NPRI=1                                                            ZEX00527
      IF(IPROG(INREG+2).NE.-62) GOTO 65001                              ZEX00528
      NCODE=9                                                           ZEX00529
      CALL ZIMAGE(NCODE,NSTOP)                                          ZEX00530
      GOTO 900                                                          ZEX00531
65001 NX=2                                                              ZEX00532
      CALL PRILIN(NX)                                                   ZEX00533
      NCARD=1                                                           ZEX00534
      GOTO 900                                                          ZEX00535
C**** ALPHANUMERIC VARIABLE WITHIN PRINT USING COMMAND                  ZEX00536
66000 INREG=INREG-1                                                     ZEX00537
      NCODE=3                                                           ZEX00538
      CALL ZIMAGE(NCODE,NSTOP)                                          ZEX00539
      IF(NSTOP.EQ.-1) GOTO 21000                                        ZEX00540
      GOTO 900                                                          ZEX00541
70000 INREG=INREG-1                                                     ZEX00542
      NCODE=9                                                           ZEX00543
      CALL ZIMAGE(NCODE,NSTOP)                                          ZEX00544
      IF(NSTOP.EQ.-1) GOTO 21000                                        ZEX00545
      GOTO 900                                                          ZEX00546
C**** ALTER NPRI                                                        ZEX00547
C**** COMMA  NPRI=1                                                     ZEX00548
C**** PUCO  NPRI=-1                                                     ZEX00549
67000 IF(NPRI.EQ.1) GOTO 67005                                          ZEX00550
      NPRI=1                                                            ZEX00551
      I1=INEXT                                                          ZEX00552
      I2=INEXT                                                          ZEX00553
      IF(NPRUS.GT.INEXT) I2=NPRUS                                       ZEX00554
      CALL CHECK(I1,I2)                                                 ZEX00555
      GOTO 67005                                                        ZEX00556
69000 IF(NPRI.EQ.-1) GOTO 67005                                         ZEX00557
      DO 67002 I=1,125                                                  ZEX00558
      J=125-I+1                                                         ZEX00559
      IF(CARP(J).NE.BLANK) GOTO 67003                                   ZEX00560
67002 CONTINUE                                                          ZEX00561
      J=0                                                               ZEX00562
67003 INEXT=J+1                                                         ZEX00563
      NPRI=-1                                                           ZEX00564
67005 INREG=INREG-1                                                     ZEX00565
      GOTO 900                                                          ZEX00566
C**** COMMAND IS TAB                                                    ZEX00567
68000 IF(ACC.GE.0.) GOTO 68001                                          ZEX00568
      NERROR=39                                                         ZEX00569
      X1=ACC                                                            ZEX00570
      GOTO 29008                                                        ZEX00571
68001 ICC=INT(ACC+0.5)                                                  ZEX00572
      IF(ICC.EQ.0) ICC=1                                                ZEX00573
      IF(ICC.GT.IWRIT) GOTO 68002                                       ZEX00574
68006 IF(ICC.LT.INEXT) GOTO 68003                                       ZEX00575
      GOTO 68004                                                        ZEX00576
68003 IF(CARP(INEXT-1).NE.BLANK) GOTO 68005                             ZEX00577
      INEXT=INEXT-1                                                     ZEX00578
      GOTO 68006                                                        ZEX00579
68002 MIV=ICC/IWRIT                                                     ZEX00580
      ICC=ICC-IWRIT*MIV                                                 ZEX00581
68005 NX=1                                                              ZEX00582
      CALL PRILIN(NX)                                                   ZEX00583
68004 INEXT=ICC                                                         ZEX00584
      INREG=INREG-1                                                     ZEX00585
      NPRI=-1                                                           ZEX00586
      GOTO 900                                                          ZEX00587
C***** RETURN TO MAIN PROGRAMM                                          ZEX00588
C---- DAS FOLGENDE STATEMENT IST DURCH DIE FOLGENDE ANWEISUNG           ZEX00589
21000 RETURN                                                            ZEX00590
C---- ZU ERSETZEN,WENN NICHT IN *OVERLAY-TECHNIK* GEARBEITET WIRD,      ZEX00591
C---- WOBEI DIE ZAHL 21000 IN DER ERSTEN SPALTE BEGINNEN MUSS.          ZEX00592
C21000 CONTINUE                                                         ZEX00593
      END                                                               ZEX00594
