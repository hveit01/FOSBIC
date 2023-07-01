C     compared to source                                                ***
C*****MAIN ROUTINE - BASIC INTERPRETIVE COMPILER                        MAI00001
C---- DAS FOLGENDE STATEMENT IST ZU ENTFERNEN ODER DURCH EIN C IN DER   MAI00002
C---- 1.SPALTE UNWIRKSAM ZU MACHEN,WENN NICHT IN *OVERLAY-TECHNIK*      MAI00003
C---- GEARBEITET WIRD.                                                  MAI00004
C---- (SIEHE BEMERKUNGEN IM HAUPTELEMENT DER *OVERLAY-TECHNIK*)         MAI00005
C     PROGRAM COMPIL                                                    MAI00006
C**** MAIN --HAUPTPROGRAMM OHNE *OVERLAY-TECHNIK*                       MAI00007
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      MAI00008
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, MAI00009
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  MAI00010
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, MAI00011
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     MAI00012
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     MAI00013
     1NIFMAX,INTZEI                                                     MAI00014
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        MAI00015
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      MAI00016
     1IRET(20),XXX(4),NFILE(25,3)                                       MAI00017
      COMMON// ISTLST(340),LISTST(340)                                  MAI00018
      COMMON// DATAN(330)                                               MAIN0019
      COMMON// DATA(3700)                                               MAI00020
      DIMENSION IPROG(3700)                                             MAI00021
      EQUIVALENCE (DATA(1),IPROG(1))                                    MAI00022
C---- DAS FOLGENDE STATEMENT IST ZU ENTFERNEN ODER DURCH EIN C IN DER   MAI00023
C---- 1.SPALTE UNWIRKSAM ZU MACHEN,WENN NICHT IN *OVERLAY-TECHNIK*      MAI00024
C---- GEARBEITET WIRD.                                                  MAI00025
C     OVERLAY(1)                                                        MAI00026
C                                                                       MAI00027
C*****INITIALIZE                                                        MAI00028
   2  CALL ZINITL                                                       MAI00029
C                                                                       MAI00030
C*****HEAD PAGE                                                         MAI00031
      WRITE(IWC,45)                                                     MAI00032
 45   FORMAT(1H ,40X,54HTESTCOMPILER -- BASIC BWL 5 GIESSEN -- VERSION 6MAI00033
     1/76-04/)                                                          MAI00034
C                                                                       MAI00035
C*****READ A CARD                                                       MAI00036
C**** CLEAR CARD AND CARDT                                              MAI00037
 50   DO 57 I=1,80                                                      MAI00038
      CARD(I)=BLANK                                                     MAI00039
      CARDT(I)=BLANK                                                    MAI00040
 57   CONTINUE                                                          MAI00041
      MCOM=0                                                            MAI00042
 6100 READ(IRC,51) CARD                                                 MAI00043
C*-*-                                                                   MAI00044
C*-*- CALL FOR A POSIBLE NON-COMPATIBLE ROUTINE -IFEOF(IRC)-            MAI00045
C*-*-     ----CHECK IT----       ----CHECK IT----                       MAI00046
C*-*-                                                                   MAI00047
      IF(IFEOF(IRC).EQ.-1) GOTO 8210                                    MAI00048
 51   FORMAT(80A1)                                                      MAI00049
      IF(CARD(1).NE.ASTRSK) GOTO 3                                      MAI00050
      IF(MCOM.NE.1) WRITE(IWC,53) CARD                                  MAI00051
C----AUF DIE FOLGENDEN ZEILEN LN=0052-0059 WIRD IM TEXT BEZUG GENOMMENvvMAI00052
 53   FORMAT(1H0/////25X,20HINFORMATIONEN DURCH://25X,29HPROF.DR.OEC.PUBMAI00053
     1L.K.WEBER,M.S./25X,36HDIPL.-ING.,DIPL.-OEC.C.W.TUERSCHMANN/25X,41HMAI00054
     2PROFESSUR FUER BETRIEBSWIRTSCHAFTSLEHRE V/25X,17HLICHER STRASSE 74MAI00055
     3/25X,14HD-6300 GIESSEN/25X,27HFEDERAL REPUBLIC OF GERMANY////25X,3MAI00056
     42(1H*),17H PROGRAMMKENNUNG ,31(1H*)//25X,80A1//25X,80(1H*)//25X,84MAI00057
     5HBASIC TEXTBOOK = WEBER, TUERSCHMANN, BASIC LEHR- UND HANDBUCH. BEMAI00058
     6RN 1977. BAND 1 = A/25X,16(1H*),58X,10HBAND 2 = B/42X,38HWEBER, TU***
     7ERSCHMANN. FOSBIC. BERN 1977.,26X,3H= C)                          ***
      GOTO 2                                                            MAI00061
C                                                                       MAI00060
 3    WRITE(IWC,52) CARD                                                MAI00061
 52   FORMAT(20X,80A1)                                                  MAI00062
C**** READ THROUGH CARDS UNTIL ASTRSK IS FOUND IF MCOM=1                MAI00063
      IF(MCOM.EQ.1) GOTO 6100                                           MAI00064
C                                                                       MAI00065
C*****CHECK STORAGE                                                     MAI00066
      IF(INEXT.LT.INREG) GOTO 60                                        MAI00067
      NN=0                                                              MAI00068
      NERROR=1                                                          MAI00069
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                MAI00070
      NERRS = NERRS + 1                                                 MAI00071
      INREG=NCELLP                                                      MAI00072
C                                                                       MAI00073
C*****COMPRESS CARD                                                     MAI00074
 60   LNGCRP=0                                                          MAI00075
      NDOL=0                                                            MAI00076
      ITOT=1                                                            MAI00077
      DO 110 I=1,80                                                     MAI00078
      CARDP(I)=BLANK                                                    MAI00079
      IF(CARD(I).EQ.DDOPU) CARD(I)=DOPU                                 MAI00080
      IF(CARD(I).EQ.DQUOTE) CARD(I)=QUOTE                               MAI00081
      IF(ITOT.EQ.-1) GOTO 66                                            MAI00082
      IF(NDOL.EQ.1) GOTO 65                                             MAI00083
      IF(CARD(I).EQ.XNULL) NDOL=2                                       MAI00084
      IF(NDOL.EQ.2) GOTO 110                                            MAI00085
      IF(CARD(I).EQ.DOPU) NDOL=1                                        MAI00086
 66   IF(CARD(I).EQ.QUOTE) ITOT=-ITOT                                   MAI00087
      IF((CARD(I).EQ.BLANK).AND.(ITOT.EQ.1)) GOTO 110                   MAI00088
 65   LNGCRP=LNGCRP+1                                                   MAI00089
      CARDP(LNGCRP)=CARD(I)                                             MAI00090
 110  CONTINUE                                                          MAI00091
C                                                                       MAI00092
C*****CHECK FOR A BLANK CARD                                            MAI00093
      IF(LNGCRP.EQ.0) GOTO 50                                           MAI00094
C                                                                       MAI00095
C*****LOAD STATEMENT NUMBER                                             MAI00096
      ISTNO=0                                                           MAI00097
      DO 120 I=1,LNGCRP                                                 MAI00098
      CALL ZDIGIT(CARDP(I),J)                                           MAI00099
      IF(J.GT.10) GOTO 130                                              MAI00100
 120  ISTNO=(ISTNO*10)+(J-1)                                            MAI00101
 130  IBEGST=I                                                          MAI00102
      IF(IBEGST.GE.LNGCRP) GOTO 50                                      MAI00103
C                                                                       MAI00104
C**** CHECK FOR COMMENT                                                 MAI00105
      IF((CARDP(IBEGST+4).EQ.ALPH(5)).AND.(CARDP(IBEGST+5).EQ.ALPH(14)) MAI00106
     1.AND.(CARDP(IBEGST+6).EQ.ALPH(20))) GOTO 50                       MAI00107
      IF((CARDP(IBEGST).EQ.ALPH(18)).AND.(CARDP(IBEGST+1).EQ.ALPH(5))   MAI00108
     1.AND.(CARDP(IBEGST+2).EQ.ALPH(13))) GOTO 140                      MAI00109
      IF(ITOT.EQ.-1) GOTO 516                                           MAI00110
 140  IF(ISTNO.LE.ISTMAX) GOTO 200                                      MAI00111
      NERROR=2                                                          MAI00112
 9999 NN=0                                                              MAI00113
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                MAI00114
      NERRS=NERRS+1                                                     MAI00115
      GOTO 50                                                           MAI00116
C                                                                       MAI00117
C*****LOAD INTO LISTST                                                  MAI00118
 200  IF(ISTNO.EQ.0) GOTO 210                                           MAI00119
      NSTLST=NSTLST+1                                                   MAI00120
      IF(NSTLST.LT.NSTEND) GOTO 9000                                    MAI00121
      NN=0                                                              MAI00122
      NERROR=24                                                         MAI00123
      CALL COMERR(NERROR,NSTEND,I2,X1,X2,NN)                            MAI00124
      MCOM=1                                                            MAI00125
      GOTO 6100                                                         MAI00126
 9000 LISTST(NSTLST)=ISTNO                                              MAI00127
      ISTLST(NSTLST)=INREG                                              MAI00128
C                                                                       MAI00129
C*********************                                                  MAI00130
C*****TRANSLATE                                                         MAI00131
C**** SEARCH FOR AN IMAGE STATEMENT                                     MAI00132
 210  IF(CARDP(IBEGST).NE.DOPU) GOTO 215                                MAI00133
      IF(ISTNO.GT.0) GOTO 201                                           MAI00134
      NERROR=25                                                         MAI00135
      GOTO 9999                                                         MAI00136
 201  IBEGST=IBEGST+1                                                   MAI00137
      LOC=0                                                             MAI00138
      DO 203 I=IBEGST,80                                                MAI00139
      LOC=LOC+1                                                         MAI00140
      CARDT(LOC)=CARDP(I)                                               MAI00141
 203  CONTINUE                                                          MAI00142
      DO 206 I=1,80                                                     MAI00143
      LOC=80-I+1                                                        MAI00144
      IF(CARDT(LOC).EQ.DOPU) GOTO 202                                   MAI00145
      IF(CARDT(LOC).EQ.BLANK) GOTO 206                                  MAI00146
      CARDT(LOC+1)=DOPU                                                 MAI00147
      GOTO 202                                                          MAI00148
 206  CONTINUE                                                          MAI00149
 202  IF(NZIM.LE.MAXIMA) GOTO 205                                       MAI00150
      NERROR=26                                                         MAI00151
      GOTO 9999                                                         MAI00152
 205  ISTLST(NSTLST)=NZIM+NCELLP                                        MAI00153
      WRITE(NIMAGE,204) ISTNO,(CARDT(I),I=1,80)                         MAI00154
 204  FORMAT(I4,80A1)                                                   MAI00155
      NZIM=NZIM+1                                                       MAI00156
      GOTO 50                                                           MAI00157
C**** SEARCH FOR A FILE COMMAND                                         MAI00158
 215  NZ=0                                                              MAI00159
C**** COMMAND IS OPEN                                                   MAI00160
      IF((CARDP(IBEGST).EQ.ALPH(15)).AND.(CARDP(IBEGST+1).EQ.ALPH(16))) MAI00161
     1 NZ=1                                                             MAI00162
C**** COMMAND IS PUT                                                    MAI00163
      IF((CARDP(IBEGST+1).EQ.ALPH(21)).AND.(CARDP(IBEGST+2).EQ.ALPH(20))MAI00164
     1) NZ=2                                                            MAI00165
C**** COMMAND IS GET                                                    MAI00166
      IF((CARDP(IBEGST).EQ.ALPH(7)).AND.(CARDP(IBEGST+1).EQ.ALPH(5)))   MAI00167
     1 NZ=3                                                             MAI00168
C**** COMMAND IS RESET                                                  MAI00169
      IF((CARDP(IBEGST).EQ.ALPH(18)).AND.(CARDP(IBEGST+2).EQ.ALPH(19))  MAI00170
     1.AND.(CARDP(IBEGST+3).EQ.ALPH(5))) NZ=4                           MAI00171
C**** COMMAND IS CLOSE                                                  MAI00172
      IF((CARDP(IBEGST).EQ.ALPH(3)).AND.(CARDP(IBEGST+1).EQ.ALPH(12)))  MAI00173
     1 NZ=5                                                             MAI00174
C**** COMMAND IS COMMON-FILE                                            MAI00175
      IF((CARDP(IBEGST).EQ.ALPH(3)).AND.(CARDP(IBEGST+7).EQ.ALPH(6)))   MAI00176
     1 NZ=6                                                             MAI00177
      IF(CARDP(IBEGST+1).EQ.EQUALS) GOTO 213                            MAI00178
      IF(NZ.EQ.0) GOTO 213                                              MAI00179
C----AUF DIE FOLGENDEN ZEILEN LN=0183-0184 WIRD IM TEXT BEZUG GENOMMENvv***
      CALL ZFILE(NZ)                                                    MAI00180
      GOTO 50                                                           MAI00181
 213  IF((CARDP(IBEGST).NE.ALPH(13)).OR.(CARDP(IBEGST+1).NE.ALPH(1)).OR.MAI00182
     1(CARDP(IBEGST+2).NE.ALPH(20))) GOTO 212                           MAI00183
C----AUF DIE FOLGENDEN ZEILEN LN=0188-0189 WIRD IM TEXT BEZUG GENOMMENvv***
      CALL MATTRA                                                       MAI00184
      GOTO 50                                                           MAI00185
C***** COMMAND IS RESTORE                                               MAI00186
 212  IF((CARDP(IBEGST).NE.ALPH(18)).OR.(CARDP(IBEGST+1).NE.ALPH(5))    MAI00187
     1.OR.(CARDP(IBEGST+2).NE.ALPH(19)).OR.(CARDP(IBEGST+3).NE.ALPH(20))MAI00188
     2) GOTO 3212                                                       MAI00189
      IF(LNGCRP.LT.IBEGST+6) GOTO 3190                                  MAI00190
      IF(CARDP(IBEGST+7).NE.BLANK) GOTO 3170                            MAI00191
      FNUM=0.                                                           MAI00192
      GOTO 3150                                                         MAI00193
 3190 NERROR=3                                                          MAI00194
      GOTO 9999                                                         MAI00195
 3150 IPROG(INREG)=-22                                                  MAI00196
      IPROG(INREG-1)=-22                                                MAI00197
      IPROG(INREG-2)=FNUM                                               MAI00198
      INREG=INREG-3                                                     MAI00199
      GOTO 50                                                           MAI00200
 3170 NZ=NERRS                                                          MAI00201
      CALL ZTRANX(IBEGST+7,LNGCRP)                                      MAI00202
      IF(NERRS.GT.NZ) GOTO 3190                                         MAI00203
      IPROG(INREG)=-22                                                  MAI00204
      INREG=INREG-1                                                     MAI00205
      GOTO 50                                                           MAI00206
 3212 IF(CARDP(IBEGST).NE.ALPH(12)) GOTO 300                            MAI00207
C*****COMMAND IS LET                                                    MAI00208
      IF(CARDP(IBEGST+2).NE.ALPH(20)) GOTO 2000                         MAI00209
      IF(CARDP(IBEGST+1).NE.ALPH(5)) GOTO 2000                          MAI00210
C                                                                       MAI00211
C*****LOOK FOR EQUAL SIGN                                               MAI00212
      DO 220 LOC=IBEGST,LNGCRP                                          MAI00213
      IF(CARDP(LOC).EQ.EQUALS) GOTO 230                                 MAI00214
 220  CONTINUE                                                          MAI00215
      NERROR=4                                                          MAI00216
      GOTO 9999                                                         MAI00217
C                                                                       MAI00218
 230  IF((LOC.GT.IBEGST+3).AND.(LOC.LT.LNGCRP)) GOTO 240                MAI00219
      NERROR=5                                                          MAI00220
      GOTO 9999                                                         MAI00221
C                                                                       MAI00222
C*****SET UP EXPRESSION ON RIGHT                                        MAI00223
240   ITOT=0                                                            MAI00224
      IF(CARDP(LOC+1).NE.QUOTE) GOTO 241                                MAI00225
      ITOT=1                                                            MAI00226
      IPROG(INREG)=-1                                                   MAI00227
      IPROG(INREG-1)=2                                                  MAI00228
      IPROG(INREG-2)=-20                                                MAI00229
      CALL STRING(LOC+2,LNGCRP-1,IX)                                    MAI00230
      IPROG(INREG-3)=IX                                                 MAI00231
      INREG=INREG-4                                                     MAI00232
      GOTO 244                                                          MAI00233
 241  LNGXX=LNGCRP                                                      MAI00234
      IF((CARDP(LOC+2).EQ.DOLSGN).OR.(CARDP(LOC+3).EQ.DOLSGN)) ITOT=1   MAI00235
      CALL ZTRANX(LOC+1,LNGXX)                                          MAI00236
C*****CHECK FOR SUBSCRIPTED VARIABLE                                    MAI00237
 244  IF((CARDP(IBEGST+4).EQ.DOLSGN).OR.(CARDP(IBEGST+5).EQ.DOLSGN))    MAI00238
     1 ITOT=ITOT-1                                                      MAI00239
      IF(CARDP(LOC-1).NE.PARRT) GOTO 260                                MAI00240
C*****SUBSCRIPTED VARIABLE FOUND                                        MAI00241
      NZ=NERRS                                                          MAI00242
      CALL ZTRANX(IBEGST+3,LOC-1)                                       MAI00243
      IF(NERRS.GT.NZ) GOTO 50                                           MAI00244
      IF(IPROG(INREG+1).EQ.-8) GOTO 255                                 MAI00245
C*****ILLEGAL EXPRESSION ON LEFT                                        MAI00246
 245  NERROR=6                                                          MAI00247
      GOTO 9999                                                         MAI00248
C                                                                       MAI00249
C*****SUBSCRIPTED VARIABLE ON LEFT IS OK                                MAI00250
 255  IPROG(INREG+1)=-19                                                MAI00251
      GOTO 242                                                          MAI00252
C                                                                       MAI00253
C*****UNSUBSCRIPTED VARIABLE ON LEFT -- FIND LETTER                     MAI00254
 260  CALL ZALPH(CARDP(IBEGST+3),K)                                     MAI00255
C*****NON-ALPHA CHARACTER FOUND                                         MAI00256
      IF(K.GT.26) GOTO 245                                              MAI00257
      IF(LOC.EQ.IBEGST+4) GOTO 290                                      MAI00258
      IF(LOC.EQ.IBEGST+5) GOTO 275                                      MAI00259
      IF(LOC.EQ.IBEGST+6) GOTO 275                                      MAI00260
      GOTO 245                                                          MAI00261
C*****TWO-CHARACTER NAME FOUND                                          MAI00262
 275  IF(CARDP(IBEGST+4).EQ.DOLSGN) GOTO 290                            MAI00263
      CALL ZDIGIT(CARDP(IBEGST+4),L)                                    MAI00264
      IF(L.GT.10) GOTO 245                                              MAI00265
C                                                                       MAI00266
      IPROG(INREG)=-8                                                   MAI00267
      IPROG(INREG-1)=K+(26*(L-1))+53                                    MAI00268
      INREG=INREG-2                                                     MAI00269
 242  IF(ITOT.EQ.0) GOTO 50                                             MAI00270
      NN=1                                                              MAI00271
      NERROR=4                                                          MAI00272
      IF(ITOT.EQ.-1) NERROR=5                                           MAI00273
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                MAI00274
      GOTO 50                                                           MAI00275
C                                                                       MAI00276
C     ONE-CHARACTER NAME FOUND                                          MAI00277
 290  IPROG(INREG)=-9                                                   MAI00278
      IPROG(INREG-1)=K                                                  MAI00279
      INREG=INREG-2                                                     MAI00280      
      GOTO 242                                                          MAI00281
C                                                                       MAI00282
 300  IF((CARDP(IBEGST).NE.ALPH(18)).OR.(CARDP(IBEGST+2).NE.ALPH(1)))   MAI00283
     1 GOTO 305                                                         MAI00284
      IF(CARDP(IBEGST+1).NE.ALPH(5)) GOTO 2000                          MAI00285
C     COMMAND IS READ                                                   MAI00286
      IX=0                                                              MAI00287
      NZ=9                                                              MAI00288
      MCOM=-7                                                           MAI00289
      CALL ZLISTE(IBEGST+4,MCOM,IX,NZ)                                  MAI00290
      GOTO 50                                                           MAI00291
C**** COMMAND IS INPUT                                                  MAI00292
 305  IF((CARDP(IBEGST).NE.ALPH(9)).OR.(CARDP(IBEGST+1).NE.ALPH(14)))   MAI00293
     1 GOTO 400                                                         MAI00294
      IX=0                                                              MAI00295
      NZ=9                                                              MAI00296
      MCOM=-45                                                          MAI00297
      CALL ZLISTE(IBEGST+5,MCOM,IX,NZ)                                  MAI00298
      GOTO 50                                                           MAI00299
C                                                                       MAI00300
 400  IF((CARDP(IBEGST).NE.ALPH(16)).OR.(CARDP(IBEGST+1).NE.ALPH(1)))   MAI00301
     1 GOTO 500                                                         MAI00302
C*****COMMAND IS PAGE                                                   MAI00303
      IF(CARDP(IBEGST+3).NE.ALPH(5)) GOTO 2000                          MAI00304
      IPROG(INREG)=-13                                                  MAI00305
      INREG=INREG-1                                                     MAI00306
      GOTO 50                                                           MAI00307
C                                                                       MAI00308
 500  IF((CARDP(IBEGST).NE.ALPH(16)).OR.(CARDP(IBEGST+1).NE.ALPH(18)))  MAI00309
     1 GOTO 600                                                         MAI00310
C*****COMMAND IS PRINT                                                  MAI00311
      IF(CARDP(IBEGST+4).NE.ALPH(20)) GOTO 2000                         MAI00312
C*****CHECK FOR A LINE SKIP                                             MAI00313
      IF(LNGCRP.GT.IBEGST+4) GOTO 505                                   MAI00314
      IPROG(INREG)=-10                                                  MAI00315
      INREG=INREG-1                                                     MAI00316
      GOTO 50                                                           MAI00317
C                                                                       MAI00318
C*****CHECK FOR A "PRINT ALL"                                           MAI00319
 505  IF(LNGCRP.NE.(IBEGST+7)) GOTO 510                                 MAI00320
      IF (CARDP(IBEGST+5).NE.ALPH(1)) GOTO 510                          MAI00321
      IF (CARDP(IBEGST+6).NE.ALPH(12)) GOTO 510                         MAI00322
      IF (CARDP(IBEGST+7).NE.ALPH(12)) GOTO 510                         MAI00323
C*****COMMAND IS PRINT ALL                                              MAI00324
      IPROG(INREG)=-20                                                  MAI00325
      INREG=INREG-1                                                     MAI00326
      GOTO 50                                                           MAI00327
C                                                                       MAI00328
 516  NERROR=7                                                          MAI00329
      GOTO 9999                                                         MAI00330
C                                                                       MAI00331
C*****SET UP EXPRESSION                                                 MAI00332
 510  IQTLOC=0                                                          MAI00333
      IQT=0                                                             MAI00334
      NPRI=0                                                            MAI00335
      ICONT=NPRI                                                        MAI00336
      LOC=0                                                             MAI00337
      IF((CARDP(IBEGST+5).NE.ALPH(21)).OR.(CARDP(IBEGST+6).NE.ALPH(19)))MAI00338
     1 GOTO 519                                                         MAI00339
C**** COMMAND IS PRINT USING                                            MAI00340
C**** COMMAND CODES FOR STANDARD PRINT ARE                              MAI00341
C****    NUMERIC VARIABLE OR EXPRESSION=-17                             MAI00342
C****    ALPHANUMERIC VARIABLE=-30                                      MAI00343
C****    COMMA=-67                                                      MAI00344
C****    PUCO=-69                                                       MAI00345
C****    END OF LINE=-18                                                MAI00346
C****    ALPHANUMERIC CONSTANT=-16                                      MAI00347
C**** COMMAND CODES FOR PRINT USING STATEMENT ARE                       MAI00348
C****    IMAGE STATEMENT NUMBER=-62                                     MAI00349
C****    NUMERIC VARIABLE OR EXPRESSION=-64                             MAI00350
C****    ALPHANUMERIC VARIABLE=-66                                      MAI00351
C****    COMMA=-67                                                      MAI00352
C****    PUCO=-69                                                       MAI00353
C****    END OF LINE=-65                                                MAI00354
C****    ALPHANUMERIC CONSTANT=-63                                      MAI00355
      IBEGST=IBEGST+10                                                  MAI00356
      IF(IBEGST.LE.LNGCRP) GOTO 563                                     MAI00357
      NERROR=27                                                         MAI00358
      GOTO 9999                                                         MAI00359
 563  DO 561 I=IBEGST,LNGCRP                                            MAI00360
      IF(CARDP(I).EQ.PUCO) GOTO 562                                     MAI00361
      IF(CARDP(I).EQ.COMMA) GOTO 562                                    MAI00362
 561  CONTINUE                                                          MAI00363
      I=LNGCRP+1                                                        MAI00364
 562  CALL ZTRANX(IBEGST,I-1)                                           MAI00365
      IPROG(INREG)=-62                                                  MAI00366
      INREG=INREG-1                                                     MAI00367
      IQT=47                                                            MAI00368
      IQTLOC=36                                                         MAI00369
      IFR=I+1                                                           MAI00370
      IF(I.LT.LNGCRP) GOTO 525                                          MAI00371
      IF(I.GT.LNGCRP) GOTO 564                                          MAI00372
      IPROG(INREG)=-70                                                  MAI00373
      INREG=INREG-1                                                     MAI00374
      ICONT=-1                                                          MAI00375
      IF(CARDP(I).EQ.COMMA) ICONT=1                                     MAI00376
      LOC=LNGCRP                                                        MAI00377
      GOTO 525                                                          MAI00378
 564  IPROG(INREG)=-65                                                  MAI00379
      INREG=INREG-1                                                     MAI00380
      GOTO 50                                                           MAI00381
 519  IFR=IBEGST+5                                                      MAI00382
C*****LOOK FOR FIRST QUOTE OR FREE COMMA                                MAI00383
 525  ITOT=0                                                            MAI00384
      IF(NPRI.EQ.ICONT) GOTO 547                                        MAI00385
      IPROG(INREG)=-67                                                  MAI00386
      IF(ICONT.EQ.-1) IPROG(INREG)=-69                                  MAI00387
      INREG=INREG-1                                                     MAI00388
      NPRI=ICONT                                                        MAI00389
 547  NDOL=0                                                            MAI00390
      IF(LOC.GT.LNGCRP) GOTO 50                                         MAI00391
      IF((LOC.EQ.LNGCRP).AND.(CARDP(LOC).EQ.COMMA)) GOTO 50             MAI00392
      IF((LOC.EQ.LNGCRP).AND.(CARDP(LOC).EQ.PUCO)) GOTO 50              MAI00393
      DO 530 LOC=IFR,LNGCRP                                             MAI00394
      IF(CARDP(LOC).EQ.QUOTE) GOTO 545                                  MAI00395
      IF(CARDP(LOC).NE.DOLSGN) GOTO 531                                 MAI00396
      NDOL=1                                                            MAI00397
      GOTO 530                                                          MAI00398
 531  IF(CARDP(LOC).EQ.PARLFT) ITOT=ITOT+1                              MAI00399
      IF(CARDP(LOC).EQ.PARRT) ITOT=ITOT-1                               MAI00400
      IF((CARDP(LOC).EQ.COMMA).AND.(ITOT.EQ.0)) GOTO 540                MAI00401
      IF((CARDP(LOC).EQ.PUCO).AND.(ITOT.EQ.0)) GOTO 546                 MAI00402
 530  CONTINUE                                                          MAI00403
C*****REMAINDER OF CARD IS AN EXPRESSION                                MAI00404
      LOC=LNGCRP+1                                                      MAI00405
      IF(IFR.GT.LNGCRP) GOTO 535                                        MAI00406
      GOTO 541                                                          MAI00407
 535  IPROG(INREG)=-18-IQT                                              MAI00408
      INREG=INREG-1                                                     MAI00409
      GOTO 50                                                           MAI00410
 540  ICONT=1                                                           MAI00411
      GOTO 541                                                          MAI00412
 546  ICONT=-1                                                          MAI00413
C**** COMMAND IS PRINT TAB(EXPRESSION)                                  MAI00414
 541  IF((CARDP(IFR).NE.ALPH(20)).OR.(CARDP(IFR+1).NE.ALPH(1)).OR.(CARDPMAI00415
     1(IFR+2).NE.ALPH(2))) GOTO 542                                     MAI00416
      CALL ZTRANX(IFR+3,LOC-1)                                          MAI00417
      IPROG(INREG)=-68                                                  MAI00418
      ICONT=-1                                                          MAI00419
      IFR=LOC+1                                                         MAI00420
      GOTO 526                                                          MAI00421
 542  CALL ZTRANX(IFR,LOC-1)                                            MAI00422
      IPROG(INREG)=-17-IQT                                              MAI00423
      IF(NDOL.EQ.1) IPROG(INREG)=-30-IQTLOC                             MAI00424
C*****CHECK FOR FINAL COMMA                                             MAI00425
 526  INREG=INREG-1                                                     MAI00426
      IF(LOC.GT.LNGCRP) GOTO 535                                        MAI00427
      IF(LOC.EQ.LNGCRP) GOTO 525                                        MAI00428
C*****RESET IFR                                                         MAI00429
      IFR=LOC+1                                                         MAI00430
      GOTO 525                                                          MAI00431
C*****QUOTE FOUND                                                       MAI00432
 545  IF(LOC.EQ.IFR) GOTO 550                                           MAI00433
C*****EXPRESSION TRAPPED WITHOUT COMMA                                  MAI00434
      NN=1                                                              MAI00435
      NERROR=7                                                          MAI00436
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                MAI00437
      CALL ZTRANX(IFR,LOC-1)                                            MAI00438
      IPROG(INREG)=-17-IQT                                              MAI00439
      IF(NDOL.EQ.1) IPROG(INREG)=-30-IQTLOC                             MAI00440
      INREG=INREG-1                                                     MAI00441
C                                                                       MAI00442
C*****FIND NEXT QUOTE                                                   MAI00443
 550  IFR=LOC+1                                                         MAI00444
      DO 555 LOC=IFR,LNGCRP                                             MAI00445
      IF(CARDP(LOC).EQ.QUOTE) GOTO 560                                  MAI00446
 555  CONTINUE                                                          MAI00447
 560  ITO=LOC-1                                                         MAI00448
C                                                                       MAI00449
C*****SET UP AND LOCAL CHARACTERS                                       MAI00450
      IST=IFR                                                           MAI00451
 570  ICT=0                                                             MAI00452
      IPROG(INREG)=-16-IQT                                              MAI00453
      NDOL=INREG-1                                                      MAI00454
      INREG=INREG-1                                                     MAI00455
 575  ICT=ICT+1                                                         MAI00456
      IEND=IST+4                                                        MAI00457
      IF(IEND.GT.ITO) GOTO 585                                          MAI00458
C*****LOAD FIVE CHARACTERS                                              MAI00459
      CALL STRING(IST,IEND,IX)                                          MAI00460
C*****INSERT IN IPROG                                                   MAI00461
      LOCN=INREG-ICT                                                    MAI00462
      IPROG(LOCN)=IX                                                    MAI00463
C*****STEP START                                                        MAI00464
      IST=IST+5                                                         MAI00465
C*****CHECK FOR END                                                     MAI00466
      IF(IST.GT.ITO) GOTO 591                                           MAI00467
C*****CHECK FOR THREE WORDS COMPLETED                                   MAI00468
      IF(ICT.LT.3) GOTO 575                                             MAI00469
C*****THIS IS THE THIRD WORD LOADED                                     MAI00470
      INREG=INREG-4                                                     MAI00471
      IPROG(NDOL)=ICT                                                   MAI00472
      GOTO 570                                                          MAI00473
C                                                                       MAI00474
C*****PARTIAL WORD FOUND                                                MAI00475
 585  CALL STRING(IST,ITO,IX)                                           MAI00476
      LOCN=INREG-ICT                                                    MAI00477
      IPROG(LOCN)=IX                                                    MAI00478
C                                                                       MAI00479
C*****ADD BLANK WORDS IF NECESSARY                                      MAI00480
 591  IF(ICT.GE.3) GOTO 595                                             MAI00481
      IF(CARDP(LOC+1).EQ.PUCO) GOTO 595                                 MAI00482
      ICT=ICT+1                                                         MAI00483
      LOCN=INREG-ICT                                                    MAI00484
      IPROG(LOCN)=200590357                                             MAI00485
      GOTO 591                                                          MAI00486
C                                                                       MAI00487
C*****COMPLETE                                                          MAI00488
 595  INREG=INREG-ICT-1                                                 MAI00489
      IPROG(NDOL)=ICT                                                   MAI00490
      ICONT=1                                                           MAI00491
      IF(CARDP(LOC+1).EQ.PUCO) ICONT=-1                                 MAI00492
C                                                                       MAI00493
C*****CHECK TO SEE IF QUOTE WAS LAST CHARACTER                          MAI00494
      IF(LOC.EQ.LNGCRP) GOTO 535                                        MAI00495
C*****SEE IF QUOTE IS FOLLOWED BY A COMMA OR A PUCO                     MAI00496
      IF((CARDP(LOC+1).EQ.PUCO).OR.(CARDP(LOC+1).EQ.COMMA)) GOTO 597    MAI00497
      NN=1                                                              MAI00498
      NERROR=7                                                          MAI00499
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                MAI00500
C*****COMMA MISSING                                                     MAI00501
      IFR=LOC+1                                                         MAI00502
      LOC=LOC+1                                                         MAI00503
      GOTO 525                                                          MAI00504
C*****COMMA PRESENT                                                     MAI00505
 597  IFR=LOC+2                                                         MAI00506
      LOC=LOC+2                                                         MAI00507
      GOTO 525                                                          MAI00508
C                                                                       MAI00509
 600  IF((CARDP(IBEGST).NE.ALPH(7)).OR.(CARDP(IBEGSZ+2).NE.ALPH(20)))   MAI00510
     1 GOTO 650                                                         MAI00511
C*****COMMAND IS GOTO                                                   MAI00512
      IF(CARDP(IBEGST+1).NE.ALPH(15)) GOTO 2000                         MAI00513
      IST=IBEGST+4                                                      MAI00514
C**** CHECK FOR COMPUTED GOTO OR GOSUB                                  MAI00515
 1130 DO 611 IX=IST,LNGCRP                                              MAI00516
      IF((CARDP(IX).EQ.ALPH(15)).AND.(CARDP(IX+1).EQ.ALPH(14))) GOTO 612MAI00517
      IF((CARDP(IX).EQ.ALPH(15)).AND.(CARDP(IX+1).EQ.ALPH( 6))) GOTO 612MAI00518
 611  CONTINUE                                                          MAI00519
      GOTO 610                                                          MAI00520
C**** COMPUTED GOTO *VERSION* GOTO(STATEMENTNUMBERS)ON/OF(EXPRESSION)   MAI00521
C**** COMPUTED GOSUB *VERSION* GOSUB(STATEMENTNUMBERS)ON/OF(EXPRESSION) MAI00522
 612  CALL ZTRANX(IX+2,LNGCRP)                                          MAI00523
 653  IPROG(INREG)=-27                                                  MAI00524
      INREG=INREG-2                                                     MAI00525
      MIC=INREG+1                                                       MAI00526
      MIV=0                                                             MAI00527
      LNGCRP=IX-1                                                       MAI00528
 615  DO 613 IX=IST,LNGCRP                                              MAI00529
      IF(CARDP(IX).EQ.COMMA) GOTO 614                                   MAI00530
 613  CONTINUE                                                          MAI00531
 614  CALL ZCONVN(IST,IX-1,FNUM)                                        MAI00532
      IF(FNUM.LT.1.) GOTO 690                                           MAI00533
      IF(FNUM.GT.FLOAT(ISTMAX)) GOTO 690                                MAI00534
      MIV=MIV+1                                                         MAI00535
      IPROG(INREG)=FNUM                                                 MAI00536
      INREG=INREG-1                                                     MAI00537
      IST=IX+1                                                          MAI00538
      IF(IST.LE.LNGCRP) GOTO 615                                        MAI00539
      IPROG(MIC)=MIV                                                    MAI00540
      GOTO 50                                                           MAI00541
C*****LOAD STATEMENT NUMBER                                             MAI00542
 610  IF(IST.GT.LNGCRP) GOTO 690                                        MAI00543
      LNGXX=LNGCRP                                                      MAI00544
      CALL ZCONVN(IST,LNGXX,STNO)                                       MAI00545
      ISTTO=STNO+.01                                                    MAI00546
      IF(ISTTO.LT.1) GOTO 690                                           MAI00547
      IF(ISTTO.GT.ISTMAX) GOTO 690                                      MAI00548
C*****COMMAND IS LEGAL                                                  MAI00549
      IPROG(INREG)=-3                                                   MAI00550
      IPROG(INREG-1)=ISTTO                                              MAI00551
      INREG=INREG-2                                                     MAI00552
      GOTO 50                                                           MAI00553
C*****ILLEGAL STATEMENT NUMBER                                          MAI00554
 690  NERROR=8                                                          MAI00555
      GOTO 9999                                                         MAI00556
C                                                                       MAI00557
 650  IF((CARDP(IBEGST).EQ.ALPH(15)).AND.(CARDP(IBEGST+1).EQ.ALPH(6)))  MAI00558
     1 GOTO 654                                                         MAI00559
      IF((CARDP(IBEGST).NE.ALPH(15)).OR.(CARDP(IBEGST+1).NE.ALPH(14)))  MAI00560
     1 GOTO 700                                                         MAI00561
C**** COMPUTED GOTO *VERSION* ON/OF(EXPRESSION)GOTO(STATEMENTNUMBERS)   MAI00562
C**** COMPUTED GOSUB *VERSION* ON/OF(EXPRESSION)GOSUB(STATEMENTNUMBERS) MAI00563
  654 IST=IBEGST+2                                                      MAI00564
      DO 651 IX=IST,LNGCRP                                              MAI00565
      IF((CARDP(IX).EQ.ALPH(7)).AND.(CARDP(IX+1).EQ.ALPH(15)).AND.      MAI00566
     1(CARDP(IX+2).EQ.ALPH(20))) GOTO 652                               MAI00567
      IF((CARDP(IX).EQ.ALPH(7)).AND.(CARDP(IX+1).EQ.ALPH(15)).AND.      MAI00568
     1(CARDP(IX+2).EQ.ALPH(19))) GOTO 655                               MAI00569
 651  CONTINUE                                                          MAI00570
      GOTO 2000                                                         MAI00571
 652  IQT=4                                                             MAI00572
 656  CALL ZTRANX(IST,IX-1)                                             MAI00573
      IST=IX+IQT                                                        MAI00574
      IX=LNGCRP+1                                                       MAI00575
      GOTO 653                                                          MAI00576
 655  IPROG(INREG)=-44                                                  MAI00577
      INREG=INREG-1                                                     MAI00578
      IQT=5                                                             MAI00579
      GOTO 656                                                          MAI00580
C                                                                       MAI00581
 700  IF(CARDP(IBEGST).NE.ALPH(9)) GOTO 800                             MAI00582
C*****COMMAND IS IF                                                     MAI00583
      IF(CARDP(IBEGST+1).NE.ALPH(6)) GOTO 2000                          MAI00584
      IF(LNGCRP.GT.IBEGST+7) GOTO 720                                   MAI00585
      NERROR=9                                                          MAI00586
      GOTO 9999                                                         MAI00587
C                                                                       MAI00588
 720  LM1=LNGCRP-1                                                      MAI00589
      IBEG=IBEGST+2                                                     MAI00590
C*****LOOK FOR AN EQUAL SIGN                                            MAI00591
      ITOT=1                                                            MAI00592
      DO 725 LOC=IBEG,LM1                                               MAI00593
      IF(CARDP(LOC).EQ.QUOTE) ITOT=-ITOT                                MAI00594
      IF(ITOT.EQ.-1) GOTO 725                                           MAI00595
      IF(CARDP(LOC).EQ.EQUALS) GOTO 726                                 MAI00596
      IF(CARDP(LOC).EQ.VLESS) GOTO 728                                  MAI00597
      IF(CARDP(LOC).EQ.VGREAT) GOTO 724                                 MAI00598
 725  CONTINUE                                                          MAI00599
      GOTO 730                                                          MAI00600
 726  ICOMP=5                                                           MAI00601
      GOTO 727                                                          MAI00602
 724  IF(CARDP(LOC+1).NE.EQUALS) GOTO 723                               MAI00603
      ICOMP=2                                                           MAI00604
      GOTO 760                                                          MAI00605
 723  ICOMP=1                                                           MAI00606
      GOTO 727                                                          MAI00607
 728  IF(CARDP(LOC+1).NE.EQUALS) GOTO 722                               MAI00608
      ICOMP=4                                                           MAI00609
      GOTO 760                                                          MAI00610
 722  IF(CARDP(LOC+1).NE.VGREAT) GOTO 721                               MAI00611
      ICOMP=6                                                           MAI00612
      GOTO 760                                                          MAI00613
 721  ICOMP=3                                                           MAI00614
 727  IEND1=LOC-1                                                       MAI00615
      IBEG2=LOC+1                                                       MAI00616
      GOTO 765                                                          MAI00617
C*****LOOK FOR TWO-CHARACTER OPERATOR                                   MAI00618
 730  DO 750 LOC=IBEG,LM1                                               MAI00619
      IF(CARDP(LOC).EQ.QUOTE) ITOT=-ITOT                                MAI00620
      IF(ITOT.EQ.-1) GOTO 750                                           MAI00621
      CALL ZALPH(CARDP(LOC),K1)                                         MAI00622
      IF (K1.GT.26) GOTO 750                                            MAI00623
      CALL ZALPH(CARDP(LOC+1),K2)                                       MAI00624
      IF(K2.GT.26) GOTO 750                                             MAI00625
      CALL ZALPH(CARDP(LOC+2),K3)                                       MAI00626
      CALL ZALPH(CARDP(LOC+3),K4)                                       MAI00627
      ICOMP=0                                                           MAI00628
      IF((K1.EQ.7).AND.(K2.EQ.20)) ICOMP=1                              MAI00629
      IF((K1.EQ.12).AND.(K2.EQ.20)) ICOMP=3                             MAI00630
      IF((K1.EQ.5 ).AND.(K2.EQ.17)) ICOMP=5                             MAI00631
      IF(ICOMP.GT.0) GOTO 760                                           MAI00632
      IF(K3.NE.17) GOTO 756                                             MAI00633
      IF(K4.GT.26) GOTO 756                                             MAI00634
      K1=K2                                                             MAI00635
      K2=K3                                                             MAI00636
 756  IF((K1.EQ.7).AND.(K2.EQ.5)) ICOMP=2                               MAI00637
      IF((K1.EQ.12).AND.(K2.EQ.5)) ICOMP=4                              MAI00638
      IF((K1.EQ.14).AND.(K2.EQ.5)) ICOMP=6                              MAI00639
      IF(ICOMP.GT.0) GOTO 760                                           MAI00640
 750  CONTINUE                                                          MAI00641
      NERROR=10                                                         MAI00642
      GOTO 9999                                                         MAI00643
C                                                                       MAI00644
 760  IEND1=LOC-1                                                       MAI00645
      IBEG2=LOC+2                                                       MAI00646
 765  IF(IBEG2.GT.LM1) GOTO 767                                         MAI00647
      DO 766 LOC=IBEG2,LM1                                              MAI00648
      IF (CARDP(LOC).EQ.QUOTE) ITOT=-ITOT                               MAI00649
      IF(ITOT.EQ.-1) GOTO 766                                           MAI00650
      IF((CARDP(LOC).EQ.ALPH(7)).AND.(CARDP(LOC+1).EQ.ALPH(15)))GOTO 780MAI00651
      IF((CARDP(LOC).EQ.ALPH(20)).AND.(CARDP(LOC+1).EQ.ALPH(8)))GOTO 780MAI00652
 766  CONTINUE                                                          MAI00653
 767  NERROR=11                                                         MAI00654
      GOTO 9999                                                         MAI00655
C                                                                       MAI00656
 780  IEND2=LOC-1                                                       MAI00657
      ISTST=LOC+4                                                       MAI00658
C*****SET UP FIRST EXPRESSION AND STORE                                 MAI00659
      MCOM=1                                                            MAI00660
      IF(CARDP(IBEG).NE.QUOTE) GOTO 781                                 MAI00661
      CALL STRING(IBEG+1,IEND1-1,IX)                                    MAI00662
 784  IPROG(INREG-3)=IX                                                 MAI00663
      IPROG(INREG)=-1                                                   MAI00664
      IPROG(INREG-1)=2                                                  MAI00665
      IPROG(INREG-2)=-20                                                MAI00666
      INREG=INREG-4                                                     MAI00667
      GOTO 786                                                          MAI00668
 781  CALL ZTRANX(IBEG,IEND1)                                           MAI00669
 786  IPROG(INREG)=-14                                                  MAI00670
      IPROG(INREG-1)=MCOM                                               MAI00671
      INREG=INREG-2                                                     MAI00672
C*****SET UP SECOND EXPRESSION AND STORE                                MAI00673
      IF(MCOM.EQ.2) GOTO 787                                            MAI00674
      MCOM=2                                                            MAI00675
      IF(CARDP(IBEG2).NE.QUOTE) GOTO 782                                MAI00676
      CALL STRING(IBEG2+1,IEND2-1,IX)                                   MAI00677
      GOTO 784                                                          MAI00678
 782  CALL ZTRANX(IBEG2,IEND2)                                          MAI00679
      GOTO 786                                                          MAI00680
C******SET UP COMPARISON                                                MAI00681
 787  IPROG(INREG)=-6                                                   MAI00682
      IPROG(INREG-1)=ICOMP                                              MAI00683
      INREG=INREG-2                                                     MAI00684
C*****FIND STATEMENT NUMBER                                             MAI00685
      IF(ISTST.GT.LNGCRP) GOTO 785                                      MAI00686
      LNGXX=LNGCRP                                                      MAI00687
      CALL ZCONVN(ISTST,LNGXX,FNUM)                                     MAI00688
      IF (FNUM.GT.FLOAT(ISTMAX)) GOTO 690                               MAI00689
      IF (FNUM.GT.0.) GOTO 795                                          MAI00690
 785  NERROR=12                                                         MAI00691
      GOTO 9999                                                         MAI00692
C*****SET UP TRANSFER                                                   MAI00693
 795  IPROG(INREG)=-3                                                   MAI00694
      IPROG(INREG-1)=FNUM+.5                                            MAI00695
      INREG=INREG-2                                                     MAI00696
      GOTO 50                                                           MAI00697
C                                                                       MAI00698
 800  IF(CARDP(IBEGST).NE.ALPH(6)) GOTO 900                             MAI00699
C*****COMMAND IS FOR                                                    MAI00700
      IF(CARDP(IBEGST+2).NE.ALPH(18)) GOTO 2000                         MAI00701
      IF(CARDP(IBEGST+1).NE.ALPH(15)) GOTO 2000                         MAI00702
      IF(LNGCRP.GT.IBEGST+5) GOTO 820                                   MAI00703
 810  NERROR=13                                                         MAI00704
      GOTO 9999                                                         MAI00705
C                                                                       MAI00706
C*****LOOK FOR EQUAL SIGN                                               MAI00707
 820  DO 825 LOCE=IBEGST,LNGCRP                                         MAI00708
      IF (CARDP(LOCE).EQ.EQUALS) GOTO 835                               MAI00709
 825  CONTINUE                                                          MAI00710
C*****NONE FOUND                                                        MAI00711
      NERROR=14                                                         MAI00712
      GOTO 9999                                                         MAI00713
C                                                                       MAI00714
C*****FIND VARIABLE NUMBER                                              MAI00715
 835  IF(LOCE.EQ.IBEGST+5) GOTO 860                                     MAI00716
      IF(LOCE.EQ.IBEGST+4) GOTO 850                                     MAI00717
 840  NERROR=15                                                         MAI00718
      GOTO 9999                                                         MAI00719
C                                                                       MAI00720
C*****ONE-CHARACTER VARIABLE                                            MAI00721
 850  CALL ZALPH(CARDP(LOCE-1),IV)                                      MAI00722
      IF(IV.LE.26) GOTO 875                                             MAI00723
      GOTO 840                                                          MAI00724
C                                                                       MAI00725
C*****TWO-CHARACTER VARIABLE                                            MAI00726
 860  CALL ZALPH(CARDP(LOCE-2),K)                                       MAI00727
      IF(K.GT.26) GOTO 840                                              MAI00728
      CALL ZDIGIT(CARDP(LOCE-1),L)                                      MAI00729
      IF(L.GT.10) GOTO 840                                              MAI00730
      IV=K+(26*(L-1))+53                                                MAI00731
C                                                                       MAI00732
 875  IBEG1=LOCE+1                                                      MAI00733
      LM1=LNGCRP-1                                                      MAI00734
C*****LOOK FOR TO                                                       MAI00735
      IF(IBEG1.GT.LM1) GOTO 881                                         MAI00736
      DO 880 LOCT=IBEG1,LM1                                             MAI00737
      IF((CARDP(LOCT).EQ.ALPH(20)).AND.(CARDP(LOCT+1).EQ.ALPH(15)))     MAI00738
     1 GOTO 885                                                         MAI00739
 880  CONTINUE                                                          MAI00740
 881  NERROR=16                                                         MAI00741
      GOTO 9999                                                         MAI00742
C                                                                       MAI00743
 885  IEND1=LOCT-1                                                      MAI00744
      IBEG2=LOCT+2                                                      MAI00745
C*****LOOK FOR STEP                                                     MAI00746
      IF(IBEG.GT.LM1) GOTO 887                                          MAI00747
      DO 886 LOCS=IBEG2,LM1                                             MAI00748
      IF((CARDP(LOCS).EQ.ALPH(19)).AND.(CARDP(LOCS+1).EQ.ALPH(20)))     MAI00749
     1 GOTO 888                                                         MAI00750
 886  CONTINUE                                                          MAI00751
 887  IEND2=LNGCRP                                                      MAI00752
      IBEG3=0                                                           MAI00753
      GOTO 890                                                          MAI00754
 888  IEND2=LOCS-1                                                      MAI00755
      IBEG3=LOCS+4                                                      MAI00756
C                                                                       MAI00757
C*****STORE EXPRESSION 1                                                MAI00758
 890  IF(IBEG1.GT.IEND1) GOTO 810                                       MAI00759
      CALL ZTRANX(IBEG1,IEND1)                                          MAI00760
      IPROG(INREG)=-8                                                   MAI00761
      ILOC=INEXT                                                        MAI00762
      INEXT=INEXT+3                                                     MAI00763
      IPROG(INREG-1)=ILOC                                               MAI00764
      INREG=INREG-2                                                     MAI00765
C*****STORE EXPRESSION 2                                                MAI00766
      IF(IBEG2.GT.IEND2) GOTO 810                                       MAI00767
      CALL ZTRANX(IBEG2,IEND2)                                          MAI00768
      IPROG(INREG)=-8                                                   MAI00769
      IPROG(INREG-1)=ILOC+1                                             MAI00770
      INREG=INREG-2                                                     MAI00771
C*****STORE EXPRESSION 3                                                MAI00772
      IF(IBEG3.EQ.0) GOTO 895                                           MAI00773
      IF(IBEG3.GT.LNGCRP) GOTO 810                                      MAI00774
      LNGXX=LNGCRP                                                      MAI00775
      CALL ZTRANX(IBEG3,LNGXX)                                          MAI00776
      IPROG(INREG)=-8                                                   MAI00777
      IPROG(INREG-1)=ILOC+2                                             MAI00778
      INREG=INREG-2                                                     MAI00779
      GOTO 896                                                          MAI00780
 895  DATA(ILOC+2)=1.                                                   MAI00781
C                                                                       MAI00782
C*****SET UP INITIAL VALUE STORE                                        MAI00783
 896  IPROG(INREG)=-1                                                   MAI00784
      IPROG(INREG-1)=3                                                  MAI00785
      IPROG(INREG-2)=ILOC                                               MAI00786
      IPROG(INREG-3)=ILOC+2                                             MAI00787
      IPROG(INREG-4)=-2                                                 MAI00788
      INREG=INREG-5                                                     MAI00789
      IF(IV.GT.26) IPROG(INREG)=-8                                      MAI00790
      IF(IV.LE.26) IPROG(INREG)=-9                                      MAI00791
      IPROG(INREG-1)=IV                                                 MAI00792
      INREG=INREG-2                                                     MAI00793
C*****RECORD VARIABLE AND LOCATION FOR                                  MAI00794
      NIFOR=NIFOR+1                                                     MAI00795
      IF(NIFOR.GT.NIFMAX) GOTO 898                                      MAI00796
      IFOR(NIFOR,1)=IV                                                  MAI00797
      IFOR(NIFOR,2)=INREG                                               MAI00798
C                                                                       MAI00799
C*****WRITE FOR STATEMENT                                               MAI00800
      IPROG(INREG)=-15                                                  MAI00801
      IPROG(INREG-1)=IV                                                 MAI00802
      IPROG(INREG-2)=ILOC                                               MAI00803
      INREG=INREG-4                                                     MAI00804
      GOTO 50                                                           MAI00805
 898  NERROR=55                                                         MAI00806
      GOTO 9999                                                         MAI00807
C                                                                       MAI00808
 900  IF((CARDP(IBEGST).NE.ALPH(14)).OR.(CARDP(IBEGST+1).NE.ALPH(5)))   MAI00809
     1 GOTO 1000                                                        MAI00810
C*****COMMAND IS NEXT                                                   MAI00811
C*****FIND VARIABLE IN STACK                                            MAI00812
      IF(NIFOR.GT.0) GOTO 920                                           MAI00813
C*****NONE IN STACK                                                     MAI00814
      NERROR=17                                                         MAI00815
      GOTO 9999                                                         MAI00816
C*****EXTRACT INFORMATION                                               MAI00817
 920  IV=IFOR(NIFOR,1)                                                  MAI00818
      LOC=IFOR(NIFOR,2)                                                 MAI00819
      NIFOR=NIFOR-1                                                     MAI00820
C*****FIND VARIABLE NAME                                                MAI00821
      IF(IV.GT.26) GOTO 930                                             MAI00822
      CH1=ALPH(IV)                                                      MAI00823
      CH2=BLANK                                                         MAI00824
      GOTO 940                                                          MAI00825
 930  L=((IV-54)/26)+1                                                  MAI00826
      K=(IV-53)-(26*(L-1))                                              MAI00827
      CH1=ALPH(K)                                                       MAI00828
      CH2=DIGIT(L)                                                      MAI00829
C*****CHECK VARIABLE IN NEXT STATEMENT                                  MAI00830
 940  IF(LNGCRP.EQ.IBEGST+4) GOTO 970                                   MAI00831
      IF(LNGCRP.EQ.IBEGST+5) GOTO 980                                   MAI00832
 950  NERROR=1                                                          MAI00833
      NN=1                                                              MAI00834
      CALL COMERR(NERROR,I1,I2,CH1,CH2,NN)                              MAI00835
      GOTO 990                                                          MAI00836
C                                                                       MAI00837
 970  IF(CARDP(IBEGST+4).EQ.CH1) GOTO 990                               MAI00838
      GOTO 950                                                          MAI00839
C                                                                       MAI00840
 980  IF((CARDP(IBEGST+4).EQ.CH1).AND.(CARDP(IBEGST+5).EQ.CH2))GOTO990  MAI00841
      GOTO 950                                                          MAI00842
C                                                                       MAI00843
C*****INSERT NEXT LOCATION IN THE ASSOCIATED FOR COMMAND                MAI00844
 990  IPROG(LOC-3)=INREG-2                                              MAI00845
C*****SET UP TRANSFER                                                   MAI00846
      IPROG(INREG)=-4                                                   MAI00847
      IPROG(INREG-1)=LOC                                                MAI00848
      IPROG(INREG-2)=-12                                                MAI00849
      INREG=INREG-3                                                     MAI00850
      GOTO 50                                                           MAI00851
C                                                                       MAI00852
 1000 IF(CARDP(IBEGST).NE.ALPH(19)) GOTO 1100                           MAI00853
C*****COMMAND IS STOP                                                   MAI00854
      IF(CARDP(IBEGST+1).NE.ALPH(20)) GOTO 2000                         MAI00855
      IF(CARDP(IBEGST+2).NE.ALPH(15)) GOTO 2000                         MAI00856
      IPROG(INREG)=-11                                                  MAI00857
      INREG=INREG-1                                                     MAI00858
      GOTO 50                                                           MAI00859
C                                                                       MAI00860
 1100 IF((CARDP(IBEGST).NE.ALPH(7)).OR.(CARDP(IBEGST+2).NE.ALPH(19)))   MAI00861
     1 GOTO 1200                                                        MAI00862
C*****COMMAND IS GOSUB                                                  MAI00863
      IF (CARDP(IBEGST+4).NE.ALPH(2)) GOTO 2000                         MAI00864
      IF (CARDP(IBEGST+1).NE.ALPH(15)) GOTO 2000                        MAI00865
      IST=IBEGST+5                                                      MAI00866
      DO 1110 I=IST,LNGCRP                                              MAI00867
      IF(CARDP(I).EQ.ALPH(15)) GOTO 1120                                MAI00868
 1110 CONTINUE                                                          MAI00869
      IPROG(INREG)=-2                                                   MAI00870
      INREG=INREG-1                                                     MAI00871
      GOTO 610                                                          MAI00872
C*****COMPUTED GOSUB-STATEMENT NUMBERS-ON(OF)-EXPRESSION-               MAI00873
 1120 IPROG(INREG)=-44                                                  MAI00874
      INREG=INREG-1                                                     MAI00875
      GOTO 1130                                                         MAI00876
C                                                                       MAI00877
 1200  IF((CARDP(IBEGST).NE.ALPH(18)).OR.(CARDP(IBEGST+2).NE.ALPH(20))) MAI00878
     1 GOTO 1300                                                        MAI00879
C*****COMMAND IS RETURN                                                 MAI00880
      IF(CARDP(IBEGST+5).NE.ALPH(14)) GOTO 2000                         MAI00881
      IF(CARDP(IBEGST+1).NE.ALPH(5)) GOTO 2000                          MAI00882
      IPROG(INREG)=-5                                                   MAI00883
      INREG=INREG-1                                                     MAI00884
      GOTO 50                                                           MAI00885
C                                                                       MAI00886
 1300 IF((CARDP(IBEGST).NE.ALPH(4)).OR.(CARDP(IBEGST+1).NE.ALPH(9)))    MAI00887
     1 GOTO 1400                                                        MAI00888
C*****COMMAND IS DIM                                                    MAI00889
      IF(CARDP(IBEGST+2).NE.ALPH(13)) GOTO 2000                         MAI00890
C*****INSERT NO-OP COMMAND                                              MAI00891
      IPROG(INREG)=-12                                                  MAI00892
      INREG=INREG-1                                                     MAI00893
C                                                                       MAI00894
C*****START                                                             MAI00895
      IST=IBEGST+3                                                      MAI00896
 1310 IF(IST.GE.LNGCRP) GOTO 1390                                       MAI00897
C                                                                       MAI00898
C*****CHECK LEFT PARENTHESIS                                            MAI00899
      IF(CARDP(IST+1).NE.DOLSGN) GOTO 1315                              MAI00900
      CARDP(IST+1)=CARDP(IST)                                           MAI00901
      IST=IST+1                                                         MAI00902
      GOTO 1310                                                         MAI00903
 1315 IF(CARDP(IST+1).NE.PARLFT) GOTO 1390                              MAI00904
C*****LOOK FOR RIGHT PARENTHESIS                                        MAI00905
      DO 1320 IRT=IST,LNGCRP                                            MAI00906
      IF(CARDP(IRT).EQ.PARRT) GOTO 1330                                 MAI00907
 1320 CONTINUE                                                          MAI00908
C*****NONE FOUND                                                        MAI00909
      GOTO 1390                                                         MAI00910
C                                                                       MAI00911
C*****FIND VARIABLE                                                     MAI00912
 1330 CALL ZALPH(CARDP(IST),JV)                                         MAI00913
C*****ILLEGAL VARIABLE                                                  MAI00914
      IF(JV.GT.26) GOTO 1390                                            MAI00915
C                                                                       MAI00916
C*****CHECK FOR COMMA                                                   MAI00917
      DO 1360 IC=IST,IRT                                                MAI00918
      IF(CARDP(IC).EQ.COMMA) GOTO 1370                                  MAI00919
 1360 CONTINUE                                                          MAI00920
C                                                                       MAI00921
C*****NONE FOUND                                                        MAI00922
      CALL ZCONVN(IST+2,IRT-1,SIZE)                                     MAI00923
      IF(SIZE.LT.0.) GOTO 1390                                          MAI00924
      IF(MERKER(JV,1).EQ.0) GOTO 1365                                   MAI00925
      NERROR=2                                                          MAI00926
      NN=1                                                              MAI00927
      CALL COMERR(NERROR,I1,I2,ALPH(JV),X2,NN)                          MAI00928
 1365 MERKER(JV,1)=SIZE                                                 MAI00929
      MERKER(JV,2)=0                                                    MAI00930
      ILNG=SIZE+2.1                                                     MAI00931
      GOTO 1380                                                         MAI00932
C                                                                       MAI00933
C*****COMMA FOUND                                                       MAI00934
 1370 CALL ZCONVN(IST+2,IC-1,ROWS)                                      MAI00935
      IF(ROWS.LT.0.) GOTO 1390                                          MAI00936
      IF(MERKER(JV,1).EQ.0) GOTO 1375                                   MAI00937
      NERROR=2                                                          MAI00938
      NN=1                                                              MAI00939
      CALL COMERR(NERROR,I1,I2,ALPH(JV),X2,NN)                          MAI00940
 1375 CALL ZCONVN(IC+1,IRT-1,COLS)                                      MAI00941
      IF(COLS.LT.0.) GOTO 1390                                          MAI00942
      MERKER(JV,2)=COLS                                                 MAI00943
      MERKER(JV,1)=ROWS                                                 MAI00944
      DATA(JV+27)=COLS+1.                                               MAI00945
      ILNG=(ROWS+1.)*(COLS+1.)+1.1                                      MAI00946
C                                                                       MAI00947
C*****ALTER ADDRESS                                                     MAI00948
 1380 IPROG(JV)=ILNG                                                    MAI00949
C                                                                       MAI00950
C*****CHECK FOR COMPLETION                                              MAI00951
      IF(IRT.EQ.LNGCRP) GOTO 50                                         MAI00952
C                                                                       MAI00953
C*****TAKE NEXT CASE                                                    MAI00954
      IST=IRT+2                                                         MAI00955
      GOTO 1310                                                         MAI00956
C                                                                       MAI00957
C*****ERROR FOUND                                                       MAI00958
 1390 NERROR=18                                                         MAI00959
      GOTO 9999                                                         MAI00960
C                                                                       MAI00961
 1400 IF((CARDP(IBEGST).NE.ALPH(18)).OR.(CARDP(IBEGST+2).NE.ALPH(13)))  MAI00962
     1 GOTO 1500                                                        MAI00963
      IF(CARDP(IBEGST+1).NE.ALPH(5)) GOTO 2000                          MAI00964
C*****COMMAND IS REM                                                    MAI00965
      IPROG(INREG)=-12                                                  MAI00966
      INREG=INREG-1                                                     MAI00967
      GOTO 50                                                           MAI00968
C                                                                       MAI00969
 1500 IF((CARDP(IBEGST).NE.ALPH(4)).OR.(CARDP(IBEGST+1).NE.ALPH(21)))   MAI00970
     1 GOTO 1600                                                        MAI00971
C*****COMMAND IS DUMP                                                   MAI00972
      IF(CARDP(IBEGST+3).NE.ALPH(16)) GOTO 2000                         MAI00973
      IPROG(INREG)=-19                                                  MAI00974
      INREG=INREG-1                                                     MAI00975
      GOTO 50                                                           MAI00976
C                                                                       MAI00977
C*****CHECK FOR AN END COMMAND                                          MAI00978
 1600 IF((CARDP(IBEGST).EQ.ALPH(5)).AND.(CARDP(IBEGST+2).EQ.ALPH(4)))   MAI00979
     1 GOTO 3000                                                        MAI00980
C                                                                       MAI00981
      IF((CARDP(IBEGST).NE.ALPH(4)).OR.(CARDP(IBEGST+2).NE.ALPH(20)))   MAI00982
     1 GOTO 2000                                                        MAI00983
C*****COMMAND IS DATA                                                   MAI00984
      IF(CARDP(IBEGST+1).NE.ALPH(1)) GOTO 2000                          MAI00985
      IFR=IBEGST+4                                                      MAI00986
C                                                                       MAI00987
C*****GET NUMBERS                                                       MAI00988
      CALL ZHOPPR(VALUE,NSTOP,IFR,0)                                    MAI00989
      IF(NSTOP.EQ.1) GOTO 1736                                          MAI00990
      IF(NSTOP.EQ.2) GOTO 1735                                          MAI00991
      IF(NUMBUF.GT.0) GOTO 1740                                         MAI00992
      GOTO 1737                                                         MAI00993
 1735 NERROR=19                                                         MAI00994
      GOTO 9999                                                         MAI00995
 1737 NERROR=3                                                          MAI00996
      NN=1                                                              MAI00997
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                MAI00998
      GOTO 50                                                           MAI00999
 1736 NERROR=30                                                         MAI01000
      GOTO 9999                                                         MAI01001
C                                                                       MAI01002
 1740 IST=NEXTDT+1                                                      MAI01003
      IEND=NEXTDT+NUMBUF                                                MAI01004
      IF(IEND.LT.NCELLD) GOTO 1750                                      MAI01005
C*****TOO MUCH DATA                                                     MAI01006
      IX=IEND-NCELLD                                                    MAI01007
      NERROR=20                                                         MAI01008
      I1=IX                                                             MAI01009
      GOTO 9999                                                         MAI01010
C                                                                       MAI01011
C*****LOAD DATA                                                         MAI01012
 1750 DO 1760 IX=IST,IEND                                               MAI01013
      LOC=IX-NEXTDT                                                     MAI01014
 1760 DATAN(IX)=BUFFER(LOC)                                             MAI01015
      NEXTDT=IEND                                                       MAI01016
      GOTO 50                                                           MAI01017
C                                                                       MAI01018
C                                                                       MAI01019
C*****TRY THE COMMAND AS A LET STATEMENT                                MAI01020
 2000 DO 2001 LOC=IBEGST,LNGCRP                                         MAI01021
      IF(CARDP(LOC).EQ.QUOTE) GOTO 2003                                 MAI01022
      IF(CARDP(LOC).EQ.EQUALS) GOTO 2002                                MAI01023
 2001 CONTINUE                                                          MAI01024
      GOTO 2003                                                         MAI01025
 2002 IBEGST=IBEGST-3                                                   MAI01026
      GOTO 230                                                          MAI01027
C*****ILLEGAL COMMAND                                                   MAI01028
 2003 NERROR=21                                                         MAI01029
      GOTO 9999                                                         MAI01030
C                                                                       MAI01031
C*****END CARD REACHED                                                  MAI01032
 3000 IF (CARDP(IBEGST+1).NE.ALPH(14)) GOTO 2000                        MAI01033
C*****LIST DATA AFTER END STATEMENT                                     MAI01034
      IF((CARDP(IBEGST+3).NE.ALPH(12)).OR.(CARDP(IBEGST+6).NE.ALPH(20)))MAI01035
     1 GOTO 3010                                                        MAI01036
      REWIND 999                                                        MAI01037
      WRITE(IWC,3012)                                                   MAI01038
 3012 FORMAT(//20X,32HLIST OF DATA AFTER END STATEMENT//)               MAI01039
 3011 READ(IRC,51) CARD                                                 MAI01040
     IF(IFEOF(IRC).EQ.-1) GOTO 3020                                     MAI01041
C*-*-                                                                   MAI01042
C*-*- CALL FOR A POSIBLE NON-COMPATIBLE ROUTINE -IFEOF(IRC)-            MAI01043
C*-*-     ----CHECK IT----       ----CHECK IT----                       MAI01044
C*-*-                                                                   MAI01045
      WRITE(999,51) CARD                                                MAI01046
      WRITE(IWC,52) CARD                                                MAI01047
      IF(CARD(1).NE.ASTRSK) GOTO 3011                                   MAI01048
 3020 IRC=999                                                           MAI01049
      REWIND 999                                                        MAI01050
 3010 IPROG(INREG)=-11                                                  MAI01051
C                                                                       MAI01052
C*****CHECK FOR DUPLICATE STATEMENT NUMBERS                             MAI01053
      IF(NSTLST.LE.1) GOTO 5000                                         MAI01054
      NM1=NSTLST-1                                                      MAI01055
      DO 3500 I=1,NM1                                                   MAI01056
      IP=I+1                                                            MAI01057
      DO 3400 J=IP,NSTLST                                               MAI01058
      IF (LISTST(I).EQ.LISTST(J)) GOTO 3450                             MAI01059
 3400 CONTINUE                                                          MAI01060
      GOTO 3500                                                         MAI01061
 3450 NERROR=22                                                         MAI01062
      NN=0                                                              MAI01063
      CALL COMERR(NERROR,LISTST(I),I2,X1,X2,NN)                         MAI01064
      NERRS=NERRS+1                                                     MAI01065
 3500 CONTINUE                                                          MAI01066
C                                                                       MAI01067
C*****CHECK FOR IMBALANCE BETWEEN FOR AND NEXT COMMANDS                 MAI01068
 5000 IF(NIFOR.EQ.0) GOTO 6000                                          MAI01069
      NERRS=NERRS+NIFOR                                                 MAI01070
      NN=0                                                              MAI01071
      NERROR=23                                                         MAI01072
      CALL COMERR(NERROR,NIFOR,I2,X1,X2,NN)                             MAI01073
C                                                                       MAI01074
C*****SEE IF PROGRAM IS ACCEPTABLE                                      MAI01075
 6000 IF(NERRS.EQ.0) GOTO 7000                                          MAI01076
C                                                                       MAI01077
C*****PROGRAM IS NOT ACCEPTABLE                                         MAI01078
      WRITE(IWC,6001) NERRS                                             MAI01079
 6001 FORMAT(1H0,26H****** SORRY BUT THERE ARE,I5,25H ERRORS I CANNOT OVMAI01080
     1ERCOME)                                                           MAI01081
      WRITE(IWC,6002)                                                   MAI01082
 6002 FORMAT(1H0,7X,21HBETTER LUCK NEXT TIME///)                        MAI01083
C                                                                       MAI01084
      MCOM=1                                                            MAI01085
      GOTO 6100                                                         MAI01086
C                                                                       MAI01087
C                                                                       MAI01088
C*****PROGRAM IS ACCEPTABLE                                             MAI01089
 7000 WRITE(IWC,7250)                                                   MAI01090
 7250 FORMAT(1H0,19(1H*),38H EVERYTHING SEEMS OK -- LET'S GO AHEAD)     ***
C                                                                       MAI01093
C*****SET UP ADDRESSES FOR SUBSCRIPTED VARIABLES                        MAI01094
      DO 8100 I=2,26                                                    MAI01095
 8100 IPROG(I)=IPROG(I)+IPROG(I-1)                                      MAI01096
      DO 8150 I=1,26                                                    MAI01097
      J=28-I                                                            MAI01098
 8150 DATA(J)=INEXT+IPROG(J-1)                                          MAI01099
      DATA(1)=INEXT                                                     MAI01100
C                                                                       MAI01101
C*****CHECK FOR OVERLAP                                                 MAI01102
      ILSTD=DATA(27)                                                    MAI01103
      IF(ILSTD.LT.INREG) GOTO 8190                                      MAI01104
      IDIFF=ILSTD-INREG                                                 MAI01105
      WRITE(IWC,8250) IDIFF                                             MAI01106
 8250 FORMAT(1H0,58H**** YOU ARE TOO GREEDY -- YOUR PROGRAM PLUS DATA REMAI01107
     1QUIRES,I10,35H MORE LOCATIONS THAN I CAN GIVE YOU)                MAI01108
      WRITE(IWC,6002)                                                   MAI01109
      MCOM=1                                                            MAI01110
      GOTO 6100                                                         MAI01111
C                                                                       MAI01112
C*****ENTER UPPER BOUNDS                                                MAI01113
 8190 PERS=FLOAT(ILSTD+NCELLP-INREG)*100./FLOAT(NCELLP)                 MAI01114
      PERS=FLOAT(ILSTD+NCELLP-INREG)*100./FLOAT(NCELLP)                 MAI01115
      PERD=FLOAT(NEXTDT)*100./FLOAT(NSTEND)                             MAI01116
      PERST=FLOAT(NSTLST)*100./FLOAT(NSTEND)                            MAI01117
      WRITE(IWC,8260) PERS,PERD,PERST                                   MAI01118
 8260 FORMAT(1H0,20X,33HPERCENT OF AVAILABLE STORAGE USED,12X,F9.3/21X,3***     
     18HPERCENT OF AVAILABLE DATA STORAGE USED,7X,F9.3/21X,45HPERCENT OF***     
     1 AVAILABLE NUMBERED STATEMENTS USED,F9.3)                         ***     
      DO 8200 I=2,27                                                    MAI01122
      ILOC=DATA(I-1)+0.1                                                MAI01123
 8200 DATA(ILOC)=DATA(I)                                                MAI01124
C                                                                       MAI01125
C*****BEGIN EXECUTION                                                   MAI01126
C     CALL ZEXEC                                                        MAI01127
C     MCOM=1                                                            MAI01128
C     GOTO 6100                                                         MAI01129
C---- DAS FOLGENDE STATEMENT IST ZU ENTFERNEN ODER DURCH EIN C IN DER   MAI01130
C---- 1.SPALTE UNWIRKSAM ZU MACHEN,WENN NICHT IN *OVERLAY-TECHNIK*      MAI01131
C---- GEARBEITET WIRD.IN DEN VORAUSGEHENDEN DREI STATEMENTS IST DAS C INMAI01132
C---- DER 1.SPALTE ZU ENTFERNEN.                                        MAI01133
      GOTO 8211                                                         MAI01134
 8210 STOP                                                              MAI01135
C---- DAS FOLGENDE STATEMENT IST ZU ENTFERNEN ODER DURCH EIN C IN DER   MAI01136
C---- 1.SPALTE UNWIRKSAM ZU MACHEN,WENN NICHT IN *OVERLAY-TECHNIK*      MAI01137
C---- GEARBEITET WIRD.                                                  MAI01138
 8211 CONTINUE                                                          MAI01139
      END                                                               MAI01140
