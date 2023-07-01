      SUBROUTINE ZTRANX(IFR,ITO)                                        ZTR00001
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZTR00002
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, ZTR00003
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZTR00004
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZTR00005
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZTR00006
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZTR00007
     1NIFMAX,INTZEI                                                     ZTR00008
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZTR00009
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZTR00010
     1IRET(20),XXX(4),NFILE(25,3)                                       ZTR00011
      COMMON// ISTLST(340),LISTST(340)                                  ZTR00012
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZTR00014
      DIMENSION IPROG(3700)                                             ZTR00015
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZTR00016
      DIMENSION ITRAN(80)                                               ZTR00017
      DIMENSION IHOLD(80)                                               ZTR00018
      DIMENSION ZMK(80)                                                 ZTR00019
      DIMENSION NTYPER(12)                                              ZTR00020
C*****SUBROUTINE TO TRANSLATE EXPRESSIONS INTO PSEUDO-MACHINE CODE      ZTR00021
C*****CLEAR ITRAN                                                       ZTR00022
C*****DELETE ALL DOLLAR SIGNS                                           ZTR00023
      IDOL=IFR                                                          ZTR00024
      IF((CARDP(IFR+1).NE.DOLSGN).AND.(CARDP(IFR+2).NE.DOLSGN)) GOTO 95 ZTR00025
      CALL ZDIGIT(CARDP(IFR),I)                                         ZTR00026
      IF(I.LE.10) GOTO 1100                                             ZTR00027
 95   DO 116 I=IFR,ITO                                                  ZTR00028
      IF(CARDP(I).EQ.DOLSGN) GOTO 116                                   ZTR00029
      ZMK(IDOL)=CARDP(I)                                                ZTR00030
      IDOL=IDOL+1                                                       ZTR00031
 116  CONTINUE                                                          ZTR00032
      ITO=IDOL-1                                                        ZTR00033
      DO 117 I=IFR,ITO                                                  ZTR00034
      CARDP(I)=ZMK(I)                                                   ZTR00035
 117  CONTINUE                                                          ZTR00036
      DO 110 I=IFR,ITO                                                  ZTR00037
 110  ITRAN(I)=0                                                        ZTR00038
      NERR=0                                                            ZTR00039
C                                                                       ZTR00040
C*****CLEAR NTYPER                                                      ZTR00041
      DO 115 I=1,12                                                     ZTR00042
 115  NTYPER(I)=0                                                       ZTR00043
C                                                                       ZTR00044
C*****CLEAR ZMK                                                         ZTR00045
      DO 120 I=1,80                                                     ZTR00046
 120  ZMK(I)=BLANK                                                      ZTR00047
C                                                                       ZTR00048
C****PROCESS ALL PARENTHESES                                            ZTR00049
      DO 300 I=IFR,ITO                                                  ZTR00050
      IF(CARDP(I).NE.PARLFT) GOTO 300                                   ZTR00051
C*****LEFT PARENTHESIS FOUND -- MARK IT                                 ZTR00052
      ITRAN(I)=-20                                                      ZTR00053
      IF(I.EQ.ITO) GOTO 290                                             ZTR00054
C*****FIND RIGHT PARENTHESIS                                            ZTR00055
      ITOT=0                                                            ZTR00056
      DO 150 IRT=I,ITO                                                  ZTR00057
      IF(CARDP(IRT).EQ.PARLFT) ITOT=ITOT+1                              ZTR00058
      IF(CARDP(IRT).EQ.PARRT) ITOT=ITOT-1                               ZTR00059
      IF(ITOT.EQ.0) GOTO 155                                            ZTR00060
 150  CONTINUE                                                          ZTR00061
      GOTO 290                                                          ZTR00062
C*****RIGHT PARENTHESIS IS IN POSITIO  IRT                              ZTR00063
 155  IF(I.EQ.IFR) GOTO 200                                             ZTR00064
C*****SEE IF PRECEEDING CHARACTER IS ALPHABETIC                         ZTR00065
      CALL ZALPH(CARDP(I-1),IM1)                                        ZTR00066
      IF(IM1.LE.26) GOTO 210                                            ZTR00067
C*****THESE PARENTHESES ARE NOT FOR A SUBSCRIPT -- MARK RIGHT ONE       ZTR00068
 200  ITRAN(IRT)=-21                                                    ZTR00069
      GOTO 300                                                          ZTR00070
C                                                                       ZTR00071
C*****PRECEEDING CHARACTERS IS ALPHABETIC -- CHECK PREVIOUS ONE         ZTR00072
 210  IF(I.EQ.IFR+1) GOTO 220                                           ZTR00073
      CALL ZALPH(CARDP(I-2),IM2)                                        ZTR00074
      IF(IM2.LE.26) GOTO 224                                            ZTR00075
C*****THIS IS A SUBSCRIPT EXPRESSION - MARK RIGHT PARENTHESIS AND INSERTZTR00076
 220  ITRAN(IRT)=-8                                                     ZTR00077
      ITRAN(I-1)=IM1                                                    ZTR00078
C*****RESERVE SPACE IF NOT ALREADY RESERVED                             ZTR00079
      IF(IPROG(IM1).NE.2) GOTO 221                                      ZTR00080
      IF(MERKER(IM1,1).NE.0) GOTO 221                                   ZTR00081
      IPROG(IM1)=122                                                    ZTR00082
      MERKER(IM1,1)=10                                                  ZTR00083
      MERKER(IM1,2)=10                                                  ZTR00084
C*****FIND AND MARK COMMA IF PRESENT                                    ZTR00085
 221  ITOT=0                                                            ZTR00086
      DO 222 ICOM=I,IRT                                                 ZTR00087
      IF(CARDP(ICOM).EQ.PARLFT) ITOT=ITOT+1                             ZTR00088
      IF(CARDP(ICOM).EQ.PARRT) ITOT=ITOT-1                              ZTR00089
      IF((CARDP(ICOM).EQ.COMMA).AND.(ITOT.EQ.1)) GOTO 223               ZTR00090
 222  CONTINUE                                                          ZTR00091
C*****NONE FOUND                                                        ZTR00092
      GOTO 300                                                          ZTR00093
C*****MARK COMMA WITH VARIABLE NUMBER                                   ZTR00094
 223  ITRAN(ICOM)=IM1                                                   ZTR00095
      GOTO 300                                                          ZTR00096
C                                                                       ZTR00097
C*****THIS IS A FUNCTION -- CHECK FIRST CHARACTER                       ZTR00098
 224  IF(I.EQ.IFR+2) GOTO 230                                           ZTR00099
      CALL ZALPH(CARDP(I-3),IM3)                                        ZTR00100
      IF(IM3.LE.26) GOTO 240                                            ZTR00101
C*****TWO-CHARACTER NAME FOUND                                          ZTR00102
 230  NTYPER(1)=1                                                       ZTR00103
      ZMK(I-2)=ALPH(1)                                                  ZTR00104
      ZMK(I-1)=ALPH(1)                                                  ZTR00105
      ITRAN(I-1)=-50                                                    ZTR00106
      ITRAN(I-2)=-50                                                    ZTR00107
      ITRAN(IRT)=-21                                                    ZTR00108
      GOTO 300                                                          ZTR00109
C                                                                       ZTR00110
C*****FUNCTION FOUND -- MARK NAME                                       ZTR00111
 240  ITRAN(I-1)=-50                                                    ZTR00112
      ITRAN(I-2)=-50                                                    ZTR00113
      ITRAN(I-3)=-50                                                    ZTR00114
C*****FIND FUNCTION                                                     ZTR00115
      IF(IM3.NE.3) GOTO 241                                             ZTR00116
      ITRAN(IRT)=-10                                                    ZTR00117
      GOTO 300                                                          ZTR00118
 241  IF(IM3.NE.20) GOTO 242                                            ZTR00119
      ITRAN(IRT)=-11                                                    ZTR00120
      GOTO 300                                                          ZTR00121
 242  IF(IM3.NE.5) GOTO 243                                             ZTR00122
      ITRAN(IRT)=-13                                                    ZTR00123
      GOTO 300                                                          ZTR00124
 243  IF(IM3.NE.12) GOTO 244                                            ZTR00125
      ITRAN(IRT)=-15                                                    ZTR00126
      GOTO 300                                                          ZTR00127
 244  IF(IM3.NE.9) GOTO 245                                             ZTR00128
      ITRAN(IRT)=-17                                                    ZTR00129
      GOTO 300                                                          ZTR00130
 245  IF(IM3.NE.18) GOTO 246                                            ZTR00131
      ITRAN(IRT)=-18                                                    ZTR00132
      GOTO 300                                                          ZTR00133
 246  IF((IM3.NE.19).OR.(IM2.NE.9)) GOTO 247                            ZTR00134
      ITRAN(IRT)=-9                                                     ZTR00135
      GOTO 300                                                          ZTR00136
 247  IF((IM3.NE.1).OR.(IM2.NE.20)) GOTO 248                            ZTR00137
      ITRAN(IRT)=-12                                                    ZTR00138
      GOTO 300                                                          ZTR00139
 248  IF((IM3.NE.1).OR.(IM2.NE.2)) GOTO 249                             ZTR00140
      ITRAN(IRT)=-14                                                    ZTR00141
      GOTO 300                                                          ZTR00142
 249  IF((IM3.NE.19).OR.(IM2.NE.17)) GOTO 250                           ZTR00143
      ITRAN(IRT)=-16                                                    ZTR00144
      GOTO 300                                                          ZTR00145
C                                                                       ZTR00146
C*****ILLEGAL FUNCTION NAME                                             ZTR00147
 250  NTYPER(1)=1                                                       ZTR00148
      ZMK(I-3)=ALPH(1)                                                  ZTR00149
      ZMK(I-2)=ALPH(1)                                                  ZTR00150
      ZMK(I-1)=ALPH(1)                                                  ZTR00151
      ITRAN(IRT)=-21                                                    ZTR00152
      GOTO 300                                                          ZTR00153
C                                                                       ZTR00154
C*****MISSING RIGHT PARENTHESIS                                         ZTR00155
 290  NTYPER(2)=1                                                       ZTR00156
      ZMK(I)=ALPH(2)                                                    ZTR00157
 300  CONTINUE                                                          ZTR00158
C                                                                       ZTR00159
C*****CHECK TO SEE IF ALL RIGHT PARENTHESES HAVE BEEN MARKED            ZTR00160
      DO 310 I=IFR,ITO                                                  ZTR00161
      IF((CARDP(I).NE.PARRT).OR.(ITRAN(I).NE.0)) GOTO 310               ZTR00162
      NTYPER(3)=1                                                       ZTR00163
      ZMK(I)=ALPH(3)                                                    ZTR00164
      ITRAN(I)=-21                                                      ZTR00165
 310  CONTINUE                                                          ZTR00166
C                                                                       ZTR00167
C*****CHECK TO SEE IF ALL COMMAS HAVE BEEN MARKED                       ZTR00168
      DO 315 I=IFR,ITO                                                  ZTR00169
      IF((CARDP(I).NE.COMMA).OR.(ITRAN(I).NE.0)) GOTO 315               ZTR00170
      NTYPER(4)=1                                                       ZTR00171
      ZMK(I)=ALPH(4)                                                    ZTR00172
 315  CONTINUE                                                          ZTR00173
C                                                                       ZTR00174
C*****CHECK FOR NON-SUBSCRIPTED VARIABLE NAMES                          ZTR00175
      DO 400 I=IFR,ITO                                                  ZTR00176
      IF(ITRAN(I).NE.0) GOTO 400                                        ZTR00177
      CALL ZALPH(CARDP(I),J)                                            ZTR00178
      IF(J.GT.26) GOTO 400                                              ZTR00179
C*****CHARACTER IS ALPHABETIC                                           ZTR00180
      IF(I.EQ.ITO) GOTO 380                                             ZTR00181
      IF(ITRAN(I+1).NE.0) GOTO 380                                      ZTR00182
      CALL ZDIGIT(CARDP(I+1),K)                                         ZTR00183
      IF(K.GT.10) GOTO 380                                              ZTR00184
C*****TWO-CHARACTER NAME FOUND                                          ZTR00185
      ITRAN(I)=J+(26*(K-1))+53                                          ZTR00186
      ITRAN(I+1)=-50                                                    ZTR00187
      GOTO 400                                                          ZTR00188
C                                                                       ZTR00189
C*****ONE-CHARACTER NAME FOUND                                          ZTR00190
 380  ITRAN(I)=J                                                        ZTR00191
 400  CONTINUE                                                          ZTR00192
C                                                                       ZTR00193
C*****TRANSLATE OPERATORS                                               ZTR00194
      LAST=-20                                                          ZTR00195
      ILAST=0                                                           ZTR00196
      DO 500 I=IFR,ITO                                                  ZTR00197
      IF(ITRAN(I).EQ.-50) GOTO 500                                      ZTR00198
      IF(ITRAN(I).EQ.0) GOTO 410                                        ZTR00199
C*****CHARACTER HAS BEEN TRANSLATED -- RECORD IT                        ZTR00200
      LAST=ITRAN(I)                                                     ZTR00201
      ILAST=I                                                           ZTR00202
      GOTO 500                                                          ZTR00203
C                                                                       ZTR00204
C*****CHARACTER NOT TRANSLATED -- CHECK FOR OPERATORS                   ZTR00205
 410  IF(CARDP(I).EQ.PLUS) GOTO 420                                     ZTR00206
      IF(CARDP(I).EQ.CMINUS) GOTO 430                                   ZTR00207
      IF(CARDP(I).EQ.ASTRSK) GOTO 440                                   ZTR00208
      IF(CARDP(I).EQ.SLASH) GOTO 450                                    ZTR00209
      IF(CARDP(I).EQ.EXSIGN) GOTO 460                                   ZTR00210
C*****CHARACTER NOT AN OPERATOR -- RECORD IT                            ZTR00211
      LAST=ITRAN(I)                                                     ZTR00212
      ILAST=I                                                           ZTR00213
      GOTO 500                                                          ZTR00214
C                                                                       ZTR00215
C*****CHARACTER IS PLUS                                                 ZTR00216
 420  IF((LAST.NE.-20).AND.(CARDP(I-1).NE.COMMA)) GOTO 422              ZTR00217
C*****FORM IS (+ OR ,+ DELETE +                                         ZTR00218
      ITRAN(I)=-50                                                      ZTR00219
      GOTO500                                                           ZTR00220
C*****INSERT + OPERATOR                                                 ZTR00221
 422  ITRAN(I)=-1                                                       ZTR00222
      GOTO 490                                                          ZTR00223
C                                                                       ZTR00224
C*****CHARACTER IS MINUS                                                ZTR00225
 430  IF((LAST.NE.-20).AND.(CARDP(I-1).NE.COMMA)) GOTO 432              ZTR00226
C*****FORM IS (- OR ,- INSERT UNARY MINUS                               ZTR00227
      ITRAN(I)=-6                                                       ZTR00228
      GOTO 500                                                          ZTR00229
C*****INSERT - OPERATOR                                                 ZTR00230
 432  ITRAN(I)=-2                                                       ZTR00231
      GOTO 490                                                          ZTR00232
C                                                                       ZTR00233
C*****CHARACTER IS *                                                    ZTR00234
 440  IF(LAST.NE.-3) GOTO 442                                           ZTR00235
C*****PREVIOUS CHARACTER WAS *                                          ZTR00236
      ITRAN(ILAST)=-50                                                  ZTR00237
      ITRAN(I)=-5                                                       ZTR00238
      GOTO 495                                                          ZTR00239
C*****PREVIOUS CHARACTER WAS NOT *                                      ZTR00240
 442  ITRAN(I)=-3                                                       ZTR00241
      GOTO 490                                                          ZTR00242
C*****CHARACTER IS EXSIGN (^)                                           ZTR00243
 460  ITRAN(I)=-5                                                       ZTR00244
      GOTO 490                                                          ZTR00245
C                                                                       ZTR00246
C*****CHARACTER IS /                                                    ZTR00247
 450  ITRAN(I)=-4                                                       ZTR00248
C                                                                       ZTR00249
C*****CHECK FOR DOUBLE OPERATOR                                         ZTR00250
 490  IF(LAST.GT.-1) GOTO 495                                           ZTR00251
      IF((LAST.LT.-6).AND.(LAST.NE.-20)) GOTO 495                       ZTR00252
C*****DOUBLE OPERATOR FOUND                                             ZTR00253
      NTYPER(5)=1                                                       ZTR00254
      DO 492 IZQ=ILAST,I                                                ZTR00255
 492  ZMK(IZQ)=ALPH(5)                                                  ZTR00256
C*****POST THIS IN LAST                                                 ZTR00257
 495  LAST=ITRAN(I)                                                     ZTR00258
      ILAST=I                                                           ZTR00259
 500  CONTINUE                                                          ZTR00260
C                                                                       ZTR00261
C*****CHECK FOR OPERATOR FOLLOWED BY A RIGHT PARENTHESIS                ZTR00262
      ITM=ITO-1                                                         ZTR00263
      IF(ITM.LT.IFR) GOTO 508                                           ZTR00264
      DO 505 I=IFR,ITM                                                  ZTR00265
      IF((ITRAN(I).GT.-1).OR.(ITRAN(I).LT.-6)) GOTO 505                 ZTR00266
      IF(CARDP(I+1).NE.PARRT) GOTO 505                                  ZTR00267
C*****OPERATOR IS FOLLOWED BY A RIGHT PARENTHESIS                       ZTR00268
      NTYPER(5)=1                                                       ZTR00269
      ZMK(I)=ALPH(5)                                                    ZTR00270
      ZMK(I+1)=ALPH(5)                                                  ZTR00271
 505  CONTINUE                                                          ZTR00272
C                                                                       ZTR00273
C*****INSERT NUMBERS                                                    ZTR00274
 508  DO 600 I=IFR,ITO                                                  ZTR00275
      IF(ITRAN(I).NE.0) GOTO 600                                        ZTR00276
C*****CHARACTER NOT TRANSLATED                                          ZTR00277
      DO 510 J=I,ITO                                                    ZTR00278
      IF(ITRAN(J).NE.0) GOTO 520                                        ZTR00279
 510  CONTINUE                                                          ZTR00280
      J=ITO+1                                                           ZTR00281
 520  JM=J-1                                                            ZTR00282
      IF(I.NE.JM) GOTO 523                                              ZTR00283
C*****SINGLE DIGIT FOUND                                                ZTR00284
      CALL ZDIGIT(CARDP(I),JXYZ)                                        ZTR00285
      IF(JXYZ.GT.10) GOTO 524                                           ZTR00286
      ITRAN(I)=314+(JXYZ-1)                                             ZTR00287
      GOTO 551                                                          ZTR00288
 523  IXM=I                                                             ZTR00289
      CALL ZCONVN(IXM,JM,FNUM)                                          ZTR00290
      IF(FNUM.GE.0) GOTO 550                                            ZTR00291
C*****NON-NUMERIC CHARACTER FOUND                                       ZTR00292
 524  DO 525 IZQ=I,JM                                                   ZTR00292
      IF(ZMK(IZQ).NE.BLANK) GOTO 525                                    ZTR00294
      NTYPER(6)=1                                                       ZTR00295
      ZMK(IZQ)=ALPH(6)                                                  ZTR00296
 525  CONTINUE                                                          ZTR00297
      GOTO 570                                                          ZTR00298
C                                                                       ZTR00299
C*****LOOK FOR NUMBER                                                   ZTR00300
 550  INM=INEXT-1                                                       ZTR00301
      IF(FNUM.EQ.0.) GOTO 5508                                          ZTR00302
      IF(INM.LT.324) GOTO 5508                                          ZTR00303
      DO 5505 IQ=324,INM                                                ZTR00304
      IF(DATA(IQ).EQ.FNUM) GOTO 5510                                    ZTR00305
 5505 CONTINUE                                                          ZTR00306
C*****NUMBER FOUND -- INSERT IN NEXT POSITION                           ZTR00307
 5508 DATA(INEXT)=FNUM                                                  ZTR00308
      ITRAN(I)=INEXT                                                    ZTR00309
      INEXT=INEXT+1                                                     ZTR00310
      GOTO 551                                                          ZTR00311
 5510 ITRAN(I)=IQ                                                       ZTR00312
C                                                                       ZTR00313
C*****CHECK FOR PARENTHESES FOLLOWING                                   ZTR00314
 551  IF(JM.EQ.ITO) GOTO 570                                            ZTR00315
      IF(CARDP(J).NE.PARLFT) GOTO 570                                   ZTR00316
C*****NUMBER IS FOLLOWED BY A LEFT PARENTHESIS                          ZTR00317
      NTYPER(7)=1                                                       ZTR00318
      DO 552 IZQ=I,J                                                    ZTR00319
 552  ZMK(IZQ)=ALPH(7)                                                  ZTR00320
C                                                                       ZTR00321
C*****DELETE REMAINING POSITIONS                                        ZTR00322
 570  IP=I+1                                                            ZTR00323
      IF(IP.GT.JM) GOTO 600                                             ZTR00324
      DO 580 K=IP,JM                                                    ZTR00325
 580  ITRAN(K)=-50                                                      ZTR00326
C                                                                       ZTR00327
 600  CONTINUE                                                          ZTR00328
C                                                                       ZTR00329
C*****CHECK FOR DANGLING OPERATOR                                       ZTR00330
      IF((ITRAN(ITO).GT.-1).OR.(ITRAN(ITO).LT.-6)) GOTO 700             ZTR00331
C*****DANGLING OPERATOR FOUND                                           ZTR00332
      NTYPER(8)=1                                                       ZTR00333
      ZMK(ITO)=ALPH(8)                                                  ZTR00334
C                                                                       ZTR00335
C*****CHECK FOR ADJACENT ADDRESSES                                      ZTR00336
 700  DO 790 I=IFR,ITO                                                  ZTR00337
      IF(ITRAN(I).EQ.-50) GOTO 790                                      ZTR00338
      IF(I.EQ.IFR) GOTO 780                                             ZTR00339
      IF(ITRAN(I).LT.0) GOTO 780                                        ZTR00340
C*****ADDRESS FOUND                                                     ZTR00341
      IF(CARDP(ILAST).NE.PARRT) GOTO 750                                ZTR00342
      IF(CARDP(I).EQ.COMMA) GOTO 750                                    ZTR00343
      NTYPER(9)=1                                                       ZTR00344
      DO 741 IZQ=ILAST,I                                                ZTR00345
 741  ZMK(IZQ)=ALPH(9)                                                  ZTR00346
      GOTO 780                                                          ZTR00347
 750  IF(LAST.LE.-1) GOTO 780                                           ZTR00348
      IF(CARDP(ILAST).EQ.COMMA) GOTO 780                                ZTR00349
      IF(CARDP(I).EQ.COMMA) GOTO 780                                    ZTR00350
C*****TWO ADDRESSES FOUND                                               ZTR00351
      IF((LAST.GT.313).OR.(ITRAN(I).GT.313)) GOTO 760                   ZTR00352
      NTYPER(10)=1                                                      ZTR00353
      DO 755 IZQ=ILAST,I                                                ZTR00354
 755  ZMK(IZQ)=ALPH(10)                                                 ZTR00355
      GOTO 780                                                          ZTR00356
 760  IF((LAST.LE.313).OR.(ITRAN(I).LE.313)) GOTO 770                   ZTR00357
      NTYPER(11)=1                                                      ZTR00358
      DO 765 IZQ=ILAST,I                                                ZTR00359
 765  ZMK(IZQ)=ALPH(11)                                                 ZTR00360
      GOTO 780                                                          ZTR00361
 770  NTYPER(12)=1                                                      ZTR00362
      DO 775 IZQ=ILAST,I                                                ZTR00363
 775  ZMK(IZQ)=ALPH(12)                                                 ZTR00364
C                                                                       ZTR00365
 780  LAST=ITRAN(I)                                                     ZTR00366
      ILAST=I                                                           ZTR00367
 790  CONTINUE                                                          ZTR00368
C                                                                       ZTR00369
C*****SEE IF EXPRESSION IS ACCEPTABLE                                   ZTR00370
      NERTOT=0                                                          ZTR00371
      DO 810 I=1,12                                                     ZTR00372
 810  NERTOT=NERTOT+NTYPER(I)                                           ZTR00373
      IF(NERTOT.EQ.0) GOTO 900                                          ZTR00374
C                                                                       ZTR00375
C*****EXPRESSION CONTAINS ERRORS                                        ZTR00376
      NERRS=NERRS+1                                                     ZTR00377
      NN=0                                                              ZTR00378
      NERROR=28                                                         ZTR00379
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                ZTR00380
      WRITE(IWC,825) (CARDP(I),I=IFR,ITO)                               ZTR00381
 825  FORMAT(25X,80A1)                                                  ZTR00382
      WRITE(IWC,825) (ZMK(I),I=IFR,ITO)                                 ZTR00383
      DO 831 I=1,12                                                     ZTR00384
      IF(NTYPER(I).NE.1) GOTO 831                                       ZTR00385
      NERROR=I                                                          ZTR00386
      NN=2                                                              ZTR00387
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                ZTR00388
 831  CONTINUE                                                          ZTR00389
      RETURN                                                            ZTR00390
C                                                                       ZTR00391
C*****EXPRESSION ACCEPTED SET UP POLISH STRING                          ZTR00392
 900  IPROG(INREG)=-1                                                   ZTR00393
      NSTK=INREG-2                                                      ZTR00394
      NHLD=1                                                            ZTR00395
      DO 1000 I=IFR,ITO                                                 ZTR00396
      IF(ITRAN(I).EQ.-50) GOTO 1000                                     ZTR00397
      IF(CARDP(I).EQ.COMMA) GOTO 930                                    ZTR00398
      IF(ITRAN(I).LT.0) GOTO 910                                        ZTR00399
C*****ADDRESS FOUND -- INSERT IN STRING                                 ZTR00400
      IPROG(NSTK)=ITRAN(I)                                              ZTR00401
      NSTK=NSTK-1                                                       ZTR00402
C*****CHECK FOR UNSCRIPTED REFERENCES TO ONE-CHARACTER NAMES            ZTR00403
      IF(ITRAN(I).GT.26) GOTO 1000                                      ZTR00404
      IF(I.EQ.ITO) GOTO 909                                             ZTR00405
      IF(CARDP(I+1).EQ.PARLFT) GOTO 1000                                ZTR00406
 909  IPROG(NSTK)=-7                                                    ZTR00407
      NSTK=NSTK-1                                                       ZTR00408
      GOTO 1000                                                         ZTR00409
C                                                                       ZTR00410
 910  IF(CARDP(I).NE.PARRT) GOTO 920                                    ZTR00411
C*****RIGHT PARENTHESIS FOUND                                           ZTR00412
      IPR=0                                                             ZTR00413
      IGT=1                                                             ZTR00414
      GOTO 970                                                          ZTR00415
 915  IF(ITRAN(I).EQ.-21) GOTO 1000                                     ZTR00416
C*****PARENTHESIS CARRIES OPERATORS -- INSERT                           ZTR00417
      IPROG(NSTK)=ITRAN(I)                                              ZTR00418
      NSTK=NSTK-1                                                       ZTR00419
      GOTO 1000                                                         ZTR00420
C                                                                       ZTR00421
 920  IF(ITRAN(I).EQ.-20) GOTO 925                                      ZTR00422
C**** OPERATOR FOUND                                                    ZTR00423
      IF((ITRAN(I).EQ.-1).OR.(ITRAN(I).EQ.-2)) IPR=1                    ZTR00424
C---- WENN GEWUENSCHT WIRD, DASS DIE VORZEICHEN (UNARY OPARATERS)       ZTR00425
C---- VOR DER EXPONENTIATION UND NACH DEN FUNKTIONEN VERARBIETET        ZTR00426
C----, SIND DIE BEIDEN FOLGENDEN KARTEN DURCH                           ZTR00427
C     IF((ITRAN(I).EQ.-3).OR.(ITRAN(I).EQ.-4)) IPR=2                    ZTR00428
C     IF((ITRAN.EQ.-5).OR.(ITRAN(I).EQ.-6)) IPR=3                       ZTR00429
C---- ZU ERSETZEN. DAS C IN DER ERSTEN SPALTE MUSS DAZU ENTFERNT WERDEN.ZTR00430
      IF((ITRAN(I).EQ.-3).OR.(ITRAN(I).EQ.-4).OR.(ITRAN(I).EQ.-6)) IPR=2ZTR00431
      IF(ITRAN(I).EQ.-5) IPR=3                                          ZTR00432
      IGT=2                                                             ZTR00433
      GOTO 970                                                          ZTR00434
 925  IHOLD(NHLD)=ITRAN(I)                                              ZTR00435
      NHLD=NHLD+1                                                       ZTR00436
      GOTO 1000                                                         ZTR00437
C                                                                       ZTR00438
C*****COMMA FOUND -- UNSTACK AND INSERT ROW MULTIPLIER                  ZTR00439
 930  IPR=1                                                             ZTR00440
      IGT=3                                                             ZTR00441
      GOTO 970                                                          ZTR00442
 935  IPROG(NSTK)=ITRAN(I)+27                                           ZTR00443
      IPROG(NSTK-1)=-3                                                  ZTR00444
      NSTK=NSTK-2                                                       ZTR00445
      IHOLD(NHLD)=-1                                                    ZTR00446
      NHLD=NHLD+1                                                       ZTR00447
      GOTO 1000                                                         ZTR00448
C                                                                       ZTR00449
C*****ROUTINE TO UNSTACK DOWN TO FIRST LEFT PARENTHESIS                 ZTR00450
C*****IPR=0 -- UNSTACK ALL AND THROW AWAY LEFT PARENTHESIS              ZTR00451
C*****IPR=1 -- UNSTACK ALL BUT RETAIN LEFT PARENTHESIS                  ZTR00452
C*****IPR=2 -- UNSTACK * / AND **                                       ZTR00453
C*****IPR=3 -- UNSTACK ** ONLY                                          ZTR00454
 970  NO=NHLD-1                                                         ZTR00455
      IF(NO.EQ.0) GOTO 980                                              ZTR00456
      DO 975 IX=1,NO                                                    ZTR00457
      LOC=NHLD-IX                                                       ZTR00458
      IF(IHOLD(LOC).NE.-20) GOTO 971                                    ZTR00459
C*****LEFT PARENTHESIS FOUND                                            ZTR00460
      IF(IPR.EQ.0) IHOLD(LOC)=+1                                        ZTR00461
      GOTO 980                                                          ZTR00462
C---- WENN GEWUENSCHT WIRD, DASS DIE VORZEICHEN (UNARY OPERATORS)       ZTR00463
C---- VOR DER EXPONENTIATION UND NACH DEN FUNKTIONEN VERARBEITET        ZTR00464
C---- WERDEN, IST DIE DIESEM TEXT FOLGENDE KARTE DURCH                  ZTR00465
C 971 IF((IHOLD(LOC).LE.-5).AND.(IPR.LE.3)) GOTO 972                    ZTR00466
C---- ZU ERSETZEN. DAS C IN DER ERSTEN SPALTE MUSS DAZU ENTFERNT WERDEN.ZTR00467
 971  IF(IHOLD(LOC).EQ.-5) GOTO 972                                     ZTR00468
      IF((IHOLD(LOC).LE.-3).AND.(IPR.LE.2)) GOTO 972                    ZTR00469
      IF((IHOLD(LOC).LE.-1).AND.(IPR.LE.1)) GOTO 972                    ZTR00470
C*****DO NOT UNSTACK ITEM                                               ZTR00471
      GOTO 975                                                          ZTR00472
C*****UNSTACK ITEM INTO STRING                                          ZTR00473
 972  IPROG(NSTK)=IHOLD(LOC)                                            ZTR00474
      NSTK=NSTK-1                                                       ZTR00475
C*****INSERT POSITIVE NUMBER IN POSITION VACATED                        ZTR00476
      IHOLD(LOC)=+1                                                     ZTR00477
 975  CONTINUE                                                          ZTR00478
 980  GOTO(915,925,935), IGT                                            ZTR00479
 1000 CONTINUE                                                          ZTR00480
C                                                                       ZTR00481
C*****DUMP REMAINING OPERATORS                                          ZTR00482
      NO=NHLD-1                                                         ZTR00483
      IF(NO.EQ.0) GOTO 1040                                             ZTR00484
      DO 1010 IX=1,NO                                                   ZTR00485
      LOC=NHLD-IX                                                       ZTR00486
      IF(IHOLD(LOC).GE.0) GOTO 1010                                     ZTR00487
      IPROG(NSTK)=IHOLD(LOC)                                            ZTR00488
      NSTK=NSTK-1                                                       ZTR00489
 1010 CONTINUE                                                          ZTR00490
C*****FIND AND INSERT LENGTH OF EXPRESSION                              ZTR00491
 1040 LNGTH=INREG-NSTK-2                                                ZTR00492
      IPROG(INREG-1)=LNGTH                                              ZTR00493
C                                                                       ZTR00494
C*****CHECK EXPRESSION                                                  ZTR00495
      NOP=0                                                             ZTR00496
      DO 1090 IQ=1,LNGTH                                                ZTR00497
      LOC=INREG-1-IQ                                                    ZTR00498
      IF(IPROG(LOC).LT.0) GOTO 1060                                     ZTR00499
C*****OPERATOR FOUND                                                    ZTR00500
      NOP=NOP+1                                                         ZTR00501
      GOTO 1090                                                         ZTR00502
 1060 IF((IPROG(LOC).GE.-5).OR.(IPROG(LOC).EQ.-8)) GOTO 1070            ZTR00503
C*****UNARY OPERATOR FOUND                                              ZTR00504
      IF(NOP.LT.1) GOTO 1100                                            ZTR00505
      GOTO 1090                                                         ZTR00506
C*****BINARY OPERATOR FOUND                                             ZTR00507
 1070 IF(NOP.LT.2) GOTO 1100                                            ZTR00508
      NOP=NOP-1                                                         ZTR00509
 1090 CONTINUE                                                          ZTR00510
      IF(NOP.NE.1) GOTO 1100                                            ZTR00511
C*****EXPRESISON IS OK                                                  ZTR00512
      INREG=NSTK                                                        ZTR00513
      RETURN                                                            ZTR00514
C*****EXPRESSION IS NOT OK                                              ZTR00515
 1100 NERRS=NERRS+1                                                     ZTR00516
      NN=0                                                              ZTR00517
      NERROR=29                                                         ZTR00518
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                ZTR00519
      WRITE(IWC,825) (CARDP(I),I=IFR,ITO)                               ZTR00520
      RETURN                                                            ZTR00521
      END                                                               ZTR00522
