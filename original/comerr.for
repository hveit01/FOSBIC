      SUBROUTINE COMERR(NERROR,I1,I2,X1,X2,NN)                          COM00001
C*****SUBROUTINE COMERR TO PRINT SYNTAX-ERROR FIND DURING COMPILATION   COM00002
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      COM00003
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, COM00004
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  COM00005
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, COM00006
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     COM00007
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     COM00008
     1NIFMAX,INTZEI                                                     COM00009
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        COM00010
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      COM00011
     1IRET(20),XXX(4),NFILE(25,3)                                       COM00012
      COMMON// ISTLST(340),LISTST(340)                                  COM00013
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               COM00015
      DIMENSION IPROG(3700)                                             COM00016
      DIMENSION IERR(55),IWAR(7)                                        COM00017
      EQUIVALENCE (DATA(1),IPROG(1))                                    COM00018
      DATA IERR(1)/8HA  331.2/                                          COM00019
      DATA IERR(2)/8HA  326.2/                                          COM00020
      DATA IERR(3)/8HA 332.15/                                          COM00021
      DATA IERR(4)/8HA  332.3/                                          COM00022
      DATA IERR(5)/8HA  332.3/                                          COM00023
      DATA IERR(6)/8HA  332.3/                                          COM00024
      DATA IERR(7)/8HA  322.2/                                          COM00025
      DATA IERR(8)/8HA 332.41/                                          COM00026
      DATA IERR(9)/8HA 332.42/                                          COM00027
      DATA IERR(10)/8HA 332.42/                                         COM00028
      DATA IERR(11)/8HA 332.42/                                         COM00029
      DATA IERR(12)/8HA 332.42/                                         COM00030
      DATA IERR(13)/8HA 332.43/                                         COM00031
      DATA IERR(14)/8HA 332.43/                                         COM00032
      DATA IERR(15)/8HA 332.43/                                         COM00033
      DATA IERR(16)/8HA 332.43/                                         COM00034
      DATA IERR(17)/8HA 332.43/                                         COM00035
      DATA IERR(18)/8HA 331.21/                                         COM00036
      DATA IERR(19)/8HA 332.11/                                         COM00037
      DATA IERR(20)/8HA 332.11/                                         COM00038
      DATA IERR(21)/8HA  326.1/                                         COM00039
      DATA IERR(22)/8HA  326.2/                                         COM00040
      DATA IERR(23)/8HA 332.43/                                         COM00041
      DATA IERR(24)/8HA  326.2/                                         COM00042
      DATA IERR(25)/8HA 332.23/                                         COM00043
      DATA IERR(26)/8HA 332.23/                                         COM00044
      DATA IERR(27)/8HA 332.23/                                         COM00045
      DATA IERR(28)/8HA  324.1/                                         COM00046
      DATA IERR(29)/8HA  324.1/                                         COM00047
      DATA IERR(30)/8HA 322.12/                                         COM00048
      DATA IERR(31)/8HB    331/                                         COM00049
      DATA IERR(32)/8HB331.111/                                         COM00050
      DATA IERR(33)/8HB 331.21/                                         COM00051
      DATA IERR(34)/8HB331.211/                                         COM00052
      DATA IERR(35)/8HB331.211/                                         COM00053
      DATA IERR(36)/8HB331.211/                                         COM00054
      DATA IERR(37)/8HB331.211/                                         COM00055
      DATA IERR(38)/8HB331.211/                                         COM00056
      DATA IERR(39)/8HB331.111/                                         COM00057
      DATA IERR(40)/8HB 331.21/                                         COM00058
      DATA IERR(41)/8HA323/324/                                         COM00059
      DATA IERR(42)/8HB    332/                                         COM00060
      DATA IERR(43)/8HB 331.21/                                         COM00061
      DATA IERR(44)/8HB 331.21/                                         COM00062
      DATA IERR(45)/8HB 331.21/                                         COM00063
      DATA IERR(46)/8HB 331.13/                                         COM00064
      DATA IERR(47)/8HB 332.11/                                         COM00065
      DATA IERR(48)/8HB 332.11/                                         COM00066
      DATA IERR(49)/8HB 332.11/                                         COM00067
      DATA IERR(50)/8HB 332.11/                                         COM00068
      DATA IERR(51)/8HB 331.21/                                         COM00069
      DATA IERR(52)/8HA 332.13/                                         COM00070
      DATA IERR(53)/8HB331.112/                                         COM00071
      DATA IERR(54)/8HB331.211/                                         COM00072
      DATA IERR(55)/8HA 331.43/                                         COM00073
      DATA IWAR(1)/8HA 332.43/                                          COM00074
      DATA IWAR(2)/8HA 332.21/                                          COM00075
      DATA IWAR(3)/8HA 332.11/                                          COM00076
      DATA IWAR(4)/8HA  323.1/                                          COM00077
      DATA IWAR(5)/8HA  323.2/                                          COM00078
      DATA IWAR(6)/8HB 332.13/                                          COM00079
      DATA IWAR(7)/8HA  332.2/                                          COM00080
      IF(NN.EQ.-1) GOTO 997                                             COM00081
      IF(NN.EQ.-2) GOTO 998                                             COM00082
      IF(NN.EQ.-3) GOTO 999                                             COM00083
      IF(NN.EQ.1) GOTO 1999                                             COM00084
      IF(NN.EQ.2) GOTO 2999                                             COM00085
      GOTO(502,504,506,508,510,512,514,516,518,520,522,524,526,528,530, COM00086
     1532,534,536,538,540,542,544,546,548,550,552,554,556,558,560,562   COM00087
     2,564,566,568,570,572,574,576,578,580,582,584,586,588,590,592,594  COM00088
     3,596,598,600,602,604,606,608,610),NERROR                          COM00089
 502  WRITE(IWC,10)                                                     COM00090
 10   FORMAT(20X,55HPROGRAM AND DATA EXCEED AVAILABLE STORAGE AT THIS POCOM00091
     1INT)                                                              COM00092
      GOTO 1000                                                         COM00093
 504  WRITE(IWC,20) ISTMAX                                              COM00094
 20   FORMAT(20X,48HSORRY - I ACCEPT ONLY LINE NUMBERS BETWEEN 1 AND,I6)COM00095
      GOTO 1000                                                         COM00096
 506  WRITE(IWC,30)                                                     COM00097
 30   FORMAT(20X,25HILLEGAL RESTORE STATEMENT)                          COM00098
      GOTO 1000                                                         COM00099
 508  WRITE(IWC,40)                                                     COM00100
 40   FORMAT(20X,54HI CANNOT FIND AN EQUAL SIGN IN THE LET STATEMENT ABOCOM00101
     1VE)                                                               COM00102
      GOTO 1000                                                         COM00103
 510  WRITE(IWC,50)                                                     COM00104
 50   FORMAT(20X,40HTHE EQUAL SIGN ABOVE IS IN A FUNNY PLACE)           COM00105
      GOTO 1000                                                         COM00106
 512  WRITE(IWC,60)                                                     COM00107
 60   FORMAT(20X,53HITEM TO THE LEFT OF THE EQUAL SIGN MUST BE A VARIABLCOM00108
     1E)                                                                COM00109
      GOTO 1000                                                         COM00110
 514  WRITE(IWC,70)                                                     COM00111
 70   FORMAT(20X,40HTHE QUOTATION MARKS ABOVE ARE NOT PAIRED)           COM00112
      GOTO 1000                                                         COM00113
 516  WRITE(IWC,80) ISTMAX                                              COM00114
 80   FORMAT(20X,57HSORRY - I CAN ONLY GO TO LINES WITH NUMBERS BETWEEN COM00115
     11 AND,I6)                                                         COM00116
      GOTO 1000                                                         COM00117
 518  WRITE(IWC,90)                                                     COM00118
 90   FORMAT(20X,34HTHE IF STATEMENT ABOVE IS HOPELESS)                 COM00119
      GOTO 1000                                                         COM00120
 520  WRITE(IWC,100)                                                    COM00121
 100  FORMAT(20X,52HI CANNOT ACCEPT AN IF STATEMENT WITHOUT A COMPARISONCOM00122
     1)                                                                 COM00123
      GOTO 1000                                                         COM00124
 522  WRITE(IWC,110)                                                    COM00125
 110  FORMAT(20X,26HI CANNOT FIND THEN OR GOTO)                         COM00126
      GOTO 1000                                                         COM00127
 524  WRITE(IWC,120)                                                    COM00128
 120  FORMAT(20X,46HTHEN OR GOTO MUST BE FOLLOWED BY A LINE NUMBER)     COM00129
      GOTO 1000                                                         COM00130
 526  WRITE(IWC,130)                                                    COM00131
 130  FORMAT(20X,35HTHE FOR STATEMENT ABOVE IS HOPELESS)                COM00132
      GOTO 1000                                                         COM00133
 528  WRITE(IWC,140)                                                    COM00134
 140  FORMAT(20X,54HI CANNOT FIND AN EQUAL SIGN IN THE FOR STATEMENT ABOCOM00135
     1VE)                                                               COM00136
      GOTO 1000                                                         COM00137
 530  WRITE(IWC,150)                                                    COM00138
 150  FORMAT(20X,53HTHE VARIABLE IN A FOR STATEMENT MUST BE UNSUBSCRIPTECOM00139
     1D)                                                                COM00140
      GOTO 1000                                                         COM00141
 532  WRITE(IWC,160)                                                    COM00142
 160  FORMAT(20X,18HI CANNOT FIND A TO)                                 COM00143
      GOTO 1000                                                         COM00144
 534  WRITE(IWC,170)                                                    COM00145
 170  FORMAT(20X,50HEITHER YOU FORGOT A FOR STATEMENT OR I REJECTED IT) COM00146
      GOTO 1000                                                         COM00147
 536  WRITE(IWC,200)                                                    COM00148
 200  FORMAT(20X,33HDIM STATEMENT INCORRECTLY WRITTEN)                  COM00149
      GOTO 1000                                                         COM00150
 538  WRITE(IWC,210)                                                    COM00151
 210  FORMAT(20X,27HWRONG CHARACTER WITHIN DATA)                        COM00152
      GOTO 1000                                                         COM00153
 540  WRITE(IWC,230) I1                                                 COM00154
 230  FORMAT(20X,11HYOU GAVE ME,I5,47H MORE DATA IN DATA STATEMENTS THANCOM00155
     1 I CAN HANDLE)                                                    COM00156     
      GOTO 1000                                                         COM00157
 542  WRITE(IWC,240)                                                    COM00158
 240  FORMAT(20X,49HYOU MAY UNDERSTAND THE COMMAND ABOVE BUT I DO NOT)  COM00159
      GOTO 1000                                                         COM00160
 544  WRITE(IWC,250) I1                                                 COM00161
 250  FORMAT(20X,16HSTATEMENT NUMBER,I5,23H APPEARS MORE THAN ONCE)     COM00162
      GOTO 1000                                                         COM00163
 546  WRITE(IWC,260) I1                                                 COM00164
 260  FORMAT(20X,9HTHERE ARE,I6,39H FOR STATEMENTS WITHOUT NEXT STATEMENCOM00165
     1TS)                                                               COM00166
      GOTO 1000                                                         COM00167
 548  WRITE(IWC,270) NSTEND                                             COM00168
 270  FORMAT(20X,54HCOMPILATION STOPPED AT THIS POINT -- I CAN ONLY HANDCOM00169
     1LE,I6,20H NUMBERED STATEMENTS)                                    COM00170
      GOTO 1000                                                         COM00171
 550  WRITE(IWC,280)                                                    COM00172
 280  FORMAT(20X,47HAN IMAGE STATEMENT MUST HAVE A STATEMENT NUMBER)    COM00173
      GOTO 1000                                                         COM00174
 552  WRITE(IWC,290) MAXIMA                                             COM00175
 290  FORMAT(20X,17HI CAN ONLY HANDLE,I5,17H IMAGE STATEMENTS)          COM00176
      GOTO 1000                                                         COM00177
 554  WRITE(IWC,300)                                                    COM00178
 300  FORMAT(20X,66HTHERE IS NO REFERENCE TO AN IMAGE STATEMENT WITHIN TCOM00179
     1HE PRINT USING)                                                   COM00180
      GOTO 1000                                                         COM00181
 556  WRITE(IWC,310)                                                    COM00182
 310  FORMAT(20X,57HI DO NOT LIKE THE EXPRESSION SHOWN BELOW (REASONS FOCOM00183
     1LLOW))                                                            COM00184
      GOTO 1000                                                         COM00185
 558  WRITE(IWC,320)                                                    COM00186
 320  FORMAT(20X,65HTHERE IS SOMETHING I DO NOT LIKE ABOUT THE EXPRESSIOCOM00187
     1N SHOWN BELOW)                                                    COM00188
      GOTO 1000                                                         COM00189
 560  WRITE(IWC,330) IEXPO,IEXPO                                        COM00190
 330  FORMAT(20X,38HI CAN ONLY HANDLE NUMBERS BETWEEN 10**,I3,11H AND 10COM00191
     1**(-,I3,1H))                                                      COM00192
      GOTO 1000                                                         COM00193
 562  WRITE(IWC,335)                                                    COM00194
 335  FORMAT(20X,39HI CANNOT ACCEPT THE MAT STATEMENT ABOVE)            COM00195
      GOTO 1000                                                         COM00196
 564  WRITE(IWC,340)                                                    COM00197
 340  FORMAT(20X,65HI DO NOT UNDERSTAND THE FOLLOWING PART IN THE MAT STCOM00198
     1ATEMENT ABOVE)                                                    COM00199
      WRITE(IWC,705) (CARDT(J),J=I1,I2)                                 COM00200
 705  FORMAT(1H ,10X,80A1)                                              COM00201
      GOTO 1000                                                         COM00202
 566  WRITE(IWC,345)                                                    COM00203
 345  FORMAT(20X,58HI CANNOT FIND AN EQUAL SIGN IN THE MAT LET STATEMENTCOM00204
     1 ABOVE)                                                           COM00205
      GOTO 1000                                                         COM00206
 568  WRITE(IWC,350)                                                    COM00207
 350  FORMAT(20X,25HILLEGAL MAT LET STATEMENT)                          COM00208
      GOTO 1000                                                         COM00209
 570  WRITE(IWC,355) X1                                                 COM00210
 355  FORMAT(20X,66HI DO NOT LIKE THE FOLLOWING CHARACTER IN THE MAT STACOM00211
     1TEMENT ABOVE *,A1,1H*)                                            COM00212
      GOTO 1000                                                         COM00213
 572  WRITE(IWC,360) I1,I2                                              COM00214
 360  FORMAT(20X,10HVARIABLE *,A1,42H* IS DIMENSIONED AS A VECTOR -- VARCOM00215
     1IABLE *,A1,28H* IS DIMENSIONED AS A MATRIX)                       COM00216
      GOTO 1000                                                         COM00217
 574  WRITE(IWC,365) X1                                                 COM00218
 365  FORMAT(20X,10HVARIABLE *,A1,36H* IS NOT DIMENSIONED AS A N*N MATRICOM00219
     1X)                                                                COM00220
      GOTO 1000                                                         COM00221
 576  WRITE(IWC,370)                                                    COM00222
 370  FORMAT(20X,35HNO ACCEPTABLE MATRIX MULTIPLICATION)                COM00223
      GOTO 1000                                                         COM00224
 578  WRITE(IWC,375) X1                                                 COM00225
 375  FORMAT(20X,10HVARIABLE *,A1,20H* IS NOT DIMENSIONED)              COM00226
      GOTO 1000                                                         COM00227
 580  WRITE(IWC,380) X1                                                 COM00228
 380  FORMAT(20X,10HVARIABLE *,A1,57H* IS DIMENSIONED AS A VECTOR BUT YOCOM00229
     1U TREAT IT AS A MATRIX)                                           COM00230
      GOTO 1000                                                         COM00231
 582  WRITE(IWC,385)                                                    COM00232
 385  FORMAT(20X,75HERROR IN INPUT OR OUTPUT LIST -- UNCORRECT VARIABLE,COM00233
     1SUBSCRIPT OR EXPRESSION)                                          COM00234
 603  WRITE(IWC,336) (CARDP(J),J=I1,I2)                                 COM00235
 336  FORMAT(20X,26HTHE PART I DO NOT LIKE IS ,80A1)                    COM00236
      GOTO 1000                                                         COM00237
 584  WRITE(IWC,390)                                                    COM00238
 390  FORMAT(20X,36HTHE FILE STATEMENT ABOVE IS HOPELESS)               COM00239
      GOTO 1000                                                         COM00240
 586  WRITE(IWC,395) X1                                                 COM00241
 395  FORMAT(20X,47HILLEGAL FIELD NAME IN THE MAT STATEMENT ABOVE *,A1,1COM00242
     1H*)                                                               COM00243
      GOTO 1000                                                         COM00244
 588  WRITE(IWC,400)                                                    COM00245
 400  FORMAT(20X,53HFILE NAME ERROR -- THE QUOTATION MARKS ARE NOT PAIRECOM00246
     1D)                                                                COM00247
      GOTO 1000                                                         COM00248
 590  I1=MAXSAT-1000                                                    COM00249
      WRITE(IWC,405) I1,I2                                              COM00250
 405  FORMAT(20X,49HI AM VERY SORRY BUT I AM ONLY ALLOWED TO GIVE YOU,I5COM00251
     1,26H SENTENCES AND YOU ASK FOR,I6)                                COM00252
      GOTO 1000                                                         COM00253
 592  WRITE(IWC,410)                                                    COM00254
 410  FORMAT(20X,56HTHE FIRST FILE STATEMENT MUST BE A COMMON-FILE STATECOM00255
     1MENT)                                                             COM00256
      GOTO 1000                                                         COM00257
 594  WRITE(IWC,415)                                                    COM00258
 415  FORMAT(20X,19HFILE SENTENCE ERROR)                                COM00259
      GOTO 1000                                                         COM00260
 596  WRITE(IWC,420)                                                    COM00261
 420  FORMAT(20X,32HTHE FILE NAME ABOVE IS REDEFINED)                   COM00262
      GOTO 1000                                                         COM00263
 598  WRITE(IWC,425) MAXFIL                                             COM00264
 425  FORMAT(20X,50HI AM VERY SORRY BUT I CAN ONLY ALLOW YOU TO DEFINE,ICOM00265
     13,6H FILES)                                                       COM00266
      GOTO 1000                                                         COM00267
  600 WRITE(IWC,430)                                                    COM00268
  430 FORMAT(20X,74HTHE QUANTITY OF WORDS IN A INDEX SEQUENTIAL OPEN MUSCOM00269
     1T BE GREATER THAN ZERO)                                           COM00270
      GOTO 1000                                                         COM00271
 602  WRITE(IWC,435)                                                    COM00272
 435  FORMAT(20X,45HERROR ON THE LEFT SIDE OF A MAT LET STATEMENT)      COM00273
      GOTO 564                                                          COM00274
 604  WRITE(IWC,440)                                                    COM00275
 440  FORMAT(20X,37HDATA CAN ONLY BE READ INTO A VARIABLE)              COM00276
      GOTO 603                                                          COM00277
 606  WRITE(IWC,445)                                                    COM00278
 445  FORMAT(20X,36HMISSING PARENTHESIS IN MAT STATEMENT)               COM00279
      GOTO 603                                                          COM00280
 608  WRITE(IWC,450)                                                    COM00281
 450  FORMAT(20X,37HDIMENSION NOT ALLOWED WITH EXPRESSION)              COM00282
      GOTO 564                                                          COM00283
 610  WRITE(IWC,455) NIFMAX                                             COM00284
 455  FORMAT(20X,17HI CAN ONLY HANDLE,I4,39H FOR STATEMENTS WITHOUT NEXTCOM00285
     1 STATEMENTS)                                                      COM00286
      GOTO 1000                                                         COM00287
  999 WRITE(IWC,1002)                                                   COM00288
 1000 WRITE(IWC,1001) NERROR                                            COM00289
 1001 FORMAT(1H+,110X,16H***ERROR NUMBER=,I3)                           COM00290
      WRITE(IWC,1003) IERR(NERROR)                                      COM00291
 1003 FORMAT(1H ,10X,18HSEE BASIC TEXTBOOK,2X,A8)                       COM00292
 9999 WRITE(IWC,1002)                                                   COM00293
 1002 FORMAT(1H )                                                       COM00294
      RETURN                                                            COM00295
 1999 GOTO(5000,5002,5004,5006,5008,5010,5012),NERROR                   COM00296
 5000 WRITE(IWC,180) X1,X2                                              COM00297
 180  FORMAT(20X,35HI EXPECTED YOU TO MENTION VARIABLE ,2A1,30H -- I WILCOM00298
     1L ASSUME THAT YOU DID)                                            COM00299
      GOTO 2000                                                         COM00300
 5002 WRITE(IWC,190) X1                                                 COM00301
 190  FORMAT(20X,9HVARIABLE ,A1,42H REDIMENSIONED -- BUT I TAKE IT AS A COM00302
     1 JOKE)                                                            COM00303
      GOTO 2000                                                         COM00304
 5004 WRITE(IWC,220)                                                    COM00305
 220  FORMAT(20X,40HTHERE ARE NO DATA IN THIS DATA STATEMENT)           COM00306
      GOTO 2000                                                         COM00307
 5006 WRITE(IWC,185)                                                    COM00308
 185  FORMAT(20X,51HYOU TREAT A NUMERIC VARIABLE AS AN ALPHANUMERIC ONE)COM00309
      GOTO 2000                                                         COM00310
 5008 WRITE(IWC,195)                                                    COM00311
 195  FORMAT(20X,51HYOU TREAT AN ALPHANUMERIC VARIABLE AS A NUMERIC ONE)COM00312
      GOTO 2000                                                         COM00313
 5010 WRITE(IWC,205)                                                    COM00314
 205  FORMAT(20X,76HI CANNOT ACCEPT A NUMBER FOR WORDS IN A COMMON-FILE COM00315
     1 STATEMENT - I IGNORE IT)                                         COM00316
      GOTO 2000                                                         COM00317
 5012 WRITE(IWC,215)                                                    COM00318
 215  FORMAT(20X,26HMISSING COMMA OR SEMICOLON)                         COM00319
      GOTO 2000                                                         COM00320
 998  WRITE(IWC,1002)                                                   COM00321
 2000 WRITE(IWC,2001) NERROR                                            COM00322
 2001 FORMAT(1H+,110X,18H***WARNING NUMBER=,I3)                         COM00323
      WRITE(IWC,1003) IWAR(NERROR)                                      COM00324
      GOTO 9999                                                         COM00325
 2999 GOTO(3002,3004,3006,3008,3010,3012,3014,3016,3018,3020,3022,3024),COM00326
     1NERROR                                                            COM00327
 3002 WRITE(IWC,3102)                                                   COM00328
 3102 FORMAT(30X,28HA -- THIS IS AN ILLEGAL NAME)                       COM00329
      GOTO 3200                                                         COM00330
 3004 WRITE(IWC,3104)                                                   COM00331
 3104 FORMAT(30X,51HB -- NO RIGHT PARENTHESIS FOR THIS LEFT PARENTHESIS)COM00332
      GOTO 3200                                                         COM00333
 3006 WRITE(IWC,3106)                                                   COM00334
 3106 FORMAT(30X,51HC -- NO LEFT PARENTHESIS FOR THIS RIGHT PARENTHESIS)COM00335
      GOTO 3200                                                         COM00336
 3008 WRITE(IWC,3108)                                                   COM00337
 3108 FORMAT(30X,35HD -- THIS COMMA IS IN A FUNNY PLACE)                COM00338
      GOTO 3200                                                         COM00339
 3010 WRITE(IWC,3110)                                                   COM00340
 3110 FORMAT(30X,38HE -- THIS PAIR OF OPERATORS IS ILLEGAL)             COM00341
      GOTO 3200                                                         COM00342
 3012 WRITE(IWC,3112)                                                   COM00343
 3112 FORMAT(30X,34HF -- THERE IS SOMETHING FUNNY HERE)                 COM00344
      GOTO 3200                                                         COM00345
 3014 WRITE(IWC,3114)                                                   COM00346
 3114 FORMAT(30X,56HG -- A CONSTANT CANNOT BE FOLLOWED BY A LEFT PARENTHCOM00347
     1ESIS)                                                             COM00348
      GOTO 3200                                                         COM00349
 3016 WRITE(IWC,3116)                                                   COM00350
 3116 FORMAT(30X,30HH -- THIS OPERATOR IS DANGLING)                     COM00351
      GOTO 3200                                                         COM00352
 3018 WRITE(IWC,3118)                                                   COM00353
 3118 FORMAT(30X,69HI -- A RIGHT PARENTHESIS CANNOT BE FOLLOWED BY A VARCOM00354
     1IABLE OR CONSTANT)                                                COM00355
      GOTO 3200                                                         COM00356
 3020 WRITE(IWC,3120)                                                   COM00357
 3120 FORMAT(30X,43HJ -- THESE VARIABLES ARE NEXT TO EACH OTHER)        COM00358
      GOTO 3200                                                         COM00359
 3022 WRITE(IWC,3122)                                                   COM00360
 3122 FORMAT(30X,43HK -- THESE CONSTANTS ARE NEXT TO EACH OTHER)        COM00361
      GOTO 3200                                                         COM00362
 3024 WRITE(IWC,3124)                                                   COM00363
 3124 FORMAT(30X,37HL -- A VARIABLE IS NEXT TO A CONSTANT)              COM00364
      GOTO 3200                                                         COM00365
 997  WRITE(IWC,1002)                                                   COM00366
 3200 WRITE(IWC, 3201) NERROR                                           COM00367
 3201 FORMAT(1H+,110X,17H***REASON NUMBER=,I3)                          COM00368
      GOTO 9999                                                         COM00369
      END                                                               COM00370
