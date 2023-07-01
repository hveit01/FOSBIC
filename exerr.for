      SUBROUTINE EXERR(NERROR,I1,I2,X1,X2)                              EXR00001
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      EXR00002
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, EXR00003
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  EXR00004
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, EXR00005
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     EXR00006
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     EXR00007
     1NIFMAX,INTZEI                                                     EXR00008
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        EXR00009
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      EXR00010
     1IRET(20),XXX(4),NFILE(25,3)                                       EXR00011
      COMMON// ISTLST(340),LISTST(340)                                  EXR00012
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               EXR00014
      DIMENSION IPROG(3700)                                             EXR00015
      DIMENSION IERR(51)                                                ***
      EQUIVALENCE (DATA(1),IPROG(1))                                    EXR00017
      DATA IERR(1)/8HA 332.46/                                          EXR00018
      DATA IERR(2)/8HA 332.41/                                          EXR00019
      DATA IERR(3)/8HA 332.46/                                          EXR00020
      DATA IERR(4)/8HA 322.12/                                          EXR00021
      DATA IERR(5)/8HA 332.11/                                          EXR00022
      DATA IERR(6)/8HA  322.2/                                          EXR00023
      DATA IERR(7)/8HA 332.15/                                          EXR00024
      DATA IERR(8)/8HA 332.13/                                          EXR00025
      DATA IERR(9)/8HC  324.3/                                          ***
      DATA IERR(10)/8HA 332.43/                                         EXR00027
      DATA IERR(11)/8HC  324.3/                                         ***
      DATA IERR(12)/8HC  324.3/                                         ***
      DATA IERR(13)/8HA 332.23/                                         EXR00030
      DATA IERR(14)/8HA  324.1/                                         EXR00031
      DATA IERR(15)/8HA  324.1/                                         EXR00032
      DATA IERR(16)/8HA  324.1/                                         EXR00033
      DATA IERR(17)/8HA 331.21/                                         EXR00034
      DATA IERR(18)/8HA  324.1/                                         EXR00035
      DATA IERR(19)/8HA  324.1/                                         EXR00036
      DATA IERR(20)/8HA  324.1/                                         EXR00037
      DATA IERR(21)/8HB 332.11/                                         EXR00038
      DATA IERR(22)/8HB 332.11/                                         EXR00039
      DATA IERR(23)/8HB 332.11/                                         EXR00040
      DATA IERR(24)/8HB 332.21/                                         EXR00041
      DATA IERR(25)/8HB 332.21/                                         EXR00042
      DATA IERR(26)/8HB 332.32/                                         EXR00043
      DATA IERR(27)/8HB 332.23/                                         EXR00044
      DATA IERR(28)/8HB 332.32/                                         EXR00045
      DATA IERR(29)/8HB 332.12/                                         EXR00046
      DATA IERR(30)/8HB331.211/                                         EXR00047
      DATA IERR(31)/8HB331.211/                                         EXR00048
      DATA IERR(32)/8HB331.312/                                         EXR00049
      DATA IERR(33)/8HB331.312/                                         EXR00050
      DATA IERR(34)/8HB331.312/                                         EXR00051
      DATA IERR(35)/8HB331.211/                                         EXR00052
      DATA IERR(36)/8HB331.221/                                         EXR00053
      DATA IERR(37)/8HA 332.23/                                         EXR00054
      DATA IERR(38)/8HA  324.1/                                         EXR00055
      DATA IERR(39)/8HA 332.22/                                         EXR00056
      DATA IERR(40)/8HA  324.1/                                         EXR00057
      DATA IERR(41)/8HA332.412/                                         EXR00058
      DATA IERR(42)/8HB331.211/                                         EXR00059
      DATA IERR(43)/8HB331.211/                                         EXR00060
      DATA IERR(44)/8HB331.211/                                         EXR00061
      DATA IERR(45)/8HB331.211/                                         EXR00062
      DATA IERR(46)/8HB 332.23/                                         EXR00063
      DATA IERR(47)/8HB 332.21/                                         EXR00064
      DATA IERR(48)/8HB331.221/                                         EXR00065
      DATA IERR(49)/8HB331.221/                                         EXR00066
      DATA IERR(50)/8HB331.221/                                         EXR00067
      DATA IERR(51)/8HB331.211/                                         ***
      NN=1                                                              EXR00068
      NX=3                                                              EXR00069
      CALL PRILIN(NX)                                                   EXR00070
      WRITE(IWC,1000)                                                   EXR00071
 1000 FORMAT(1H0,19HPROGRAM STOPPED -- )                                EXR00072
      IF(NN.EQ.0) GOTO 2000                                             EXR00073
      GOTO(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44EXR00074
     1,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,86EXR00075
     2,90,92,94,62,98,100,103),NERROR                                   ***
 102  FORMAT(1H+,19X,17HI CAN ONLY HANDLE,I4,23H GOSUBS WITHOUT RETURNS)EXR00077
 2    WRITE(IWC,102) NIRMAX                                             EXR00078
      GOTO 2000                                                         EXR00079
 4    WRITE(IWC,104) I1                                                 EXR00080
 104  FORMAT(1H+,19X,14HI CANNOT GO TO,I5,24H BECAUSE IT IS NOT THERE)  EXR00081
      GOTO 2000                                                         EXR00082
 6    WRITE(IWC,106)                                                    EXR00083
 106  FORMAT(1H+,19X,49HI CANNOT RETURN SINCE I DID NOT COME FROM A GOSUEXR00084
     1B)                                                                EXR00085
      GOTO 2000                                                         EXR00086
 8    WRITE(IWC,108) IEXPO,IEXPO                                        EXR00087
 108  FORMAT(1H+,19X,38HI CAN HANDLE ONLY NUMBERS BETWEEN 10**,I3,11H ANEXR00088
     1D 10**(-,I3,1H))                                                  EXR00089
      GOTO 1999                                                         EXR00090
 10   WRITE(IWC,112)                                                    EXR00091
 110  FORMAT(1H+,19X,23HWRONG CHARACTER IN DATA)                        EXR00092
      GOTO 1999                                                         EXR00093
 12   WRITE(IWC,112)                                                    EXR00094
 112  FORMAT(1H+,19X,57HQUOTATION MARKS ARE NOT PAIRED IN THE FOLLOWING EXR00095
     1DATA-CARD)                                                        EXR00096
      GOTO 1999                                                         EXR00097
 14   WRITE(IWC,114) I1,NCELLD                                          EXR00098
 114  FORMAT(1H+,19X,39HYOU SET DATA POINTER ON POSITION NUMBER,I6,27H BEXR00099
     1UT I ONLY CAN DO IT UNTIL,I6,8H OR ZERO)                          EXR00100
      GOTO 2000                                                         EXR00101
 16   WRITE(IWC,116)                                                    EXR00102
 116  FORMAT(1H+,19X,11HEND OF DATA)                                    EXR00103
      GOTO 2000                                                         EXR00104
 18   WRITE(IWC,118) INREG,IPROG(INREG)                                 EXR00105
 118  FORMAT(1H+,19X,8HERROR AT,I6,3X,5HCODE=,I12)                      EXR00106
      GOTO 2000                                                         EXR00107
 20   WRITE(IWC,120)                                                    EXR00108
 120  FORMAT(1H+,19X,33HZERO STEP SIZE IN A FOR STATEMENT)              EXR00109
      GOTO 2000                                                         EXR00110
 22   WRITE(IWC,122) INREG,IPROG(INREG)                                 EXR00111
 122  FORMAT(1H+,19X,17HMAT CODE ERROR AT,I6,6H CODE=,I12)              EXR00112
      GOTO 2000                                                         EXR00113
 24   WRITE(IWC,124) INREG,IPROG(INREG)                                 EXR00114
 124  FORMAT(1H+,19X,18HFILE CODE ERROR AT,I6,6H CODE=,I12)             EXR00115
      GOTO 2000                                                         EXR00116
 26   WRITE(IWC,126) I1                                                 EXR00117
 126  FORMAT(1H+,19X,71HPERHAPS I AM CRAZY BUT I CANNOT FIND AN IMAGE STEXR00118
     1ATEMENT WITH THE NUMBER,I6)                                       EXR00119
      GOTO 2000                                                         EXR00120
 28   WRITE(IWC,128)                                                    EXR00121
 128  FORMAT(1H+,19X,37HTHERE IS AN ATTEMPT TO DIVIDE BY ZERO)          EXR00122
      GOTO 2000                                                         EXR00123
 30   WRITE(IWC,130) X1,X2                                              EXR00124
 130  FORMAT(1H+,19X,50HILLEGAL EXPONENTIATION -- THE ARGUMENT IS NEGATIEXR00125
     1VE,E16.8,16H THE EXPONENT IS,E16.8)                               EXR00126
      GOTO 2000                                                         EXR00127
 32   WRITE(IWC,132) IEXPO                                              ***
 132  FORMAT(1H+,19X,35HEXPONENT OVERFLOW IN EXPONENTIATION,2X,9HMAXIMUM***
     1 =,I3)                                                            ***
      GOTO 2000                                                         EXR00130
 34   WRITE(IWC,134) I1,X1                                              EXR00131
 134  FORMAT(1H+,19X,41HTHERE IS NOT ENOUGH SPACE FOR ITEM NUMBER,I10,18EXR00132
     1H IN LIST OR TABLE ,A1)                                           EXR00133
      GOTO 2000                                                         EXR00134
 36   WRITE(IWC,136)                                                    EXR00135
 136  FORMAT(1H+,19X,27HTHE ARGUMENT OF LOG IS ZERO)                    EXR00136
      GOTO 2000                                                         EXR00137
 38   WRITE(IWC,138)                                                    EXR00138
 138  FORMAT(1H+,19X,31HTHE ARGUMENT OF LOG IS NEGATIVE)                EXR00139
      GOTO 2000                                                         EXR00140
 40   WRITE(IWC,140)                                                    EXR00141
 140  FORMAT(1H+,19X,31HTHE ARGUMENT OF SQR IS NEGATIVE)                EXR00142
      GOTO 2000                                                         EXR00143
 42   I1=MAXSAT-1000                                                    EXR00144
      WRITE(IWC,142) I1                                                 EXR00145
 142  FORMAT(1H+,19X,30HYOU WANT TO ALLOCATE MORE THAN,I5,10H SENTENCES)EXR00146
      GOTO 2000                                                         ***
 44   WRITE(IWC,144)                                                    EXR00148
 144  FORMAT(1H+,19X,50HTHERE IS AN ATTEMPT TO OPEN AN ALREADY OPENED FIEXR00149
     1LE)                                                               EXR00150
      GOTO 2000                                                         ***
 46   WRITE(IWC,146) MAXFIL                                             EXR00152
 146  FORMAT(1H+,19X,15HI CAN OPEN ONLY,I5,6H FILES)                    EXR00153
      GOTO 2000                                                         ***
 48   WRITE(IWC,148)                                                    EXR00155
 148  FORMAT(1H+,19X,67HINDEX-SEQUENTIAL FILE MUST BE OPENED BY A PRECEEEXR00156                                                    EXR00156
     1DING OPEN STATEMENT)                                              EXR00157
      GOTO 2000                                                         ***
 50   WRITE(IWC,150) I1,I2                                              EXR00159
 150  FORMAT(1H+,19X,15HSENTENCE NUMBER,I6,32H EXCEEDS MAXIMAL SENTENCE EXR00160
     1NUMBER,I6)                                                        EXR00161
      GOTO 2000                                                         ***
 52   WRITE(IWC,152)                                                    EXR00163
 152  FORMAT(1H+,19X,41HYOU CANNOT GET DATA FROM AN UNOPENED FILE)      EXR00164
      GOTO 2000                                                         ***
 54   WRITE(IWC,154)                                                    EXR00166
 154  FORMAT(1H+,19X,33HYOU CANNOT RESET AN UNOPENED FILE)              EXR00167
      GOTO 2000                                                         ***
 56   WRITE(IWC,156)                                                    EXR00169
 156  FORMAT(1H+,19X,19HEND OF FILE REACHED)                            EXR00170
      GOTO 2000                                                         ***
 58   WRITE(IWC,158)                                                    EXR00172
 158  FORMAT(1H+,19X,33HYOU CANNOT CLOSE AN UNOPENED FILE)              EXR00173
      GOTO 2000                                                         ***
 60   WRITE(IWC,160)                                                    EXR00175
 160  FORMAT(1H+,19X,30HERROR IN MATRIX MULTIPLICATION)                 EXR00176
      GOTO 2000                                                         ***
 62   WRITE(IWC,162)                                                    EXR00178
 162  FORMAT(1H+,19X,50HNO CORRESPONDING SIZE OF THE SUBSCRIPTS FOR MATREXR00179
     1IX)                                                               EXR00180
      IF(NERROR.EQ.31) WRITE(IWC,161)                                   EXR00181
      IF(NERROR.EQ.48) WRITE(IWC,163)                                   EXR00182
 161  FORMAT(1H+,70X,13HTRANSPONATION)                                  EXR00183
 163  FORMAT(1H+,70X,9HINVERSION)                                       EXR00184
      GOTO 2000                                                         ***
 64   WRITE(IWC,164) X1,X2                                              EXR00186
 164  FORMAT(1H+,19X,28HCOLUMN INDEX OF THE MATRIX *,A1,21H* IS NEGATIVEEXR00187
     1 OR ZERO,F12.2)                                                   EXR00188
      GOTO 2000                                                         ***
 66   WRITE(IWC,166) X1,X2                                              EXR00190
 166  FORMAT(1H+,19X,25HROW INDEX OF THE MATRIX *,A1,21H* IS NEGATIVE OREXR00191
     1 ZERO,F12.2)                                                      EXR00192
      GOTO 2000                                                         ***
 68   WRITE(IWC,168) X1,I1,I2,X2                                        EXR00194
 168  FORMAT(1H+,19X,35HILLEGAL REDIMENSION OF THE MATRIX *,A1,7H* ROWS=EXR00195
     1,I5,9H COLUMNS=,I5,26H  MAXIMAL STORAGE ELEMENTS,F7.0)            EXR00196
      GOTO 2000                                                         ***
 70   WRITE(IWC,170) X1,I1,I2                                           EXR00198
 170  FORMAT(1H+,19X,8HMATRIX *,A1,30H* IS NOT DIMENSIONED N*N ROWS=,I5,EXR00199
     19H COLUMNS=,I5)                                                   EXR00200
      GOTO 2000                                                         ***
 72   WRITE(IWC,172)                                                    EXR00202
 172  FORMAT(1H+,19X,52HERROR IN MATRIX OPERATION -- ADDITION OR SUBTRACEXR00203
     1TION)                                                             EXR00204
      GOTO 2000                                                         ***
 74   WRITE(IWC,174) IIMAGE,MAXIMA                                      EXR00206                                                   EXR001
 174  FORMAT(1H+,19X,15HOUTPUT EXCEEDS ,I4,30H CHARACTERS AT IMAGE STATEEXR00207
     1MENT,I5)                                                          EXR00208
      GOTO 2000                                                         EXR00209
 76   WRITE(IWC,176) IEXPO                                              ***
 176  FORMAT(1H+,19X,28HEXPONENT OVERFLOW IN TANGENT,2X,9HMAXIMUM =,I3) ***
      GOTO 2000                                                         EXR00212
 78   WRITE(IWC,178) X1                                                 EXR00213
 178  FORMAT(1H+,19X,47HNO NEGATIVE COLUMN-NUMBER IN PRINT TAB ALLOWED=,EXR00214
     1F10.0)                                                            EXR00215
      GOTO 2000                                                         EXR00216
 80   WRITE(IWC,180) X1,X2,IEXPO                                        ***
 180  FORMAT(1H+,19X,48HEXPONENT OVERFLOW OR UNDERFLOW IN MULTIPLICATIONEXR00218
     1,E16.9,3H * ,E16.9,2X,9HMAXIMUM =,I3)                             ***
      GOTO 2000                                                         EXR00220
 82   WRITE(IWC,182) I1                                                 EXR00221
 182  FORMAT(1H ,109HTHE VALUE OF THE EXPRESSION IN A COMPUTED GOTO OR GEXR00222
     1OSUB CANNOT BE USED TO SELECT A STATEMENT NUMBER - VALUE =,I10)   EXR00223
      GOTO 2000                                                         EXR00224
 84   WRITE(IWC,184)                                                    EXR00225
 184  FORMAT(1H+,19X,35HDIVISION ERROR IN MAT LET STATEMENT/)           EXR00226
      GOTO 28                                                           EXR00227
 86   WRITE(IWC,186)                                                    EXR00228
 186  FORMAT(1H+,19X,41HEXPONENTIATION ERROR IN MAT LET STATEMENT/)     EXR00229
      IF(NERROR.EQ.44) GOTO 32                                          EXR00230
      GOTO 30                                                           EXR00231
 90   WRITE(IWC,190)                                                    EXR00232
 190  FORMAT(1H+,19X,41HMULTIPLICATION ERROR IN MAT LET STATEMENT/)     EXR00233
      GOTO 80                                                           EXR00234
 92   WRITE(IWC,192) I2,I1                                              EXR00235
 192  FORMAT(1H+,19X,8HSENTENCE,I4,43H CANNOT BE RESET TO NEGATIVE INDEX***
     1-POSITION,I4)                                                     ***
      GOTO 2000                                                         ***
 94   WRITE(IWC,194) I1                                                 EXR00239
 194  FORMAT(1H+,19X,15HSENTENCE NUMBER,I4,25H IS LESS OR EQUAL TO ZERO)EXR00240
      GOTO 2000                                                         ***
 98   WRITE(IWC,198) ALPH(I1)                                           EXR00242
 198  FORMAT(1H+,19X,27HDETERMINANT OF THE MATRIX *,A1,9H* IS ZERO)     EXR00243
      GOTO 2000                                                         ***
 100  WRITE(IWC,101) I1,I1                                              EXR00245
 101  FORMAT(1H+,19X,20HI CAN ONLY INVERT A ,I3,3H * ,I3,7H MATRIX)     EXR00246
      GOTO 2000                                                         ***
 103  WRITE(IWC,105) ALPH(I1),ALPH(I1),ALPH(I2),ALPH(I1)                ***
 105  FORMAT(1H+,19X,34HMATRIX MULTIPLICATION OF THE FORM ,A1,1H=,A1,1H****
     1,A1,15H NOT ALLOWED - ,A1,40H MUST BE DIMENSIONED AS A ONE-ROW MAT***
     2RIX)                                                              ***
      GOTO 2000                                                         ***
 1999 WRITE(IWC,1998) CARDP                                             EXR00248
 1998 FORMAT(10X,80A1)                                                  EXR00249
 2000 WRITE(IWC,2001) NERROR                                            EXR00250
 2001 FORMAT(20X,11HERROR CODE=,I3)                                     EXR00251
      WRITE(IWC,2002) IERR(NERROR)                                      EXR00252
 2002 FORMAT(1H ,13X,5(1H*),21H SEE BASIC TEXTBOOK =,2X,A8,2X,5(1H*))   EXR00253
      NERROR=-1                                                         EXR00254
      IF(NSTLST.GT.0) GOTO 841                                          EXR00255
      NSTAT=0                                                           EXR00256
      GOTO 840                                                          EXR00257
 841  DO 835 K=1,NSTLST                                                 EXR00258
      IF(ISTLST(K).LT.INREG) GOTO 837                                   EXR00259
 835  CONTINUE                                                          EXR00260
      K=NSTLST                                                          EXR00261
 837  IF(K.LT.2) K=2                                                    EXR00262
      NSTAT=LISTST(K-1)                                                 EXR00263
 840  WRITE(IWC,838) NSTAT                                              EXR00264
 838  FORMAT(13X,27H ***** I WAS AT LINE NUMBER,I5)                     EXR00265
      NX=3                                                              EXR00266
      CALL PRILIN(NX)                                                   EXR00267
      IRC=IMIRC                                                         EXR00268
      RETURN                                                            EXR00269
      END                                                               EXR00270
