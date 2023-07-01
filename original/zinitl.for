      SUBROUTINE ZINITL                                                 ZIN00001
C*****ROUTINE TO INITIALIZE SYSTEM                                      ZIN00002
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZIN00003
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST, INEXT,ZIN00004
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZIN00005
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZIN00006
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZIN00007
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZIN00008
     1NIFMAX,INTZEI                                                     ZIN00009
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZIN00010
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZIN00011
     1IRET(20),XXX(4),NFILE(25,3)                                       ZIN00012
      COMMON// ISTLST(340),LISTST(340)                                  ZIN00013
      COMMON// DATAN(330)                                               ***
      COMMON// XDATA(3700)                                              ZIN00015
      DIMENSION IPROG(3700)                                             ZIN00016
      EQUIVALENCE (XDATA(1),IPROG(1))                                   ZIN00017
      DIMENSION ATEMP(26),DIGTMP(10),CHARTM(20)                         ZIN00018
      DATA ATEMP(1)/1HA/                                                ZIN00019
      DATA ATEMP(2)/1HB/                                                ZIN00020
      DATA ATEMP(3)/1HC/                                                ZIN00021
      DATA ATEMP(4)/1HD/                                                ZIN00022
      DATA ATEMP(5)/1HE/                                                ZIN00023
      DATA ATEMP(6)/1HF/                                                ZIN00024
      DATA ATEMP(7)/1HG/                                                ZIN00025
      DATA ATEMP(8)/1HH/                                                ZIN00026
      DATA ATEMP(9)/1HI/                                                ZIN00027
      DATA ATEMP(10)/1HJ/                                               ZIN00028
      DATA ATEMP(11)/1HK/                                               ZIN00029
      DATA ATEMP(12)/1HL/                                               ZIN00030
      DATA ATEMP(13)/1HM/                                               ZIN00031
      DATA ATEMP(14)/1HN/                                               ZIN00032
      DATA ATEMP(15)/1HO/                                               ZIN00033
      DATA ATEMP(16)/1HP/                                               ZIN00034
      DATA ATEMP(17)/1HQ/                                               ZIN00035
      DATA ATEMP(18)/1HR/                                               ZIN00036
      DATA ATEMP(19)/1HS/                                               ZIN00037
      DATA ATEMP(20)/1HT/                                               ZIN00038
      DATA ATEMP(21)/1HU/                                               ZIN00039
      DATA ATEMP(22)/1HV/                                               ZIN00040
      DATA ATEMP(23)/1HW/                                               ZIN00041
      DATA ATEMP(24)/1HX/                                               ZIN00042
      DATA ATEMP(25)/1HY/                                               ZIN00043
      DATA ATEMP(26)/1HZ/                                               ZIN00044
      DATA DIGTMP(1)/1H0/                                               ZIN00045
      DATA DIGTMP(2)/1H1/                                               ZIN00046
      DATA DIGTMP(3)/1H2/                                               ZIN00047
      DATA DIGTMP(4)/1H3/                                               ZIN00048
      DATA DIGTMP(5)/1H4/                                               ZIN00049
      DATA DIGTMP(6)/1H5/                                               ZIN00050
      DATA DIGTMP(7)/1H6/                                               ZIN00051
      DATA DIGTMP(8)/1H7/                                               ZIN00052
      DATA DIGTMP(9)/1H8/                                               ZIN00053
      DATA DIGTMP(10)/1H9/                                              ZIN00054 
      DATA CHARTM(1)/1H*/                                               ZIN00055
      DATA CHARTM(2)/1H /                                               ZIN00056
      DATA CHARTM(3)/1H,/                                               ZIN00057
      DATA CHARTM(4)/1H./                                               ZIN00058
      DATA CHARTM(5)/1H=/                                               ZIN00059
      DATA CHARTM(6)/1H)/                                               ZIN00060
      DATA CHARTM(7)/1H(/                                               ZIN00061
      DATA CHARTM(8)/1H+/                                               ZIN00062
      DATA CHARTM(9)/1H'/                                               ZIN00063
      DATA CHARTM(10)/1H$/                                              ZIN00064
      DATA CHARTM(11)/1H-/                                              ZIN00065
      DATA CHARTM(12)/1H//                                              ZIN00066
      DATA CHARTM(13)/1H;/                                              ZIN00067
      DATA CHARTM(14)/1H</                                              ZIN00068
      DATA CHARTM(15)/1H>/                                              ZIN00069
      DATA CHARTM(16)/1H"/                                              ZIN00070
      DATA CHARTM(17)/1H%/                                              ZIN00071
      DATA CHARTM(18)/1H^/                                              ZIN00072
      DATA CHARTM(19)/1H!/                                              ZIN00073
      DATA CHARTM(20)/1H:/                                              ZIN00074
      DATA ICONT/8HBUILD---/                                            ***
      DATA ILIST/8HLIST-ERR/                                            ZIN00076
      DATA ILINE/8HNO-HEADL/                                            ZIN00077
C----AUF DIE FOLGENDE ZEILE LN=0079 WIRD IM TEXT BEZUG GENOMMEN vv      ***
      NSTZEI=1                                                          ZIN00078
C*****INITIALIZE                                                        ZIN00079
C                                                                       ZIN00080
C*****DATA AND IPROG MUST BE DIMENSIONED TO EQUAL NCELLP                ZIN00081
      NCELLP=3700                                                       ZIN00082
C*****DATAN MUST BE DIMENSIONED TO EQUAL NCELLD                         ZIN00083
      NCELLD=330                                                        ***
C*****CHANNEL NUMBER FOR READ IS IRC=60 (STANDARD INPUT SYSTEM)         ZIN00085
C******IMIRC RESETS IRC TO THE VALUE OF STANDARD INPUT SYSTEM IF IRC IS ZIN00086
C     CHANGED                                                           ZIN00087
      IRC=60                                                            ZIN00088
      IMIRC=IRC                                                         ZIN00089
C*****CHANNEL NUMBER FOR WRITE IS IWC=61  (STANDARD OUTPUT SYSTEM)      ZIN00090
      IWC=61                                                            ZIN00091
C**** BUILT INTERNAL CODE FOR ALL CHARACTERS IF NSTZEI NOT EQUAL 1      ZIN00092
C**** AND IF THE FIRST CARD INCLUDS THE WORD *BUILD* BEGINNING WITH     ZIN00093
C**** COLUMN 3                                                          ZIN00094
      REWIND 999                                                        ZIN00095
      WRITE(999,2) (CARD(I),I=3,10)                                     ZIN00096
      REWIND 999                                                        ZIN00097
      READ(999,3) IBUILT                                                ZIN00098
 3    FORMAT(A8)                                                        ZIN00099
      IF((IBUILT.NE.ICONT).OR.(NSTZEI.EQ.1)) GOTO 1                     ZIN00100
C**** LETTERS                                                           ZIN00101
      READ(IRC,2) (ATEMP(I),I=1,26)                                     ZIN00102
 2    FORMAT(80A1)                                                      ZIN00103
C**** NUMBERS                                                           ZIN00104
      READ(IRC,2) (DIGTMP(I),I=1,10)                                    ZIN00105
C**** SPECIAL CHARACTERS                                                ZIN00106
      READ(IRC,2) (CHARTM(I),I=1,20)                                    ZIN00107
      NSTZEI=1                                                          ZIN00108
C**** MAXIMAL INTERNAL EXPONENT FOR FLOATING POINT NUMBERS IS IEXPO     ZIN00109
 1    IEXPO=99                                                          ZIN00110
C**** MAXIMAL INTEGER VALUE FOR 48-BIT-WORD                             ZIN00111
      INTMAX=140737488355327                                            ZIN00112
C**** MAXIMAL EXPONENT FOR 48-BIT-WORD                                  ZIN00113
      INTNUM=14                                                         ZIN00114
C*****NZIM COUNTS IMAGE STATEMENTS                                      ZIN00115
      NZIM=1                                                            ZIN00116
C*****CHANNEL NUMBER FOR IMAGE MEMORY ON DISK                           ZIN00117
      NIMAGE=100                                                        ZIN00118
      REWIND NIMAGE                                                     ZIN00119
C**** MAXIMAL NUMBER OF IMAGE STATEMENTS                                ZIN00120
      MAXIMA=300                                                        ZIN00121
C*****CHARACTERS PER PRINT ZONE                                         ZIN00122
      IZONE=15                                                          ZIN00123
C**** CHARACTERS PER LINE WITH PRINT USING                              ZIN00124
      IIMAGE=135                                                        ZIN00125
C**** SMALLEST ABSOLUTE VALUE WHICH CAN BE PRINTED WITH STANDARD PRINT  ZIN00126
      SMALL=1.E-38                                                      ZIN00127
C**** MAXIMAL STATEMENT NUMBER                                          ZIN00128
      ISTMAX=9999                                                       ZIN00129
C**** MAXIMAL NESTED FOR/NEXT LOOPS                                     ZIN00130
C**** IFOR MUST BE DIMENSIONED EQUAL TO NIFMAX                          ZIN00131
      NIFMAX=20                                                         ZIN00132
C**** MAXIMAL NESTED GOSUB/RETURN                                       ZIN00133
C**** IRET MUST BE DIMENSIONED EQUAL TO NIRMAX                          ZIN00134
      NIRMAX=20                                                         ZIN00135
      WRITE(IWC,5)                                                      ZIN00136
 5    FORMAT(1H1)                                                       ZIN00137
      INREG=NCELLP                                                      ZIN00138
      IF (IBUILT.NE.ILIST) GOTO 9                                       ZIN00139
      NSTZEI=2                                                          ZIN00140
C**** NM=55  NUMBER OF ERRORS                                           ZIN00141
      NM=55                                                             ZIN00142
      NN=-1                                                             ZIN00143
 8    NN=NN+1                                                           ZIN00144
      DO 6 I=1,NM                                                       ZIN00145
      NERROR=I                                                          ZIN00146
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                ZIN00147
 6    CONTINUE                                                          ZIN00148
      IF (NN.EQ.1) GOTO 7                                               ZIN00149
      IF(NN.GE.2) GOTO 4                                                ZIN00150
C**** NM=7 NUMBER OF WARNNIGS                                           ZIN00151
C**** WARNING NUMBER 8 AND 9 CONTAINS SUBROUTINE *STRING*               ZIN00152
      NM=7                                                              ZIN00153
      GOTO 8                                                            ZIN00154
C**** NM=12 NUMBER OF REASONS                                           ZIN00155
 7    NM=12                                                             ZIN00156
      GOTO 8                                                            ZIN00157
 4    WRITE(IWC,5)                                                      ZIN00158
 9    IF(IBUILT.EQ.ILINE) NSTZEI=3                                      ZIN00159
      DO 60 I=1,10                                                      ZIN00160
      IF(CARD(2).EQ.DIGTMP(I)) GOTO 61                                  ZIN00161
 60   CONTINUE                                                          ZIN00162
      IPEND=5                                                           ZIN00163
      GOTO 62                                                           ZIN00164
 61   IPEND=I-1                                                         ZIN00165
      IF(IPEND.EQ.0)IPEND=1                                             ZIN00166
      IF(IPEND.GT.8) IPEND=8                                            ZIN00167
 62   IWRIT=IZONE*IPEND+1                                               ZIN00168
C*****ISTLST AND LISTST MUST BE DIMENSIONED EQUAL TO NSTEND             ZIN00169
      NSTEND=340                                                        ZIN00170
C**** INITIALIZE FILE-COMMAND                                           ZIN00171
      NERRS=-1                                                          ZIN00172
C**** MAXFIL = MAXIMAL NUMBER OF FILE-NAMES                             ZIN00173
      MAXFIL=25                                                         ZIN00174
C**** MAXSAT LESS 1000 = MAXIMAL NUMBER OF ALLOCATED SENTENCES          ZIN00175
      MAXSAT=1024                                                       ZIN00176
C----AUF DIE FOLGENDE ZEILE LN=179 WIRD IM TEXT BEZUG GENOMMENvv        ***
      CALL ZFILE(NERRS)                                                 ZIN00177
      INEXT=324                                                         ZIN00178
      NERRS=0                                                           ZIN00179
      NEXTDT=0                                                          ZIN00180
      NIFOR=0                                                           ZIN00181
      NSTLST=0                                                          ZIN00182
      DO 20 I=1,26                                                      ZIN00183
 20   IPROG(I)=2                                                        ZIN00184
      DO 30 I=28,53                                                     ZIN00185
  30  XDATA(I)=11.                                                      ZIN00186
      DO 35 I=54,313                                                    ZIN00187
 35   XDATA(I)=0.                                                       ZIN00188
      DO 36 I=314,323                                                   ZIN00189
 36   XDATA(I)=I-314                                                    ZIN00190
      DO 38 I=324,NCELLP                                                ZIN00191
 38   XDATA(I)=0.                                                       ZIN00192
C                                                                       ZIN00193
C*****SET UP VOCABULARY                                                 ZIN00194
C*****LOAD ALPHABETIC CHARACTERS                                        ZIN00195
      DO 40 I=1,26                                                      ZIN00196
 40   ALPH(I)=ATEMP(I)                                                  ZIN00197
C                                                                       ZIN00198
C*****LOAD DIGITS                                                       ZIN00199
      DO 41 I=1,10                                                      ZIN00200
 41   DIGIT(I)=DIGTMP(I)                                                ZIN00201
C                                                                       ZIN00202
C*****LOAD SPECIAL CHARACTERS                                           ZIN00203
      ASTRSK=CHARTM(1)                                                  ZIN00204
      BLANK=CHARTM(2)                                                   ZIN00205
      COMMA=CHARTM(3)                                                   ZIN00206
      DECMAL=CHARTM(4)                                                  ZIN00207
      EQUALS=CHARTM(5)                                                  ZIN00208
      PARRT=CHARTM(6)                                                   ZIN00209
      PARLFT=CHARTM(7)                                                  ZIN00210
      PLUS=CHARTM(8)                                                    ZIN00211
      QUOTE=CHARTM(9)                                                   ZIN00212
      DOLSGN=CHARTM(10)                                                 ZIN00213
      CMINUS=CHARTM(11)                                                 ZIN00214
      SLASH=CHARTM(12)                                                  ZIN00215
      PUCO=CHARTM(13)                                                   ZIN00216
      VLESS=CHARTM(14)                                                  ZIN00217
      VGREAT=CHARTM(15)                                                 ZIN00218
      DQUOTE=CHARTM(16)                                                 ZIN00219
      DOPU=CHARTM(17)                                                   ZIN00220
      EXSIGN=CHARTM(18)                                                 ZIN00221
      XNULL=CHARTM(19)                                                  ZIN00222
      DDOPU=CHARTM(20)                                                  ZIN00223
C                                                                       ZIN00224
C***** CLEAR MERKER(I,J)                                                ZIN00225
      DO 44 I=1,2                                                       ZIN00226
      DO 44 J=1,26                                                      ZIN00227
      MERKER(J,I)=0                                                     ZIN00228
 44   CONTINUE                                                          ZIN00229
C                                                                       ZIN00230
C*****LOAD DIGITS INTO POSITIONS 27 THROUGH 36 OF ALPH                  ZIN00231
      LOC=26                                                            ZIN00232
      DO 42 I=1,10                                                      ZIN00233
      LOC=LOC+1                                                         ZIN00234
 42   ALPH(LOC)=DIGIT(I)                                                ZIN00235
C                                                                       ZIN00236
C*****LOAD OTHER CHARACTERS INTO POSITIONS 37 THROUGH 48 OF ALPH        ZIN00237
      DO 43 I=1,12                                                      ZIN00238
      LOC=LOC+1                                                         ZIN00239
 43   ALPH(LOC)=CHARTM(I)                                               ZIN00240
      CALL ZHOPPR(VALUE,NSTOP,1,-1)                                     ZIN00241
      RETURN                                                            ZIN00242
      END                                                               ZIN00243

      