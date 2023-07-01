      SUBROUTINE ZFILE(MCOM)                                            ZFI00001
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZFI00002
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, ZFI00003
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZFI00004
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZFI00005
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZFI00006
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZFI00007
     1NIFMAX,INTZEI                                                     ZFI00008
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZFI00009
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZFI00010
     1IRET(20),XXX(4),NFILE(25,3)                                       ZFI00011
      COMMON// ISTLST(340),LISTST(340)                                  ZFI00012
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZFI00014
      DIMENSION IPROG(3700)                                             ZFI00015
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZFI00016
      IF(MCOM.NE.-1) GOTO 1                                             ZFI00017
      MSTAT=0                                                           ZFI00018
C**** MAXFIL MUST BE EQUAL TO THE FIRST SUBSCRIPT IN NFILE              ZFI00019
      NUMFIL=0                                                          ZFI00020
      DO 100 I=1,MAXFIL                                                 ZFI00021
      NFILE(I,1)=-1                                                     ZFI00022
      NFILE(I,2)=0                                                      ZFI00023
      NFILE(I,3)=0                                                      ZFI00024
 100  CONTINUE                                                          ZFI00025
      RETURN                                                            ZFI00026
C***  MCOM=1 COMMAND IS OPEN       CODE IS -48                          ZFI00027
C***  MCOM=2 COMMAND IS PUT        CODE IS -49/-50                      ZFI00028
C***  MCOM=3 COMMAND IS GET        CODE IS -51/-52                      ZFI00029
C***  MCOM=4 COMMAND IS RESET      CODE IS -53/-54                      ZFI00030
C***  MCOM=5 COMMAND IS CLOSE      CODE IS -55                          ZFI00031
C***  MCOM=6 COMMAND IS COMMON-FILE  NO OPERATION CODE                  ZFI00032
C***  MCOM=7 COMMAND IS MAT GET    CODE IS -58/-59                      ZFI00033
C***  MCOM=8 COMMAND IS MAT PUT    CODE IS -60/-61                      ZFI00034
C***  COMPILATION OF A POINTER POSITION IN A RESET COMMAND  CODE IS -56 ZFI00035
C***  COMPILATION OF A SENTENCE NUMBER IN A STATEMENT CODE IS -57       ZFI00036
 1    GOTO(1000,2000,3000,4000,5000,6000,7500,8000),MCOM                ZFI00037
C**** COMMAND IS OPEN                                                   ZFI00038
 1000 ITANF=IBEGST+4                                                    ZFI00039
      MOP=-48                                                           ZFI00040
 1200 MSTAT=MSTAT+1                                                     ZFI00041
 1100 IF(ITANF.GE.LNGCRP) GOTO 7000                                     ZFI00042
 1004 IF(CARDP(ITANF).NE.QUOTE) GOTO 7001                               ZFI00043
      DO 1001 I=ITANF,LNGCRP                                            ZFI00044
      IF(CARDP(I).EQ.COMMA) GOTO 1002                                   ZFI00045
 1001 CONTINUE                                                          ZFI00046
      I=LNGCRP+1                                                        ZFI00047
C**** SEQUENTIAL TEST                                                   ZFI00048
 1002 IF(CARDP(I-1).NE.QUOTE) GOTO 1003                                 ZFI00049
      FNUM=1.1                                                          ZFI00050
      FFNUM=0.                                                          ZFI00051
      K=I                                                               ZFI00052
 1007 CALL STRING(ITANF+1,K-2,IX)                                       ZFI00053
      IF(MCOM.NE.6) GOTO 1011                                           ZFI00054
C**** COMMAND IS COMMON-FILE                                            ZFI00055
      IF(FFNUM.EQ.0.) GOTO 1028                                         ZFI00056
      NN=1                                                              ZFI00057
      NERROR=6                                                          ZFI00058
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                ZFI00059
 1028 CALL FINDFI(IX,K)                                                 ZFI00060
      IF(K.LE.NUMFIL) GOTO 7005                                         ZFI00061
      NUMFIL=NUMFIL+1                                                   ZFI00062
      IF(NUMFIL.GT.MAXFIL) GOTO 7006                                    ZFI00063
      NFILE(NUMFIL,1)=IX                                                ZFI00064
      NFILE(NUMFIL,2)=1000                                              ZFI00065
      NFILE(NUMFIL,3)=FNUM                                              ZFI00066
      IF(NUMFIL.GT.1) NFILE(NUMFIL,2)=NFILE(NUMFIL-1,2)+NFILE(NUMFIL-1,3ZFI00067
     1)                                                                 ZFI00068
      IF(NFILE(NUMFIL,2)+NFILE(NUMFIL,3).GT.MAXSAT) GOTO 7002           ZFI00069
      GOTO 1030                                                         ZFI00070
 1011 IPROG(INREG)=MOP                                                  ZFI00071
      IPROG(INREG-1)=IX                                                 ZFI00072
      IPROG(INREG-2)=FNUM                                               ZFI00073
      IPROG(INREG-3)=FFNUM                                              ZFI00074
      INREG=INREG-4                                                     ZFI00075
 1030 ITANF=I+1                                                         ZFI00076
      IF(ITANF.LT.LNGCRP) GOTO 1004                                     ZFI00077
      RETURN                                                            ZFI00078
C**** INDEX-SEQUENTIAL                                                  ZFI00079
 1003 DO 1005 K=ITANF,I                                                 ZFI00080
      IF(CARDP(K).EQ.PUCO) GOTO 1006                                    ZFI00081
 1005 CONTINUE                                                          ZFI00082
      K=I                                                               ZFI00083
 1006 IF(CARDP(K-1).NE.QUOTE) GOTO 7001                                 ZFI00084
      IF(MCOM.EQ.4) GOTO 1500                                           ZFI00085
      CALL ZCONVN(K+1,I-1,FNUM)                                         ZFI00086
      IF(FNUM.GT.FLOAT(MAXSAT)) GOTO 7002                               ZFI00087
      IF(FNUM.LT.0.) GOTO 7004                                          ZFI00088
      IX=I+1                                                            ZFI00089
      IF((CARDP(IX).NE.QUOTE).AND.(I.LT.LNGCRP)) GOTO 1027              ZFI00090
      FFNUM=0.                                                          ZFI00091
      GOTO 1007                                                         ZFI00092
 1027 DO 1025 I=IX,LNGCRP                                               ZFI00093
      IF(CARDP(I).EQ.COMMA) GOTO 1026                                   ZFI00094
 1025 CONTINUE                                                          ZFI00095
      I=LNGCRP+1                                                        ZFI00096
 1026 CALL ZCONVN(IX,I-1,FFNUM)                                         ZFI00097
      IF(FFNUM.LT.0) GOTO 7007                                          ZFI00098
      GOTO 1007                                                         ZFI00099
C**** RESET -- INDEX-SEQUENTIAL                                         ZFI00100
 1500 IX=K+1                                                            ZFI00101
      MOP=-54                                                           ZFI00102
      NZ=NERRS                                                          ZFI00103
      DO 1501 KK=IX,I                                                   ZFI00104
      IF(CARDP(KK).EQ.PUCO) GOTO 1502                                   ZFI00105
 1501 CONTINUE                                                          ZFI00106
      KK=I                                                              ZFI00107
      FNUM=2.1                                                          ZFI00108
      GOTO 1503                                                         ZFI00109
C**** POINTER POSITION                                                  ZFI00110
 1502 CALL ZTRANX(KK+1,I-1)                                             ZFI00111
      IF(NZ.NE.NERRS) GOTO 7004                                         ZFI00112
      FNUM=3.1                                                          ZFI00113
      IPROG(INREG)=-56                                                  ZFI00114
      INREG=INREG-1                                                     ZFI00115
C**** SENTENCE NUMBER                                                   ZFI00116
 1503 CALL ZTRANX(K+1,KK-1)                                             ZFI00117
      IF(NZ.NE.NERRS) GOTO 7004                                         ZFI00118
      IPROG(INREG)=-57                                                  ZFI00119
      INREG=INREG-1                                                     ZFI00120
      GOTO 1007                                                         ZFI00121
C**** COMMAND IS COMMON-FILE                                            ZFI00122
 6000 ITANF=IBEGST+11                                                   ZFI00123
      IF(MSTAT.NE.0) GOTO 7003                                          ZFI00124
      GOTO 1100                                                         ZFI00125
C**** COMMAND IS CLOSE                                                  ZFI00126
 5000 MOP=-55                                                           ZFI00127
      ITANF=IBEGST+5                                                    ZFI00128
      GOTO 1200                                                         ZFI00129
C**** COMMAND IS PUT                                                    ZFI00130
 2000 MOP=-49                                                           ZFI00131
 3001 MSTAT=MSTAT+1                                                     ZFI00132
      IBEGST=IBEGST+3                                                   ZFI00133
      IF(CARDP(IBEGST).NE.QUOTE) GOTO 7001                              ZFI00134
      IF(IBEGST.GT.LNGCRP-3) GOTO 7000                                  ZFI00135
      DO 2001 I=IBEGST,LNGCRP                                           ZFI00136
      IF(CARDP(I).EQ.COMMA) GOTO 2003                                   ZFI00137
 2001 CONTINUE                                                          ZFI00138
 7000 NERROR=42                                                         ZFI00139
 2222 NN=0                                                              ZFI00140
      CALL COMERR(NERROR,I1,I2,X1,X2,NN)                                ZFI00141
      NERRS=NERRS+1                                                     ZFI00142
      RETURN                                                            ZFI00143
 2003 IF(CARDP(I-1).NE.QUOTE) GOTO 2004                                 ZFI00144
      K=I                                                               ZFI00145
 2007 CALL STRING(IBEGST+1,K-2,IX)                                      ZFI00146
      IF(MOP.LT.-57) GOTO 7010                                          ZFI00147
      CALL ZLISTE(I+1,MOP,IX,MCOM)                                      ZFI00148
      RETURN                                                            ZFI00149
 2004 DO 2005 K=IBEGST,I                                                ZFI00150
      IF(CARDP(K).EQ.PUCO) GOTO 2006                                    ZFI00151
 2005 CONTINUE                                                          ZFI00152
 2006 IF(CARDP(K-1).NE.QUOTE) GOTO 7001                                 ZFI00153
      CALL ZTRANX(K+1,I-1)                                              ZFI00154
      IPROG(INREG)=-57                                                  ZFI00155
      INREG=INREG-1                                                     ZFI00156
      MOP=MOP-1                                                         ZFI00157
      GOTO 2007                                                         ZFI00158
C**** COMMAND IS GET                                                    ZFI00159
 3000 MOP=-51                                                           ZFI00160
      GOTO 3001                                                         ZFI00161
C**** COMMAND IS RESET                                                  ZFI00162
 4000 ITANF=IBEGST+5                                                    ZFI00163
      MOP=-53                                                           ZFI00164
      GOTO 1200                                                         ZFI00165
C**** COMMAND IS MAT GET                                                ZFI00166
 7500 MOP=-58                                                           ZFI00167
      GOTO 3001                                                         ZFI00168
 7010 ITANF=I+1                                                         ZFI00169
 7013 DO 7011 K=ITANF,LNGCRP                                            ZFI00170
      IF(CARDP(K).EQ.COMMA) GOTO 7012                                   ZFI00171
 7011 CONTINUE                                                          ZFI00172
      K=LNGCRP+1                                                        ZFI00173
 7012 I=K-1                                                             ZFI00174
      IF(CARDP(I).EQ.DOLSGN) I=I-1                                      ZFI00175
      CALL ZALPH(CARDP(I),KK)                                           ZFI00176
      IF(KK.GT.26) GOTO 7050                                            ZFI00177
      CALL ZKLAM(I,I,KK)                                                ZFI00178
      IPROG(INREG)=MOP                                                  ZFI00179
      IPROG(INREG-1)=IX                                                 ZFI00180
      IPROG(INREG-2)=KK                                                 ZFI00181
      INREG=INREG-3                                                     ZFI00182
      IF(K.GE.LNGCRP) RETURN                                            ZFI00183
      ITANF=K+1                                                         ZFI00184
      GOTO 7013                                                         ZFI00185
C**** COMMAND IS MAT PUT                                                ZFI00186
 8000 MOP=-60                                                           ZFI00187
      GOTO 3001                                                         ZFI00188
 7050 NERROR=43                                                         ZFI00189
      X1=CARDP(K)                                                       ZFI00190
      GOTO 2222                                                         ZFI00191
 7001 NERROR=44                                                         ZFI00192
      GOTO 2222                                                         ZFI00193
 7002 NERROR=45                                                         ZFI00194
      I2=FNUM                                                           ZFI00195
      GOTO 2222                                                         ZFI00196
 7003 NERROR=46                                                         ZFI00197
      GOTO 2222                                                         ZFI00198
 7004 NERROR=47                                                         ZFI00199
      GOTO 2222                                                         ZFI00200
 7005 NERROR=48                                                         ZFI00201
      GOTO 2222                                                         ZFI00202
 7006 NERROR=49                                                         ZFI00103
      GOTO 2222                                                         ZFI00104
 7007 NERROR=50                                                         ZFI00105
      GOTO 2222                                                         ZFI00106
      END                                                               ZFI00107

