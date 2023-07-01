      SUBROUTINE ZEXFIL(IOP)                                            EXF00001
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      EXF00002
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, EXF00003
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  EXF00004
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, EXF00005
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     EXF00006
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     EXF00007
     1NIFMAX,INTZEI                                                     EXF00008
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        EXF00009
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      EXF00010
     1IRET(20),XXX(4),NFILE(25,3)                                       EXF00011
      COMMON// ISTLST(340),LISTST(340)                                  EXF00012
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               EXF00014
      DIMENSION IPROG(3700)                                             EXF00015
      EQUIVALENCE (DATA(1),IPROG(1))                                    EXF00016
      IF(IOP.GT.14) GOTO 62000                                          EXF00017
      IF(IOP.GT.0) GOTO 1000                                            EXF00018
      IF(NFILE(1,1).EQ.-1) RETURN                                       EXF00019
C**** REWIND COMMON-FILE                                                EXF00020
      DO 10 I=1,NUMFIL                                                  EXF00021
      MIC=NFILE(I,3)                                                    EXF00022
      DO 20 LOCU=1,MIC                                                  EXF00023
      LOCS=NFILE(I,2)+LOCU-1                                            EXF00024
      REWIND LOCS                                                       EXF00025
 20   CONTINUE                                                          EXF00026
 10   CONTINUE                                                          EXF00027
      RETURN                                                            EXF00028
 1000 GOTO(48000,49000,50000,51000,52000,53000,54000,55000,56000,57000,5EXF00029
     18000,59000,60000,61000),IOP                                       EXF00030
C**** COMMAND IS OPEN                                                   EXF00031
48000 IX=IPROG(INREG-1)                                                 EXF00032
      NWORT=IPROG(INREG-3)                                              EXF00033
      IF(NUMFIL.EQ.0) GOTO 48007                                        EXF00034
      ITOP=1                                                            EXF00035
      IB=1                                                              EXF00036
C**** TRY TO FIND THE FILE NAME                                         EXF00037
48005 DO 48001 IA=ITOP,NUMFIL                                           EXF00038
      IF(NFILE(IA,1).EQ.IX) GOTO 48010                                  EXF00039
      IF(IB.EQ.1) GOTO 48001                                            EXF00040
      IF(NFILE(IA,1).EQ.-1) GOTO 48003                                  EXF00041
48001 CONTINUE                                                          EXF00042
      IF(IB.EQ.2) GOTO 48007                                            EXF00043
      IB=IB+1                                                           EXF00044
      ITOP=1                                                            EXF00045
      GOTO 48005                                                        EXF00046
C**** ALLOCATE NEW FILE                                                 EXF00047
48007 NUMFIL=NUMFIL+1                                                   EXF00048
      IF(NUMFIL.GT.MAXFIL) GOTO 48012                                   EXF00049
      IA=NUMFIL                                                         EXF00050
      NFILE(IA,3)=IPROG(INREG-2)                                        EXF00051
      NFILE(IA,1)=IX                                                    EXF00052
      IF((IOP.EQ.2).OR.(IOP.EQ.13)) NFILE(IA,3)=1                       EXF00053
      IF(IA.GT.1) GOTO 48008                                            EXF00054
      NFILE(IA,2)=1000                                                  EXF00055
      GOTO 48002                                                        EXF00056
48008 NFILE(IA,2)=NFILE(IA-1,2)+NFILE(IA-1,3)                           EXF00057
      IF(NFILE(IA,2).LE.MAXSAT) GOTO 48002                              EXF00058
      NERROR=21                                                         EXF00059
      GOTO 48013                                                        EXF00060
48002 MIC=NFILE(IA,3)                                                   EXF00061
      DO 48004 LOCU=1,MIC                                               EXF00062
      LOCS=NFILE(IA,2)+LOCU-1                                           EXF00063
      REWIND LOCS                                                       EXF00064
      IF(NWORT.LE.0) GOTO 48004                                         EXF00065
      DO 48009 K=1,NWORT                                                EXF00066
      WRITE(LOCS) BLANK                                                 EXF00067
48009 CONTINUE                                                          EXF00068
      REWIND LOCS                                                       EXF00069
48004 CONTINUE                                                          EXF00070
      IF(IOP.EQ.2) GOTO 51011                                           EXF00071
      IF(IOP.EQ.13) GOTO 60001                                          EXF00072
      IB=4                                                              EXF00073
      GOTO 900                                                          EXF00074
C**** FREE FILE NAME FOUND --TRY TO INSERT                              EXF00075
48003 IF(IPROG(INREG-2).GT.NFILE(IA,3)) GOTO 48006                      EXF00076
      IF((NFILE(IA,3).NE.1).AND.(IPROG(INREG-2).EQ.1)) GOTO 48006       EXF00077
      NFILE(IA,1)=IX                                                    EXF00078
      GOTO 48002                                                        EXF00079
48006 ITOP=IA+1                                                         EXF00080
      GOTO 48005                                                        EXF00081
48010 NERROR=22                                                         EXF00082
48013 CALL EXERR(NERROR,I1,I2,X1,X2)                                    EXF00083
      IOP=-1                                                            EXF00084
      RETURN                                                            EXF00085
48012 NERROR=23                                                         EXF00086
      I1=MSATZ                                                          EXF00087
      I2=NFILE(IA,3)                                                    EXF00088
      GOTO 48013                                                        EXF00089
C**** COMMAND IS PUT -- SEQUENTIAL                                      EXF00090
49000 IX=IPROG(INREG-1)                                                 EXF00091
      MSATZ=1                                                           EXF00092
      CALL FINDFI(IX,IA)                                                EXF00093
      IF(IA.GT.NUMFIL) GOTO 48007                                       EXF00094
51011 IF(MSATZ.GT.NFILE(IA,3)) GOTO 51012                               EXF00095
      IF(MSATZ.LE.0) GOTO 54011                                         EXF00096
      LOCS=NFILE(IA,2)+MSATZ-1                                          EXF00097
      IF(IPROG(INREG-2).EQ.-16) GOTO 49002                              EXF00098
C**** FILE NAME FOUND                                                   EXF00099
      IB=2                                                              EXF00100
      WRITE(LOCS),ACC                                                   EXF00101
 900  INREG=INREG-IB                                                    EXF00102
      RETURN                                                            EXF00103
49002 IC=IPROG(INREG-3)                                                 EXF00104
      DO 49003 K=1,IC                                                   EXF00105
      LOCU=INREG-3-K                                                    EXF00106
      ACC=IPROG(LOCU)                                                   EXF00107
      WRITE(LOCS) ACC                                                   EXF00108
49003 CONTINUE                                                          EXF00109
      IB=4+IC                                                           EXF00110
      GOTO 900                                                          EXF00111
C**** COMMAND IS PUT -- INDEX-SEQUENTIAL                                EXF00112
50000 IX=IPROG(INREG-1)                                                 EXF00113
      CALL FINDFI(IX,IA)                                                EXF00114
      IF(IA.GT.NUMFIL) GOTO 51010                                       EXF00115
      GOTO 51011                                                        EXF00116
51010 NERROR=24                                                         EXF00117
      GOTO 48013                                                        EXF00118
51012 NERROR=25                                                         EXF00119
      I1=MSATZ                                                          EXF00120
      I2=NFILE(IA,3)                                                    EXF00121
      GOTO 48013                                                        EXF00122
C**** COMMAND IS GET -- SEQUENTIAL                                      EXF00123
51000 MSATZ=1                                                           EXF00124
C**** COMMAND IS GET -- INDEX-SEQUENTIAL                                EXF00125
52000 IX=IPROG(INREG-1)                                                 EXF00126
      CALL FINDFI(IX,IA)                                                EXF00127
      IF(IA.GT.NUMFIL) GOTO 51100                                       EXF00128
      IF(MSATZ.GT.NFILE(IA,3)) GOTO 51012                               EXF00129
      IF(MSATZ.LE.0) GOTO 54011                                         EXF00130
      LOCS=NFILE(IA,2)+MSATZ-1                                          EXF00131
      READ(LOCS) ACC                                                    EXF00132
C*-*-                                                                   EXF00133
C*-*- CALL FOR A POSIBLE NON-COMPATIBLE ROUTINE -IFEOF(LOCS)-           EXF00134
C*-*-     ----CHECK IT----       ----CHECK IT----                       EXF00135
C*-*-                                                                   EXF00136
      IF(IFEOF(LOCS).EQ.-1) GOTO 51111                                  EXF00137
      IB=2                                                              EXF00138
      GOTO 900                                                          EXF00139
51100 NERROR=26                                                         EXF00140
      GOTO 48013                                                        EXF00141
C**** COMMAND IS RESET -- SEQUENTIAL                                    EXF00142
53000 IX=IPROG(INREG-1)                                                 EXF00143
      CALL FINDFI(IX,IA)                                                EXF00144
      IF(IA.GT.NUMFIL) GOTO 53001                                       EXF00145
53002 MSATZ=NFILE(IA,3)                                                 EXF00146
      DO 53003 K=1,MSATZ                                                EXF00147
      LOCS=NFILE(IA,2)+K-1                                              EXF00148
      REWIND LOCS                                                       EXF00149
53003 CONTINUE                                                          EXF00150
      IB=4                                                              EXF00151
      GOTO 900                                                          EXF00152
C**** COMMAND IS RESET -- INDEX-SEQUENTIAL                              EXF00153
54000 IX=IPROG(INREG-1)                                                 EXF00154
      CALL FINDFI(IX,IA)                                                EXF00155
      IF(IA.GT.NUMFIL) GOTO 53001                                       EXF00156
      IF(IPROG(INREG-2).LE.1) GOTO 53002                                EXF00157
      IF(MSATZ.GT.NFILE(IA,3)) GOTO 51012                               EXF00158
      IF(MSATZ.LE.0) GOTO 54011                                         EXF00159
      LOCS=NFILE(IA,2)+MSATZ-1                                          EXF00160
      REWIND LOCS                                                       EXF00161
      IF(IPROG(INREG-2).LE.2) GOTO 54001                                EXF00162
C**** RESET POINTER INTO POSITION MPOINT                                EXF00163
      IF((MPOINT.EQ.1).OR.(MPOINT.EQ.0)) GOTO 54001                     EXF00164
      MPOINT=MPOINT-1                                                   EXF00165
      IF(MPOINT.LT.0) GOTO 54010                                        EXF00166
      DO 54002 K=1,MPOINT                                               EXF00167
      READ(LOCS) ACC                                                    EXF00168
C*-*-                                                                   EXF00169
C*-*- CALL FOR A POSIBLE NON-COMPATIBLE ROUTINE -IFEOF(LOCS)-           EXF00170
C*-*-     ----CHECK IT----       ----CHECK IT----                       EXF00171
C*-*-                                                                   EXF00172
      IF(IFEOF(LOCS).EQ.-1) GOTO 51111                                  EXF00173
54002 CONTINUE                                                          EXF00174
54001 IB=4                                                              EXF00175
      GOTO 900                                                          EXF00176
53001 NERROR=27                                                         EXF00177
      GOTO 48013                                                        EXF00178
51111 NERROR=28                                                         EXF00179
      GOTO 48013                                                        EXF00180
54010 NERROR=46                                                         EXF00181
      I2=MSATZ                                                          EXF00182
      I1=MPOINT                                                         EXF00183
      GOTO 48013                                                        EXF00184
54011 NERROR=47                                                         EXF00185
      I1=MSATZ                                                          EXF00186
      GOTO 48013                                                        EXF00187
C**** COMMAND IS CLOSE                                                  EXF00188
55000 IX=IPROG(INREG-1)                                                 EXF00189
      CALL FINDFI(IX,IA)                                                EXF00190
      IF(IA.GT.NUMFIL) GOTO 55001                                       EXF00191
      NFILE(IA,1)=-1                                                    EXF00192
      IB=4                                                              EXF00193
      GOTO 900                                                          EXF00194
55001 NERROR=29                                                         EXF00195
      GOTO 48013                                                        EXF00196
C**** POINTER POSITION IN MPOINT                                        EXF00197
56000 MPOINT=ACC                                                        EXF00198
      IB=1                                                              EXF00199
      GOTO 900                                                          EXF00200
C**** GET SENTENCE NUMBER INTO MSATZ                                    EXF00201
57000 MSATZ=ACC                                                         EXF00202
      IB=1                                                              EXF00203
      GOTO 900                                                          EXF00204
C**** COMMAND IS MAT GET                                                EXF00205
58000 MSATZ=1                                                           EXF00206
59000 IX=IPROG(INREG-1)                                                 EXF00207
      CALL FINDFI(IX,IA)                                                EXF00208
      IF(IA.GT.NUMFIL) GOTO 51100                                       EXF00209
60001 IB=IPROG(INREG-2)                                                 EXF00210
      IZ=DATA(IB+27)+0.1                                                EXF00211
      MPOINT=NFILE(IA,2)+MSATZ-1                                        EXF00212
      IF(MSATZ.GT.NFILE(IA,3)) GOTO 51012                               EXF00213
      IF(MSATZ.LE.0) GOTO 54011                                         EXF00214
      LOCS=DATA(IB)+0.1                                                 EXF00215
      LOCU=DATA(LOCS)+0.1                                               EXF00216
      IF(MERKER(IB,2).EQ.0) GOTO 58005                                  EXF00217
      IS=(LOCU-LOCS-1)/(IZ+1)-1                                         EXF00218
      IBPLUS=1                                                          EXF00219
      ISS=IZ                                                            EXF00220
      GOTO 58004                                                        EXF00221
58005 IZ=LOCU-LOCS-2                                                    EXF00222
      ISS=1                                                             EXF00223
      IS=1                                                              EXF00224
      IBPLUS=-1                                                         EXF00225
58004 DO 58001 IN=1,IS                                                  EXF00226
      DO 58002 IM=1,IZ                                                  EXF00227
      JA=INT(DATA(IB))+IBPLUS+(ISS+1)*IN+IM                             EXF00228
      IF(IOP.GE.13) WRITE(MPOINT) DATA(JA)                              EXF00229
      IF(IOP.LT.13) READ(MPOINT) DATA(JA)                               EXF00230
C*-*-                                                                   EXF00231
C*-*- CALL FOR A POSIBLE NON-COMPATIBLE ROUTINE -IFEOF(LOCS)-           EXF00232
C*-*-     ----CHECK IT----       ----CHECK IT----                       EXF00233
C*-*-                                                                   EXF00234
      IF(IFEOF(MPOINT).EQ.-1) GOTO 51111                                EXF00235
58002 CONTINUE                                                          EXF00236
58001 CONTINUE                                                          EXF00237
      IB=3                                                              EXF00238
      GOTO 900                                                          EXF00239
C**** COMMAND IS MAT PUT -- SEQUENTIAL                                  EXF00240
60000 GOTO 58000                                                        EXF00241
C**** COMMAND IS MAT PUT -- INDEX-SEQUENTIAL                            EXF00242
61000 GOTO 59000                                                        EXF00243
C**** WRONG CODE NUMBER                                                 EXF00244
62000 NERROR=12                                                         EXF00245
      CALL EXERR(NERROR,INREG,IPROG(INREG),X1,X2)                       EXF00246
      IOP=-1                                                            EXF00247
      RETURN                                                            EXF00248
      END                                                               EXF00249

