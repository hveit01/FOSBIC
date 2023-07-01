C     compared to source                                                ***
      SUBROUTINE ZHOPPR(VALUE,NSTOP,IFR,NSW)                            ZHO00001
C---- DIE SUBROUTINE **ZHOPPR** MUSS NORMALERWEISE DEM PROGRAMM **MAIN**ZHO00002
C---- FOLGEN UND NICHT WIE JETZT VORAUSGEHEN,WENN IN OVERLAY-TECHNIK    ZHO00003
C---- GEARBEITET WIRD.DIESE ROUTINE WIRD SOWOHL WAEHREND                ZHO00004
C---- COMPILIERUNG ALS AUCH WAEHREND DER AUSFUEHRUNG DURCH ZEXEC BENUTZTZHO00005
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZHO00006
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST, INEXT,ZHO00007
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZHO00008
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZHO00009
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZHO00010
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZHO00011
     1NIFMAX,INTZEI                                                     ZHO00012
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZHO00013
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZHO00014
     1IRET(20),XXX(4),NFILE(25,3)                                       ZHO00015
      COMMON// ISTLST(340),LISTST(340)                                  ZHO00016
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZHO00018
      DIMENSION IPROG(3700)                                             ZHO00019
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZHO00020
C                                                                       ZHO00021
C*****IF NSW=-1 THIS IS AN INITIALIZATION CALL                          ZHO00022
C*****IF NSW= 0 THIS IS A COMPILATION OF A DATA STATEMENT               ZHO00023
C*****IF NSW= 1 THIS IS AN EXECUTION TO ABTAIN A DATA                   ZHO00024
C*****IF NSW= 2 THIS IS AN EXECUTION OF A RESTORE COMMAND               ZHO00025
C*****IF NSW= 3 THIS IS AN EXECUTION OF AN INPUT COMMAND                ZHO00026
      NSTOP=0                                                           ZHO00027
      IF(NSW.EQ.3) GOTO 10                                              ZHO00028
      IF(NSW.GE.0) GOTO 5                                               ZHO00029
      IPOS=0                                                            ZHO00030
      IPOSDN=1                                                          ZHO00031
      RETURN                                                            ZHO00032
  5   IF(NSW.EQ.0) GOTO 15                                              ZHO00033
      IF(NSW.EQ.2) GOTO 500                                             ZHO00034
C                                                                       ZHO00035
C*****EXTRACT VALUE FROM INTERNAL DATA VECTOR IF ANY LEFT               ZHO00036
      IF(IPOSDN.GT.NEXTDT) GOTO 10                                      ZHO00037
C*****INTERNAL DATA LEFT -- EXTRACT NEXT ONE                            ZHO00038
      VALUE=DATAN(IPOSDN)                                               ZHO00039
      IPOSDN=IPOSDN+1                                                   ZHO00040
      RETURN                                                            ZHO00041
C*****NEW POSITION OF IPOSDN BY RESTORE COMMAND                         ZHO00042
 500  NEWPOS=IFR                                                        ZHO00043
      IPOSDN=IPOSDN-NEWPOS                                              ZHO00044
      IF(NEWPOS.EQ.0) IPOSDN=1                                          ZHO00045
      IF(IPOSDN.GT.NCELLD) GOTO 502                                     ZHO00046
      IF(IPOSDN.GT.0) GOTO 501                                          ZHO00047
 502  VALUE=IPOSDN                                                      ZHO00048
      NSTOP=4                                                           ZHO00049
 501  RETURN                                                            ZHO00050
C                                                                       ZHO00051
C*****NO INTERNAL DATA LEFT -- CHECK READING BUFFER                     ZHO00052
 10   IF(IPOS.LE.0) GOTO 12                                             ZHO00053
C                                                                       ZHO00054
C*****READING BUFFER NOT EMPTY -- DELIVER NEXT NUMBER                   ZHO00055
      IWORD=ITOT-IPOS+1                                                 ZHO00056
      VALUE=BUFFER(IWORD)                                               ZHO00057
      IPOS=IPOS-1                                                       ZHO00058
      RETURN                                                            ZHO00059
C                                                                       ZHO00060
C*****READING BUFFER EMPTY -- FILL IT UP BY READING A CARD              ZHO00061
 12   READ(IRC,1) (CARDT(I),I=1,80)                                     ZHO00062
C*-*-                                                                   ZHO00065
C*-*- CALL FOR A POSIBLE NON-COMPATIBLE ROUTINE -IFEOF(IRC)-            ZHO00066
C*-*-     ----CHECK IT----       ----CHECK IT----                       ZHO00067
C*-*-                                                                   ZHO00068
      IF(IFEOF(IRC).EQ.-1) GOTO 220                                     ZHO00069
      NSW=3                                                             ***
      NQUOTE=1                                                          ***
  1   FORMAT(80A1)                                                      ZHO00070
C     COMPRESS CARDT                                                    ZHO00071
      LNGCRP=0                                                          ZHO00072
      DO 400 I=1,80                                                     ZHO00073
      CARDP(I)=BLANK                                                    ZHO00074
      IF(CARDT(I).EQ.DQUOTE) CARDT(I)=QUOTE                             ZHO00075
      IF(CARDT(I).EQ.QUOTE) NQUOTE=-NQUOTE                              ZHO00076
      IF((CARDT(I).EQ.BLANK).AND.(NQUOTE.EQ.1)) GOTO 400                ZHO00077
      LNGCRP=LNGCRP+1                                                   ZHO00078
      CARDP(LNGCRP)=CARDT(I)                                            ZHO00079
 400  CONTINUE                                                          ZHO00080
      IF(LNGCRP.EQ.0) GOTO 12                                           ZHO00081
      IF(NQUOTE.EQ.1) GOTO 403                                          ZHO00082
      NSTOP=3                                                           ZHO00083
      RETURN                                                            ZHO00084
C*****CHECK FOR A CONTROL CARD (ASTERISK IN COLUMN 1)                   ZHO00085
 403  IF(CARDT(1).NE.ASTRSK) GOTO 15                                    ***
 220  NSTOP=5                                                           ZHO00087
      RETURN                                                            ZHO00088
C                                                                       ZHO00089
 15   DO 20 K=1,40                                                      ZHO00090
 20   BUFFER(K)=0.                                                      ZHO00091
      IPOS=1                                                            ZHO00092
      IDEC=-1                                                           ZHO00093
      ISGN=0                                                            ZHO00094
      INUM=0                                                            ZHO00095
      IEF=0                                                             ZHO00096
 90   DO 200 I=IFR,81                                                   ZHO00097
      IF(I.GE.LNGCRP+1) GOTO 180                                        ZHO00098
      IF(CARDP(I).EQ.QUOTE) GOTO 405                                    ZHO00099
      CALL ZDIGIT(CARDP(I),J)                                           ZHO00100
      IF(J.LE.10) GOTO 150                                              ZHO00101
      IF((CARDP(I).EQ.ALPH(5)).OR.(CARDP(I).EQ.DECMAL).OR.(CARDP(I).EQ. ZHO00102
     1PLUS).OR.(CARDP(I).EQ.COMMA).OR.(CARDP(I).EQ.CMINUS)) GOTO 2302   ZHO00103
      NSTOP=2                                                           ZHO00104
       RETURN                                                           ZHO00105
 2302 IF(CARDP(I).NE.DECMAL) GOTO 120                                   ZHO00106
      IDEC=I                                                            ZHO00107
      GOTO 200                                                          ZHO00108
 120  IF(CARDP(I).NE.CMINUS) GOTO 180                                   ZHO00109
      ISGN=-1                                                           ZHO00110
      GOTO 200                                                          ZHO00111
C*****DIGIT FOUND                                                       ZHO00112
 150  INUM=1                                                            ZHO00113
      DIG=J-1                                                           ZHO00114
      BUFFER(IPOS)=DIG+(BUFFER(IPOS)*10.)                               ZHO00115
      GOTO 200                                                          ZHO00116
C*****NON-NUMERIC CHARACTER FOUND                                       ZHO00117
 180  IF(CARDP(I).EQ.PLUS) GOTO 200                                     ZHO00118
      IF(INUM.EQ.0) GOTO 200                                            ZHO00119
      INUM=0                                                            ZHO00120
      IF(ISGN.LT.0) BUFFER(IPOS)=-BUFFER(IPOS)                          ZHO00121
      ISGN=0                                                            ZHO00122
      IF(IDEC.LT.1) GOTO 190                                            ZHO00123
      IDIFF=I-IDEC-1                                                    ZHO00124
      IDEC=-1                                                           ZHO00125
      IF(IDIFF.LE.0) GOTO 190                                           ZHO00126
      BUFFER(IPOS)=BUFFER(IPOS)/(10.**IDIFF)                            ZHO00127
 190  IF(IEF.EQ.1) GOTO 240                                             ZHO00128
      IF(CARDP(I).NE.ALPH(5)) GOTO 230                                  ZHO00129
      IEF=1                                                             ZHO00130
      BUFIPO=BUFFER(IPOS)                                               ZHO00131
      BUFFER(IPOS)=0.                                                   ZHO00132
      GOTO 200                                                          ZHO00133
 240  IEF=0                                                             ZHO00134
      IBUFF=BUFFER(IPOS)                                                ZHO00135
      IF(IABS(IBUFF).GE.IEXPO) GOTO 219                                 ZHO00136
      BUFF=10**IABS(IBUFF)                                              ZHO00137
      IF(IBUFF.GE.0) BUFFER(IPOS)=BUFIPO*BUFF                           ZHO00138
      IF(IBUFF.LT.0) BUFFER(IPOS)=BUFIPO/BUFF                           ZHO00139
 230  IPOS=IPOS+1                                                       ZHO00140
 200  CONTINUE                                                          ZHO00141
      IPOS=IPOS-1                                                       ZHO00142
      ITOT=IPOS                                                         ZHO00143
      IF(NSW.EQ.3) GOTO 10                                              ZHO00144
      NUMBUF=ITOT                                                       ZHO00145
      IPOS=0                                                            ZHO00146
      RETURN                                                            ZHO00147
 219  NSTOP=1                                                           ZHO00148
      RETURN                                                            ZHO00149
C                                                                       ZHO00150
C*****TAKE STRINGS                                                      ZHO00151
 405  IFR=I+1                                                           ZHO00152
      DO 406 J=IFR,LNGCRP                                               ZHO00153
      IF(CARDP(J).EQ.QUOTE) GOTO 407                                    ZHO00154
 406  CONTINUE                                                          ZHO00155
 407  ITO=J-1                                                           ZHO00156
 412  IEND=IFR+4                                                        ZHO00157
      IF(IEND.GT.ITO) IEND=ITO                                          ZHO00158
      CALL STRING(IFR,IEND,IX)                                          ZHO00159
      BUFFER(IPOS)=IX                                                   ZHO00160
      IPOS=IPOS+1                                                       ZHO00161
      IF(IEND.GE.ITO) GOTO 415                                          ZHO00162
      IFR=IEND+1                                                        ZHO00163
      GOTO 412                                                          ZHO00164
 415  IFR=ITO+2                                                         ZHO00165
      GOTO 90                                                           ZHO00166
      END                                                               ZHO00167
