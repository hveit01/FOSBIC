      SUBROUTINE ZLISTE(IANF,MOP,IX,MCOM)                               ZLI00001
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZLI00002
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, ZLI00003
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZLI00004
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZLI00005
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZLI00006
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZLI00007
     1NIFMAX,INTZEI                                                     ZLI00008
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZLI00009
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZLI00010
     1IRET(20),XXX(4),NFILE(25,3)                                       ZLI00011
      COMMON// ISTLST(340),LISTST(340)                                  ZLI00012
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZLI00014
      DIMENSION IPROG(3700)                                             ZLI00015
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZLI00016
      IF(MCOM.EQ.2) GOTO 311                                            ZLI00017
      IB=1                                                              ZLI00018
 310  IF(MCOM.LE.3) IB=2                                                ZLI00019
      IPROG(INREG)=MOP                                                  ZLI00020
      IPROG(INREG-1)=IX                                                 ZLI00021
      INREG=INREG-IB                                                    ZLI00022
 311  NTOT=0                                                            ZLI00023
      DO 315 LOC=IANF,LNGCRP                                            ZLI00024
      IF(CARDP(LOC).EQ.PARLFT) NTOT=NTOT+1                              ZLI00025
      IF(CARDP(LOC).EQ.PARRT) NTOT=NTOT-1                               ZLI00026
      IF((CARDP(LOC).EQ.COMMA).AND.(NTOT.EQ.0)) GOTO 320                ZLI00027
 315  CONTINUE                                                          ZLI00028
      LOC=LNGCRP+1                                                      ZLI00029
 320  IEND=LOC-1                                                        ZLI00030
      IF(MCOM.EQ.2) GOTO 321                                            ZLI00031
      IF(CARDP(IEND).EQ.DOLSGN) IEND=IEND-1                             ZLI00032
      IF(IEND.EQ.IANF) GOTO 370                                         ZLI00033
      IF(IEND.EQ.IANF+1) GOTO 345                                       ZLI00034
C**** SUBSCRIPTED VARIABLE FOUND                                        ZLI00035
 321  IF(CARDP(IANF).EQ.QUOTE) GOTO 322                                 ZLI00036
      NZ=NERRS                                                          ZLI00037
      CALL ZTRANX(IANF,IEND)                                            ZLI00038
      IF(NZ.NE.NERRS) GOTO 330                                          ZLI00039
      IF(MCOM.EQ.2) GOTO 385                                            ZLI00040
      IF(IPROG(INREG+1).EQ.-8) GOTO 340                                 ZLI00041
 330  NERROR=41                                                         ZLI00042
 338  NN=0                                                              ZLI00043
      CALL COMERR(NERROR,IANF,IEND,X1,X2,NN)                            ZLI00044
      NERRS=NERRS+1                                                     ZLI00045
      GOTO 385                                                          ZLI00046
C**** SUBSCRIPTED VARIABLE FOUND                                        ZLI00047
 340  IPROG(INREG+1)=-19                                                ZLI00048
      GOTO 385                                                          ZLI00049
C**** UNSUBSCRIPTED VARIABLE FOUND -- DOUBLE CHARACTER NAME             ZLI00050
 345  CALL ZALPH(CARDP(IANF),K)                                         ZLI00051
      IF(K.GT.26) GOTO 330                                              ZLI00052
      CALL ZDIGIT(CARDP(IANF+1),L)                                      ZLI00053
      IF(L.GT.10) GOTO 330                                              ZLI00054
      IPROG(INREG)=-8                                                   ZLI00055
      IPROG(INREG-1)=K+(26*(L-1))+53                                    ZLI00056
      INREG=INREG-2                                                     ZLI00057
      GOTO 385                                                          ZLI00058
C                                                                       ZLI00059
C**** SINGLE CHARACTER NAME FOUND                                       ZLI00060
 370  CALL ZALPH(CARDP(IANF),K)                                         ZLI00061
      IF(K.GT.26) GOTO 330                                              ZLI00062
      IPROG(INREG)=-9                                                   ZLI00063
      IPROG(INREG-1)=K                                                  ZLI00064
      INREG=INREG-2                                                     ZLI00065
 385  IANF=LOC+1                                                        ZLI00066
      IF(IANF.GT.LNGCRP) GOTO 386                                       ZLI00067
      GOTO 310                                                          ZLI00068
 386  IF(MCOM.NE.2) GOTO 387                                            ZLI00069
      IPROG(INREG)=MOP                                                  ZLI00070
      IPROG(INREG-1)=IX                                                 ZLI00071
      INREG=INREG-2                                                     ZLI00072
 387  RETURN                                                            ZLI00073
C*****ALPHANUMERIC CONSTANT FOUND IN A PUT COMMAND                      ZLI00074
 322  ICT=0                                                             ZLI00075
      IF(MCOM.EQ.2) GOTO 405                                            ZLI00076
      IF((MCOM.GE.7).OR.(MCOM.LE.9)) GOTO 400                           ZLI00077
 405  IF(CARDP(IEND).NE.QUOTE) GOTO 323                                 ZLI00078
      LOC=IANF                                                          ZLI00079
      K=INREG                                                           ZLI00080
      INREG=INREG-4                                                     ZLI00081
 324  IANF=LOC+1                                                        ZLI00082
      LOC=IANF+4                                                        ZLI00083
      IF(LOC.GE.IEND) LOC=IEND-1                                        ZLI00084
      CALL STRING(IANF,LOC,IA)                                          ZLI00085
      L=INREG-ICT                                                       ZLI00086
      IPROG(L)=IA                                                       ZLI00087
      ICT=ICT+1                                                         ZLI00088
      IF(LOC.LT.IEND-1) GOTO 324                                        ZLI00089
      IPROG(K)=MOP                                                      ZLI00090
      IPROG(K-1)=IX                                                     ZLI00091
      IPROG(K-2)=-16                                                    ZLI00092
      IPROG(K-3)=ICT                                                    ZLI00093
      INREG=INREG-ICT                                                   ZLI00094
      IANF=IEND+2                                                       ZLI00095
      IF(IANF.GT.LNGCRP) RETURN                                         ZLI00096
      GOTO 311                                                          ZLI00097
 323  NERROR=7                                                          ZLI00098
      GOTO 338                                                          ZLI00099
 400  NERROR=52                                                         ZLI00100
      GOTO 338                                                          ZLI00001
      END                                                               ZLI00002
