      SUBROUTINE PRILIN(NX)                                             PRI00001
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      PRI00002
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, PRI00003
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  PRI00004
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, PRI00005
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     PRI00006
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     PRI00007
     1NIFMAX,INTZEI                                                     PRI00008
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        PRI00009
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      PRI00010
     1IRET(20),XXX(4),NFILE(25,3)                                       PRI00011
      COMMON// ISTLST(340),LISTST(340)                                  PRI00012
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               PRI00014
      DIMENSION IPROG(3700)                                             PRI00015
      EQUIVALENCE (DATA(1),IPROG(1))                                    PRI00016
      DO 20 I=1,IIMAGE                                                  ***
      IF(CARP(I).EQ.QUOTE) CARP(I)=BLANK                                ***
 20   CONTINUE                                                          ***
      IF(NX.EQ.1) GOTO 10                                               PRI00017
      IF((NX.EQ.3).AND.(NPRUS.EQ.1)) GOTO 10                            PRI00018
      IF(NPRUS.EQ.1) GOTO 40                                            PRI00019
      WRITE(IWC,998) (CARP(I),I=1,IIMAGE)                               PRI00020
      GOTO 30                                                           PRI00021
 10   IF(INEXT.EQ.1) GOTO 40                                            PRI00022
      WRITE(IWC,999) (CARP(I),I=1,IWRIT)                                PRI00023
 30   CALL CLEAR(1,140)                                                 PRI00024
      INEXT=1                                                           PRI00025
      NPRUS=1                                                           PRI00026
 999  FORMAT(1X,125A1)                                                  PRI00027
 998  FORMAT(1X,135A1)                                                  PRI00028
 40   RETURN                                                            PRI00029
      END                                                               PRI00030
