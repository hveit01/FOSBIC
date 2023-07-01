C     compared to source                                                ***
      SUBROUTINE ZKLAM(IFR,ITO,MI)                                      ZKL00001
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      ZKL00002
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, ZKL00003
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  ZKL00004
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, ZKL00005
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     ZKL00006
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     ZKL00007
     1NIFMAX,INTZEI                                                     ZKL00008
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        ZKL00009
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      ZKL00010
     1IRET(20),XXX(4),NFILE(25,3)                                       ZKL00011
      COMMON// ISTLST(340),LISTST(340)                                  ZKL00012
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               ZKL00014
      DIMENSION IPROG(3700)                                             ZKL00015
      EQUIVALENCE (DATA(1),IPROG(1))                                    ZKL00016
      NZ=NERRS                                                          ZKL00017
      IF(MERKER(MI,1) .NE.0) GOTO 707                                   ZKL00018
      NERROR=39                                                         ZKL00019
706   NERRS=NERRS+1                                                     ZKL00020
      NN=0                                                              ZKL00021
      CALL COMERR(NERROR,I1,I2,ALPH(MI),X2,NN)                          ZKL00022
      GOTO 703                                                          ZKL00023
 707  IF(IFR.EQ.ITO) RETURN                                             ZKL00024
      DO 700 I=IFR,ITO                                                  ZKL00025
      IF(CARDT(I).EQ.COMMA) GOTO 709                                    ZKL00026
 700  CONTINUE                                                          ZKL00027
      I=ITO                                                             ZKL00028
      GOTO 710                                                          ZKL00029
 709  CALL ZTRANX(I+1,ITO-1)                                            ZKL00030
      IPROG(INREG)=-14                                                  ZKL00031
      IPROG(INREG-1)=4                                                  ZKL00032
      IF(MERKER(MI,2).EQ.0) GOTO 704                                    ZKL00033
      INREG=INREG-2                                                     ZKL00034
 710  CALL ZTRANX(IFR+1,I-1)                                            ZKL00035
      IPROG(INREG)=-14                                                  ZKL00036
      IPROG(INREG-1)=3                                                  ZKL00037
      INREG=INREG-2                                                     ZKL00038
 703  IF(NZ.NE.NERRS) MI=-1                                             ZKL00039
      RETURN                                                            ZKL00040
 704  NERROR=40                                                         ZKL00041
      GOTO 706                                                          ZKL00042
      END                                                               ZKL00043
