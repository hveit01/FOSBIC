      SUBROUTINE CHECK(I1,I2)                                           CHE00001
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      CHE00002
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, CHE00003
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  CHE00004
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, CHE00005
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     CHE00006
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     CHE00007
     1NIFMAX,INTZEI                                                     CHE00008
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        CHE00009
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      CHE00010
     1IRET(20),XXX(4),NFILE(25,3)                                       CHE00011
      COMMON// ISTLST(340),LISTST(340)                                  CHE00012
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               CHE00014
      DIMENSION IPROG(3700)                                             CHE00015
      EQUIVALENCE (DATA(1),IPROG(1))                                    CHE00016
      NX=2                                                              CHE00017
      IF(I2.GE.IWRIT) CALL PRILIN(NX)                                   CHE00018
      IF(I2.LE.1) RETURN                                                CHE00019
      I1=1                                                              CHE00020
 1    I1=I1+IZONE                                                       CHE00021
      IF(I1.LT.I2) GOTO 1                                               CHE00022
      I2=1                                                              CHE00023
      NPRUS=I2                                                          CHE00024
      INEXT=I1                                                          CHE00025
      RETURN                                                            CHE00026
      END                                                               CHE00027
