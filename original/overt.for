C---- PROGRAMM OVERT                                                    STE00001
C---- DIE FOLGENDE ROUTINE IST EINE ZUSATZROUTINE = HAUPTELEMENT, DIE   STE00002
C---- NUR BEIM ARBEITEN IN OVERLAY-TECHNIK NOTWENDIG IST.               STE00003
C---- DIESE ROUTINE IST BEI NORMALEM ABLAUF DES SYSTEMS ZU ENTFERNEN.   STE00004
C---- AUSSERDEM SIND NOCH EINIGE EINFACHE AENDERUNGEN IN DEN PROGRAMMEN STE00005
C----              MAIN                                                 STE00006
C---- UND          ZEXEC                                                STE00007
C---- VORZUNEHMEN UND EINIGE UNTERPROGRAMME UMZUSTELLEN.                STE00008
C---- DIE UMZUSTELLENDEN UNTERPROGRAMME SIND                            STE00009
C----              ZHOPPR                                               STE00010
C----              STRING                                               STE00011
C----              ZDIGIT                                               STE00012
C----              ZALPH                                                STE00013
C----              FINDFT                                               STE00014
C---- DIE VORZUNEHMENDEN AENDERUNGEN SIND DURCH TEXT AUSFUEHRLICH       STE00015
C---- GEKENNZEICHNET,WOBEI DER TEXT DURCH C---- EINGELEITET WIRD.       STE00016
C---- ES EMPFIEHLT SICH DIE JEWEILIGEN AENDERUNGEN SO VORZUNEHMEN,DASS  STE00017
C---- DAS URSPRUENGLICHE PROGRAMM ERKENNBAR BLEIBT.                     STE00018
C***  ZUSATZROUTINE -HAUPTELEMENT BEI OVERLAY-TECHNIK                   STE00019
      COMMON// ACC,ASTRSK,BLANK,CMINUS,COMMA,DECMAL,DOLSGN,EQUALS,      STE00020
     1INREG,LNGCRP,NCELLD,NCELLP,NERRS,NEXTDT,NIFOR,NIRET,NSTLST,INEXT, STE00021
     2NUMBUF,PARLFT,PARRT,PLUS,QUOTE,SLASH,VLESS,VGREAT,DQUOTE,MAXFIL,  STE00022
     3IRC,IWC,NSTEND,IEXPO,IBEGST,IWRIT,IPEND,IZONE,IIMAGE,NPRI,NIMAGE, STE00023
     4NPRUS,NCARD,MAXIMA,PUCO,DOPU,EXSIGN,MAXSAT,NUMFIL,NZIM,NSTZEI     STE00024
      COMMON// INTMAX,INTNUM,XNULL,DDOPU,IMIRC,SMALL,ISTMAX,NIRMAX,     STE00025
     1NIFMAX,INTZEI                                                     STE00026
      COMMON// CARDT(80),MERKER(26,2),CARP(140),                        STE00027
     3ALPH(48),BUFFER(40),CARD(80),CARDP(80),DIGIT(10),IFOR(20,2),      STE00028
     1IRET(20),XXX(4),NFILE(25,3)                                       STE00029
      COMMON// ISTLST(340),LISTST(340)                                  STE00030
      COMMON// DATAN(330)                                               ***
      COMMON// DATA(3700)                                               STE00032
      DIMENSION IPROG(3700)                                             STE00033
      EQUIVALENCE (DATA(1),IPROG(1))                                    STE00034
      OVERLAY(0)                                                        STE00035
      DATA IDISK/3HPUM/                                                 STE00036
 2    CALL UFOVER(1,IDISK)                                              STE00037
      CALL UFOVER(2,IDISK)                                              STE00038
      IF(CARDP(1).EQ.ASTRSK) GOTO 2                                     STE00039
 20   READ(IRC,1) CARD                                                  STE00040
 1    FORMAT(80A1)                                                      STE00041
      IF(IFEOF(IRC).EQ.-1) GOTO 10                                      STE00042
      IF(CARD(1).EQ.ASTRSK) GOTO 2                                      STE00043
      WRITE(IWC,3) CARD                                                 STE00044
 3    FORMAT(20X,80A1)                                                  STE00045
      GOTO 20                                                           STE00046
 10   STOP                                                              STE00047
      END                                                               STE00048
