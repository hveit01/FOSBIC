*8
5    DIM I(3),J(3),K(3),Z(3),L(3),C(3),M$(3)
10   LET U$=' '
20   LET V$='*'
30   LET Z$='I'
31   REM EINLESEN DER MODELLPARAMETER
32   READ C1,V1,A0,R,A1,R1,A2,R2,A3,R3,A4,L(2),Z(1),Z(2),N9,M$(1),M$(2),M$(3)
33   DATA 0.75,3,250,0.03,1000,0.03,500,0.03,1000,0.03,-0.1,3000,1000,1000
34   DATA 3,'GES.INVESTITION'
35   COMMENT C1 = KONSUMQUOTE
36   COMMENT V1 = RATE DER INDUZIERTEN INVESTITIONEN
37   COMMENT A0 UND E FUNKTIONSPARAMETER DER AUTONOMEN INVESTITIONEN
38   COMMENT A1 UND R1 KONSUMPARAMETER DER OBERGRENZE
39   COMMENT A2 UND R2 KONSUMPARAMETER DER UNTERGRENZE
40   COMMENT A3 UND R3 INDUZIERTE INVESTITIONS PARAMETER DER OBERGRENZE
41   COMMENT A4 INDIZIERTE INVESTITIONS-PARAMETER DER UNTERGRENZE
42   COMMENT L(2) = ANFANGSKAPITALBESTAND VOR DER PERIODE T
43   COMMENT Y(1) UND Y(2) SIND VOLKSEINKOMMEN VOR DER PERIODE T
44   COMMENT N9 = STEUERPARAMETER ZUR AUSWAHL DER AUFZUZEICHNENDEN VARIABLEN
45   COMMENT N9=1 ZEICHNET K(T)
46   COMMENT N9=2 ZEICHNET J(T)
47   COMMENT N9=3 ZEICHNET I(T)
48   COMMENT N9=4 ZEICHNET C(T)
49   COMMENT N9=5 ZEICHNET Z(T)
50   COMMENT N9=6 ZEICHNET L(T)
51   PRINT '          C1 =',C1,'          V1 =',V1
52   PRINT '          A0 =',A0,'           R =',R
53   PRINT '          A1 =',A1,'          R1 =',R1
54   PRINT '          A2 =',A2,'          R2 =',R2
55   PRINT '          A3 =',A3,'          R3 =',R3
56   PRINT '          A4 =',A4,'        L(2) =',L(2)
57   PRINT '        Z(1) =',Z(1),'        Z(2) =',Z(2)
58   PRINT '          N9 =',N9
59   REM X1 = ANFANG
60   REM X2 = ANZAHL DER PERIODEN
61   READ X1,X2
65   DATA 0,50
70   LET X2=X1+X2
80   LET X3=1
95   PRINT ' ','    AUTONOME','   INDUZIERTE','    GESAMT-'
96   PRINT '    PERIODE',' INVESTITIONEN',' INVESTITIONEN',' INVESTITIONEN',
97   PRINT '      KONSUM','VOLKSEINKOMMEN','KAPITALBESTAND'
100  REM BESTIMMEN DES MINIMALEN UND MAXIMALEN Y-WERTES
102  LET I9=0
105  FOR X=X1 TO X2 STEP X3
109    GOSUB 1000
110    LET I9=I9+1
111    IF I9 > 1 GOTO 115
112    LET Y1=Y
113    LET Y2=Y
114    LET Y4=Y
115    IF Y GT Y1 THEN 130
120    IF Y LT Y2 THEN 140
125    GOTO 142
130    LET Y1=Y
135    GOTO 142
140    LET Y2=Y
142    PRINT  X,K(T),J(T),I(T),C(T),Z(T),L(T)
145  NEXT X
146  IF N9 LE 0 GOTO 790
147  IF N9 GT 6 GOTO 790
148  COMMENT FUER N9 < 1 UND N9 > 6 WIRD NICHT GEZEICHNET
151  FOR T=1 TO 3
152    LET C(T)=0
153    LET I(T)=0
154    LET J(T)=0
155    LET K(T)=0
156    LET L(T)=0
157    LET Z(T)=0
158  NEXT T
159  RESTORE
160  PAGE
161  READ C1,V1,A0,R,A1,R1,A2,R2,A3,R3,A4,L(2),Z(1),Z(2),N9
162  COMMENT Y4 = MAXIMALE ANZAHL DER ZEICHENPUNKTE IN Y-RICHTUNG
163  LET Y4=75
164  LET Y3=ABS((Y1-Y2)/Y4)
165  PRINT 'STARTWERTE   X=',X1,'            Y=',Y4
170  PRINT 'ENDWERTE     X=',X,'            Y=',Y
175  PRINT '  SCHRITTWEITE ','IN X-RICHTUNG =',X3
180  PRINT '  SCHRITTWEITE ','IN Y-RICHTUNG =',Y3
185  PRINT 'MAXIMALER Y-WERT',Y1
190  PRINT 'MINIMALER Y-WERT',Y2
195  PRINT
197  PRINT  'AUFGEZEICHNET WIRD *';M$(1);M$(2);M$(3);'* ALS FUNKTION DER ZEIT'
200  FOR I=1 TO Y4+20
210    PRINT USING 710,V$;
220  NEXT I
230  PRINT USING 710,V$
380  REM NULLPUNKT WENN IN ZEICHENEBENE BERECHNEN
390  LET T1=0
400  IF Y2 >= 0 GOTO 480
410  IF Y1 < 0 GOTO 480
420  LET T3=INT(ABS(Y2)/Y3+0.5)
480  FOR X=X1 TO X2 STEP X3
500    GOSUB 1000
530    LET T1=INT(ABS((Y-Y2)/Y3))+1
565    IF T1 LT T3 GOTO 570
566    LET T2=T1
567    GOTO 610
570    LET T2=T3
610    PRINT USING 620,X;Y;
620    %###.#  #######.##%
630    FOR I=1 TO T2
640      IF I NE T3 GOTO 670
650      PRINT USING 710,Z$;
660      GOTO 720
670      IF I <> T1 GOTO 700
680      PRINT USING 710,V$;
690      GOTO 720
700      PRINT USING 710,U$;
710      %#%
720    NEXT I
730    PRINT USING 710,U$
740  NEXT X
750  STOP
1000 REM UNTERPROGRAMM DES OSZILLATIONSMODELLLS NACH HICKS
761  REM K= AUTONOME INVESTITIONEN
762  LET T=3
763  LET K(T)=A0*(1+R)**X
764  REM J = INDUZIERTE INVESTITIONEN
765  LET  J(T)=V1*(Z(T-1)-Z(T-2))
766  IF J(T) < A3*(1+R3)**X GOTO 800
767  LET J(T)=A3*(1+R3)**X
800  IF J(T) > A4*L(T-1) GOTO 810
805  LET J(T)=A4*L(T-1)
810  REM I = GESAMTINVESTITIONEN
813  LET I(T)=K(T)+J(T)
814  REM C = KONSUM
815  LET C(T)=C1*Z(T-1)
816  IF C(T) <= A1*(1+R1)**X GOTO 820
817  LET C(T)=A1*(1+R1)**X
818  GOTO 830
820  IF C(T) > A2*(1+R2)**X GOTO 830
825  LET C(T) = A2*(1+R2)**X
830  REM Z = VOLKSEINKOMMEN
831  LET Z(T)=I(T)+C(T)
832  REM L = KAPITALBESTAND
833  LET L(T)=L(T-1)+J(T)
834  LET L(T-1)=L(T)
835  LET Z(T-2)=Z(T-1)
836  LET Z(T-1)=Z(T)
837  IF N9 <= 0 GOTO 900
838  IF N9 > 6 GOTO 900
839  GOTO 840,850,860,870,880,890 ON N9
840  LET Y=K(T)
845  GOTO 900
850  LET Y=J(T)
855  GOTO 900
860  LET Y=I(T)
865  GOTO 900
870  LET Y=C(T)
875  GOTO 900
880  LET Y=Z(T)
885  GOTO 900
890  LET Y=L(T)
895  GOTO 900
900  RETURN
790  END