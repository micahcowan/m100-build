0  ' ********************************
1  ' *                              *
2  ' * AVENJ - 8085 Assembler using *
10 ' *         Z80 - or custom -    *
20 ' *         syntax!              *
89 ' *                              *
90 ' ********************************
100 MAXFILES=2
101 CLEAR 512
105 BF$=STRING$(255,".")
110 WS$=" "+CHR$(9) ' whitespace
112 AE$=WS$+"," ' end of arg
120 DIM AR$(1) ' args list
130 HX$="0123456789ABCDEF"
210 FI$="INASM.DO" ' replace with prompt later
220 OPEN FI$ FOR INPUT AS 1
250 BI=0:LN=0
260 IF EOF(1) THEN E=1:GOTO 400
270 C$=INPUT$(1,1):IF ASC(C$)=13 OR ASC(C$)=10 THEN GOTO 400
280 BI=BI+1:MID$(BF$,BI,1)=C$:GOTO 260
395 ' * process input line
400 LN=LN+1:IF BI=0 THEN 800 ' early return
405 L$=LEFT$(BF$,BI)
408 P=1
409 GOSUB 1300 ' skip comments
410 GOSUB 1000 ' get label
415 IF LB$<>"" THEN PRINT "LABEL: ";LB$
420 S=P:GOSUB 1400 ' mandatory whitespace
430 IF S<>P OR P=LEN(L$) THEN GOTO 450
440 PRINT "ERR: Missing initial whitespace at line ";LN:PRINT L$:PRINT "Exiting.":END
450 GOSUB 1450 ' get mnemonic
455 IF MN$<>"" THEN PRINT"MN="MN$
460 GOSUB 1400 ' optional ws
470 AR$(0)="":AR$(1)="" ' clear args
480 ' * first arg
490 S=P:GOSUB 1420:IF P>S THEN AR$(0)=MID$(L$,S,P-S)
500 GOSUB 1400 ' skip opt ws
510 IF P>LEN(L$) THEN GOTO 600
520 IF MID$(L$,P,1)="," THEN P=P+1
530 GOSUB 1400 ' skip opt ws
540 S=P:GOSUB 1420:IF P>S THEN AR$(1)=MID$(L$,S,P-S)
600 FOR I=0 TO 1:IF AR$(I)<>"" THEN PRINT"AR"I": "AR$(I):NEXT
610 ' FIXME: kludgey disassembler
620 R$="BCDEHL(A"
630 IF MN$<>"LD" OR AR$(0)="" OR AR$(1)="" THEN GOTO 800
640 CD=64:FOR I=0 TO 1:V=INSTR(R$,LEFT$(AR$(I),1)):IF V<>0 THEN CD=CD+((V-1)*(1+(7*(1-I))))
650 NEXT
660 PRINT "CODE: ";:V=CD:GOSUB 1550
800 IF E<>0 THEN PRINT:PRINT "Done.":END
810 GOTO 250
995 ' * check for line label
1000 LE=0:LB$=""
1005 IF P>LEN(L$) THEN GOTO 1070
1010 C$=MID$(L$,P,1):C=ASC(C$)
1020 GOSUB 1100:IF T$<>"A" AND (P=1 OR T$<>"1") THEN GOTO 1070
1030 LE=P:P=P+1:GOTO 1005
1070 IF LE<>0 THEN LB$=LEFT$(L$,LE)
1080 RETURN
1095 ' * determine character category
1100 T$=""
1110 IF C=9 THEN T$=" ":RETURN
1120 IF C<32 THEN T$="^":RETURN
1130 IF C=32 THEN T$=" ":RETURN
1140 IF C<ASC("0") THEN T$=".":RETURN
1150 IF C<=ASC("9") THEN T$="1":RETURN
1160 IF C<ASC("A") THEN T$=".":RETURN
1170 IF C<=ASC("Z") THEN T$="A":RETURN
1180 IF C<ASC("a") THEN T$=".":RETURN
1190 IF C<=ASC("z") THEN T$="A":RETURN
1200 IF C<128 THEN T$=".":RETURN
1210 IF C=128 THEN T$="^":RETURN
1220 T$="H":RETURN
1295 ' * remove comments
1296 '   - will need adjustment if we
1297 '     start supporting
1298 '     string tokens
1300 FOR I=1 TO LEN(L$):C$=MID$(L$,I,1):IF C$=";" THEN GOSUB 1350
1310 NEXT
1320 RETURN
1350 IF I=1 THEN L$=" " ELSE L$=LEFT$(L$,I-1):I=I-1
1360 RETURN
1395 ' * skip whitespace
1400 CN=0:NL$=WS$:GOTO 1500
1415 ' * get arg
1420 CN=1:NL$=AE$:GOTO 1500
1445 ' * get mnemonic
1450 S=P:MN$="":CN=1:NL$=WS$:GOSUB 1500
1460 IF P>S THEN MN$=MID$(L$,S,P-S)
1470 RETURN
1495 ' * generic skip-forward until
1496 '   char is/is not in search list.
1497 '   NL$ is what we're looking for,
1498 '   CN is true if we stop on find,
1499 '   false for stop on not found.
1500 IF P>LEN(L$) THEN GOTO 1530
1510 C$=MID$(L$,P,1):IF (INSTR(NL$,C$)<>0) <> (CN<>0) THEN P=P+1:GOTO 1500
1530 RETURN
1545 ' * print hex value of V (0-255)
1550 PRINT MID$(HX$,(V\16+1),1);MID$(HX$,(V AND 15)+1,1)
1560 RETURN
