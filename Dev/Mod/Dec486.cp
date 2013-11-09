MODULE DevDec486;
(* bh, 09 Sep 1999 *)

	(* 09.09.99	bh	fixed WriteInt for MAX(INTEGER) *)
	
	IMPORT DevDecBase;
	
	VAR
		ad, tab, num, end, jmptab: INTEGER;
		buf: ARRAY 16 OF BYTE;
		len: INTEGER;
		Act32BitAddr, Act32BitData: BOOLEAN;
		RegList1, RegList2, OpList1, OpList2, OpList3, CcList1, CcList2, FSList: ARRAY 32 OF CHAR;
		Prefix: ARRAY 16 OF CHAR;
		
	PROCEDURE DecodeLine* (VAR ad: INTEGER; Put: DevDecBase.PutProc; Get: DevDecBase.GetProc; mode: INTEGER);
		VAR op, ext, i: INTEGER; pre: BOOLEAN; ls: INTEGER;
		
		PROCEDURE WriteHex(x, n: INTEGER);
		BEGIN
			IF n > 1 THEN WriteHex(x DIV 16, n-1) END;
			x := x MOD 16;
			IF x <= 9 THEN Put(CHR(x + ORD("0")))
			ELSE Put(CHR(x -10 + ORD("A")))
			END
		END WriteHex;
		
		PROCEDURE WriteString (s: ARRAY OF CHAR);
			VAR i: INTEGER;
		BEGIN
			i := 0; WHILE s[i] # 0X DO Put(s[i]); INC(i) END
		END WriteString;
		
		PROCEDURE WriteInt (x: INTEGER);
		BEGIN
			IF x = MIN(INTEGER) THEN WriteString("-2147483648")
			ELSIF x < 0 THEN Put("-"); WriteInt(-x)
			ELSE
				IF x > 9 THEN WriteInt(x DIV 10) END;
				Put(CHR(x MOD 10 + ORD("0")))
			END
		END WriteInt;
		
		PROCEDURE WriteChar(ch: CHAR);
		BEGIN
			IF (ch >= " ") & (ch < 7FX) THEN Put(ch)
			ELSE Put(".")
			END
		END WriteChar;
		
		PROCEDURE WriteWord(x: INTEGER);
		BEGIN
			IF (x > 32767) OR (x < -32768) THEN
				Put("$"); WriteHex(x, 8)
			ELSE
				WriteInt(x)
			END
		END WriteWord;
		
		PROCEDURE WriteUndef;
		BEGIN
			WriteString("*****")
		END WriteUndef;
		
		PROCEDURE WriteReg(r: INTEGER);
		BEGIN
			IF (r < 8) & Act32BitData THEN Put("E") END;
			Put(RegList1[r]);
			Put(RegList2[r])
		END WriteReg;
		
		PROCEDURE WriteAReg(r: INTEGER);
		BEGIN
			IF (r < 8) & Act32BitAddr THEN Put("E") END;
			Put(RegList1[r]);
			Put(RegList2[r])
		END WriteAReg;
		
		PROCEDURE WriteOp(n: INTEGER);
		BEGIN
			Put(OpList1[n]);
			Put(OpList2[n]);
			Put(OpList3[n])
		END WriteOp;
		
		PROCEDURE WriteCC(n: INTEGER);
		BEGIN
			Put(CcList1[n]);
			Put(CcList2[n])
		END WriteCC;

		PROCEDURE Next(): INTEGER;
			VAR b: BYTE;
		BEGIN
			Get(b); INC(ad);
			buf[len] := b; INC(len);
			RETURN b MOD 256
		END Next;
		
		PROCEDURE Word(long: BOOLEAN): INTEGER;
			VAR w, x: INTEGER;
		BEGIN
			w := Next();
			w := w + 256 * Next();
			IF long THEN
				w := w + 65536 * Next();
				x := Next();
				IF x >= 128 THEN x := x - 256 END;
				w := w + 16777216 * x;
			ELSIF w >= 32768 THEN w := w - 65536
			END;
			RETURN w
		END Word;
		
		PROCEDURE WriteExt();
			VAR sib: INTEGER;
		BEGIN
			IF ext >= 192 THEN
				WriteReg(ext MOD 8 + 8 * (1 - op MOD 2))
			ELSIF Act32BitAddr THEN
				IF Prefix # "" THEN WriteString(Prefix) END;
				IF (ext MOD 8 = 5) & (ext DIV 64 = 0) THEN
					Put("[");
					WriteWord(Word(TRUE));
					Put("]")
				ELSE
					Put("[");
					IF ext MOD 8 = 4 THEN
						sib := Next();
						IF (sib MOD 8 = 5) & (ext DIV 64 = 0) THEN
							tab := Word(TRUE); WriteWord(tab)
						ELSE
							WriteAReg(sib MOD 8)
						END;
						Put(",");
						IF sib DIV 8 MOD 8 # 4 THEN
							WriteAReg(sib DIV 8 MOD 8);
							Put(":");
							WriteInt(ASH(1, sib DIV 64));
							Put(",");
						END
					ELSE
						WriteAReg(ext MOD 8);
						Put(",")
					END;
					CASE ext DIV 64 OF
						0: Put("0")
					|  1: WriteWord((Next() + 128) MOD 256 - 128)
					|  2: WriteWord(Word(TRUE))
					END;
					Put("]")
				END
			ELSE
				IF Prefix # "" THEN WriteString(Prefix) END;
				IF (ext MOD 8 = 6) & (ext DIV 64 = 0) THEN
					Put("[");
					WriteWord(Word(FALSE));
					Put("]")
				ELSE
					Put("[");
					CASE ext MOD 8 OF
						0: WriteString("BX,SI,")
					|  1: WriteString("BX,DI,")
					|  2: WriteString("BP,SI,")
					|  3: WriteString("BP,DI,")
					|  4: WriteString("SI,")
					|  5: WriteString("DI,")
					|  6: WriteString("BP,")
					|  7: WriteString("BX,")
					END;
					CASE ext DIV 64 OF
						0: Put("0")
					|  1: WriteWord((Next() + 128) MOD 256 - 128)
					|  2: WriteWord(Word(FALSE))
					END;
					Put("]")
				END
			END
		END WriteExt;

		PROCEDURE Write(s: ARRAY OF CHAR);
			VAR i: INTEGER; ch: CHAR; o: INTEGER;
		BEGIN
			i := 0; ch := s[0];
			WHILE ch # 0X DO
				CASE ch OF
				| "a": (* address *) Put("$"); end := Word(Act32BitAddr); INC(end, ad); WriteHex(end, 8)
				| "b": (* byte *) num := (Next() + 128) MOD 256 - 128; WriteWord(num)
				| "c": (* short address *) Put("$"); o := (Next() + 128) MOD 256 - 128; WriteHex(ad + o , 8)
				| "d": (* register 16 *) WriteReg(op MOD 8)
				| "e": (* ext reg with w *) WriteReg(ext DIV 8 MOD 8 + 8 * (1 - op MOD 2))
				| "f": (* 16 bit offset *) WriteWord(Word(FALSE))
				| "h": (* shift op *) WriteOp(ext DIV 8 MOD 8 + 8)
				| "i": (* size *) IF op MOD 2 = 0 THEN Put("B") ELSE Put(" ") END
				| "j": (* ext reg field as num *) WriteInt(ext DIV 8 MOD 8)
				| "l": (* float types *) Put(FSList[op DIV 2 MOD 4])
				| "m": (* mem ad with w *) WriteExt()
				| "n": (* condition *) WriteCC(op MOD 16)
				| "o": (* operation *) WriteOp(ext DIV 8 MOD 8)
				| "q": (* float reverse *) IF ODD(op DIV 4 - ext DIV 8) THEN Put("R") END
				| "r": (* register 8 *) WriteReg(op MOD 8 + 8)
				| "s": (* segment *) WriteReg(op DIV 8 MOD 8 + 16)
				| "t": (* float stack *) WriteString("ST("); WriteInt(ext MOD 8); Put(")")
				| "u": (* accu *) WriteReg(0)
				| "w": (* word *) num := Word(Act32BitData); WriteWord(num)
				| "x": (* mem reg with dw *) IF op DIV 2 MOD 2 = 0 THEN Write("m,e") ELSE Write("e,m") END
				| "y": (* float stack with d *) IF ODD(op DIV 4) THEN Write("t,ST") ELSE Write("ST,t") END
				| "z": (* space*) WriteString("    ")
				ELSE Put(ch)
				END;
				INC(i); ch := s[i]
			END
		END Write;
		
		PROCEDURE WriteFPage;
		BEGIN
			op := Next();
			CASE op OF
				00: ext := Next();
					CASE ext DIV 8 MOD 8 OF
						0: Write("SLDT    m")
					|  1: Write("STR     m")
					|  2: Write("LLDT    m")
					|  3: Write("LTR     m")
					|  4: Write("VERR    m")
					|  5: Write("VERW    m")
					|  6, 7: WriteUndef
					END
			|  01H: ext := Next();
					CASE ext DIV 8 MOD 8 OF
						0: Write("SGDT    m")
					|  1: Write("SIDT    m")
					|  2: Write("LGDT    m")
					|  3: Write("LIDT    m")
					|  4: Write("SMSW    m")
					|  5: WriteUndef
					|  6: Write("LMSW    m")
					|  7: Write("INVLPG  m")
					END
			|  02H: ext := Next(); Write("LAR     e,m")
			|  03H: ext := Next(); Write("LSL     e,m")
			|  04H, 05H, 07H, 0AH..1FH: WriteUndef
			|  06H: Write("CLTS")
			|  08H: Write("INVD")
			|  09H: Write("WBINVD")
			|  20H: ext := Next(); Write("MOV     m,CRj")
			|  21H: ext := Next(); Write("MOV     m,DRj")
			|  22H: ext := Next(); Write("MOV     CRj,m")
			|  23H: ext := Next(); Write("MOV     DRj,m")
			|  24H: ext := Next(); Write("MOV     m,TRj")
			|  26H: ext := Next(); Write("MOV     TRj,m")
			|  25H, 27H..7FH: WriteUndef
			|  80H..8FH: Write("Jn     a")
			|  90H..9FH: ext := Next(); Write("SETn   m")
			|  0A0H, 0A8H: Write("PUSH    s")
			|  0A1H, 0A9H: Write("POP     s")
			|  0A2H, 0A6H, 0A7H, 0AAH, 0AEH: WriteUndef
			|  0A3H: ext := Next(); Write("BT      m,e")
			|  0A4H: ext := Next(); Write("SHLD    e,m,b")
			|  0A5H: ext := Next(); Write("SHLD    e,m,CL")
			|  0ABH: ext := Next(); Write("BTS     m,e")
			|  0ACH: ext := Next(); Write("SHRD    e,m,b")
			|  0ADH: ext := Next(); Write("SHRD    e,m,CL")
			|  0AFH: ext := Next(); Write("IMUL    e,m")
			|  0B0H,0B1H: ext := Next(); Write("XCMPi   e,m")
			|  0B2H: ext := Next(); Write("LSS     e,m")
			|  0B3H: ext := Next(); Write("BTR     m,e")
			|  0B4H: ext := Next(); Write("LFS     e,m")
			|  0B5H: ext := Next(); Write("LGS     e,m")
			|  0B6H, 0B7H: ext := Next();
					Write("MOVZXi  "); WriteReg(ext DIV 8 MOD 8);
					Act32BitData := FALSE; Write(",m")
			|  0B8H, 0B9H: WriteUndef
			|  0BAH: ext := Next();
					CASE ext DIV 8 MOD 8 OF
						0..3: WriteUndef
					|  4: Write("BT      m,b")
					|  5: Write("BTS     m,b")
					|  6: Write("BTR     m,b")
					|  7: Write("BTC     m,b")
					END
			|  0BBH: ext := Next(); Write("BTC     m,e")
			|  0BCH: ext := Next(); Write("BSF     m,e")
			|  0BDH: ext := Next(); Write("BSR     m,e")
			|  0BEH, 0BFH: ext := Next();
					Write("MOVSXi  "); WriteReg(ext DIV 8 MOD 8);
					Act32BitData := FALSE; Write(",m")
			|  0C0H,0C1H: ext := Next(); Write("XADDi   e,m")
			|  0C8H..0CFH: Write("BSWAP   r")
			|  0C2H..0C7H, 0D0H..0FFH: WriteUndef
			END
		END WriteFPage;
		
		PROCEDURE WriteMath;
			VAR n, m: INTEGER;
		BEGIN
			n := op MOD 8; ext := Next(); m := ext DIV 8 MOD 8;
			IF ext < 0C0H THEN
				CASE n OF
				| 0, 2, 4, 6:
						CASE m OF
						| 0: Write("FADDl   m")
						| 1: Write("FMULl   m")
						| 2: Write("FCOMl   m")
						| 3: Write("FCOMPl  m")
						| 4: Write("FSUBl   m")
						| 5: Write("FSUBRl  m")
						| 6: Write("FDIVl   m")
						| 7: Write("FDIVRl  m")
						END
				| 1:
						CASE m OF
						| 0: Write("FLD     m")
						| 1: WriteUndef
						| 2: Write("FST     m")
						| 3: Write("FSTP    m")
						| 4: Write("FLDENV  m")
						| 5: Write("FLDCW   m")
						| 6: Write("FSTENV  m")
						| 7: Write("FSTCW   m")
						END
				| 3:
						CASE m OF
						| 0: Write("FLDD    m")
						| 1: WriteUndef
						| 2: Write("FSTD    m")
						| 3: Write("FSTPD   m")
						| 4: Write("FLDT    m")
						| 5: Write("FLDE    m")
						| 6: Write("FSTPT   m")
						| 7: Write("FSTPE   m")
						END
				| 5:
						CASE m OF
						| 0: Write("FLDL    m")
						| 2: Write("FSTL    m")
						| 3: Write("FSTPL   m")
						| 4: Write("FRSTOR  m")
						| 6: Write("FSAVE   m")
						| 7: Write("FSTSW   m")
						| 1, 5: WriteUndef
						END
				| 7:
						CASE m OF
						| 0: Write("FLDW    m")
						| 1: WriteUndef
						| 2: Write("FSTW    m")
						| 3: Write("FSTPW   m")
						| 4: Write("FLDB    m")
						| 5: Write("FLDQ    m")
						| 6: Write("FSTPB   m")
						| 7: Write("FSTPQ   m")
						END
				END
			ELSE
				CASE n OF
				| 0, 4: 
						CASE m OF
						| 0: Write("FADD    y")
						| 1: Write("FMUL    y")
						| 2: Write("FCOM    y")
						| 3: Write("FCOMP   y")
						| 4, 5: Write("FSUBq  y")
						| 6, 7: Write("FDIVq  y")
						END
				| 1:
						CASE m OF
						| 0: Write("FLD     t")
						| 1: Write("FXCH    t")
						| 2: IF ext MOD 8 = 0 THEN Write("FNOP") ELSE WriteUndef END
						| 3: WriteUndef
						| 4:
								CASE ext MOD 8 OF
								| 0: Write("FCHS")
								| 1: Write("FABS")
								| 4: Write("FTST")
								| 5: Write("FXAM")
								| 2, 3, 6, 7: WriteUndef
								END
						| 5: 
								CASE ext MOD 8 OF
								| 0: Write("FLD1")
								| 1: Write("FLD2T")
								| 2: Write("FLDL2E")
								| 3: Write("FLDPI")
								| 4: Write("FLDLG2")
								| 5: Write("FLDLN2")
								| 6: Write("FLDZ")
								| 7: WriteUndef
								END
						| 6: 
								CASE ext MOD 8 OF
								| 0: Write("F2XM1")
								| 1: Write("FYL2X")
								| 2: Write("FPTAN")
								| 3: Write("FPATAN")
								| 4: Write("FXTRACT")
								| 5: Write("FPREM1")
								| 6: Write("FDECSTP")
								| 7: Write("FINCSTP")
								END
						| 7: 
								CASE ext MOD 8 OF
								| 0: Write("FPREM")
								| 1: Write("FYL2XP1")
								| 2: Write("FSQRT")
								| 3: Write("FSINCOS")
								| 4: Write("FRNDINT")
								| 5: Write("FSCALE")
								| 6: Write("FSIN")
								| 7: Write("FCOS")
								END
						END
				| 2:
						IF m = 1 THEN Write("FUCOMPP")
						ELSE WriteUndef
						END
				| 3:
						IF m = 4 THEN
							IF ext MOD 8 = 2 THEN Write("FCLEX")
							ELSIF ext MOD 8 = 3 THEN Write("FINIT")
							ELSE WriteUndef
							END
						ELSE WriteUndef
						END
				| 5:
						CASE m OF
						| 0: Write("FFREE   t")
						| 2: Write("FST     t")
						| 3: Write("FSTP    t")
						| 4: Write("FUCOM   t")
						| 5: Write("FUCOMP  t")
						| 1, 6, 7: WriteUndef
						END
				| 6:
						CASE m OF
						| 0: Write("FADDP   y")
						| 1: Write("FMULP   y")
						| 2: WriteUndef
						| 3: Write("FCOMPP")
						| 4, 5: Write("FSUBqP  y")
						| 6, 7: Write("FDIVqP  y")
						END
				| 7:
						IF m = 4 THEN Write("FSTSW   AX")
						ELSE WriteUndef
						END
				END
			END
		END WriteMath;
	
	BEGIN	(* WriteLine *)
		WriteHex(ad, 8); WriteString(":     ");
		len := 0; pre := TRUE;
		Act32BitData := mode = 0;
		Act32BitAddr := mode = 0;
		Prefix := ""; tab := 0;
		IF (ad = jmptab) & (end > jmptab) & (num > 0) THEN
			WriteString("WORD    $"); WriteHex(Word(TRUE), 8); INC(jmptab, 4); DEC(num);
			IF jmptab = end THEN jmptab := -1 END;
		ELSE
			REPEAT
				op := Next();
				IF op = 0F2H THEN WriteString("REP:")
				ELSIF op = 0F3H THEN WriteString("REPE:")
				ELSIF op = 0F0H THEN WriteString("LOCK:")
				ELSIF op = 66H THEN Act32BitData := ~Act32BitData
				ELSIF op = 67H THEN Act32BitAddr := ~Act32BitAddr
				ELSIF op = 2EH THEN Prefix := "CS:"
				ELSIF op = 3EH THEN Prefix := "DS:"
				ELSIF op = 26H THEN Prefix := "ES:"
				ELSIF op = 64H THEN Prefix := "FS:"
				ELSIF op = 65H THEN Prefix := "GS:"
				ELSIF op = 36H THEN Prefix := "SS:"
				ELSE pre := FALSE
				END
			UNTIL ~pre;
			IF op < 80H THEN
				CASE op OF
					0..5H, 8H..0DH, 10H..15H, 18H..1DH, 20H..25H, 28H..2DH, 30H..35H, 38H..3DH:
						WriteOp(op DIV 8 MOD 8);
						IF op MOD 8 < 4 THEN ext := Next(); Write("i    x")
						ELSIF op MOD 8 = 4 THEN Write("     AL,b")
						ELSE Write("W    u,w")
						END
				|  6H, 0EH, 16H, 1EH: Write("PUSH    s")
				|  7H, 17H, 1FH: Write("POP     s")
				|  0FH: WriteFPage;
				|  27H: Write("DAA")
				|  2FH: Write("DAS")
				|  37H: Write("AAA")
				|  3FH: Write("AAS")
				|  40H..47H: Write("INC     d")
				|  48H..4FH: Write("DEC     d")
				|  50H..57H: Write("PUSH    d")
				|  58H..5FH: Write("POP     d")
				|  60H: Write("PUSHA")
				|  61H: Write("POPA")
				|  62H: ext := Next();
					IF ext < 0C0H THEN Write("BOUND   e,m") ELSE WriteUndef END
				|  63H: ext := Next(); Write("ARPL    e,m")
				|  68H: Write("PUSH    w")
				|  69H: ext := Next(); Write("IMUL    e,m,w")
				|  6AH: Write("PUSH    b")
				|  6BH: ext := Next(); Write("IMUL    e,m,b")
				|  6CH, 6DH: Write("INSi")
				|  6EH, 6FH: Write("OUTSi")
				|  70H..7FH: Write("Jn     c")
				END
			ELSE
				CASE op OF
					80H: ext := Next(); Write("oB    m,b")
				|  81H: ext := Next(); Write("o     m,w")
				|  82H: WriteUndef
				|  83H: ext := Next(); Write("o     m,b")
				|  84H, 85H: ext := Next(); Write("TESTi   m,e")
				|  86H, 87H: ext := Next(); Write("XCHGi   m,e")
				|  88H..8BH: ext := Next(); Write("MOVi    x")
				|  8CH: ext := Next(); Write("MOV     m,"); WriteReg(ext DIV 8 MOD 8 + 16)
				|  8DH: ext := Next();
					IF ext < 0C0H THEN Write("LEA     e,m")
					ELSIF ext = 0F0H THEN Write("HALT    b")
					ELSIF ext DIV 16 = 0EH THEN Write("TRAP    "); WriteInt(ext MOD 16)
					ELSE WriteUndef
					END
				|  8EH: ext := Next(); Write("MOV     "); WriteReg(ext DIV 8 MOD 8 + 16); Write(",m")
				|  8FH: ext := Next();
						IF ext DIV 8 MOD 8 = 0 THEN Write("POP     m") ELSE WriteUndef END
				|  90H..97H: Write("XCHG    d,u")
				|  98H: IF Act32BitData THEN Write("CWDE") ELSE Write("CBW") END;
				|  99H: IF Act32BitData THEN Write("CDQ") ELSE Write("CWD") END;
				|  9AH: Write("CALL    w,w")
				|  9BH: Write("WAIT")
				|  9CH: Write("PUSHF")
				|  9DH: Write("POPF")
				|  9EH: Write("SAHF")
				|  9FH: Write("LAHF")
				|  0A0H: ext := 5; Write("MOVB    AL,m")
				|  0A1H: ext := 5; Write("MOV     u,m")
				|  0A2H: ext := 5; Write("MOVB    m,AL")
				|  0A3H: ext := 5; Write("MOV     m,u")
				|  0A4H, 0A5H: Write("MOVSi")
				|  0A6H, 0A7H: Write("CMPSi")
				|  0A8H: Write("TESTB    AL,b")
				|  0A9H: Write("TEST    u,w")
				|  0AAH, 0ABH: Write("STOSi")
				|  0ACH, 0ADH: Write("LODSi")
				|  0AEH, 0AFH: Write("SCASi")
				|  0B0H..0B7H: Write("MOVB    r,b")
				|  0B8H..0BFH: Write("MOV     d,w")
				|  0C0H, 0C1H: ext := Next(); Write("hi    m,b")
				|  0C2H: Write("RET     f")
				|  0C3H: Write("RET")
				|  0C4H: ext := Next(); Write("LES     e,m")
				|  0C5H: ext := Next(); Write("LDS     e,m")
				|  0C6H: ext := Next(); Write("MOVB    m,b")
				|  0C7H: ext := Next(); Write("MOV     m,w")
				|  0C8H: Write("ENTER   f,b")
				|  0C9H: Write("LEAVE")
				|  0CAH: Write("RETS    f")
				|  0CBH: Write("RETS")
				|  0CCH: Write("INT     3")
				|  0CDH: Write("INT     b")
				|  0CEH: Write("INTO")
				|  0CFH: Write("IRET")
				|  0D0H, 0D1H: ext := Next(); Write("hi    m")
				|  0D2H, 0D3H: ext := Next(); Write("hi    m,CL")
				|  0D4H: Write("AAM")
				|  0D5H: Write("AAD")
				|  0D6H: WriteUndef
				|  0D7H: Write("XLAT")
				|  0D8H..0DFH: WriteMath;
				|  0E0H: Write("LOOPNE  c")
				|  0E1H: Write("LOOPE   c")
				|  0E2H: Write("LOOP    c")
				|  0E3H: Write("JCXZ    c")
				|  0E4H: Write("INB     AL,b")
				|  0E5H: Write("IN      u,b")
				|  0E6H: Write("OUTB    b,AL")
				|  0E7H: Write("OUT     b,u")
				|  0E8H: Write("CALL    a")
				|  0E9H: Write("JMP     a")
				|  0EAH: Write("JMP     w,w")
				|  0EBH: Write("JMP     c")
				|  0ECH: Write("INB     AL")
				|  0EDH: Write("IN      u")
				|  0EEH: Write("OUTB    AL")
				|  0EFH: Write("OUT     u")
				|  0F1H: WriteUndef
				|  0F4H: Write("HALT")
				|  0F5H: Write("CMC")
				|  0F6H, 0F7H: ext := Next();
						CASE ext DIV 8 MOD 8 OF
							0: IF ODD(op) THEN Write("TEST    m,w") ELSE Write("TESTB   m,b") END
						|  1: WriteUndef
						|  2: Write("NOTi    m")
						|  3: Write("NEGi    m")
						|  4: Write("MULi    m")
						|  5: Write("IMULi   m")
						|  6: Write("DIVi    m")
						|  7: Write("IDIVi   m")
						END
				|  0F8H: Write("CLC")
				|  0F9H: Write("STC")
				|  0FAH: Write("CLI")
				|  0FBH: Write("STI")
				|  0FCH: Write("CLD")
				|  0FDH: Write("STD")
				|  0FEH, 0FFH: ext := Next();
						CASE ext DIV 8 MOD 8 OF
							0: Write("INCi    m")
						|  1: Write("DECi    m")
						|  2: Write("CALL    m")
						|  3: Write("CALLS   m")
						|  4: Write("JMP     m"); jmptab := tab
						|  5: Write("JMPS    m")
						|  6: Write("PUSH    m")
						|  7: WriteUndef
						END
				END
			END
		END;
		i := 0;
		Put(09X); Put(09X);
		WHILE i < len DO WriteHex(buf[i] MOD 256, 2); (* Put(" "); *) INC(i) END
	END DecodeLine;

BEGIN
	RegList1 := "ACDBSBSDACDBACDBECSDFG**";
	RegList2 := "XXXXPPIILLLLHHHHSSSSSS**";
	OpList1 := "AOASASXCRRRRSS*SAMCCSSDD";
	OpList2 := "DRDBNUOMOOCCHH*ADUOMUBIV";
	OpList3 := "D CBDBRPLRLRLR*RDLMPBRVR";
	CcList1 := "ONBAENBASNPNLGLG";
	CcList2 := " O E EE  S P EE ";
	FSList := " DLW";
	jmptab := -1
END DevDec486.
