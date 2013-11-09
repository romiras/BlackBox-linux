MODULE DevDecPPC;
(* bh, 30 Dec 1998 *)
	
	IMPORT DevDecBase;
	
	VAR
		hiVal, hiAdr: INTEGER;
		
		
	PROCEDURE DecodeLine* (VAR ad: INTEGER; Put: DevDecBase.PutProc; Get: DevDecBase.GetProc; mode: INTEGER);
		VAR op, w, pc, r1, r2, hop: INTEGER; b: BYTE;
		
		PROCEDURE ReadWord (OUT x: INTEGER);
			VAR b: BYTE;
		BEGIN
			IF mode = 0 THEN	(* little endian *)
				Get(b); x := b MOD 256;
				Get(b); x := x + b MOD 256 * 100H;
				Get(b); x := x + b MOD 256 * 10000H;
				Get(b); x := x + b * 1000000H
			ELSE	(* big endian *)
				Get(b); x := b;
				Get(b); x := x * 256 + b MOD 256;
				Get(b); x := x * 256 + b MOD 256;
				Get(b); x := x * 256 + b MOD 256
			END
		END ReadWord;
		
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
		
		PROCEDURE WriteUndef;
		BEGIN
			WriteString("*****")
		END WriteUndef;
		
		PROCEDURE WriteCond (bo, bi: INTEGER);
		BEGIN
			IF ~ODD(bo DIV 4) THEN
				Put("d"); IF ~ODD(bo DIV 2) THEN Put("n") END; Put("z")
			END;
			IF bo < 8 THEN
				CASE bi MOD 4 OF
				| 0: WriteString("ge")
				| 1: WriteString("le")
				| 2: WriteString("ne")
				| 3: WriteString("ns")
				END
			ELSIF bo < 16 THEN
				CASE bi MOD 4 OF
				| 0: WriteString("lt")
				| 1: WriteString("gt")
				| 2: WriteString("eq")
				| 3: WriteString("so")
				END
			END
		END WriteCond;
		
		PROCEDURE WriteTCond (to: INTEGER);
		BEGIN
			CASE to OF
			| 1: WriteString("llt")
			| 2: WriteString("lgt")
			| 4: WriteString("eq")
			| 5: WriteString("lge")
			| 6: WriteString("lle")
			| 8: WriteString("gt")
			| 12: WriteString("ge")
			| 16: WriteString("lt")
			| 20: WriteString("le")
			| 3, 24: WriteString("ne")
			| 7, 28, 31: Put("a")
			ELSE Put("?")
			END
		END WriteTCond;
		
		PROCEDURE WriteReg (n: INTEGER);
		BEGIN
			IF n = 1 THEN WriteString("SP")
			ELSIF n = 2 THEN WriteString("TOC")
			ELSE Put("r"); WriteInt(n)
			END
		END WriteReg;
		
		PROCEDURE WriteSPR (n: INTEGER);
		BEGIN
			IF n = 1 THEN WriteString("XER")
			ELSIF n = 8 THEN WriteString("LR")
			ELSIF n = 9 THEN WriteString("CTR")
			ELSIF n = 268 THEN WriteString("TBL")
			ELSIF n = 269 THEN WriteString("TBH")
			ELSE WriteInt(n)
			END
		END WriteSPR;
		
		PROCEDURE Write(s: ARRAY OF CHAR);
			VAR ch: CHAR; i, a: INTEGER;
		BEGIN
			i := 0; ch := s[0];
			WHILE ch # 0X DO
				CASE ch OF
				| "A": (* rA *) WriteReg(op DIV 10000H MOD 32)
				| "B": (* rB *) WriteReg(op DIV 800H MOD 32)
				| "D": (* rD *) WriteReg(op DIV 200000H MOD 32)
				| "X": (* A *) WriteInt(op DIV 10000H MOD 32)
				| "Y": (* B *) WriteInt(op DIV 800H MOD 32)
				| "Z": (* D *) WriteInt(op DIV 200000H MOD 32)
				| "W": (* rA|0 *) a := op DIV 10000H MOD 32; IF a # 0 THEN WriteReg(a) ELSE Put("0") END
				| "M": (* C/MB *) WriteInt(op DIV 40H MOD 32)
				| "N": (* ME *) WriteInt(op DIV 2 MOD 32)
				| "H": (* sh *) WriteInt(op DIV 800H MOD 32 + op DIV 2 MOD 2 * 32)
				| "Q": (* mb/me *) WriteInt(op DIV 40H MOD 32 + op DIV 20H MOD 2 * 32)
				| "C": (* Rc *) IF ODD(op) THEN Put(".") END
				| "O": (* OE *) IF ODD(op DIV 400H) THEN Put("o") END
				| "P": (* spr/tbr *) WriteSPR(op DIV 800H MOD 32 * 32 + op DIV 10000H MOD 32)
				| ">": (* crfS *) WriteInt(op DIV 40000H MOD 8)
				| "<": (* crfD *) WriteInt(op DIV 800000H MOD 8)
				| "L": (* L *) IF ODD(op DIV 200000H) THEN Put("d") ELSE Put("w") END
				| "K": (* LK *) IF ODD(op) THEN Put("l") END
				| "$": (* AA *) IF ODD(op DIV 2) THEN Put("a") END
				| "#": (* LI,AA *) a := ((op + 2000000H) MOD 4000000H - 2000000H) DIV 4 * 4;
					IF ~ODD(op DIV 2) THEN INC(a, pc) END; WriteHex(a, 8)
				| "@": (* BD,AA *) a := ((op + 8000H) MOD 10000H - 8000H) DIV 4 * 4;
					IF ~ODD(op DIV 2) THEN INC(a, pc) END; WriteHex(a, 8)
				| "S": (* SIMM *)  WriteInt((op + 8000H) MOD 10000H - 8000H)
				| "U": (* UIMM *)  WriteInt(op MOD 10000H)
				| "V": (* ds *) WriteInt((op DIV 4 * 4 + 8000H) MOD 10000H - 8000H)
				| "R": (* CRM *) WriteInt(op DIV 1000H MOD 256)
				| "E": (* FM *) WriteInt(op DIV 20000H MOD 256)
				| "J": (* IMM *) WriteInt(op DIV 1000H MOD 16)
				| "T": (* BO,BI *) WriteCond(op DIV 200000H MOD 32, op DIV 10000H MOD 32)
				| "G": (* TO *) WriteTCond(op DIV 200000H MOD 32)
				| "+", "-": (* BO[4] *) IF ODD(op DIV 200000H) THEN Put(ch) END
				ELSE Put(ch)
				END;
				INC(i); ch := s[i]
			END
		END Write;
		
	BEGIN	(* DecodeLine *)
		WriteHex(ad, 8); WriteString(":     ");
		ReadWord(op);
		WriteHex(op, 8); WriteString("     ");
		pc := ad; INC(ad, 4);
		hop := op DIV 4000000H MOD 64;
		CASE hop OF
		| 2: Write("tdGi   A,S")
		| 3: Write("twGi   A,S")
		| 7: Write("mulli   D,A,S")
		| 8: Write("subfic   D,A,S")
		| 0AH: Write("cmplLi   crf<,A,U")
		| 0BH: Write("cmpLi   crf<,A,S")
		| 0CH: Write("addic   D,A,S")
		| 0DH: Write("addic.   D,A,S")
		| 0EH: IF op DIV 10000H MOD 32 = 0 THEN Write("li   D,S") ELSE Write("addi   D,A,S") END
		| 0FH: hiVal := op; hiAdr := pc;
				IF op DIV 10000H MOD 32 = 0 THEN Write("lis   D,S") ELSE Write("addis   D,A,S") END
		| 10H: IF ODD(op DIV 8000H) THEN Write("bTK$-   cr>,@") ELSE Write("bTK$+   cr>,@") END
		| 11H: Write("sc")
		| 12H: Write("bK$   #")
		| 13H:
			CASE op DIV 2 MOD 400H OF
			| 0: Write("mcrf   cr<,cr>")
			| 10H: Write("bTlrK+   cr>")
			| 21H: Write("crnor   crbZ,crbX,crbY")
			| 32H: Write("rfi")
			| 81H: Write("crandc   crbZ,crbX,crbY")
			| 96H: Write("isync")
			| 0C1H: Write("crxor   crbZ,crbX,crbY")
			| 0E1H: Write("crnand   crbZ,crbX,crbY")
			| 101H: Write("crand   crbZ,crbX,crbY")
			| 121H: Write("creqv   crbZ,crbX,crbY")
			| 1A1H: Write("crorc   crbZ,crbX,crbY")
			| 1C1H: Write("cror   crbZ,crbX,crbY")
			| 210H: Write("bTctrK+   cr>")
			ELSE WriteUndef
			END
		| 14H: Write("rlwimiC   A,D,Y,M,N")
		| 15H: Write("rlwinmC   A,D,Y,M,N")
		| 17H: Write("rlwnmC   A,D,B,M,N")
		| 18H: IF op MOD 4000000H = 0 THEN Write("nop")
				ELSIF op MOD 10000H = 0 THEN Write("mr   A,D")
				ELSE Write("ori   A,D,U")
				END
		| 19H: Write("oris   A,D,U")
		| 1AH: Write("xori   A,D,U")
		| 1BH: Write("xoris   A,D,U")
		| 1CH: Write("andi.   A,D,U")
		| 1DH: Write("andis.   A,D,U")
		| 1EH:
			CASE op DIV 2 MOD 16 OF
			| 0,1: Write("rldiclC   A,D,H,Q")
			| 2,3: Write("rldicrC   A,D,H,Q")
			| 4,5: Write("rldicC   A,D,H,Q")
			| 6,7: Write("rldimiC   A,D,H,Q")
			| 8: Write("rldclC   A,D,B,Q")
			| 9: Write("rldcrC   A,D,B,Q")
			ELSE WriteUndef
			END
		| 1FH:
			CASE op DIV 2 MOD 400H OF
			| 0: Write("cmpL   cr<,A,B")
			| 4: IF op DIV 200000H MOD 32 = 31 THEN Write("trap") ELSE Write("twG   A,B") END
			| 8,208H: Write("subfcOC   D,A,B")
			| 9: Write("mulhduC   D,A,B")
			| 0AH,20AH: Write("addcOC   D,A,B")
			| 0BH: Write("mulhwuC   D,A,B")
			| 13H: Write("mfcr   D")
			| 14H: Write("lwarx   D,W,B")
			| 15H: Write("ldx   D,W,B")
			| 17H: Write("lwzx   D,W,B")
			| 18H: Write("slwC   A,D,B")
			| 1AH: Write("cntlzwC   A,D")
			| 1BH: Write("sldC   A,D,B")
			| 1CH: Write("andC   A,D,B")
			| 20H: Write("cmplL   cr<,A,B")
			| 28H,228H: Write("subfOC   D,A,B")
			| 35H: Write("ldux   D,A,B")
			| 36H: Write("dcbst   A,B")
			| 37H: Write("lwzux   D,A,B")
			| 3AH: Write("cntlzdC   A,D")
			| 3CH: Write("andcC   A,D,B")
			| 44H: Write("tdG   A,B")
			| 49H: Write("mulhdC   D,A,B")
			| 4BH: Write("mulhwC   D,A,B")
			| 53H: Write("mfmsr   D")
			| 54H: Write("ldarx   D,W,B")
			| 56H: Write("dcbf   A,B")
			| 57H: Write("lbzx   D,W,B")
			| 68H, 268H: Write("negOC   D,A")
			| 77H: Write("lbzux   D,A,B")
			| 7CH: IF (op DIV 800H - op DIV 200000H) MOD 32 = 0 THEN Write("notC   A,D") ELSE Write("norC   A,D,B") END
			| 88H,288H: Write("subfeOC   D,A,B")
			| 8AH,28AH: Write("addeOC   D,A,B")
			| 90H: Write("mtcrf   R,D")
			| 92H: Write("mtmsr   D")
			| 95H: Write("stdx   D,A,B")
			| 96H: Write("stwcxC   D,W,B")
			| 97H: Write("stwx   D,W,B")
			| 0B5H: Write("stdux   D,A,B")
			| 0B7H: Write("stwux   D,A,B")
			| 0C8H, 2C8H: Write("subfzeOC   D,A")
			| 0CAH, 2CAH: Write("addzeOC   D,A")
			| 0D2H: Write("mtsr   X,D")
			| 0D6H: Write("stdcxC   D,W,B")
			| 0D7H: Write("stbx   D,W,B")
			| 0E8H,2E8H: Write("subfmeOC   D,A")
			| 0E9H,2E9H: Write("mulldOC   D,A,B")
			| 0EAH,2EAH: Write("addmeOC   D,A")
			| 0EBH,2EBH: Write("mullwOC   D,A,B")
			| 0F2H: Write("mtsrin   D,B")
			| 0F6H: Write("dcbtst   A,B")
			| 0F7H: Write("stbux   D,A,B")
			| 10AH,30AH: Write("addOC   D,A,B")
			| 116H: Write("dcbt   A,B")
			| 117H: Write("lhzx   D,W,B")
			| 11CH: Write("eqvC   A,D,B")
			| 132H: Write("tlbie   B")
			| 136H: Write("eciwx   D,A,B")
			| 137H: Write("lhzux   D,A,B")
			| 13CH: Write("xorC   A,D,B")
			| 153H: Write("mfspr   D,P")
			| 155H: Write("lwax   D,W,B")
			| 157H: Write("lhax   D,W,B")
			| 172H: Write("tlbia")
			| 173H: Write("mftb   D,P")
			| 175H: Write("lwaux   D,A,B")
			| 177H: Write("lhaux   D,A,B")
			| 197H: Write("sthx   D,W,B")
			| 19CH: Write("orcC   A,D,B")
			| 1B2H: Write("slbie   B")
			| 1B6H: Write("ecowx   D,A,B")
			| 1B7H: Write("sthux   D,A,B")
			| 1BCH: IF (op DIV 800H - op DIV 200000H) MOD 32 = 0 THEN Write("mrC   A,D") ELSE Write("orC   A,D,B") END
			| 1C9H,3C9H: Write("divduOC   D,A,B")
			| 1CBH,3CBH: Write("divwuOC   D,A,B")
			| 1D3H: Write("mtspr   P,D")
			| 1D6H: Write("dcbi   A,B")
			| 1DCH: Write("nandC   A,D,B")
			| 1E9H,3E9H: Write("divdOC   D,A,B")
			| 1EBH,3EBH: Write("divwOC   D,A,B")
			| 1F2H: Write("slbia")
			| 200H: Write("mcrxr   cr<")
			| 215H: Write("lswx   D,W,B")
			| 216H: Write("lwbrx   D,W,B")
			| 217H: Write("lfsx   frZ,W,B")
			| 218H: Write("srwC   A,D,B")
			| 21BH: Write("srdC   A,D,B")
			| 236H: Write("tlbsync")
			| 237H: Write("lfsux   frZ,A,B")
			| 253H: Write("mfsr   D,X")
			| 255H: Write("lswi   D,W,Y")
			| 256H: Write("sync")
			| 257H: Write("lfdx   frZ,W,B")
			| 277H: Write("lfdux   frZ,A,B")
			| 293H: Write("mfsrin   D,B")
			| 295H: Write("stswx   D,W,B")
			| 296H: Write("stwbrx   D,W,B")
			| 297H: Write("stfsx   frZ,W,B")
			| 2B7H: Write("stfsux   frZ,A,B")
			| 2D5H: Write("stswi   D,W,Y")
			| 2D7H: Write("stfdx   frZ,W,B")
			| 2F7H: Write("stfdux   frZ,A,B")
			| 316H: Write("lhbrx   D,W,B")
			| 318H: Write("srawC   A,D,B")
			| 31AH: Write("sradC   A,D,B")
			| 338H: Write("srawiC   A,D,Y")
			| 33AH, 33BH: Write("sradiC   A,D,H")
			| 356H: Write("eieio")
			| 396H: Write("sthbrx   D,W,B")
			| 39AH: Write("extshC   A,D")
			| 3BAH: Write("extsbC   A,D")
			| 3D6H: Write("icbi   A,B")
			| 3D7H: Write("stfiwx   frZ,W,B")
			| 3DAH: Write("extswC   A,D")
			| 3F6H: Write("dcbz   A,B")
			ELSE WriteUndef
			END
		| 20H: Write("lwz   D,S(W)")
		| 21H: Write("lwzu   D,S(A)")
		| 22H: Write("lbz   D,S(W)")
		| 23H: Write("lbzu   D,S(A)")
		| 24H: Write("stw   D,S(W)")
		| 25H: Write("stwu   D,S(A)")
		| 26H: Write("stb   D,S(W)")
		| 27H: Write("stbu   D,S(A)")
		| 28H: Write("lhz   D,S(W)")
		| 29H: Write("lhzu   D,S(A)")
		| 2AH: Write("lha   D,S(W)")
		| 2BH: Write("lhau   D,S(A)")
		| 2CH: Write("sth   D,S(W)")
		| 2DH: Write("sthu   D,S(A)")
		| 2EH: Write("lmw   D,S(W)")
		| 2FH: Write("stmw   D,S(W)")
		| 30H: Write("lfs   frZ,S(W)")
		| 31H: Write("lfsu   frZ,S(A)")
		| 32H: Write("lfd   frZ,S(W)")
		| 33H: Write("lfdu   frZ,S(A)")
		| 34H: Write("stfs   frZ,S(W)")
		| 35H: Write("stfsu   frZ,S(A)")
		| 36H: Write("stfd   frZ,S(W)")
		| 37H: Write("stfdu   frZ,S(A)")
		| 3AH:
			CASE op MOD 4 OF
			| 0: Write("ld   D,V(W)")
			| 1: Write("ldu   D,V(A)")
			| 2: Write("lwa   D,V(W)")
			ELSE WriteUndef
			END
		| 3BH:
			CASE op DIV 2 MOD 32 OF
			| 12H: Write("fdivsC   frZ,fA,frY")
			| 14H: Write("fsubsC   frZ,fA,frY")
			| 15H: Write("faddsC   frZ,fA,frY")
			| 16H: Write("fsqrtsC   frZ,frY")
			| 18H: Write("fresC   frZ,frY")
			| 19H: Write("fmulsC   frZ,fA,frM")
			| 1CH: Write("fmsubsC   frZ,fA,frM,frY")
			| 1DH: Write("fmaddsC   frZ,fA,frM,frY")
			| 1EH: Write("fnmsubsC   frZ,fA,frM,frY")
			| 1FH: Write("fnmaddsC   frZ,fA,frM,frY")
			ELSE WriteUndef
			END
		| 3EH:
			CASE op MOD 4 OF
			| 0: Write("std   D,V(W)")
			| 1: Write("stdu   D,V(A)")
			ELSE WriteUndef
			END
		| 3FH:
			IF ODD(op DIV 32) THEN
				CASE op DIV 2 MOD 16 OF
				| 2: Write("fdivC   frZ,frX,frY")
				| 4: Write("fsubC   frZ,frX,frY")
				| 5: Write("faddC   frZ,frX,frY")
				| 6: Write("fsqrtC   frZ,frY")
				| 7: Write("fselC   frZ,frX,frM,frY")
				| 9: Write("fmulC   frZ,frX,frM")
				| 0AH: Write("frsqrteC   frZ,frY")
				| 0CH: Write("fmsubC   frZ,frX,frM,frY")
				| 0DH: Write("fmaddC   frZ,frX,frM,frY")
				| 0EH: Write("fnmsubC   frZ,frX,frM,frY")
				| 0FH: Write("fnmaddC   frZ,frX,frM,frY")
				ELSE WriteUndef
				END
			ELSE
				CASE op DIV 2 MOD 400H OF
				| 0: Write("fcmpu   cr<,frX,frY")
				| 0CH: Write("frspC   frZ,frY")
				| 0EH: Write("fctiwC   frZ,frY")
				| 0FH: Write("fctiwzC   frZ,frY")
				| 20H: Write("fcmpo   cr<,frX,frY")
				| 26H: Write("mtfsb1C   crbZ")
				| 28H: Write("fnegC   frZ,frY")
				| 40H: Write("mcrfs   cr<,cr>")
				| 46H: Write("mtfsb0C   crbZ")
				| 48H: Write("fmrC   frZ,frY")
				| 86H: Write("mtfsfiC   cr<,J")
				| 88H: Write("fnabsC   frZ,frY")
				| 108H: Write("fabsC   frZ,frY")
				| 247H: Write("mffsC   frZ")
				| 2C7H: Write("mtfsfC   E,frY")
				| 32EH: Write("fctidC   frZ,frY")
				| 32FH: Write("fctidzC   frZ,frY")
				| 34EH: Write("fcfidC   frZ,frY")
				ELSE WriteUndef
				END
			END
		ELSE WriteUndef
		END;
		IF hiAdr = pc - 4 THEN
			r1 := hiVal DIV 200000H MOD 32;
			IF hop = 18H THEN
				r2 := op DIV 200000H MOD 32;
				IF r1 = r2 THEN
					WriteString("     // ");
					WriteHex(hiVal, 4); WriteHex(op, 4)
				END
			ELSIF (hop = 0EH) OR (hop >= 20H) & (hop <= 37H) THEN
				r2 := op DIV 10000H MOD 32;
				IF r1 = r2 THEN
					WriteString("     // ");
					WriteHex(hiVal * 10000H + (op + 8000H) MOD 10000H - 8000H, 8)
				END
			END
		END
	END DecodeLine;

END DevDecPPC.
