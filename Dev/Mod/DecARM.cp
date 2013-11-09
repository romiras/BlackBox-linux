MODULE DevDecARM;
(* bh, 06 Dec 1999 *)
	
	IMPORT DevDecBase;
	
	VAR
		dataAdr: ARRAY 4096 OF BOOLEAN;
		
		
	PROCEDURE DecodeLine* (VAR ad: INTEGER; Put: DevDecBase.PutProc; Get: DevDecBase.GetProc; mode: INTEGER);
		VAR op, w, pc: INTEGER; b: BYTE; 
		
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
		
		PROCEDURE WriteCond (cond: INTEGER);
		BEGIN
			CASE cond OF
			| 0: WriteString("EQ")
			| 1: WriteString("NE")
			| 2: WriteString("CS")
			| 3: WriteString("CC")
			| 4: WriteString("MI")
			| 5: WriteString("PL")
			| 6: WriteString("VS")
			| 7: WriteString("VC")
			| 8: WriteString("HI")
			| 9: WriteString("LS")
			| 10: WriteString("GE")
			| 11: WriteString("LT")
			| 12: WriteString("GT")
			| 13: WriteString("LE")
			| 14:
			END
		END WriteCond;
		
		PROCEDURE WriteReg (n: INTEGER);
		BEGIN
			IF n = 13 THEN WriteString("SP")
			ELSIF n = 14 THEN WriteString("LR")
			ELSIF n = 15 THEN WriteString("PC")
			ELSE Put("R"); WriteInt(n)
			END
		END WriteReg;
		
		PROCEDURE WriteRegList (list: SET);
			VAR i, j: INTEGER;
		BEGIN
			Put("{"); i := 0;
			WHILE list # {} DO
				IF i > 0 THEN Put(",") END;
				WHILE ~(i IN list) DO INC(i) END;
				WriteReg(i); EXCL(list, i); j := i;
				WHILE i + 1 IN list DO INC(i); EXCL(list, i) END;
				IF i > j THEN
					IF i = j + 1 THEN Put(",") ELSE Put("-") END;
					WriteReg(i)
				END;
				INC(i)
			END;
			Put("}")
		END WriteRegList;
		
		PROCEDURE WriteField (n: INTEGER);
		BEGIN
			Put("_");
			IF ODD(n) THEN Put("c") END;
			IF ODD(n DIV 2) THEN Put("x") END;
			IF ODD(n DIV 4) THEN Put("s") END;
			IF ODD(n DIV 8) THEN Put("f") END;
		END WriteField;
		
		PROCEDURE WriteShift (op: INTEGER);
			VAR sh: INTEGER;
		BEGIN
			WriteReg(op MOD 16); sh := op DIV 16 MOD 256;
			IF sh = 2 THEN WriteString(",LSR#32")
			ELSIF sh = 4 THEN WriteString(",ASR#32")
			ELSIF sh = 6 THEN WriteString(",RRX")
			ELSIF sh # 0 THEN
				CASE sh DIV 2 MOD 4 OF
				| 0: WriteString(",LSL")
				| 1: WriteString(",LSR")
				| 2: WriteString(",ASR")
				| 3: WriteString(",ROR")
				END;
				IF ~ODD(sh) THEN Put("#"); WriteInt(sh DIV 8)	(* imm shift *)
				ELSIF ~ODD(sh DIV 8) THEN Put(" "); WriteReg(sh DIV 16)	(* reg shift *)
				ELSE HALT(100)
				END
			END
		END WriteShift;
		
		PROCEDURE WriteOp2 (op: INTEGER);
			VAR val, sh: INTEGER;
		BEGIN
			IF ODD(op DIV 2000000H) THEN	(* immediate *)
				val := op MOD 256; sh := op DIV 256 MOD 16 * 2;
				Put("#");
				IF sh = 0 THEN WriteInt(val)
				ELSE Put("$"); WriteHex(ASH(val, -sh) + ASH(val, 32 - sh), 8)
				END
			ELSE	(* register *)
				WriteShift(op)
			END
		END WriteOp2;
		
		PROCEDURE WriteAddr1 (op: INTEGER);
			VAR val, sh, off: INTEGER;
		BEGIN
			Put("["); WriteReg(op DIV 10000H MOD 16); 
			IF ~ODD(op DIV 1000000H) THEN Put("]") END;	(* post *)
			Put(",");
			IF ODD(op DIV 2000000H) THEN	(* register *)
				IF ~ODD(op DIV 800000H) THEN Put("-") END;
				WriteShift(op)
			ELSE	(* immediate *)
				off := op MOD 1000H;
				IF ~ODD(op DIV 800000H) THEN off := -off END;
				IF op DIV 10000H MOD 16 = 15 THEN
					off := off + pc + 8; Put("$"); WriteHex(off, 8); dataAdr[off MOD 4096] := TRUE
				ELSE
					Put("#"); WriteInt(off)
				END
			END;
			IF ODD(op DIV 1000000H) THEN	(* pre *)
				Put("]");
				IF ODD(op DIV 200000H) THEN Put("!") END
			END
		END WriteAddr1;
		
		PROCEDURE WriteAddr3 (op: INTEGER);
			VAR val, off: INTEGER;
		BEGIN
			Put("["); WriteReg(op DIV 10000H MOD 16); 
			IF ~ODD(op DIV 1000000H) THEN Put("]") END;	(* post *)
			Put(",");
			IF ODD(op DIV 400000H) THEN	(* immediate *)
				off := op DIV 100H MOD 16 * 16 + op MOD 16;
				IF ~ODD(op DIV 800000H) THEN off := -off END;
				IF op DIV 10000H MOD 16 = 15 THEN
					off := off + pc + 8; Put("$"); WriteHex(off, 8); dataAdr[off MOD 4096] := TRUE
				ELSE
					Put("#"); WriteInt(off)
				END
			ELSE	(* register *)
				IF ~ODD(op DIV 800000H) THEN Put("-") END;
				WriteReg(op MOD 16)
			END;
			IF ODD(op DIV 1000000H) THEN	(* pre *)
				Put("]");
				IF ODD(op DIV 200000H) THEN Put("!") END
			END
		END WriteAddr3;
		
		PROCEDURE WriteAddr5 (op: INTEGER);
			VAR val, off: INTEGER;
		BEGIN
			Put("["); WriteReg(op DIV 10000H MOD 16); 
			IF ~ODD(op DIV 1000000H) THEN Put("]") END;	(* post *)
			Put(","); off := op MOD 100H * 4;
			IF ~ODD(op DIV 800000H) THEN off := -off END;
			IF op DIV 10000H MOD 16 = 15 THEN
				off := off + pc + 8; Put("$"); WriteHex(off, 8); dataAdr[off MOD 4096] := TRUE
			ELSE
				Put("#"); WriteInt(off)
			END;
			IF ODD(op DIV 1000000H) THEN	(* pre *)
				Put("]");
				IF ODD(op DIV 200000H) THEN Put("!") END
			END
		END WriteAddr5;
		
		PROCEDURE Write(s: ARRAY OF CHAR);
			VAR ch: CHAR; i, a, c: INTEGER;
		BEGIN
			i := 0; ch := s[0];
			WHILE ch # 0X DO
				CASE ch OF
				| "b": (* byte *) IF ODD(op DIV 400000H) THEN Put("B") END;
				| "c": (* cond *) WriteCond(op DIV 10000000H MOD 16)
				| "d": (* Rd *) WriteReg(op DIV 1000H MOD 16)
				| "e": (* reglist *) WriteRegList(BITS(op MOD 10000H))
				| "f": (* b offset *) WriteHex(pc + 8 + ((op + 800000H) MOD 1000000H - 800000H) * 4, 8)
				| "g": (* psrf *) WriteField(op DIV 10000H MOD 16)
				| "h": (* halfword *) IF ODD(op DIV 20H) THEN Put("H") ELSE Put("B") END
				| "i": (* signed *) IF ODD(op DIV 40H) THEN Put("S") END
				| "k": (* link *) IF ODD(op DIV 1000000H) THEN Put("L") END
				| "l": (* long *) IF ODD(op DIV 400000H) THEN Put("L") END;
				| "m": (* Rm *) WriteReg(op MOD 16)
				| "n": (* Rn *) WriteReg(op DIV 10000H MOD 16)
				| "o": (* op2 *) WriteOp2(op)
				| "p": (* pre/post *) IF ODD(op DIV 1000000H) THEN Put("B") ELSE Put("A") END
				| "q": (* Rs *) WriteReg(op DIV 100H MOD 16)
				| "r": (* psr *) IF ODD(op DIV 400000H) THEN WriteString("CPSR") ELSE WriteString("SPSR") END
				| "s": (* set cc *) IF ODD(op DIV 100000H) THEN Put("S") END
				| "t": (* privilege *) IF ODD(op DIV 200000H) & ~ODD(op DIV 1000000H) THEN Put("T") END
				| "u": (* up/down *) IF ODD(op DIV 800000H) THEN Put("I") ELSE Put("D") END
				| "v": (* swi value *) WriteInt(op MOD 1000000H)
				| "x": (* CRn *) WriteString("CR"); WriteInt(op DIV 10000H MOD 16)
				| "y": (* CRm *) WriteString("CR"); WriteInt(op MOD 16)
				| "z": (* CRd *) WriteString("CR"); WriteInt(op DIV 1000H MOD 16)
				| "1": (* address1 *) WriteAddr1(op)
				| "3": (* address3 *) WriteAddr3(op)
				| "5": (* address5 *) WriteAddr5(op)
				| "7": (* cop *) WriteInt(op DIV 100000H MOD 16)
				| "8": (* cop *) WriteInt(op DIV 200000H MOD 8)
				| "9": (* cop *) WriteInt(op DIV 20 MOD 8)
				| "$": (* load/store *) IF ODD(op DIV 100000H) THEN WriteString("LD") ELSE WriteString("ST") END
				| "%": (* load/store *) IF ODD(op DIV 100000H) THEN WriteString("RC") ELSE WriteString("CR") END
				| "!": (* write back *) IF ODD(op DIV 200000H) THEN Put("!") END
				| "^": (* force user *) IF ODD(op DIV 400000H) THEN Put("^") END
				| "#": (* cp num *) Put("p"); WriteInt(op DIV 256 MOD 16)
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
		IF dataAdr[pc MOD 4096] THEN
			WriteString("DCD "); WriteInt(op); dataAdr[pc MOD 4096] := FALSE
		ELSIF BITS(op) * BITS(0F0000000H) = BITS(0F0000000H) THEN WriteUndef
		ELSIF BITS(op) * BITS(0E000000H) = BITS(0A000000H) THEN Write("Bkc f")
		ELSIF BITS(op) * BITS(0E000010H) = BITS(06000010H) THEN WriteUndef
		ELSIF BITS(op) * BITS(0C000000H) = BITS(04000000H) THEN Write("$Rcbt d,1")
		ELSIF BITS(op) * BITS(0E000000H) = BITS(08000000H) THEN Write("$Mcup n!,e^")
		ELSIF BITS(op) * BITS(0F000010H) = BITS(0E000000H) THEN Write("CDPc #,7,z,x,y,9")
		ELSIF BITS(op) * BITS(0F000010H) = BITS(0E000010H) THEN Write("M%c #,8,d,x,y,9")
		ELSIF BITS(op) * BITS(0E000000H) = BITS(0C000000H) THEN Write("$Ccl #,z,5")
		ELSIF BITS(op) * BITS(0F000000H) = BITS(0F000000H) THEN Write("SWIc v")
		ELSIF BITS(op) * BITS(01900000H) = BITS(01000000H) THEN	(* compare without s bit *)
			IF BITS(op) * BITS(0FB00FF0H) = BITS(01000090H) THEN Write("SWPcb d,m,[n]")
			ELSIF BITS(op) * BITS(0FBF0FFFH) = BITS(010F0000H) THEN Write("MRSc d,r")
			ELSIF BITS(op) * BITS(0FB0FFF0H) = BITS(0120F000H) THEN Write("MSRc rg,m")
			ELSIF BITS(op) * BITS(0FB0F000H) = BITS(0320F000H) THEN Write("MSRc rg,o")
			ELSIF BITS(op) * BITS(0FFFFFF0H) = BITS(012FFF10H) THEN Write("BXc m")
			ELSE WriteUndef
			END
		ELSIF BITS(op) * BITS(0E0000F0H) = BITS(00000090H) THEN	(* multiply *)
			CASE op DIV 200000H MOD 16 OF
			| 0: Write("MULcs n,m,q")
			| 1: Write("MLAcs n,m,q,d")
			| 4: Write("UMULLcs d,n,m,q")
			| 5: Write("UMLALcs d,n,m,q")
			| 6: Write("SMULLcs d,n,m,q")
			| 7: Write("SMLALcs d,n,m,q")
			ELSE WriteUndef
			END
		ELSIF BITS(op) * BITS(0E000090H) = BITS(00000090H) THEN	(* load/store extensions *)
			Write("$Rciht d,3")
		ELSE	(* data processing *)
			CASE op DIV 200000H MOD 16 OF
			| 0: Write("ANDcs d,n,o")
			| 1: Write("EORcs d,n,o")
			| 2: Write("SUBcs d,n,o")
			| 3: Write("RSBcs d,n,o")
			| 4: Write("ADDcs d,n,o")
			| 5: Write("ADCcs d,n,o")
			| 6: Write("SBCcs d,n,o")
			| 7: Write("RSCcs d,n,o")
			| 8: Write("TSTc n,o")
			| 9: Write("TEQc n,o")
			| 10: Write("CMPc n,o")
			| 11: Write("CMNc n,o")
			| 12: Write("ORRcs d,n,o")
			| 13: Write("MOVcs d,o")
			| 14: Write("BICcs d,n,o")
			| 15: Write("MVNcs d,o")
			END
		END
	END DecodeLine;

END DevDecARM.
