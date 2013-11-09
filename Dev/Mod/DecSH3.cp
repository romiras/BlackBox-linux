MODULE DevDecSH3;
(* bh, 24 Aug 1997 *)
	
	IMPORT DevDecBase;
	
	CONST
		wordAdr = 1; longAdr = 2; stringAdr = 3; caseBAdr = 4; caseWAdr = 5; caseLAdr = 6;
	
	VAR
		dataAdr: ARRAY 1024 OF BYTE;
		
		
	PROCEDURE DecodeLine* (VAR ad: INTEGER; Put: DevDecBase.PutProc; Get: DevDecBase.GetProc; mode: INTEGER);
		VAR op, w, pc: INTEGER; b: BYTE; 
		
		PROCEDURE ReadWord (OUT x: INTEGER);
			VAR b: BYTE;
		BEGIN
			IF mode = 0 THEN	(* little endian *)
				Get(b); x := b MOD 256;
				Get(b); x := x + b MOD 256 * 256;
			ELSE	(* big endian *)
				Get(b); x := b MOD 256;
				Get(b); x := x * 256 + b MOD 256;
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
		
		PROCEDURE WriteInt (x: INTEGER);
		BEGIN
			IF x < 0 THEN Put("-"); WriteInt(-x)
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
		
		PROCEDURE WriteString (s: ARRAY OF CHAR);
			VAR i: INTEGER;
		BEGIN
			i := 0; WHILE s[i] # 0X DO Put(s[i]); INC(i) END
		END WriteString;
		
		PROCEDURE WriteUndef;
		BEGIN
			WriteString("*****")
		END WriteUndef;
		
		PROCEDURE WriteCtrlReg (n: INTEGER);
		BEGIN
			CASE n OF
			| 0: WriteString("SR")
			| 1: WriteString("GBR")
			| 2: WriteString("VBR")
			| 3: WriteString("SSR")
			| 4: WriteString("SPC")
			| 5..7: WriteString("??")
			| 8: WriteString("R0_BANK")
			| 9: WriteString("R1_BANK")
			| 10: WriteString("R2_BANK")
			| 11: WriteString("R3_BANK")
			| 12: WriteString("R4_BANK")
			| 13: WriteString("R5_BANK")
			| 14: WriteString("R6_BANK")
			| 15: WriteString("R7_BANK")
			END
		END WriteCtrlReg;
		
		PROCEDURE WriteSysReg (n: INTEGER);
		BEGIN
			CASE n OF
			| 0: WriteString("MACH")
			| 1: WriteString("MACL")
			| 2: WriteString("PR")
			| 3..15: WriteString("??")
			END
		END WriteSysReg;
		
		PROCEDURE Write(s: ARRAY OF CHAR);
			VAR ch: CHAR; i, a: INTEGER;
		BEGIN
			i := 0; ch := s[0];
			WHILE ch # 0X DO
				CASE ch OF
				| "n": (* Rn *) Put("R"); WriteInt(op DIV 256 MOD 16)
				| "m": (* Rm *) Put("R"); WriteInt(op DIV 16 MOD 16)
				| "s": (* disp:4 *) WriteInt(op MOD 16)
				| "d": (* 2 * disp:4 *) WriteInt(op MOD 16 * 2)
				| "q": (* 4 * disp:4 *) WriteInt(op MOD 16 * 4)
				| "u": (* uns:8 *) WriteInt(op MOD 256)
				| "v": (* 2 * uns:8 *) WriteInt(op MOD 256 * 2)
				| "w": (* 4 * uns:8 *) WriteInt(op MOD 256 * 4)
				| "b": (* sig:8 *) WriteInt((op + 128) MOD 256 - 128)
				| "r": (* offset:8 *) Put("$"); WriteHex(pc + ((op + 128) MOD 256 - 128) * 2, 8)
				| "o": (* offset:12 *) Put("$"); WriteHex(pc + ((op + 2048) MOD 4096 - 2048) * 2, 8)
				| "x": (* 2 * d:8(PC) *) Put("$"); a := pc + (op MOD 256) * 2; WriteHex(a, 8);
					dataAdr[a MOD 1024] := wordAdr
				| "y": (* 4 * d:8(PC) *) Put("$"); a := (pc DIV 4 + op MOD 256) * 4; WriteHex(a, 8);
					dataAdr[a MOD 1024] := longAdr
				| "z": (* 4 * d:8(PC) *) Put("$"); a := (pc DIV 4 + op MOD 256) * 4; WriteHex(a, 8);
					dataAdr[a MOD 1024] := stringAdr
				| "c": (* ctrl reg *) WriteCtrlReg(op DIV 16 MOD 16)
				| "t": (* sys reg *) WriteSysReg(op DIV 16 MOD 16)
				ELSE Put(ch)
				END;
				INC(i); ch := s[i]
			END
		END Write;
		
	BEGIN	(* DecodeLine *)
		WriteHex(ad, 8); WriteString(":     ");
		ReadWord(op);
		WriteHex(op, 4); WriteString("     ");
		IF dataAdr[ad MOD 1024] = wordAdr THEN
			WriteString("WORD    $"); WriteHex(op, 4);
			dataAdr[ad MOD 1024] := 0; INC(ad, 2)
		ELSIF dataAdr[ad MOD 1024] = longAdr THEN
			ReadWord(w);
			WriteString("LONG    $");
			IF mode = 0 THEN WriteHex(w, 4); WriteHex(op, 4)	(* little endian *)
			ELSE  WriteHex(op, 4); WriteHex(w, 4)	(* big endian *)
			END;
			dataAdr[ad MOD 1024] := 0; INC(ad, 2);
			dataAdr[ad MOD 1024] := 0; INC(ad, 2)
		ELSIF dataAdr[ad MOD 1024] = stringAdr THEN
			IF mode = 0 THEN w := op DIV 256; op := op MOD 256	(* little endian *)
			ELSE  w := op MOD 256; op := op DIV 256	(* big endian *)
			END;
			WriteString("BYTE    $"); WriteHex(op, 2);
			WriteString(", $"); WriteHex(w, 2);
			WriteString(", $"); Get(b); WriteHex(b, 2);
			WriteString(", $"); Get(b); WriteHex(b, 2);
			dataAdr[ad MOD 1024] := 0; INC(ad, 2);
			dataAdr[ad MOD 1024] := 0; INC(ad, 2)
		ELSE
			INC(ad, 2); pc := ad + 2;
			CASE op DIV 4096 OF
			| 0: 
				CASE op MOD 16 OF
				| 0, 1: WriteUndef
				| 2: Write("STC   c,n")
				| 3: 
					CASE op DIV 16 MOD 16 OF
					| 0: Write("BSRF n")
					| 2: Write("BRAF n")
					| 8: Write("PREF @n")
					| 1, 3..7, 9..15: WriteUndef
					END
				| 4: Write("MOV.B m,@(R0,n)")
				| 5: Write("MOV.W m,@(R0,n)")
				| 6: Write("MOV.L m,@(R0,n)")
				| 7: Write("MUL.L m,n")
				| 8: 
					IF op DIV 256 MOD 16 = 0 THEN
						CASE op DIV 16 MOD 16 OF
						| 0: Write("CLRT")
						| 1: Write("SETT")
						| 2: Write("CLRMAC")
						| 3: Write("LDTLB")
						| 4: Write("CLRS")
						| 5: Write("SETS")
						| 6..15: WriteUndef
						END
					ELSE WriteUndef
					END
				| 9: 
					IF op DIV 16 MOD 16 = 2 THEN Write("MOVT  n")
					ELSIF op DIV 256 MOD 16 = 0 THEN
						CASE op DIV 16 MOD 16 OF
						| 0: Write("NOP")
						| 1: Write("DIV0U")
						| 2..15: WriteUndef
						END
					ELSE WriteUndef
					END
				| 10: Write("STS   t,n")
				| 11: 
					IF op DIV 256 MOD 16 = 0 THEN
						CASE op DIV 16 MOD 16 OF
						| 0: Write("RTS")
						| 1: Write("SLEEP")
						| 2: Write("RTE")
						| 3..15: WriteUndef
						END
					ELSE WriteUndef
					END
				| 12: Write("MOV.B @(R0,m),n")
				| 13: Write("MOV.W @(R0,m),n")
				| 14: Write("MOV.L @(R0,m),n")
				| 15: Write("MAC.L @m+,@n+")
				END
			| 1: Write("MOV.L m,@(q,n)")
			| 2: 
				CASE op MOD 16 OF
				| 0: Write("MOV.B m,@n")
				| 1: Write("MOV.W m,@n")
				| 2: Write("MOV.L m,@n")
				| 3: WriteUndef
				| 4: Write("MOV.B m,@-n")
				| 5: Write("MOV.W m,@-n")
				| 6: Write("MOV.L m,@-n")
				| 7: Write("DIV0S m,n")
				| 8: Write("TST   m,n")
				| 9: Write("AND   m,n")
				| 10: Write("XOR   m,n")
				| 11: Write("OR    m,n")
				| 12: Write("CMP/STR m,n")
				| 13: Write("XTRCT m,n")
				| 14: Write("MULU.W m,n")
				| 15: Write("MULS.W m,n")
				END
			| 3: 
				CASE op MOD 16 OF
				| 0: Write("CMP/EQ m,n")
				| 1: WriteUndef
				| 2: Write("CMP/HS m,n")
				| 3: Write("CMP/GE m,n")
				| 4: Write("DIV1  m,n")
				| 5: Write("DMULU.L m,n")
				| 6: Write("CMP/HI m,n")
				| 7: Write("CMP/GT m,n")
				| 8: Write("SUB   m,n")
				| 9: WriteUndef
				| 10: Write("SUBC  m,n")
				| 11: Write("SUBV  m,n")
				| 12: Write("ADD   m,n")
				| 13: Write("DMULS.L m,n")
				| 14: Write("ADDC  m,n")
				| 15: Write("ADDV  m,n")
				END
			| 4: 
				CASE op MOD 16 OF
				| 0: 
					CASE op DIV 16 MOD 16 OF
					| 0: Write("SHLL  n")
					| 1: Write("DT    n")
					| 2: Write("SHAL  n")
					| 3..15: WriteUndef
					END
				| 1: 
					CASE op DIV 16 MOD 16 OF
					| 0: Write("SHLR  n")
					| 1: Write("CMP/PZ n")
					| 2: Write("SHAR  n")
					| 3..15: WriteUndef
					END
				| 2: Write("STS.L t,@-n")
				| 3: Write("STC.L   c,@-n")
				| 4: 
					CASE op DIV 16 MOD 16 OF
					| 0: Write("ROTL  n")
					| 2: Write("ROTCL n")
					| 1, 3..15: WriteUndef
					END
				| 5: 
					CASE op DIV 16 MOD 16 OF
					| 0: Write("ROTR  n")
					| 1: Write("CMP/PL n")
					| 2: Write("ROTCR n")
					| 3..15: WriteUndef
					END
				| 6: Write("LDS.L @n+,t")
				| 7:  Write("LDC.L @n+,c")
				| 8: 
					CASE op DIV 16 MOD 16 OF
					| 0: Write("SHLL2 n")
					| 1: Write("SHLL8 n")
					| 2: Write("SHLL16 n")
					| 3..15: WriteUndef
					END
				| 9: 
					CASE op DIV 16 MOD 16 OF
					| 0: Write("SHLR2 n")
					| 1: Write("SHLR8 n")
					| 2: Write("SHLR16 n")
					| 3..15: WriteUndef
					END
				| 10: Write("LDS   n,t")
				| 11: 
					CASE op DIV 16 MOD 16 OF
					| 0: Write("JSR   @n")
					| 1: Write("TAS.B @n")
					| 2: Write("JMP   @n")
					| 3..15: WriteUndef
					END
				| 12: Write("SHAD  m,n")
				| 13: Write("SHLD  m,n")
				| 14: Write("LDC   n,c")
				| 15: Write("MAC.W @m+,@n+")
				END
			| 5: Write("MOV.L @(q,m),n")
			| 6: 
				CASE op MOD 16 OF
				| 0: Write("MOV.B @m,n")
				| 1: Write("MOV.W @m,n")
				| 2: Write("MOV.L @m,n")
				| 3: Write("MOV   m,n")
				| 4: Write("MOV.B @m+,n")
				| 5: Write("MOV.W @m+,n")
				| 6: Write("MOV.L @m+,n")
				| 7: Write("NOT   m,n")
				| 8: Write("SWAP.B m,n")
				| 9: Write("SWAP.W m,n")
				| 10: Write("NEGC  m,n")
				| 11: Write("NEG   m,n")
				| 12: Write("EXTU.B m,n")
				| 13: Write("EXTU.W m,n")
				| 14: Write("EXTS.B m,n")
				| 15: Write("EXTS.W m,n")
				END
			| 7: Write("ADD   #b,n")
			| 8: 
				CASE op DIV 256 MOD 16 OF
				| 0: Write("MOV.B R0,@(s,m)")
				| 1: Write("MOV.W R0,@(d,m)")
				| 4: Write("MOV.B @(s,m),R0")
				| 5: Write("MOV.W @(d,m),R0")
				| 8: Write("CMP/EQ #b,R0")
				| 9: Write("BT    r")
				| 11: Write("BF    r")
				| 13: Write("BT/S  r")
				| 15: Write("BF/S  r")
				| 2, 3, 6, 7, 10, 12, 14: WriteUndef
				END
			| 9: Write("MOV.W @(x),n")
			| 10: Write("BRA   o")
			| 11: Write("BSR   o")
			| 12: 
				CASE op DIV 256 MOD 16 OF
				| 0: Write("MOV.B R0,@(u,GBR)")
				| 1: Write("MOV.W R0,@(v,GBR)")
				| 2: Write("MOV.L R0,@(w,GBR)")
				| 3: Write("TRAPA #u")
				| 4: Write("MOV.B @(u,GBR),R0")
				| 5: Write("MOV.W @(v,GBR),R0")
				| 6: Write("MOV.L @(w,GBR),R0")
				| 7: Write("MOVA  @(z),R0")
				| 8: Write("TST   u,R0")
				| 9: Write("AND   u,R0")
				| 10: Write("XOR   u,R0")
				| 11: Write("OR    u,R0")
				| 12: Write("TST   u,@(R0,GBR)")
				| 13: Write("AND   u,@(R0,GBR)")
				| 14: Write("XOR   u,@(R0,GBR)")
				| 15: Write("OR    u,@(R0,GBR)")
				END
			| 13: Write("MOV.L @(y),n")
			| 14: Write("MOV   #b,n")
			| 15: WriteUndef
			END
		END
	END DecodeLine;

END DevDecSH3.
