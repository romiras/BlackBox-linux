MODULE DevDec68k;	(*mf  25.7.91 / tb 022992*)

	(* 09.09.99	DevDec486	fixed WriteInt for MAX(INTEGER) *)

	IMPORT DevDecBase, SYSTEM;

	VAR
		hDigs: ARRAY 17 OF SHORTCHAR;	(* Hexadecimal Digits *)
		cDirs: ARRAY 3 OF SHORTCHAR;	(* Shift and Rotate Directions *)
		cScale: ARRAY 5 OF SHORTCHAR;	(* Scale Factors *)
		cSizes: ARRAY 8 OF SHORTCHAR;	(* Operation Sizes *)
		cSROps: ARRAY 13 OF SHORTCHAR;	(* Shift and Rotate Operations *)
		cBOps: ARRAY 13 OF SHORTCHAR;	(* Bit Operations *)
		cBFOps: ARRAY 33 OF SHORTCHAR;	(* Bit Field Operations *)
		cCodes: ARRAY 33 OF SHORTCHAR;	(* Conditions *)
		cCRegs: ARRAY 53 OF SHORTCHAR;	(* Control Registers *)

		mnem: ARRAY 1200 OF SHORTCHAR;	(* String Table *)
		mask, code, div, mod, off: ARRAY 80 OF SHORTINT;	(* Code Table *)
		index: ARRAY 17 OF SHORTINT;	(* Offset into Code Table, indexed by first Byte of Instruction *)
		maxMnem, maxOpcd: SHORTINT;	(* Pointer to Next Free Position in Table *)

		w, xw, xw2: INTEGER;	(* Command Word and Extensions *)
		dw: INTEGER; dl: INTEGER;	(* Displacement Operands *)
		pcmode: BOOLEAN;	(* Differentiates between Ax and PC Indirect with Index Modes *)
		size: SHORTINT;	(* Size of Instruction *)
		okay: BOOLEAN;	(* ~ Error *)

		Refs: INTEGER;
		CEnt: ARRAY 256 OF INTEGER;

	procname, lname, varname: ARRAY 32 OF SHORTCHAR;  lch: SHORTCHAR;
	procadr, ladr, constSize: INTEGER;

	
	PROCEDURE DecodeLine* (VAR pc: INTEGER; Put: DevDecBase.PutProc; Get: DevDecBase.GetProc;  mode: INTEGER);
	VAR 
		pcbuf: ARRAY 256 OF BYTE;
		pcpos: INTEGER;

		PROCEDURE WriteHexR(x, n: INTEGER);
		BEGIN
			IF n > 1 THEN WriteHexR(x DIV 16, n-1) END;
			x := x MOD 16;
			IF x <= 9 THEN Put(SHORT(CHR(x + ORD("0"))))
			ELSE Put(SHORT(CHR(x -10 + ORD("A"))))
			END
		END WriteHexR;
		
		PROCEDURE WriteString (s: ARRAY OF SHORTCHAR);
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
		
		PROCEDURE WriteChar(ch: SHORTCHAR);
		BEGIN
			IF (ch >= " ") & (ch < 7FX) THEN Put(ch)
			ELSE Put(".")
			END
		END WriteChar;
		
		PROCEDURE WriteWord(x: INTEGER);
		BEGIN
			IF (x > 32767) OR (x < -32768) THEN
				Put("$"); WriteHexR(x, 8)
			ELSE
				WriteInt(x)
			END
		END WriteWord;
		
		PROCEDURE WriteUndef;
		BEGIN
			WriteString("*****")
		END WriteUndef;
		
		PROCEDURE WriteLn;
		BEGIN
			Put(0DX)
		END WriteLn;
		
		PROCEDURE WriteTab;
		BEGIN
			Put(09X)
		END WriteTab;
		
	PROCEDURE WriteHex(x: INTEGER);
	BEGIN 
		Put("$"); WriteHexR(x, 8);
	END WriteHex;
	
		PROCEDURE Hex(x: SHORTCHAR);
		BEGIN	WriteHexR(ORD(x),2)
		END Hex;

		PROCEDURE HexWord(w: INTEGER);
		BEGIN WriteChar(" "); WriteChar(" "); Hex(SHORT(CHR(w DIV 256))); Hex(SHORT(CHR(w MOD 256)));
		END HexWord;

		PROCEDURE GetWord(VAR w: INTEGER);
			VAR b: BYTE;  a: INTEGER;
		BEGIN
			Get(pcbuf[pcpos]); Get(pcbuf[pcpos+1]); 
			a:=pcpos; INC(pcpos,2);INC(pc, 2); 
			w:= pcbuf[a] * 256 + pcbuf[a+1] MOD 256			
		END GetWord;
	
		PROCEDURE GetLong (VAR i: INTEGER);
			VAR b: BYTE; a: INTEGER;
		BEGIN
			Get(pcbuf[pcpos]); Get(pcbuf[pcpos+1]); Get(pcbuf[pcpos+2]); Get(pcbuf[pcpos+3]); 
			a:=pcpos; INC(pcpos,4);INC(pc, 4); 
			i:= ((pcbuf[a] * 256 + pcbuf[a+1] MOD 256) * 256 + pcbuf[a+2] MOD 256) * 256 + pcbuf[a+3] MOD 256			
		END GetLong;

		PROCEDURE SyntaxError;
		BEGIN	WriteLn; WriteString("Syntax Error"); WriteLn; okay:=FALSE
		END SyntaxError;
		
		PROCEDURE WByte(b: SHORTINT);	(* Write Byte *)
		BEGIN	IF	b<128	THEN	WriteInt(b)	ELSE	WriteInt(b-256)	END
		END WByte;
	
		PROCEDURE WGReg(xw: INTEGER);	(* Write Register Indicator, General Register *)
		BEGIN	IF	xw<0	THEN	WriteChar("A")	ELSE	WriteChar("D")	END; 
			WriteInt((xw DIV 4096) MOD 16)
		END WGReg;
		
		PROCEDURE WCR;	(* Write Control Register *)
			VAR ix: SHORTINT;
		BEGIN	ix:=SHORT((((xw DIV 2048) MOD 2)*2+w MOD 8)*4);
			WriteChar(cCRegs[ix]); INC(ix); WriteChar(cCRegs[ix]); INC(ix); WriteChar(cCRegs[ix]); INC(ix);
			IF	cCRegs[ix] # "@"	THEN	WriteChar(cCRegs[ix])	END
		END WCR;
	
		PROCEDURE WIRI;	(* Write Index Register Indicator *)
		BEGIN	IF	xw<0	THEN	WriteChar("A")	ELSE	WriteChar("D")	END; WriteInt((xw DIV 4096) MOD 8);
			IF	ODD(xw DIV 2048)	THEN	WriteString(".L*")	ELSE	WriteString(".W*")	END;
			WriteChar(cScale[(xw DIV 512) MOD 4])
		END WIRI;
		
		PROCEDURE WBDR;	(* Write Base Displacement and Register *)
		BEGIN
			CASE (xw DIV 16) MOD 4 OF
			| 0:	(*Reserved*)	SyntaxError
			| 1:	(*Null Displacement*)	WriteString("null, ")
			| 2:	(*Word Displacement*)	GetWord(dw); WriteInt(dw); WriteString(", ")
			| 3:	(*Long Displacement*)	GetLong(dl); WriteInt(dl); WriteString(", ")
			END;
			IF	ODD(xw DIV 128)	THEN	(* Base Register Suppress *)
				IF	pcmode	THEN	WriteString("ZPC")	ELSE	WriteString("nobasreg")	END
			ELSIF	pcmode	THEN	WriteString("PC")	ELSE	WriteChar("A"); WriteInt(w MOD 8)
			END
		END WBDR;
	
		PROCEDURE WOD;	(* Write Outer Displacement *)
		BEGIN
			CASE xw MOD 4 OF
			| 1:	SyntaxError
			| 2:	WriteString("null")
			| 3:	GetWord(dw); WriteInt(dw)
			| 4:	GetLong(dl); WriteInt(dl)
			END
		END WOD;
	
		PROCEDURE WEAMI;	(* Write Effective Address, Memory Indirect Addressing Modes *)
		BEGIN
			IF	ODD(xw DIV 64)	THEN	(* Suppress Index Operand *)
				CASE xw MOD 8 OF
				| 0:		(* No Memory Indirection *)	WriteChar("("); WBDR; WriteChar(")")
				| 1..3:	(* Memory Indirect *)	WriteString("(["); WBDR; WriteString("], "); WOD; WriteChar(")")
				| 4..7:	(* Reserved *)	SyntaxError
				END
			ELSE	(* Evaluate Index Operand *)
				CASE xw MOD 8 OF
				| 0:		(* No Memory Indirection *)	WriteChar("("); WBDR; WriteString(", "); WIRI; WriteChar(")")
				| 1..3:	(* Indirect Pre-Indexed *)	WriteString("(["); WBDR; WriteString(", "); WIRI; WriteString("], "); WOD; WriteChar(")")
				| 4:		(*Reserved*)	SyntaxError
				| 5..7:	(* Indirect Post-Indexed *)	WriteString("(["); WBDR; WriteString("], "); WIRI; WriteString(", "); WOD; WriteChar(")")
				END
			END
		END WEAMI;
	
		PROCEDURE WEA(mode, reg, size: SHORTINT);	(* Write Effective Address *)
			VAR fixcode: INTEGER;
		BEGIN
			CASE mode OF
			| 0:	(*Data Register Direct*)	WriteChar("D"); WriteInt(reg)
			| 1:	(*Address Register Direct*)	WriteChar("A"); WriteInt(reg)
			| 2:	(*Address Register Indirect*)	WriteString("(A"); WriteInt(reg); WriteChar(")")
			| 3:	(*Address Register Indirect with Postincrement*)	WriteString("(A"); WriteInt(reg); WriteString(")+")
			| 4:	(*Address Register Indirect with Predecrement*)	WriteString("-(A"); WriteInt(reg); WriteChar(")")
			| 5:	(* Address Register Indirect with Displacement *)	WriteChar("("); GetWord(dw); WriteInt(dw); WriteString(", A"); WriteInt(reg); WriteChar(")")
			| 6:	(* Register Indirect with Index Modes *)	GetWord(xw);
						IF	~ODD(xw DIV 256)	THEN	(* Register Indirect with Index (8-bit Displacement) *)
							WriteChar("("); WByte(SHORT(xw MOD 256)); WriteString(", A");
							WriteInt(reg); WriteString(", "); WIRI; WriteChar(")")
						ELSE	pcmode:=FALSE; WEAMI
						END
			| 7:
				CASE reg OF
				| 0:	(*Absolute Short*)	WriteChar("("); GetWord(dw); WriteHex(dw); WriteString(").W")
				| 1:	(*Absolute Long*)	WriteChar("("); GetLong(dl); WriteHex(dl); WriteString(").L")
							(* GetLong(dl); fixcode:=dl MOD 10000H;
							IF	fixcode<4000H	THEN	WriteString("Const #"); WriteInt(fixcode)
							ELSIF	fixcode<8000H	THEN	WriteString("Proc #"); WriteInt(fixcode MOD 4000H)
							ELSE	WriteString("Var #"); WriteInt((fixcode MOD 8000H) DIV 100H);
								IF	(fixcode MOD 100H) > 0	THEN	WriteString(", off="); WriteInt(fixcode MOD 100H)	END
							END; *)
							(* WriteString("(f="); WriteHex(W,pc + dl DIV 10000H - 4); WriteChar(")") *)
				| 2:	(*Program Counter Indirect with Displacement*)	WriteChar("("); GetWord(xw); WriteInt(xw); WriteString(", PC)")
				| 3:	(*Program Counter and Memory Indirect with Index*)	GetWord(xw);
							IF	~ODD(xw DIV 256)	THEN	(*Program Counter Indirect with Index (8-bit Displacement)*)
								WriteChar("("); WByte(SHORT(xw MOD 256)); WriteString(", PC, "); WIRI; WriteChar(")")
							ELSE	pcmode:=TRUE;  WEAMI
							END
				| 4:	(*Immediate Data*)	WriteChar("#");
							CASE size OF
							| 0: GetWord(dw); WriteHex(dw MOD 256)
							| 1: GetWord(dw); WriteHex(dw)
							| 2: GetLong(dl); WriteHex(dl)
							| 3: WriteString("***Bad Effective Address")
							| 4: GetLong(dl); WriteHex(dl)
							| 5: GetLong(dl); WriteHex(dl); GetLong(dl); WriteHex(dl)
							END
				| 5..7:	(*Reserved*)	SyntaxError
				ELSE	WriteString("***Bad Effective Address")
				END
			END
		END WEA;


		PROCEDURE DSize(mode: SHORTCHAR);	(* Decode Size of Instruction *)
		BEGIN
			CASE mode OF
			| "0":	size:=0
			| "1":	size:=1
			| "2":	size:=2
			| "3":	size:=SHORT((w DIV 64) MOD 4)
			| "4":	size:=SHORT((w DIV 512) MOD 4)
			| "5":	IF	ODD(w)	THEN	size:=2	ELSE	size:=1	END
			| "6":	IF	ODD(w DIV 64)	THEN	size:=2	ELSE	size:=1	END
			| "7":	IF	ODD(w DIV 256)	THEN	size:=2	ELSE	size:=1	END
			| "8":
					CASE	(xw DIV 400H) MOD 8H	OF
					| 0:	size:=2
					| 1:	size:=4
					| 2:	size:=6
					| 3:	size:=3
					| 4:	size:=1
					| 5:	size:=5
					| 6:	size:=0
					ELSE
					END
			END
		END DSize;
	
		PROCEDURE DOpd(ch: SHORTCHAR);	(* Decode Operand *)
			VAR base: INTEGER;
		BEGIN
			CASE ch OF
			| "a":	WEA(7, 4, 0)
			| "b":	WEA(7, 4, 1)
			| "c":	WEA(7, 4, 2)
			| "d":	WEA(7, 4, size)
			| "e":	WriteString("#1")
			| "f":	dw:=SHORT((w DIV 512) MOD 8); IF	dw=0	THEN	WriteString("#8")	ELSE	WriteChar("#"); WriteInt(dw)	END
			| "g":	WriteChar("#"); WByte(SHORT(w MOD 256))
			| "h":	WriteChar("#"); WriteInt(w MOD 8)
			| "i":	WriteChar("#"); WriteInt(w MOD 16)
			| "j":	dl:=(w + 128) MOD 256 - 128;  base:=pc;
						IF	dl=0	THEN	GetWord(dw); dl:=dw	ELSIF	dl=-1	THEN	GetLong(dl)	END;
						(* WriteInt(dl); WriteString("   -> "); *) WriteHex(dl+base);
			| "k":	GetWord(dw); WriteInt(dw)
			| "l":	GetLong(dl); WriteInt(dl)
			| "m":	WEA(SHORT((w DIV 8) MOD 8), SHORT(w MOD 8), size)
			| "n":	WEA(SHORT((w DIV 64) MOD 8), SHORT((w DIV 512) MOD 8), size)
			| "o":	WEA(0, SHORT((w DIV 512) MOD 8), 0)
			| "p":	WEA(0, SHORT(w MOD 8), 0)
			| "q":	WEA(1, SHORT((w DIV 512) MOD 8), 0)
			| "r":	WEA(1, SHORT(w MOD 8), 0)
			| "s":	WEA(4, SHORT((w DIV 512) MOD 8), 0)
			| "t":	WEA(4, SHORT(w MOD 8), 0)
			| "u":	WEA(3, SHORT((w DIV 512) MOD 8), 0)
			| "v":	WEA(3, SHORT(w MOD 8), 0)
			| "w":	WEA(5, SHORT(w MOD 8), 0)
			| "x":	WriteString("CCR")
			| "y":	WriteString("SR")
			| "z":	WriteString("USP")
			| "A":	WCR
			| "B":	WGReg(xw)
			| "C":	WGReg(xw);
						IF	ODD(xw DIV 400H)	THEN	WriteString(":D"); WriteInt(xw MOD 8)	END
			| "D":	WEA(SHORT((w DIV 8) MOD 8), SHORT(w MOD 8), size); WriteString(" {");
						IF	ODD(xw DIV 2048)	THEN	WriteChar("D"); WriteInt((xw DIV 64) MOD 8)
						ELSE	WriteChar("#"); WriteInt((xw DIV 64) MOD 32)	END;
						WriteString(": ");
						IF	ODD(xw DIV 32)	THEN	WriteChar("D"); WriteInt(xw MOD 8)
						ELSE	WriteChar("#"); WriteInt(xw MOD 32)	END;
			| "E":	GetWord(xw); WEA(0, SHORT(xw MOD 8), 0); WriteString(", "); WEA(0, SHORT((xw DIV 64) MOD 8), 0)
			| "F":	GetWord(xw); GetWord(xw2);
						WEA(0, SHORT(xw MOD 8), 0); WriteChar(":"); WEA(0, SHORT(xw2 MOD 8), 0); WriteString(", ");
						WEA(0, SHORT((xw DIV 64) MOD 8), 0); WriteChar(":"); WEA(0, SHORT((xw2 DIV 64) MOD 8), 0);
						WriteString(", ("); WGReg(xw); WriteString("):(");
						WGReg(xw2); WriteChar(")")
			| "G":	GetWord(xw); WriteChar("<"); WriteInt(xw); WriteChar(">")
			| "H":	GetWord(xw); WriteChar("<"); WriteInt(xw); WriteChar(">")
			| "I":	GetWord(xw);
						IF	ODD(xw DIV 2048)	THEN	WGReg(xw); WriteString(", "); WEA(SHORT((w DIV 8) MOD 8), SHORT(w MOD 8), size)
						ELSE	WEA(SHORT((w DIV 8) MOD 8), SHORT(w MOD 8), size); WriteString(", "); WGReg(xw);
						END
			| "J":	WriteChar("%"); WriteHex(w)
			| "K":	WEA(7, 4, 1); 
			| "L": 
			| "Z":	WriteChar("%"); WriteHex(xw)
			ELSE	WriteString("?????"); WriteChar(ch)
			END
		END DOpd;
	
	
		PROCEDURE WMnCmpl(ch: SHORTCHAR);	(* Complete Writing of Mnemonic *)
			VAR ix,  i: SHORTINT; n, lb: INTEGER;
		BEGIN
			CASE ch OF
			| "&":	(* Bit Operation *)	ix:=SHORT(((w DIV 64) MOD 4)*3);
							WriteChar(cBOps[ix]); INC(ix); WriteChar(cBOps[ix]); INC(ix); WriteChar(cBOps[ix])
			| "=":	(* Condition *)	ix:=SHORT(((w DIV 256) MOD 16)*2);
							WriteChar(cCodes[ix]); INC(ix); IF	cCodes[ix] # "@"	THEN	WriteChar(cCodes[ix])	END
			| ">":	(* Shift/Rotate Register Operation *)	ix:=SHORT(((w DIV 8) MOD 4)*3);
							WriteChar(cSROps[ix]); INC(ix); WriteChar(cSROps[ix]); INC(ix);
							IF	cSROps[ix] # "@"	THEN	WriteChar(cSROps[ix])	END; WriteChar(cDirs[(w DIV 256) MOD 2])
			| "<":	(* Shift/Rotate Memory Operation *)	ix:=SHORT(((w DIV 512) MOD 4)*3);
							WriteChar(cSROps[ix]); INC(ix); WriteChar(cSROps[ix]); INC(ix);
							IF	cSROps[ix] # "@"	THEN	WriteChar(cSROps[ix])	END; WriteChar(cDirs[(w DIV 256) MOD 2])
			| "~":	(* Bit Field Operation *)	GetWord(xw); ix:=SHORT(((w DIV 256) MOD 8)*4);
							WriteChar(cBFOps[ix]); INC(ix); WriteChar(cBFOps[ix]); INC(ix);
							WriteChar(cBFOps[ix]); INC(ix); IF	cBFOps[ix] # "@"	THEN	WriteChar(cBFOps[ix])	END
			| "*":	(* MULS / MULU *)	GetWord(xw); size:=2;
							IF	ODD(xw DIV 2048)	THEN	WriteString("S.L")	ELSE	WriteString("U.L")	END
			| "!":	(* CMP2/CHK2 *)	GetWord(xw);
							IF	ODD(xw DIV 2048)	THEN	WriteString("CHK2")	ELSE	WriteString("CMP2")	END
			| "C":	(* MOVEC Requires Extension Word Preload *)	WriteString("MOVEC"); GetWord(xw)
			| "P":	(* cpXXX *)	WriteString("cp["); WriteInt((w DIV 512) MOD 8); WriteChar("]"); 
			| "F":	(* fpcp *)	WriteChar("F"); GetWord(xw)
			| "@":	(* A-TRAP *)	WriteString("WORD"); WriteTab; WriteHex(w MOD 10000H)
			| "K":	(* Coprocessor Condition *)	WriteString("{cpcond="); WriteHex((xw MOD 64)); WriteChar("}")
			| "k":	(* FBcc *)	GetWord(xw); WriteString("{cpcond="); WriteHex((w MOD 64)); WriteChar("}");
							WriteTab; WriteInt(xw)
			| "Y":	(* Case Table Decoder Hint *)	GetWord(n); i:=0;
							WHILE	i<n	DO	WriteString("DC.W"); WriteTab;
								GetWord(lb); WriteInt(lb); WriteLn; INC(i)
							END
			END
		END WMnCmpl;
	
	
		PROCEDURE DOpc;	(* Decode Opcode *)
			VAR  max, j, s: SHORTINT; ch: SHORTCHAR; pos, i: INTEGER; 
				ch0,ch1: SHORTCHAR; word: INTEGER;
	
		BEGIN	i:=index[(w DIV 4096) MOD 16];  max:=index[(w DIV 4096) MOD 16+1];	(* HexWord(w); WriteTab; *)
			LOOP
				IF	i >= max	THEN WriteString("***Illegal Instruction"); (* WriteLn; *) WriteTab; WriteTab; HexWord(w); RETURN
				ELSIF SYSTEM.VAL(SET, w)*SYSTEM.VAL(SET, LONG(mask[i]))=SYSTEM.VAL(SET, LONG(code[i]))	THEN
					i:=off[i]+((w DIV div[i]) MOD mod[i])*9; ch:=mnem[i]; INC(i);
					IF	ch=" "	THEN	WriteString("undefined");
					ELSE
						pcpos:=0; 
						WHILE	(ch>="A")&(ch<="Z")	DO	WriteChar(ch); ch:=mnem[i]; INC(i)	END;
						IF	ch="\"	THEN	ch:=mnem[i]; INC(i); WMnCmpl(ch); ch:=mnem[i]; INC(i);
							WHILE	(ch>="A")&(ch<="Z")	DO	WriteChar(ch); ch:=mnem[i]; INC(i)	END;
						END;
						IF	ch="."	THEN	ch:=mnem[i]; INC(i); DSize(ch); WriteChar("."); WriteChar(cSizes[size]); ch:=mnem[i]; INC(i)	END;
						WriteTab;
						IF	ch # " "	THEN	DOpd(ch); ch:=mnem[i]; INC(i);
							IF	ch # " "	THEN	WriteString(", "); DOpd(ch)	END
						END;
						WriteTab; HexWord(w);
						IF  pcpos>0  THEN (* WriteLn; WriteString("         "); *)  pos:=0;
							WHILE pcpos > pos DO 
								word:= pcbuf[pos] * 256 + pcbuf[pos+1] MOD 256; INC(pos,2);
								HexWord(word); 
							END;
							pcpos:=0;
						END;
					END;
					RETURN
				END;
				INC(i)
			END
		END DOpc;

	BEGIN	pcpos:=0;
		WriteHexR(pc, 8); WriteString(":     ");
				GetWord(w); DOpc; (* WriteLn *)
	END DecodeLine;

	PROCEDURE I(a, b: INTEGER; x, y: SHORTINT; s: ARRAY OF SHORTCHAR);
		VAR i: SHORTINT;
	BEGIN	mask[maxOpcd]:=SHORT(a); code[maxOpcd]:=SHORT(b); div[maxOpcd]:=x; mod[maxOpcd]:=y; off[maxOpcd]:=maxMnem;
		i:=0;
		WHILE	i<LEN(s)	DO	mnem[maxMnem]:=s[i]; INC(maxMnem); INC(i)	END;	INC(maxOpcd)
	END I;

	PROCEDURE Mark(inx: BYTE);
	BEGIN	index[inx]:=maxOpcd
	END Mark;


BEGIN	
	maxOpcd:=0; maxMnem:=0;
	hDigs:="0123456789ABCDEF"; cDirs:="RL"; cScale :="1248"; cSizes:="BWLUSDX"; cSROps:="AS@LS@ROXRO@"; cBOps:="TSTCHGCLRSET";
	cCRegs:="SFC@DFC@CACR@@@@@@@@@@@@@@@@@@@@USP@VBR@CAARMSP@ISP@";
	cBFOps:="TST@EXTUCHG@EXTSCLR@FF0@SET@INS@"; cCodes:="T@F@HILSCCCSNEEQVCVSPLMIGELTGTLE";

	Mark(0);
	I(0FFBFH, 0003CH,   64,  2, "ORIax    ORIby");                       (* 0000 0000 0-11 1100  ORI,  0=to CCR, 1=to SR *)
	I(0FFBFH, 0023CH,   64,  2, "ANDIax   ANDIby");                      (* 0000 0010 0-11 1100  ANDI, 0=to CCR, 1=to SR *)
	I(0FFBFH, 00A3CH,   64,  2, "EORIax   EORIby");                      (* 0000 1010 0-11 1100  EORI, 0=to CCR, 1=to SR *)
	I(0FFF0H, 006C0H,    8,  2, "RTMp     RTMr ");                       (* 0000 0110 1100 rrrr  RTM         !68020-only! *)
	I(0FFC0H, 006C0H,    1,  1, "CALLMam");                              (* 0000 0110 11ee eeee  CALLM       !68020-only! *)
	I(0F9C0H, 000C0H,    1,  1, "\!.4mB");                               (* 0000 0ss0 11ee eeee rrrr 0000 0000 0000  CMP2 *)
	                                                                     (* 0000 0ss0 11ee eeee rrrr 1000 0000 0000  CHK2 *)
	I(0F900H, 00000H,  512,  4, "ORI.3dm  ANDI.3dm SUBI.3dm ADDI.3dm");  (* 0000 0000 ssee eeee  ORI *)
	                                                                     (* 0000 0010 ssee eeee  ANDI *)
	                                                                     (* 0000 0100 ssee eeee  SUBI *)
	                                                                     (* 0000 0110 ssee eeee  ADDI *)
	I(0F9FFH, 008FCH,    1,  1, "CASII.4F ");                            (* 0000 1ss0 1111 1100 rrrr 000d dd00 0ddd  CAS2 *)
	                                                                     (*                     rrrr 000d dd00 0ddd  CAS2 *)
	                                                                     (* CAS2:                  !68030-only:size#byte! *)
	I(0FF00H, 00800H,    1,  1, "B\&am");                                (* 0000 1000 --ee eeee  Bit static *)
	I(0F9C0H, 008C0H,    1,  1, "CAS.4Em");                              (* 0000 1ss0 11ee eeee 0000 000d dd00 0ddd  CAS *)
	I(0F900H, 00800H,  512,  4, "         EORI.3dm CMPI.3dm MOVES.3I "); (* 0000 1010 ssee eeee  EORI *)

	                                                                     (* 0000 1100 ssee eeee  CMPI *)
	                                                                     (* 0000 1110 ssee eeee rrrr -000 0000 0000  MOVES 0=Ea->R 1=R->Ea *)
	I(0F138H, 00108H,  128,  2, "MOVEP.6woMOVEP.6ow");                   (* 0000 ddd1 -w00 1aaa  MOVEP, 0=M->R, 1=R->M *)
	I(0F100H, 00100H,    1,  1, "B\&om");                                (* 0000 ddd1 --ee eeee  Bit dynamic *)

	Mark(1);
	I(0F000H, 01000H,    1,  1, "MOVE.0mn");                             (* 0001 ffff ffee eeee  MOVE.B  *)
	Mark(2);
	I(0F1C0H, 02040H,    1,  1, "MOVEA.2mq");                            (* 0010 aaa0 01ee eeee  MOVEA.L *)
	I(0F000H, 02000H,    1,  1, "MOVE.2mn");                             (* 0010 ffff ffee eeee  MOVE.L  *)
	Mark(3);
	I(0F1C0H, 03040H,    1,  1, "MOVEA.1mq");                            (* 0011 aaa0 01ee eeee  MOVEA.W *)
	I(0F000H, 03000H,    1,  1, "MOVE.1mn");                             (* 0011 ffff ffee eeee  MOVE.W  *)

	Mark(4);
	I(0FFFFH, 04AFCH,    1,  1, "ILLEGAL ");                             (* 0100 1010 1111 1100  ILLEGAL *)
	I(0FFF8H, 04E70H,    1,  8, "RESET    NOP      STOP     RTE      RTD.1K   RTS.1L   TRAPV    RTR ");
	                                                                     (* 0100 1110 0111 0000  RESET *)
	                                                                     (* 0100 1110 0111 0001  NOP   *)
	                                                                     (* 0100 1110 0111 0010  STOP  *)
	                                                                     (* 0100 1110 0111 0011  RTE   *)
	                                                                     (* 0100 1110 0111 0100  RTD   *)
	                                                                     (* 0100 1110 0111 0101  RTS   *)
	                                                                     (* 0100 1110 0111 0110  TRAPV *)
	                                                                     (* 0100 1110 0111 0111  RTR   *)
	I(0FFFEH, 04E7AH,    1,  2, "\C.2AB   \C.2BA");                      (* 0100 1110 0111 1010 rrrr kkkk kkkk kkkk  MOVEC 0=K->R 1=R->K *)
	I(0FFF8H, 049C0H,    1,  1, "EXTB.2p ");                             (* 0100 1001 1100 0ddd  EXTB *)
	I(0FFF8H, 04808H,    1,  1, "LINK.2rc");                             (* 0100 1000 0000 1aaa  LINK Long *)
	I(0FFF0H, 04840H,    8,  2, "SWAPp    BKPTh ");                      (* 0100 1000 0100 0ddd  SWAP *)
	                                                                     (* 0100 1000 0100 1vvv  BKPT *)
	I(0FFB8H, 04880H,    1,  1, "EXT.6p ");                              (* 0100 1000 1w00 0ddd  EXT *)
	I(0FFE0H, 04E40H,    8,  4, "TRAPi    TRAPi    LINK.1rb UNLKr ");    (* 0100 1110 0100 vvvv  TRAP *)
	                                                                     (* 0100 1110 0101 0aaa  LINK Word *)
	                                                                     (* 0100 1110 0101 1aaa  UNLK *)
	I(0FFF0H, 04E60H,    8,  2, "MOVErz   MOVEzr ");                     (* 0100 1110 0110 -aaa  MOVE 0=to USP 1=from USP *)
	I(0F9C0H, 040C0H,  512,  4, "MOVEym   MOVExm   MOVEmx   MOVEmy");    (* 0100 0000 11ee eeee  MOVE from SR *)
	                                                                     (* 0100 0010 11ee eeee  MOVE from CCR *)
	                                                                     (* 0100 0100 11ee eeee  MOVE to CCR *)
	                                                                     (* 0100 0110 11ee eeee  MOVE to SR *)
	I(0FF00H, 04800H,   64,  4, "NBCDm    PEAm     MOVEM.1GmMOVEM.2Gm"); (* 0100 1000 00ee eeee  NBCD *)
	                                                                     (* 0100 1000 01ee eeee  PEA *)
	                                                                     (* 0100 1000 1wee eeee  MOVEM reg to ea *)
	I(0FFC0H, 04AC0H,    1,  1, "TASm ");                                (* 0100 1010 11ee eeee  TAS *)
	I(0FF00H, 04C00H,   64,  4, "MUL\*mC  DIV\*mC  MOVEM.1mHMOVEM.2mH"); (* 0100 1100 00ee eeee 0ddd -u00 0000 0ddd  MUL, 0=U, 1=S *)
	                                                                     (* 0100 1100 01ee eeee 0ddd -u00 0000 0ddd  DIV, 0=U, 1=S *)
	                                                                     (* 0100 1100 1wee eeee  MOVEM ea to reg *)
	I(0FF80H, 04E80H,   64,  2, "JSRm     JMPm ");                       (* 0100 1110 10ee eeee  JSR *)
	                                                                     (* 0100 1110 11ee eeee  JMP *)
	I(0F900H, 04000H,  512,  4, "NEGX.3m  CLR.3m   NEG.3m   NOT.3m ");   (* 0100 0000 ssee eeee  NEGX *)
	                                                                     (* 0100 0010 ssee eeee  CLR *)
	                                                                     (* 0100 0100 ssee eeee  NEG *)
	                                                                     (* 0100 0110 ssee eeee  NOT *)
	I(0FF00H, 04A00H,    1,  1, "TST.3m ");                              (* 0100 1010 ssee eeee  TST *)
	I(0F140H, 04100H,  128,  2, "CHK.2mo  CHK.1mo ");                    (* 0100 ddd1 -0ee eeee  CHK, 0=L 1=W *)
	I(0F1C0H, 041C0H,    1,  1, "LEAmq ");                               (* 0100 aaa1 11ee eeee  LEA *)

	Mark(5);
	I(0F0FFH, 050FCH,    1,  1, "TRAP\=   ");                            (* 0101 cccc 1111 1100  TRAPcc No Operand *)
	I(0F0FEH, 050FAH,    1,  1, "TRAP\=.5d ");                           (* 0101 cccc 1111 101w  TRAPcc *)
	I(0F0F8H, 050C8H,    1,  1, "DB\=pk   ");                            (* 0101 cccc 1100 1ddd  DBcc *)
	I(0F0C0H, 050C0H,    1,  1, "S\=m     ");                            (* 0101 cccc 11ee eeee  Scc *)
	I(0F000H, 05000H,  256,  2, "ADDQ.3fm SUBQ.3fm");                    (* 0101 iii0 ssee eeee  ADDQ *)
	                                                                     (* 0101 iii1 ssee eeee  SUBQ *)

	Mark(6);
	I(0FE00H, 06000H,  256,  2, "BRAj     BSRj ");                       (* 0110 0000 bbbb bbbb  BRA *)
	                                                                     (* 0110 0001 bbbb bbbb  BSR *)
	I(0F000H, 06000H,    1,  1, "B\=j ");                                (* 0110 cccc bbbb bbbb  Bcc *)

	Mark(7);
	I(0F100H, 07000H,    1,  1, "MOVEQgo");                              (* 0111 ddd0 jjjj jjjj  MOVEQ *)

	Mark(8);
	I(0F1F0H, 08100H,    8,  2, "SBCDpo   SBCDts");                      (* 1000 ggg1 0000 -ggg  SBCD, 0=d.d, 1=-(a).-(a) *)
	I(0F1F0H, 08140H,    8,  2, "PACKpo   SBCDts");                      (* 1000 ggg1 0100 -ggg  PACK, 0=d.d, 1=-(a).-(a) *)
	I(0F1F0H, 08180H,    8,  2, "UNPKpo   UNPKts");                      (* 1000 ggg1 1000 -ggg  UNPK, 0=d.d, 1=-(a).-(a) *)
	I(0F0C0H, 080C0H,  256,  2, "DIVU.1mo DIVS.1mo");                    (* 1000 ddd0 11ee eeee  DIVU *)
	                                                                     (* 1000 ddd1 11ee eeee  DIVS *)
	I(0F000H, 08000H,  256,  2, "OR.3mo   OR.3om");                      (* 1000 ddd- ssee eeee  OR, Src.Dest 0=Ea.R, 1=R.Ea *)

	Mark(9);
	I(0F0C0H, 090C0H,    1,  1, "SUBA.7mq");                             (* 1001 aaaw 11ee eeee  SUBA *)
	I(0F130H, 09100H,    8,  2, "SUBXpo   SUBXts");                      (* 1001 ggg1 ss00 -ggg  SUBX, 0=d.d, 1=-(a).-(a) *)
	I(0F000H, 09000H,  256,  2, "SUB.3mo  SUB.3om");                     (* 1001 ddd- ssee eeee  SUB, Src.Dest 0=Ea.R, 1=R.Ea *)

	Mark(10);
	I(0F000H, 0A000H,    1,  1, "\@ ");                                  (* 1010 A-Line Emulator *)

	Mark(11);
	I(0F0C0H, 0B0C0H,    1,  1, "CMPA.7mq");                             (* 1011 aaaw 11ee eeee  CMPA *)
	I(0F138H, 0B108H,    1,  1, "CMPM.3vu");                             (* 1011 aaa1 ss00 1aaa  CMPM *)
	I(0F000H, 0B000H,  256,  2, "CMP.3mo  EOR.3om");                     (* 1011 ddd0 ssee eeee  CMP *)
	                                                                     (* 1011 ddd1 ssee eeee  EOR *)
	Mark(12);
	I(0F1F0H, 0C100H,    8,  2, "ABCDpo   ABCDts");                      (* 1100 ggg1 0000 -ggg  ABCD, 0=d.d, 1=-(a).-(a) *)
	I(0F1F0H, 0C140H,    8,  2, "EXGop    EXGqr");                       (* 1100 ddd1 0100 0ddd  EXG dregs *)
	                                                                     (* 1100 aaa1 0100 1aaa  EXG aregs *)
	I(0F1F8H, 0C188H,    1,  1, "EXGor");                                (* 1100 ddd1 1000 1aaa  EXG dreg areg *)

	I(0F0C0H, 0C0C0H,  256,  2, "MULU.1mo MULS.1mo");                    (* 1100 ddd0 11ee eeee  MULU Word *)
	                                                                     (* 1100 ddd1 11ee eeee  MULS Word *)
	I(0F000H, 0C000H,  256,  2, "AND.3mo  AND.3om");                     (* 1100 ddd- ssee eeee  AND, Src.Dest 0=Ea.R, 1=R.Ea *)

	Mark(13);
	I(0F0C0H, 0D0C0H,    1,  1, "ADDA.7mq");                             (* 1101 aaaw 11ee eeee  ADDA *)
	I(0F130H, 0D100H,    8,  2, "ADDXpo   ADDXts");                      (* 1101 ggg1 ss00 -ggg  ADDX, 0=d.d, 1=-(a).-(a) *)
	I(0F000H, 0D000H,  256,  2, "ADD.3mo  ADD.3om");                     (* 1101 ddd- ssee eeee  ADD, Src.Dest 0=Ea.R, 1=R.Ea *)

	Mark(14);
	I(0F0C0H, 0E0C0H, 2048,  2, "\<em     BF\~D ");                      (* 1110 0--x 11ee eeee  Sh/Ro Mem 0=AS 1=LS 2=ROX 3=RO *)
	                                                                     (* 1110 1--- 11ee eeee  BF 0=TST 1=EXTU 2=CHG 3=EXTS   *)
	                                                                     (*                      BF 4=CLR 5=FFO  6=SET 7=INS    *)
	I(0F000H, 0E000H,   32,  2, "\>.3fp   \>.3op");                      (* 1110 iiix ss0- -ddd  Sh/Ro Reg 0=AS 1=LS 2=ROX 3=RO *)
	                                                                     (* 1110 dddx ss1- -ddd  Sh/Ro Reg 0=AS 1=LS 2=ROX 3=RO *)

	Mark(15);                                                            (* qqq: 000=PMMU 001:FPP *)
	I(0FFFFH, 0FFFFH,    1,  1, "\Y ");                                  (* Case Table Mark *)
	I(0FFC0H, 0F000H,    1,  1, "PMMUmb");                               (* 1111 0000 00ee eeee 0000 1$$$ 0000 0000 PMOVE TT  !68030-only! *)
	                                                                     (* 1111 0000 00ee eeee 0010 00$0 000$ $$$$ PLOAD     !68030-only! *)
	                                                                     (* 1111 0000 00ee eeee 001$ $$00 $$$$ $$$$ PFLUSH    !68030-only! *)
	                                                                     (* 1111 0000 00ee eeee 010$ $$$$ 0000 0000 PMOVE TC  !68030-only! *)
	                                                                     (* 1111 0000 00ee eeee 0110 00$0 0000 0000 PMV MMUSR !68030-only! *)
	                                                                     (* 1111 0000 00ee eeee 100$ $$$$ $$$$ $$$$ PTEST     !68030-only! *)

	I(0FFFFH, 0F27CH,    1,  1, "\FTRAP\K.8");                           (* 1111 qqq0 0111 1100 0000 0000 00cc cccc  cpTRAPcc No Displac *)
	I(0FFC0H, 0F200H,    1,  1, "\FGEN.8mZ ");                           (* 1111 qqq0 00ee eeee  cpGEN *)
	I(0FFF8H, 0F248H,    1,  1, "\FDB\K.8b ");                           (* 1111 qqq0 0100 1ddd 0000 0000 00cc cccc  cpDBcc *)
	I(0FFC0H, 0F240H,    1,  1, "\FS\K.8m ");                            (* 1111 qqq0 01ee eeee 0000 0000 00cc cccc  cpScc *)
	I(0FF80H, 0F280H,    1,  1, "FB\k ");                                (* 1111 qqq0 1wcc cccc  cpBcc *)
	I(0F1C0H, 0F100H,    1,  1, "\PSAVEm ");                             (* 1111 qqq1 00ee eeee  cpSAVE *)
	I(0F1C0H, 0F140H,    1,  1, "\PRESTm ");                             (* 1111 qqq1 01ee eeee  cpRESTORE *)
	Mark(16);
END DevDec68k.
