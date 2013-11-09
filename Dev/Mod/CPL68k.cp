MODULE DevCPL68k;
(* low level code generator for MC68020: based on IOPL bh 16.6.94 and OPL Samuel Urech 23.2.93 *)
(* ip 06 Aug 1998 *)

(* 
	bh	27.10.97	CPE version
	ip	10.8.97	OutDesc: check descriptor/type attribute and write in symbol file
	ip	10.8.97	OutRefs: obj.name[0] # "@"
	ip	10.8.97	TypeObj: records if used
	ip	10.8.97	Imports renamed to DevCPx
	ip	2.6.97	Move: remove redundant move to same address
	cp	28.6.97	mProcTyp set to 0
	bh	14.5.97	OutDesc changed for abstract methods & FINALIZE
	bh	14.5.97	closeLbl & adaptions in OUTModDesc
	ip	6.4.97	SubX introduced
	ip	5.4.97	AddX introduced
	ip		OutStrings, OutReals: handle String16 and Int64 correctly
	ip	24.2.97	AllocConst: handle String16
	ip	22.7.96	Free*: frees registers or space on the stack (xreal)
	ip		added item mode xreal, xrealstack

	RELEASE 1.2
	ip	28.3.96	FreeAllRegs eingefuehrt
	ip	13.2.96	SetLastNIL: test ob dieses Objekt eingefuegt werden darf
	ip	4.2.96	SetLabel ruft ClearHints auf
	ip	4.2.96	lastNIL, ClearHints, InLastNIL, SetLastNIL, AssignTo: eingeführt
	ip	19.1.96	PrepDesc
	ip	19.1.96	OutStruct neue Meta-Konstanten
	ip	19.1.96	OutRefs neue Meta-Konstanten
	ip	19.1.96	verwenden/definieren von neuen Meta-Konstanten
	bh/ip	4.12.95	imports ueberall verwenden
	bh/ip	22.9.95	Support fuer DLLs eingefuegt
	
	RELEASE 1.1
	ip	28.8.95	OutImport benutzt FPIntfProc für CFM-Prozeduren (sysflag=-20)
	ip/bh	18.8.95	kein Descriptorlink für Any
	ip/bh	19.5.95	mAny-Behandlung eingeführt
	ip	18.4.95	Keep codeblocks until the end of the command, "deallocation" in Action.
	ip	18.4.95	Write blocks instead of single bytes.
	ip	16.4.95	Clr-Procedure inserted.
*)

	IMPORT DevCPM, DevCPT, DevCPE, Services, Dates, SYSTEM;

	CONST
		NewLabel* = 0;
		
		processor* = 20;	(* 68000 *)	(* bh *)
		
		(* object modes *)
		Var = 1; VarPar = 2; Con = 3; Fld = 4; Typ = 5; LProc = 6; XProc = 7;
		SProc = 8; CProc = 9; IProc = 10; Head = 12; TProc = 13;
	
		(* item modes *)
		dreg = 0; areg = 1; freg = 2; postinc = 3; predec = 4; regx = 5; absL = 7;
		imm = 8; immL = 9; pcx = 10; coc = 12; fcoc = 13; abs = 14; xreal = 15;
	
		(* structure forms *)
		Undef = 0; Byte = 1; Bool = 2; Char8 = 3; Int8 = 4; Int16 = 5; Int32 = 6;
		Real32 = 7; Real64 = 8; Set = 9; String8 = 10; NilTyp = 11; NoTyp = 12;
		Pointer = 13; ProcTyp = 14; Comp = 15;
		Char16 = 16; String16 = 17; Int64 = 18;
		
		(* composite structure forms *)
		Basic = 1; Array = 2; DynArr = 3; Record = 4;
		
		(* module visibility of objects *)
		internal = 0; external = 1; externalR = 2;

		(* history of imported objects *)
		inserted = 0; same = 1; pbmodified = 2; pvmodified = 3; removed = 4; inconsistent = 5;
		
		(* meta interface consts *)
		mConst = 1; mTyp = 2; mVar = 3; mProc = 4; mField = 5;
		mBool = 1; mChar8 = 2; mChar16 = 3; mInt8 = 4; mInt16 = 5; mInt32 = 6;
		mReal32 = 7; mReal64 = 8; mSet = 9;
		mInt64 = 10; mAnyRec = 11; mAnyPtr = 12; mSysPtr = 13;
		mProctyp = 0; mRecord = 1; mArray = 2; mPointer = 3;
		mInternal = 1; mReadonly = 2; mPrivate = 3; mExported = 4;

		(* attribute flags (attr.adr, struct.attribute, proc.conval.setval) *)
		newAttr = 16; absAttr = 17; limAttr = 18; empAttr = 19; extAttr = 20;

		(* instruction formats *)
		noext = 0; briefext = 1; fullext = 2; wordDispl = 3; longDispl = 4; extern = 5; extension = 6;

		(* sizes *)
		byte = 0; word = 1; long = 2;
		
		(* coprozessor word *)
		CP = 0F200H;
		
		(* ops *)
		DIVS = 81C0H; DIVU = 80C0H; MULS = 0C1C0H; MULU = 0C0C0H;
		
		(* condition codes *)
		CC = 4; CS = 5; EQ = 7; false = 1; GE = 12; GT = 14; HI = 2; LE = 15;
		LS = 3; LT = 13; MI = 11; NE = 6; PL = 10; true = 0; VC = 8; VS = 9;
		
		(* floating point condition codes *)
		FEQ = 1; FNE = 0EH; FGT = 12H; FNGT = 1DH; FGE = 13H; FNGE = 1CH;
		FLT = 14H; FNLT = 1BH; FLE = 15H; FNLE = 1AH; Ffalse = 0; Ftrue = 0FH;
		
		(* floating point control registers *)
		FPCR = 4; FPSR = 2; FPIAR = 1;
		
		IntSet = {Int8 .. Int32};
		RealSet = {Real32, Real64};
		ByteSet = {Int8, Byte, Char8, Bool};
		WordSet = {Int16};
		LongSet = {Int32, Set, Pointer, ProcTyp};
		
		None = -1; (* no index or offset register *)
		
		
		(* TypeDesc Parameters *)
		BaseTypeOffs* = 12;
		
		(* fixup types *)
		absolute = 100; relative = 101; copy = 102; table = 103; tableend = 104; deref = 105;
		
		(* system trap numbers *)
		withTrap = 129; caseTrap = 130; funcTrap = 131; typTrap = 132;
		recTrap = 133; ranTrap = 134; inxTrap = 135; copyTrap = 136;
		stackTrap = 137;
		NILTrap = 201;
		
		expAllFields = TRUE;
	
		(* implementation restrictions *)
		CodeBlocks = 64;	(* 256 *)
		CodeLength = 16384;	(* 32768 *)
		MaxNameTab = 800000H;
		
		(* registers *)
		D0 = 0; D1 = 1; D2 = 2; D3 = 3; D4 = 4; D5 = 5; D6 = 6; D7 = 7;
		A0 = 8; A1 = 9; A2 = 10; A3 = 11; A4 = 12; A5 = 13; A6 = 14; A7 = 15;
		
		reservedRegs* = {A5, A6, A7};
		calleeSavedRegs* = {D0..D7, A0..A4};
	
	TYPE
		Label* = INTEGER;  (* 0: unassigned, > 0: address, < 0: - linkadr *)
		
		Item* = RECORD
			mode*: SHORTINT; (* dreg, areg, freg, postinc, predec, regx, abs, imm, immL, pcx, coc, fcoc *)
			tmode*: SHORTINT;
			typ*, acttyp*: DevCPT.Struct;
			obj*:  DevCPT.Object;
			reg*: SHORTINT; (* D0 .. D7: 0 .. 7, A0 .. A7: 8 .. 15, FP0 .. FP7: 16 .. 23 *)
			bd*, ext*: INTEGER;
			inxReg*: SHORTINT; (* None = -1, D0 .. D7: 0 .. 7 *)
			xsize*: SHORTINT; (* word: 0; long: 1 *)
			scale*: SHORTINT; (* 0, 1, 2, 3 for sizes 1, 2, 4, 8 bytes *)
			tJump*, fJump*: Label; (* for coc- and fcoc-items only *)
			offsReg*: SHORTINT; (* for multidimensional dynamic arrays only *)
			nolen*: SHORTINT; (* pointer to dynamic array: number of lengths; string: length; 0 otherwise *)
		END;
(* Items:

mode       |  bd          reg     inxReg     xsize     scale    tJump    fJump
------------------------------------------------------------------------------
dreg       |              reg                                                  (0 .. 7)
areg       |              reg                                                  (8 .. 15)
freg       |              reg                                                  (16 .. 23)
postinc    |              reg
predec     |              reg
regx       |  bd          reg     inxReg     xsize     scale
abs        |  adr
absL       |  offset    linktype                                                 (absolute Ref.)
immL       |                                                                   (int. Proz.)
imm        |  val
pcx        |  bd                  inxReg     xsize     scale
coc        |  t/fcond                                           tJump    fJump
fcoc       |  t/fcond                                           tJump    fJump
*)
	
	
	VAR
		level*: BYTE;
		xrealstack*: SHORTINT;

		lastNIL: ARRAY 4 OF DevCPT.Object;
		
		usedRegs*: SET; (* data registers: 0..7; address registers: 8 .. 15; floating point registers: 16 .. 23 *)

PROCEDURE [1] Debugger 0A9H, 0FFH;

	PROCEDURE err(n: SHORTINT);
	BEGIN DevCPM.err(n)
	END err;

	PROCEDURE ClearHints*;
	VAR i: BYTE;
	BEGIN
		i := 0;
		WHILE i < LEN(lastNIL) DO
			lastNIL[i] := NIL;
			INC(i);
		END;
	END ClearHints;
	
	PROCEDURE InLastNIL*(obj: DevCPT.Object): BYTE;
	VAR i: BYTE;
	BEGIN
		IF obj = NIL THEN RETURN -1; END;
		i := 0;
		WHILE i < LEN(lastNIL) DO
			IF lastNIL[i] = obj THEN RETURN i; END;
			INC(i);
		END;
		RETURN -1;
	END InLastNIL;
	
	PROCEDURE SetLastNIL*(obj: DevCPT.Object);
	VAR i: BYTE;
	BEGIN
		IF (obj # NIL) & (obj.mode = Var) & (obj.mnolev = level) THEN
			IF InLastNIL(obj) < 0 THEN
				i := LEN(lastNIL) - 2;
				WHILE i >= 0 DO
					lastNIL[i+1] := lastNIL[i];
					DEC(i);
				END;
				lastNIL[0] := obj;
			END;
		END;
	END SetLastNIL;
	
	PROCEDURE AssignTo*(obj: DevCPT.Object);
	VAR i: BYTE;
	BEGIN
		i := InLastNIL(obj);
		IF i >= 0 THEN
			WHILE i < LEN(lastNIL) - 1 DO
				lastNIL[i] := lastNIL[i+1];
				INC(i);
			END;
			lastNIL[LEN(lastNIL)-1] := NIL;
		END;
	END AssignTo;

	PROCEDURE AllocConst* (VAR x: Item; con: DevCPT.Const; form: BYTE);
		VAR r: REAL; short: SHORTREAL; c: DevCPT.Const; i: INTEGER;
	BEGIN
		IF form IN {String8, String16} THEN
			DevCPE.AllocConst(con, form, x.obj, x.bd);
			x.nolen := SHORT(con.intval2)	(* string length *)
		ELSE
			IF form IN {Real32, Real64} THEN
				r := con.realval;
				IF ABS(r) <= MAX(SHORTREAL) THEN
					short := SHORT(r);
					IF short = r THEN form := Real32	(* a shortreal can represent the exact value *)
					ELSE form := Real64	(* use a real *)
					END
				ELSE form := Real64	(* use a real *)
				END
			END;
			DevCPE.AllocConst(con, form, x.obj, x.tJump);
			x.bd := 0; x.fJump := con.intval2
		END;
		x.mode := absL; x.reg := absolute; x.inxReg := None; x.offsReg := None
	END AllocConst;

	(*******************************************************)
	
	PROCEDURE BegStat*; (* general-purpose procedure which is called before each statement *)
	BEGIN
		usedRegs := reservedRegs;
		xrealstack := 0;
	END BegStat;

	PROCEDURE EndStat*; (* general-purpose procedure which is called after each statement *)
	BEGIN
(*		IF usedRegs # reservedRegs THEN err(-800); END;
		IF xrealstack # 0 THEN err(-801); END; *)
	END EndStat;

	(*******************************************************)

	PROCEDURE GenCh*(ch: SHORTCHAR);
	BEGIN
		DevCPE.GenByte(ORD(ch));
	END GenCh;
	
	PROCEDURE SetLabel* (VAR L: Label);
		VAR next: INTEGER;
	BEGIN
		ClearHints;
		ASSERT(L <= 0);
		L := -L;
		WHILE L # NewLabel DO
			next := DevCPE.ThisWord(L); DevCPE.PutWord(L, DevCPE.pc - L); L := next;
		END;
		L := DevCPE.pc;
	END SetLabel;

	(*******************************************************)
	
	PROCEDURE GenLinked (VAR x: Item; type: BYTE);
	VAR
		link: DevCPT.LinkList;
		obj: DevCPT.Object;
	BEGIN
		obj := x.obj;
		IF obj = NIL THEN DevCPE.GenWord(x.bd);
		ELSE
ASSERT((type >= absolute) & (type <= deref), 42);
			link := DevCPE.OffsetLink(obj, x.bd);
			DevCPE.GenWord(type * 1000000H + link.linkadr MOD 1000000H);
			link.linkadr := DevCPE.pc - 4
		END
	END GenLinked;
	
	(*******************************************************)
	
	PROCEDURE DispSize(disp: INTEGER): SHORTINT;
	BEGIN
		IF disp = 0 THEN RETURN 1;
		ELSIF (disp >= MIN(SHORTINT)) & (disp <= MAX(SHORTINT)) THEN RETURN 2;
		ELSE RETURN 3;
		END;
	END DispSize;
	
	PROCEDURE LengthCode(size: INTEGER): SHORTINT;
	BEGIN
		CASE size OF
		1: RETURN byte;
		| 2: RETURN word;
		| 4: RETURN long;
		END;
	END LengthCode;
	
	PROCEDURE FloatFormat(typ: DevCPT.Struct): SHORTINT;
	BEGIN
		IF typ.form IN ByteSet THEN RETURN 6;
		ELSIF typ.form IN WordSet THEN RETURN 4;
		ELSIF typ.form IN LongSet THEN RETURN 0;
		ELSIF typ = DevCPT.real32typ THEN RETURN 1;
		ELSIF typ = DevCPT.real64typ THEN RETURN 5;
		ELSE HALT(68);
		END;
	END FloatFormat;
	
	PROCEDURE Scale*(size: INTEGER): SHORTINT;
	BEGIN
		CASE size OF 
		1: RETURN 0;
		| 2: RETURN 1;
		| 4: RETURN 2;
		| 8: RETURN 3;
		END;
	END Scale;
	
	PROCEDURE MergedLinks*(L0, L1: Label): Label;
	VAR cur, next: Label;
	BEGIN
		IF L0 < 0 THEN
			next := -L0;
			REPEAT
				cur := next;
				next := DevCPE.ThisWord(cur);
			UNTIL next = NewLabel;
			DevCPE.PutWord(cur, -L1);
			RETURN L0;
		ELSE
			RETURN L1;
		END;
	END MergedLinks;
	
	PROCEDURE Jump*(condition: SHORTINT; VAR L: Label);
	VAR disp: INTEGER;
	BEGIN
		IF L > 0 THEN
			disp := L - DevCPE.pc - 2;
			IF (disp >= MIN(BYTE)) & (disp <= MAX(BYTE)) THEN
				DevCPE.GenShort(6000H + SYSTEM.LSH(condition, 8) + (disp MOD 256));
			ELSIF (disp >= MIN(SHORTINT)) & (disp <= MAX(SHORTINT)) THEN
				DevCPE.GenShort(6000H + SYSTEM.LSH(condition, 8));
				DevCPE.GenShort(disp);
			ELSE
				DevCPE.GenShort(60FFH + SYSTEM.LSH(condition, 8));
				DevCPE.GenWord(disp);
			END;
		ELSE
			DevCPE.GenShort(60FFH + SYSTEM.LSH(condition, 8));
			DevCPE.GenWord(-L);
			L := -(DevCPE.pc - 4); (* compensate for the Longword just generated *)
		END;
	END Jump;
	
(*	PROCEDURE FJump*(condition: INTEGER; VAR L: Label);
	VAR disp: LONGINT;
	BEGIN
		IF L > 0 THEN
			disp := L - pc - 2;
			IF (disp >= MIN(INTEGER)) & (disp <= MAX(INTEGER)) THEN
				DevCPE.GenShort(CP + 80H + condition);
				DevCPE.GenShort(disp);
			ELSE
				DevCPE.GenShort(CP + 0C0H + condition);
				DevCPE.GenWord(disp);
			END;
		ELSE
			DevCPE.GenShort(CP + 0C0H + condition);
			DevCPE.GenWord(-L);
			L := -(pc - 4); (* compensate for the Longword just generated *)
		END;
	END FJump;*)
	
	PROCEDURE Bsr*(VAR L: Label);
	VAR disp: INTEGER;
	BEGIN
		IF L > 0 THEN
			disp := L - DevCPE.pc - 2;
			IF (disp >= MIN(BYTE)) & (disp <= MAX(BYTE)) THEN
				DevCPE.GenShort(6100H + (disp MOD 256));
			ELSIF (disp >= MIN(SHORTINT)) & (disp <= MAX(SHORTINT)) THEN
				DevCPE.GenShort(6100H);
				DevCPE.GenShort(disp);
			ELSE
				DevCPE.GenShort(61FFH);
				DevCPE.GenWord(disp);
			END;
		ELSE
			DevCPE.GenShort(61FFH);
			DevCPE.GenWord(-L);
			L := -(DevCPE.pc - 4); (* compensate for the Longword just generated *)
		END;
	END Bsr;

	PROCEDURE Encode(VAR item: Item; VAR mode, reg, extWord, format: SHORTINT; VAR bd: INTEGER; offset: SHORTINT);
	BEGIN
		bd := item.bd;
		CASE item.mode OF
		dreg: mode := 0; reg := item.reg; format := noext;
		| areg: mode := 1; reg := SHORT(item.reg MOD 8); format := noext;
		| freg: mode := 0; reg := 0; format := noext;
		| postinc: mode := 3; reg := SHORT(item.reg MOD 8); format := noext;
		| predec: mode := 4; reg := SHORT(item.reg MOD 8); format := noext;
		| regx:
			reg := SHORT(item.reg MOD 8);
			IF item.inxReg = None THEN
				CASE DispSize(bd) OF
				1: mode := 2; format := noext;
				| 2: mode := 5; format := wordDispl;
				| 3: mode := 6; extWord := 170H; format := fullext;
				END;
			ELSE
				mode := 6;
				IF (bd >= MIN(BYTE)) & (bd <= MAX(BYTE)) THEN
					extWord := SHORT(SYSTEM.LSH(item.inxReg, 12) + SYSTEM.LSH(item.xsize, 11) + SYSTEM.LSH(item.scale, 9)
					 + SHORT(bd MOD 256));
					format := briefext;
				ELSE
					extWord := SHORT(SYSTEM.LSH(item.inxReg, 12) + SYSTEM.LSH(item.xsize, 11) + SYSTEM.LSH(item.scale, 9)
					 + SYSTEM.LSH(DispSize(bd), 4) + 100H);
					format := fullext;
				END;
			END;
		| abs: mode := 7; reg := 1; format := longDispl;
		| absL: mode := 7; reg := 1; format := extern;
		| imm:
			mode := 7; reg := 4;
			CASE item.typ.size OF
			8: format := extension;
			| 4: format := longDispl;
			ELSE
				format := wordDispl;
			END;
		| immL: mode := 7; reg := 4; format := extern;
		| pcx:
			DEC(bd, DevCPE.pc + offset); mode := 7;
			IF item.inxReg = None THEN
				IF DispSize(bd) < 3 THEN
					reg := 2; format := wordDispl;
				ELSE
					reg := 3; format := fullext; extWord := 170H;
				END;
			ELSE
				reg := 3;
				IF (bd >= MIN(BYTE)) & (bd <= MAX(BYTE)) THEN
					extWord := SHORT(SYSTEM.LSH(item.inxReg, 12) + SYSTEM.LSH(item.xsize, 11) + SYSTEM.LSH(item.scale, 9)
					 + SHORT(bd MOD 256));
					format := briefext;
				ELSE
					extWord := SHORT(SYSTEM.LSH(item.inxReg, 12) + SYSTEM.LSH(item.xsize, 11) + SYSTEM.LSH(item.scale, 9)
					 + SYSTEM.LSH(DispSize(bd), 4) + 100H);
					format := fullext;
				END;
			END;
		END;
	END Encode;
	
	PROCEDURE GenExtension(format, extWord: SHORTINT; bd: INTEGER; item: Item);
	VAR val: INTEGER;
	BEGIN
		CASE format OF
		noext: (* nothing *)
		| briefext: DevCPE.GenShort(extWord);
		| fullext:
			DevCPE.GenShort(extWord);
			CASE DispSize(bd) OF
			1: (* nothing *)
			| 2: DevCPE.GenShort(bd);
			| 3: DevCPE.GenWord(bd);
			END;
		| wordDispl: DevCPE.GenShort(bd);
		| longDispl: DevCPE.GenWord(bd);
		| extern: GenLinked(item, SHORT(item.reg));
		| extension: DevCPE.GenWord(item.bd); DevCPE.GenWord(item.ext);
		END;
	END GenExtension;
	
	PROCEDURE GetReg*(): SHORTINT;
	VAR i: SHORTINT;
	BEGIN
		i := 0;
		WHILE (i<8) & (i IN usedRegs) DO INC(i) END;
		IF i = 8 THEN err(215) END; (* ??? *)
		INCL(usedRegs, i);
		RETURN i;
	END GetReg;
	
	PROCEDURE GetAdrReg*(): SHORTINT;
	VAR i: SHORTINT;
	BEGIN
		i := 8;
		WHILE (i < 16) & (i IN usedRegs) DO INC(i) END;
		IF i = 16 THEN err(215) END; (* ??? *)
		INCL(usedRegs, i);
		RETURN i;
	END GetAdrReg;
	
	PROCEDURE GetFReg*(): SHORTINT;
	VAR i: SHORTINT;
	BEGIN
		i := 16;
		WHILE (i < 24) & (i IN usedRegs) DO INC(i) END;
		IF i = 24 THEN err(215) END; (* ??? *)
		INCL(usedRegs, i);
		RETURN i;
	END GetFReg;
	
	PROCEDURE FreeReg*(VAR item: Item);
	BEGIN
		IF item.mode IN {dreg, areg, freg, postinc, predec, regx} THEN
			IF ~(item.reg IN reservedRegs) THEN
				EXCL(usedRegs, item.reg);
			END;
		END;
		IF (item.inxReg # None) & (item.mode IN {regx, pcx}) THEN
			IF ~(item.inxReg IN reservedRegs) THEN
				EXCL(usedRegs, item.inxReg);
			END;
		END;
	END FreeReg;
	
	PROCEDURE Free*(VAR item: Item);
	BEGIN
		IF item.mode = xreal THEN
			IF item.reg # xrealstack THEN DevCPM.err(-802) END;
			DevCPE.GenShort(0DFFCH); DevCPE.GenWord(12);
			DEC(xrealstack);
			item.mode := None;
		ELSE
			FreeReg(item);
		END;
	END Free;
	
	PROCEDURE FreeAllRegs*;
	BEGIN
		usedRegs := reservedRegs;
	END FreeAllRegs;

	PROCEDURE Lea*(VAR source: Item; destReg: SHORTINT);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Encode(source, mode, reg, extWord, format, bd, 2);
		DevCPE.GenShort(41C0H + SYSTEM.LSH(destReg MOD 8, 9) + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, source);
	END Lea;
	
	PROCEDURE Pea*(VAR item: Item);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Encode(item, mode, reg, extWord, format, bd, 2);
		DevCPE.GenShort(4840H + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, item);
	END Pea;
	
	PROCEDURE Jsr*(VAR item: Item);
	VAR
		mode, reg,extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Encode(item,mode, reg, extWord, format, bd, 2);
		DevCPE.GenShort(4E80H + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, item);
	END Jsr;
	
	PROCEDURE LoadAdr*(VAR item: Item);
	VAR reg: SHORTINT;
	BEGIN
		IF item.mode = pcx THEN
			reg := GetAdrReg();
			Lea(item, reg);
			item.mode := regx; item.reg := reg; item.bd := 0; item.inxReg := None; item.offsReg := None;
		END;
	END LoadAdr;
	
	PROCEDURE LoadExternal*(VAR item: Item);
	VAR reg: SHORTINT;
	BEGIN
		IF item.mode = absL THEN
			reg := GetAdrReg();
			Lea(item, reg);
			item.mode := regx; item.reg := reg; item.bd := 0; item.inxReg := None; item.offsReg := None;
		END;
	END LoadExternal;
	
	PROCEDURE Clr*(VAR item: Item);
	VAR
		mode, reg,extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Encode(item, mode, reg, extWord, format, bd, 2);
		DevCPE.GenShort(4200H + SYSTEM.LSH(LengthCode(item.typ.size), 6) + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, item);
	END Clr;
	
	PROCEDURE Move*(VAR source, dest: Item);
	VAR
		sMode, sReg, sExtWord, sFormat, dMode, dReg, dExtWord, dFormat, sizeCode: SHORTINT;
		sBd, dBd: INTEGER;
	BEGIN
		IF (source.mode # postinc) OR (dest.mode # predec) OR (source.reg # dest.reg) THEN
			CASE LengthCode(source.typ.size) OF
			byte: sizeCode := 1;
			| word: sizeCode := 3;
			| long: sizeCode := 2;
			END;
			Encode(source, sMode, sReg, sExtWord, sFormat, sBd, 2);
			Encode(dest, dMode, dReg, dExtWord, dFormat, dBd, 0);
			DevCPE.GenShort(SYSTEM.LSH(sizeCode, 12) + SYSTEM.LSH(dReg, 9) + SYSTEM.LSH(dMode, 6)
			 + SYSTEM.LSH(sMode, 3) + sReg);
			GenExtension(sFormat, sExtWord, sBd, source);
			GenExtension(dFormat, dExtWord, dBd, dest);
		END;
	END Move;
	
	PROCEDURE Moveq*(val, reg: SHORTINT);
	BEGIN
		DevCPE.GenShort(7000H + SYSTEM.LSH(reg, 9) + (val MOD 256));
	END Moveq;
	
	PROCEDURE Movem*(dir, regList: SHORTINT; VAR item: Item);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Encode(item, mode, reg, extWord, format, bd, 0);
		DevCPE.GenShort(48C0H + SYSTEM.LSH(dir, 10) + SYSTEM.LSH(mode, 3) + reg);
		DevCPE.GenShort(regList);
		GenExtension(format, extWord, bd, item);
	END Movem;
	
	PROCEDURE FMove*(VAR source, dest: Item);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		IF dest.mode = freg THEN
			Encode(source, mode, reg, extWord, format, bd, 4);
			DevCPE.GenShort(CP + SYSTEM.LSH(mode, 3) + reg);
			DevCPE.GenShort(4000H + SYSTEM.LSH(FloatFormat(source.typ), 10) + SYSTEM.LSH(dest.reg MOD 8, 7));
			GenExtension(format, extWord, bd, source);
		ELSIF source.mode = freg THEN
			Encode(dest, mode, reg, extWord, format, bd, 4);
			DevCPE.GenShort(CP + SYSTEM.LSH(mode, 3) + reg);
			DevCPE.GenShort(6000H + SYSTEM.LSH(FloatFormat(source.typ), 10) + SYSTEM.LSH(source.reg MOD 8, 7));
			GenExtension(format, extWord, bd, dest);
		ELSE
			HALT(68);
		END;
	END FMove;
	
(*	PROCEDURE FMovecr*(VAR item: Item; dr, controlReg: INTEGER);
	VAR
		mode, reg, extWord, format: INTEGER;
		bd: LONGINT;
	BEGIN
		Encode(item, mode, reg, extWord, format, bd, 4);
		DevCPE.GenShort(CP + SYSTEM.LSH(mode, 3) + reg);
		DevCPE.GenShort(8000H + SYSTEM.LSH(dr, 13) + SYSTEM.LSH(controlReg, 10));
		GenExtension(format, extWord, bd, item);
	END FMovecr;*)
	
(*	PROCEDURE FMovem*(dir, regList: INTEGER; VAR item: Item);
	VAR
		mode, reg, extWord, format: INTEGER;
		bd: LONGINT;
	BEGIN
		Encode(item, mode, reg, extWord, format, bd, 4);
		DevCPE.GenShort(CP + SYSTEM.LSH(mode, 3) + reg);
		DevCPE.GenShort(0C000H + SYSTEM.LSH(1 - dir, 13) + SYSTEM.LSH(dir, 12) + regList);
		(* ??? no GenExtension *)
	END FMovem;*)
	
	PROCEDURE Load*(VAR item: Item);
	VAR source: Item;
	BEGIN
		IF item.mode # dreg THEN
			source := item; item.mode := dreg;
			IF (source.mode = regx) & (source.inxReg # None) THEN
				item.reg := source.inxReg;
			ELSE
				item.reg := GetReg();
			END;
			IF source.mode = freg THEN FMove(source, item) ELSE
			Move(source, item)
			END;
		END;
	END Load;
	
(*	PROCEDURE FLoad*(VAR item: Item);
	VAR regItem: Item;
	BEGIN
		IF item.mode # freg THEN
			regItem.mode := freg; regItem.typ := item.typ; regItem.acttyp := item.typ;
			regItem.reg := GetFReg();
			FMove(item, regItem);
			item := regItem;
		END;
	END FLoad;*)
	
	PROCEDURE FP68K*;
	BEGIN
		DevCPE.GenShort(0A9EBH);
	END FP68K;
	
	PROCEDURE AssertDestReg*(typ: DevCPT.Struct; VAR source, dest: Item);
	VAR swap: Item;
	BEGIN
		IF (typ = DevCPT.real32typ) OR (typ = DevCPT.real64typ) THEN
			IF dest.mode # freg THEN
				IF source.mode = freg THEN
					swap := dest; dest := source; source := swap;
				ELSE
(*					FLoad(dest);*)
				END;
			END;
		ELSE
			IF dest.mode # dreg THEN
				IF source.mode = dreg THEN
					swap := dest; dest := source; source := swap;
				ELSE
					Load(dest);
				END;
			END;
		END;
	END AssertDestReg;
	
	PROCEDURE TFConds*(tcond: INTEGER): INTEGER;
	VAR fcond: SHORTINT;
	BEGIN
		CASE tcond OF
		CC: fcond := CS;
		| CS: fcond := CC;
		| EQ: fcond := NE;
		| NE: fcond := EQ;
		| false: fcond := true;
		| true: fcond := false;
		| GE: fcond := LT;
		| LT: fcond := GE;
		| GT: fcond := LE;
		| LE: fcond := GT;
		| HI: fcond := LS;
		| LS: fcond := HI;
		| MI: fcond := PL;
		| PL: fcond := MI;
		| VC: fcond := VS;
		| VS: fcond := VC;
		END;
		RETURN tcond * 10000H + fcond;
	END TFConds;
	
	PROCEDURE TFFConds*(tcond: INTEGER): INTEGER;
	VAR fcond: SHORTINT;
	BEGIN
		CASE tcond OF
		FEQ: fcond := FNE;
		| FNE: fcond := FEQ;
		| FGE: fcond := FNGE;
		| FLT: fcond := FNLT;
		| FGT: fcond := FNGT;
		| FLE: fcond := FNLE;
		END;
		RETURN tcond * 10000H + fcond;
	END TFFConds;
	
	PROCEDURE Chk*(VAR item, chk: Item);
	VAR
		mode, reg, extWord, format, size: SHORTINT;
		bd: INTEGER;
	BEGIN
		IF item.typ = DevCPT.int32typ THEN size := 0 ELSE size := 1 END;
		Load(item);
		Load(chk);
		Encode(chk, mode, reg, extWord, format, bd, 2);
		DevCPE.GenShort(4100H + SYSTEM.LSH(item.reg, 9) + SYSTEM.LSH(size, 7) + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, chk);
	END Chk;
	
	PROCEDURE DBcc*(condition: SHORTINT; VAR reg: SHORTINT; VAR L: Label);
	BEGIN
		DevCPE.GenShort(50C8H + SYSTEM.LSH(condition, 8) + reg);
		DevCPE.GenShort(L - DevCPE.pc);
		(* ??? genuegt ein Word-Displacement? *)
	END DBcc;
	
	PROCEDURE Test*(item: Item);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Encode(item, mode, reg, extWord, format, bd, 0);
		DevCPE.GenShort(4A00H + SYSTEM.LSH(LengthCode(item.typ.size), 6) + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, item);
	END Test;
	
	PROCEDURE Unlk*;
	BEGIN
		DevCPE.GenShort(4E5EH);
	END Unlk;
	
	PROCEDURE Trapcc*(condition, trapnr: SHORTINT);
	BEGIN
		DevCPE.GenShort(50FAH + SYSTEM.LSH(condition, 8));
		DevCPE.GenShort(trapnr);
	END Trapcc;
	
	PROCEDURE Ext*(VAR reg: Item; destSize: SHORTINT);
	BEGIN
		Load(reg);
		IF reg.typ.size = 1 THEN
			IF destSize = word THEN DevCPE.GenShort(4880H + reg.reg) ELSE DevCPE.GenShort(49C0H + reg.reg) END;
		ELSIF reg.typ.size = 2 THEN
			DevCPE.GenShort(48C0H + reg.reg);
		END;
	END Ext;
	
	PROCEDURE Divsl*(VAR source, remainder, quotient: Item);
	VAR
		mode, reg, extWord, format, size: SHORTINT;
		bd: INTEGER;
	BEGIN
		Load(remainder);
		Load(quotient);
		Encode(source, mode, reg, extWord, format, bd, 4);
		DevCPE.GenShort(4C40H + SYSTEM.LSH(mode, 3) + reg);
		DevCPE.GenShort(0800H + SYSTEM.LSH(quotient.reg, 12) + remainder.reg);
		GenExtension(format, extWord, bd, source);
	END Divsl;
	
	PROCEDURE Swap*(VAR dest: Item);
	BEGIN
		Load(dest);
		DevCPE.GenShort(4840H + dest.reg);
	END Swap;
	
	PROCEDURE Eor*(VAR source, dest: Item);
	VAR
		mode, reg, extWord, format, size: SHORTINT;
		bd: INTEGER;
	BEGIN
		Load(source);
		Encode(dest, mode, reg, extWord, format, bd, 0);
		DevCPE.GenShort(0B100H + SYSTEM.LSH(source.reg, 9) + SYSTEM.LSH(LengthCode(dest.typ.size), 6)
		 + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, dest);
	END Eor;
	
	PROCEDURE Cmp*(VAR source, dest: Item);
	VAR
		mode, reg, extWord, format, size: SHORTINT;
		bd: INTEGER;
	BEGIN
		Load(dest);
		Encode(source, mode, reg, extWord, format, bd, 0);
		DevCPE.GenShort(0B000H + SYSTEM.LSH(dest.reg, 9) + SYSTEM.LSH(LengthCode(source.typ.size), 6)
		 + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, source);
	END Cmp;
	
	PROCEDURE Enter*(val: INTEGER);
	BEGIN
		IF (val >= MIN(SHORTINT)) & (val <= MAX(SHORTINT)) THEN
			DevCPE.GenShort(4E56H);
			DevCPE.GenShort(val);
		ELSE
			DevCPE.GenShort(480EH);
			DevCPE.GenWord(val);
		END;
	END Enter;
	
	PROCEDURE Return*(val: INTEGER);
	BEGIN
		DevCPE.GenShort(4E5EH);
		IF val = 0 THEN
			DevCPE.GenShort(4E75H); (* RTS *)
		ELSE
			DevCPE.GenShort(4E74H); DevCPE.GenShort(val); (* RTD # val *)
		END;
	END Return;
	
	PROCEDURE WriteCProc*(code: DevCPT.ConstExt);
	VAR i, n: SHORTINT;
	BEGIN
		n := ORD(code^[0]);
		FOR i := 1 TO n DO DevCPE.GenByte(ORD(code^[i])) END;
	END WriteCProc;
	
	PROCEDURE SetCaseEntry*(tab, from, to: INTEGER);
	VAR entry: INTEGER;
	BEGIN
		from := tab + 4*from; to := tab + 4*to; entry := DevCPE.pc - tab;
		WHILE from <= to DO DevCPE.PutWord(from, entry); INC(from, 4); END;
	END SetCaseEntry;
	
	PROCEDURE GenCaseJump*(VAR i: Item; num: INTEGER; VAR tab: INTEGER);
	VAR
		else, entry: INTEGER;
		reg: SHORTINT;
	BEGIN
		reg := GetReg();
		DevCPE.GenShort(203BH + reg*512); DevCPE.GenShort(i.reg*4096 + 0C06H); (* MOVE.L (6,PC,Di*4), Dreg *)
		DevCPE.GenShort(4EFBH); DevCPE.GenShort(reg*4096 + 0802H); (* JMP (2,PC,Dreg) *)
		tab := DevCPE.pc; else := tab+num*4;
		entry := else - tab;
		WHILE DevCPE.pc < else DO DevCPE.GenWord(entry) END;
	END GenCaseJump;
	
	PROCEDURE AddX*(VAR source, dest: Item);
	VAR size, type: INTEGER;
	BEGIN
		IF (source.mode = dreg) & (dest.mode = dreg) THEN
			type := 0;
		ELSIF (source.mode = predec) & (dest.mode = predec) THEN
			type := 1;
		ELSE
			HALT(20);
		END;
		IF (source.typ.form = Int32) & (dest.typ.form = Int32) THEN
			size := 2;
		ELSIF (source.typ.form = Int16) & (dest.typ.form = Int16) THEN
			size := 1;
		ELSIF (source.typ.form = Int8) & (dest.typ.form = Int8) THEN
			size := 0;
		ELSE
			HALT(21);
		END;
		DevCPE.GenShort(0D100H + (dest.reg MOD 8)*512 + size*64 + type*8 + (source.reg MOD 8));
	END AddX;
	
	PROCEDURE SubX*(VAR source, dest: Item);
	VAR size, type: INTEGER;
	BEGIN
		IF (source.mode = dreg) & (dest.mode = dreg) THEN
			type := 0;
		ELSIF (source.mode = predec) & (dest.mode = predec) THEN
			type := 1;
		ELSE
			HALT(20);
		END;
		IF (source.typ.form = Int32) & (dest.typ.form = Int32) THEN
			size := 2;
		ELSIF (source.typ.form = Int16) & (dest.typ.form = Int16) THEN
			size := 1;
		ELSIF (source.typ.form = Int8) & (dest.typ.form = Int8) THEN
			size := 0;
		ELSE
			HALT(21);
		END;
		DevCPE.GenShort(09100H + (dest.reg MOD 8)*512 + size*64 + type*8 + (source.reg MOD 8));
	END SubX;
	
	PROCEDURE Format1*(opcode: INTEGER; data: SHORTINT; VAR dest: Item);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Encode(dest, mode, reg, extWord, format, bd, 0);
		DevCPE.GenShort(5000H + SYSTEM.LSH(data MOD 8, 9) + SYSTEM.LSH(opcode, 8) +
						SYSTEM.LSH(LengthCode(dest.typ.size), 6) + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, dest);
	END Format1;
	
	PROCEDURE Format2*(opcode: INTEGER; VAR source, dest: Item);
	VAR
		mode, reg, extWord, format, size: SHORTINT;
		bd: INTEGER;
	BEGIN
		size := LengthCode(source.typ.size);
		IF dest.mode = dreg THEN
			Encode(source, mode, reg, extWord, format, bd, 2);
			DevCPE.GenShort(SYSTEM.LSH(opcode, 12) + SYSTEM.LSH(dest.reg, 9) + SYSTEM.LSH(size, 6) +
							SYSTEM.LSH(mode, 3) + reg);
			GenExtension(format, extWord, bd, source);
		ELSE
			Load(source);
			Encode(dest, mode, reg, extWord, format, bd, 0);
			DevCPE.GenShort(0100H + SYSTEM.LSH(opcode, 12) + SYSTEM.LSH(source.reg, 9) +
							SYSTEM.LSH(size, 6) + SYSTEM.LSH(mode, 3) + reg);
			GenExtension(format, extWord, bd, dest);
		END;
	END Format2;
	
	PROCEDURE Format3*(opcode: INTEGER; VAR source: Item; destreg: SHORTINT);
	VAR
		mode, reg, extWord, format, size: SHORTINT;
		bd: INTEGER;
	BEGIN
		IF LengthCode(source.typ.size) = long THEN size := 1 ELSE size := 0 END;
		Encode(source, mode, reg, extWord, format, bd, 2);
		DevCPE.GenShort(00C0H + SYSTEM.LSH(opcode, 12) + SYSTEM.LSH(destreg MOD 8, 9) +
						SYSTEM.LSH(size, 8) + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, source);
	END Format3;
	
	PROCEDURE Format4*(opcode, bitnr: INTEGER; VAR dest: Item);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Load(dest);
		Encode(dest, mode, reg, extWord, format, bd, 0);
		DevCPE.GenShort(0800H + SYSTEM.LSH(opcode, 6) + SYSTEM.LSH(mode, 3) + reg);
		DevCPE.GenShort(bitnr);
		GenExtension(format, extWord, bd, dest);
	END Format4;
	
	PROCEDURE Format5*(opcode: INTEGER; VAR bitnr, dest: Item);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Load(bitnr);
		Load(dest);
		Encode(dest, mode, reg, extWord, format, bd, 0);
		DevCPE.GenShort(0100H + SYSTEM.LSH(bitnr.reg, 9) + SYSTEM.LSH(opcode, 6) + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, dest);
	END Format5;
	
	PROCEDURE Format6*(opcode: INTEGER; data: INTEGER; VAR dest: Item);
	VAR
		mode, reg, extWord, format, size: SHORTINT;
		bd: INTEGER;
	BEGIN
		size := LengthCode(dest.typ.size);
		Encode(dest, mode, reg, extWord, format, bd, 0);
		DevCPE.GenShort(SYSTEM.LSH(opcode, 8) + SYSTEM.LSH(size, 6) + SYSTEM.LSH(mode, 3) + reg);
		IF size = long THEN DevCPE.GenWord(data) ELSE DevCPE.GenShort(data) END;
		GenExtension(format, extWord, bd, dest);
	END Format6;
	
	PROCEDURE Format7*(opcode: INTEGER; VAR dest: Item);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Encode(dest, mode, reg, extWord, format, bd, 0);
		DevCPE.GenShort(4000H + SYSTEM.LSH(opcode, 8) + SYSTEM.LSH(LengthCode(dest.typ.size), 6)
		 + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, dest);
	END Format7;
	
(*	PROCEDURE Format8*(opcode: LONGINT; VAR source, dest: Item);
	VAR
		mode, reg, extWord, format: INTEGER;
		bd: LONGINT;
	BEGIN
		FLoad(dest);
		IF source.mode = freg THEN
			DevCPE.GenShort(CP);
			DevCPE.GenShort(SYSTEM.LSH(source.reg MOD 8, 10) + SYSTEM.LSH(dest.reg MOD 8, 7) + opcode);
		ELSE
			Encode(source, mode, reg, extWord, format, bd, 4);
			DevCPE.GenShort(CP + SYSTEM.LSH(mode, 3) + reg);
			DevCPE.GenShort(4000H + SYSTEM.LSH(FloatFormat(source.typ), 10) + SYSTEM.LSH(dest.reg MOD 8, 7) + opcode);
			GenExtension(format, extWord, bd, source);
		END;
	END Format8;*)
	
	PROCEDURE Format9*(opcode: INTEGER; VAR dest: Item; offset, width: SHORTINT);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Load(dest);
		IF width > 0 THEN
			IF width = 32 THEN width := 0 END;
			Encode(dest, mode, reg, extWord, format, bd, 0);
			DevCPE.GenShort(0E0C0H + SYSTEM.LSH(opcode, 8) + SYSTEM.LSH(mode, 3) + reg);
			DevCPE.GenShort(SYSTEM.LSH(offset, 6) + width);
			GenExtension(format, extWord, bd, dest);
		END;
	END Format9;
	
	PROCEDURE Format10*(opcode: INTEGER; offset: SHORTINT; VAR width, dest: Item);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Load(width);
		Load(dest);
		Encode(dest, mode, reg, extWord, format, bd, 0);
		DevCPE.GenShort(0E0C0H + SYSTEM.LSH(opcode, 8) + SYSTEM.LSH(mode, 3) + reg);
		DevCPE.GenShort(0020H + SYSTEM.LSH(offset, 6) + width.reg);
		GenExtension(format, extWord, bd, dest);
	END Format10;
	
	PROCEDURE Format11*(opcode: INTEGER; VAR source, dest: Item);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Load(dest);
		Encode(source, mode, reg, extWord, format, bd, 2);
		DevCPE.GenShort(opcode + SYSTEM.LSH(dest.reg, 9) + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, source);
	END Format11;
	
	PROCEDURE Format12*(opcode: INTEGER; VAR source, dest: Item);
	VAR
		mode, reg, extWord, format, bit6, bit11: SHORTINT;
		bd: INTEGER;
	BEGIN
		IF opcode = MULU THEN bit6 := 0; bit11 := 0;
		ELSIF opcode = MULS THEN bit6 := 0; bit11 := 1;
		ELSIF opcode = DIVU THEN bit6 := 1; bit11 := 0;
		ELSIF opcode = DIVS THEN bit6 := 1; bit11 := 1;
		END;
		Load(dest);
		Encode(source, mode, reg, extWord, format, bd, 4);
		DevCPE.GenShort(4C00H + SYSTEM.LSH(bit6, 6) + SYSTEM.LSH(mode, 3) + reg);
		DevCPE.GenShort(SYSTEM.LSH(dest.reg, 12) + SYSTEM.LSH(bit11, 11) + dest.reg);
		GenExtension(format, extWord, bd, source);
	END Format12;
	
	PROCEDURE Format13*(opcode, shiftleft: SHORTINT; VAR dest: Item);
	VAR dr, size: SHORTINT;
	BEGIN
		size := LengthCode(dest.typ.size);
		IF shiftleft > 0 THEN dr := 1 ELSE dr := 0 END;
		IF ABS(shiftleft) = 8 THEN shiftleft := 0 END;
		Load(dest);
		DevCPE.GenShort(0E000H + SYSTEM.LSH(ABS(shiftleft), 9) + SYSTEM.LSH(dr, 8) + SYSTEM.LSH(size, 6)
		 + SYSTEM.LSH(opcode, 3) + dest.reg);
	END Format13;
	
	PROCEDURE Format14*(opcode, dr: SHORTINT; VAR shift, dest: Item);
	BEGIN
		Load(shift);
		Load(dest);
		DevCPE.GenShort(0E020H + SYSTEM.LSH(shift.reg, 9) + SYSTEM.LSH(dr, 8) + SYSTEM.LSH(LengthCode(dest.typ.size), 6)
		 + SYSTEM.LSH(opcode, 3) + dest.reg);
	END Format14;
	
	PROCEDURE Format15*(opcode: SHORTINT; VAR item: Item);
	VAR
		mode, reg, extWord, format: SHORTINT;
		bd: INTEGER;
	BEGIN
		Encode(item, mode, reg, extWord, format, bd, 2);
		DevCPE.GenShort(4000H + SYSTEM.LSH(opcode, 6) + SYSTEM.LSH(mode, 3) + reg);
		GenExtension(format, extWord, bd, item);
	END Format15;
	
	(*******************************************************)


	PROCEDURE FPIntfProc(obj: DevCPT.Object; VAR fprint: INTEGER);
	
		PROCEDURE Par(par: DevCPT.Object; no: INTEGER);
		VAR
			typ: DevCPT.Struct; fpinc: INTEGER;
		BEGIN
			IF par # NIL THEN
				Par(par.link, no+1);
				typ := par.typ;
				IF (par.mode = VarPar) OR ((typ.form = Comp) & (typ.size > 4)) THEN
					fpinc := 3;
				ELSE
					CASE typ.size OF
					1: fpinc := 1;
					|2: fpinc := 2;
					ELSE fpinc := 3;
					END;
				END;
				fpinc := fpinc * 64;
				WHILE no # 0 DO
					fpinc := fpinc * 4;
					DEC(no);
				END;
				INC(fprint, fpinc);
			END;
		END Par;
		
	BEGIN
		fprint := 0;
		Par(obj.link, 0);
		IF (obj.typ # NIL) & (obj.typ.form # NoTyp) THEN
			CASE obj.typ.size OF
			1: INC(fprint, 16);
			|2: INC(fprint, 32);
			|4: INC(fprint, 48);
			ELSE
			END;
		END;
	END FPIntfProc;

	PROCEDURE PrepImport (obj: DevCPT.Object; isDll: BOOLEAN);
	BEGIN
		IF obj # NIL THEN
			PrepImport(obj.left, isDll);
			IF obj.used & (obj.mode IN {XProc, IProc}) & (isDll OR (obj.sysflag = -20)) THEN
				(* Interface Procedure, generate Fingerprint for MixedModeMgr *)
				FPIntfProc(obj, obj.fprint); obj.fpdone := TRUE
			END;
			PrepImport(obj.right, isDll)
		END
	END PrepImport;
	
	PROCEDURE Prepare*;
		VAR i: INTEGER; obj: DevCPT.Object;
	BEGIN
		i := 1;
		WHILE i < DevCPT.nofGmod DO
			obj := DevCPT.GlbMod[i]; INC(i);
			PrepImport(obj.right, obj.library # NIL)
		END;
	END Prepare;

	PROCEDURE Init*(opt: SET);
		CONST obj = 3; ref = 4; allref = 5; srcpos =6; key = 7; bigEnd = 15; pVarInd = 14;
	BEGIN
		DevCPE.Init(processor, opt + {bigEnd, pVarInd});
		level := 0;
		ClearHints;
		usedRegs := reservedRegs;
		xrealstack := 0
	END Init;

	PROCEDURE Close*;
	BEGIN
		ClearHints;
		DevCPE.Close
	END Close;

BEGIN
	xrealstack := 0;
END DevCPL68k.
