MODULE DevCPCP;	(* NW, RC 6.3.89 / 10.2.94 / object model 4.12.93 / bh 7.9.94 *)

(* Component Pascal Conversion Parser, bh, 5 Oct 2009 *)


	IMPORT
		CPT := DevCPT, CPS := DevCPS, CPM := DevCPM, CPB := DevCPB;
		
	CONST
		(* numtyp values *)
		char = 1; integer = 2; real = 4; int64 = 5;

		(*symbol values*)
		null = 0; times = 1; slash = 2; div = 3; mod = 4;
		and = 5; plus = 6; minus = 7; or = 8; eql = 9;
		neq = 10; lss = 11; leq = 12; gtr = 13; geq = 14;
		in = 15; is = 16; arrow = 17; dollar = 18; period = 19;
		comma = 20; colon = 21; upto = 22; rparen = 23; rbrak = 24;
		rbrace = 25; of = 26; then = 27; do = 28; to = 29;
		by = 30; not = 33;
		lparen = 40; lbrak = 41; lbrace = 42; becomes = 44;
		number = 45; nil = 46; string = 47; ident = 48; semicolon = 49;
		bar = 50; end = 51; else = 52; elsif = 53; until = 54;
		if = 55; case = 56; while = 57; repeat = 58; for = 59;
		loop = 60; with = 61; exit = 62; return = 63; array = 64;
		record = 65; pointer = 66; begin = 67; const = 68; type = 69;
		var = 70; out = 71; procedure = 72; import = 74;
		module = 75; eof = 76;

		(* object modes *)
		Var = 1; VarPar = 2; Con = 3; Fld = 4; Typ = 5; LProc = 6; XProc = 7;
		SProc = 8; CProc = 9; IProc = 10; Mod = 11; Head = 12; TProc = 13;

		(* Structure forms *)
		Undef = 0; Byte = 1; Bool = 2; Char8 = 3; Int8 = 4; Int16 = 5; Int32 = 6;
		Real32 = 7; Real64 = 8; Set = 9; String8 = 10; NilTyp = 11; NoTyp = 12;
		Pointer = 13; ProcTyp = 14; Comp = 15;
		Char16 = 16; String16 = 17; Int64 = 18;
		intSet = {Int8..Int32, Int64}; realSet = {Real32, Real64}; charSet = {Char8, Char16};
		
		(* composite structure forms *)
		Basic = 1; Array = 2; DynArr = 3; Record = 4;

		(*function number*)
		haltfn = 0; newfn = 1; incfn = 13; sysnewfn = 30;
		entierfn = 5; chrfn = 9; shortfn = 10; longfn = 11; copyfn = 18; putfn = 25; putrfn = 27;

		(* nodes classes *)
		Nvar = 0; Nvarpar = 1; Nfield = 2; Nderef = 3; Nindex = 4; Nguard = 5; Neguard = 6;
		Nconst = 7; Ntype = 8; Nproc = 9; Nupto = 10; Nmop = 11; Ndop = 12; Ncall = 13;
		Ninittd = 14; Nif = 15; Ncaselse = 16; Ncasedo = 17; Nenter = 18; Nassign = 19;
		Nifelse = 20; Ncase = 21; Nwhile = 22; Nrepeat = 23; Nloop = 24; Nexit = 25;
		Nreturn = 26; Nwith = 27; Ntrap = 28;

		(* node subclasses *)
		super = 1;
		
		(* module visibility of objects *)
		internal = 0; external = 1; externalR = 2; inPar = 3; outPar = 4;

		(* procedure flags (conval.setval) *)
		hasBody = 1; isRedef = 2; slNeeded = 3; imVar = 4;
		
		(* attribute flags (attr.adr, struct.attribute, proc.conval.setval)*)
		newAttr = 16; absAttr = 17; limAttr = 18; empAttr = 19; extAttr = 20; asgnAttr = 21;
		
		(* case statement flags (conval.setval) *)
		useTable = 1; useTree = 2;
		
		(* sysflags *)
		nilBit = 1; inBit = 2; outBit = 4; newBit = 8; iidBit = 16; interface = 10; som = 20;
		
		(* patch kind *)
		large2long = 1; long2int = 2; int2short = 3; short2byte = 4; long2char = 5; char2short = 6;
		real2short = 7; long2real = 8; realExp = 9; intConst = 10; hexConst = 11; copy = 12;
		ccopy = 13; short = 14; lchr = 15; lentier = 16; varIn = 17; varOut = 18; anyrec = 19; anyptr = 20;
		meta = 21; metaAuto = 22; math = 23; stores = 24; storesAuto = 25; formatters = 26; attributes = 27;
		shortEntier = 28; append = 29; concat = 30; swap = 31;
		varInNil = 42; varOutNil = 43; varNew = 44; varIid = 45; delete = 46; pointerTo = 47; close = 48;
		conSC = 50; conC = 51; conB = 52; conSI = 53; conI = 54; conSR = 55; conR = 56; conSS = 57; conS = 58;
		procSC = 60; procC = 61; procB = 62; procSI = 63; procI = 64; procSR = 65; procR = 66; procSS = 67; procS = 68;
		procXC = 70; procXI = 71; procXR = 72; procXS = 73;
		services = 80; stdLog = 81; copyFrom = 82; readonly = 83; become = 84; setRect = 85; thisDomain = 86;
		insertCopy = 87; label = 88; notifierProc = 89; min = 90; max = 91; len = 92; stringOp = 93;
		item = 94; index = 95; dates = 96; selection = 97; list = 98; leftSide = 99; rightSide = 100; collapsed = 101;
		dir = 102; stop = 103; tabsTab = 104; lenFld = 105; copyOf = 106; store = 107; insert = 108;
		storesInitDomain = 109; commaSp = 110; domain = 111; cloneOf = 112; get = 113; propagateDomain = 114;
		propagate = 115; propagateMeth = 116;
		copy1 = 200; copy2 = 201; delete1 = 202;
		append1 = 203; append2 = 204; concat1 = 205; concat2 = 20; concat3 = 207; swap1 = 208;
		
	TYPE
		Patch* = POINTER TO EXTENSIBLE RECORD
			pos*: INTEGER;
			kind*, len*: SHORTINT;
			next*: Patch
		END;
		RecAttrPatch* = POINTER TO RECORD (Patch)
			typ*: CPT.Struct
		END;
		ProcAttrPatch* = POINTER TO RECORD (Patch)
			obj*: CPT.Object
		END;
		MethodPatch* = POINTER TO RECORD (Patch)
			typ*: CPT.Struct;
			needSc*: BOOLEAN
		END;
	
	VAR
		sym, level: BYTE;
		LoopLevel: SHORTINT;
		TDinit, lastTDinit: CPT.Node;
		nofFwdPtr: SHORTINT;
		FwdPtr: ARRAY 256 OF CPT.Struct;
		patchList*: Patch;
		changes: SET;
		impPos: INTEGER;
		rootObj: CPT.Object;
		strAppend, strConcat, strLen: CPT.Object;
		self: CPT.Object;
		

	PROCEDURE^ Type(VAR typ, banned: CPT.Struct);
	PROCEDURE^ Expression(VAR x: CPT.Node);
	PROCEDURE^ Block(VAR procdec, statseq: CPT.Node; VAR endpos: INTEGER);

	PROCEDURE Insert (pos, kind, len: INTEGER);
		VAR p: Patch;
	BEGIN
		NEW(p); p.pos := pos; p.kind := SHORT(kind); p.len := SHORT(len); p.next := patchList; patchList := p
	END Insert;
	
	PROCEDURE CheckedExpression (VAR x: CPT.Node; typ: CPT.Struct);
		VAR p: INTEGER;
	BEGIN
		p := CPM.startpos;
		Expression(x);
		IF (short IN changes) & (x.typ.form IN intSet + realSet + charSet) THEN
			IF (x.typ.form # typ.form) & CPT.Includes(x.typ.form, typ.form) THEN
				IF (x.class # Nconst) OR (x.typ.form = Real64) & (typ.form = Real32) THEN
					Insert(p, short, CPM.errpos - p);
					IF (x.typ.form = Int32) & (typ.form = Int8) THEN Insert(p, short, CPM.errpos - p) END
				END
			END
		END;
		IF typ.comp # DynArr THEN x.typ := typ END
	END CheckedExpression;
	
	PROCEDURE err(n: SHORTINT);
	BEGIN CPM.err(n)
	END err;

	PROCEDURE CheckSym(s: SHORTINT);
	BEGIN
		IF sym = s THEN CPS.Get(sym) ELSE CPM.err(s) END
	END CheckSym;

	PROCEDURE qualident(VAR id: CPT.Object);
		VAR obj, mod: CPT.Object; lev: BYTE; p, i: INTEGER; name: CPT.Name; done: BOOLEAN;
	BEGIN (*sym = ident*)
		IF CPS.name = "ANYRECORD" THEN
			IF anyrec IN changes THEN Insert(CPM.startpos, anyrec, 9); CPS.name := "ANYREC" END
		ELSIF CPS.name = "ANYPOINTER" THEN
			IF anyptr IN changes THEN Insert(CPM.startpos, anyptr, 10); CPS.name := "ANYPTR" END
		ELSIF CPS.name = "LCHR" THEN
			IF lchr IN changes THEN Insert(CPM.startpos, lchr, 4) END	(* don't change *)
		ELSIF CPS.name = "LENTIER" THEN
			IF lentier IN changes THEN Insert(CPM.startpos, lentier, 7) END	(* don't change *)
		END;
		CPT.Find(CPS.name, obj);
		IF (obj # NIL) & (obj.mode = Typ) THEN
			IF obj.typ = CPT.lint64typ THEN
				IF large2long IN changes THEN Insert(CPM.startpos, large2long, 8); obj := CPT.int64typ.strobj END
			ELSIF obj.typ = CPT.int64typ THEN
				IF long2int IN changes THEN Insert(CPM.startpos, long2int, 7); obj := CPT.int32typ.strobj END
			ELSIF obj.typ = CPT.int32typ THEN
				IF int2short IN changes THEN Insert(CPM.startpos, int2short, 7); obj := CPT.int16typ.strobj END
			ELSIF obj.typ = CPT.int16typ THEN
				IF short2byte IN changes THEN Insert(CPM.startpos, short2byte, 8); obj := CPT.int8typ.strobj END
			ELSIF obj.typ = CPT.lchar16typ THEN
				IF long2char IN changes THEN Insert(CPM.startpos, long2char, 8); obj := CPT.char16typ.strobj END
			ELSIF obj.typ = CPT.char16typ THEN
				IF char2short IN changes THEN Insert(CPM.startpos, char2short, 4); obj := CPT.char8typ.strobj END
			ELSIF obj.typ = CPT.lreal64typ THEN
				IF long2real IN changes THEN Insert(CPM.startpos, long2real, 8); obj := CPT.real64typ.strobj END
			ELSIF obj.typ = CPT.real64typ THEN
				IF real2short IN changes THEN Insert(CPM.startpos, real2short, 4); obj := CPT.real32typ.strobj END
			END
		END;
		p := CPM.startpos; CPS.Get(sym);
		IF (sym = period) & (obj # NIL) & (obj.mode = Mod) THEN
			CPS.Get(sym);
			IF sym = ident THEN
				IF (meta IN changes) & (obj.name^ = "Meta") THEN
					IF CPS.name = "charTyp" THEN
						IF (char2short IN changes) OR ~(metaAuto IN changes) THEN
							Insert(CPM.startpos, conSC, 4); CPS.name := "sCharTyp"
						END
					ELSIF CPS.name = "sintTyp" THEN 
						IF (short2byte IN changes) OR ~(metaAuto IN changes) THEN
							Insert(CPM.startpos, conB, 4); CPS.name := "byteTyp"
						END
					ELSIF CPS.name = "intTyp" THEN 
						IF (int2short IN changes) OR ~(metaAuto IN changes) THEN
							Insert(CPM.startpos, conSI, 3); CPS.name := "sIntTyp"
						END
					ELSIF CPS.name = "lintTyp" THEN 
						IF (long2int IN changes) OR ~(metaAuto IN changes) THEN
							Insert(CPM.startpos, conI, 4); CPS.name := "intTyp"
						END
					ELSIF CPS.name = "realTyp" THEN 
						IF (real2short IN changes) OR ~(metaAuto IN changes) THEN
							Insert(CPM.startpos, conSR, 4); CPS.name := "sRealTyp"
						END
					ELSIF CPS.name = "lrealTyp" THEN 
						IF (long2real IN changes) OR ~(metaAuto IN changes) THEN
							Insert(CPM.startpos, conR, 5); CPS.name := "realTyp"
						END
					END
				ELSIF obj.name^ = "Math" THEN
					IF (CPS.name[0] = "L") & (CPS.name[1] <= "Z") THEN
						IF (CPS.name # "LMin") & (CPS.name # "LMax") THEN Insert(CPM.startpos, delete, 1) END;
						i := 1;
						WHILE CPS.name[i] # 0X DO CPS.name[i-1] := CPS.name[i]; INC(i) END;
						CPS.name[i-1] := 0X
					END
				ELSIF (stores IN changes) & (obj.name^ = "Stores") THEN
					IF (CPS.name = "GetTypeName") OR (CPS.name = "SameType") THEN
						Insert(p, services, CPM.startpos - p);
						name := CPS.name; CPS.name := "Services"; CPT.Find(CPS.name, obj);
						IF obj = NIL THEN CPT.Import(CPS.name, CPS.name, done) END;
						CPT.Find(CPS.name, obj); CPS.name := name
					ELSIF CPS.name = "Clone" THEN
						(* Insert(CPM.startpos, copyOf, 5); *) CPS.name := "CopyOf"
					ELSIF CPS.name = "New" THEN
						CPS.name := "CopyOf"	(* compatible signature *)
					ELSIF CPS.name = "Elem" THEN
						Insert(CPM.startpos, store, 4); CPS.name := "Store"
					END
				ELSIF (obj.name^ = "Kernel") & (CPS.name = "InstallTerminator") THEN
					CPS.name := "InstallCleaner"	(* same signature *)
				ELSIF (obj.name^ = "Properties") & (CPS.name = "StorePref") THEN
					CPS.name := "TypePref"	(* same signature *)
				ELSIF (obj.name^ = "Models") & (CPS.name = "Clone") THEN
					Insert(CPM.startpos, cloneOf, 5); CPS.name := "CloneOf"
				ELSIF (obj.name^ = "TextModels") & (CPS.name = "Clone") THEN
					Insert(CPM.startpos, cloneOf, 5); CPS.name := "CloneOf"
				ELSIF (obj.name^ = "FormModels") & (CPS.name = "Clone") THEN
					Insert(CPM.startpos, cloneOf, 5); CPS.name := "CloneOf"
				ELSIF obj.name^ = "Dialog" THEN
					IF CPS.name = "CheckGuards" THEN
						Insert(p, delete, CPM.curpos - 1 - p);
						CPS.name := "Beep"	(* same signature *)
					ELSIF CPS.name = "NotifyProc" THEN
						Insert(CPM.startpos, notifierProc, 10);
						CPS.name := "NotifierProc"
					ELSIF CPS.name = "Enumeration" THEN
						Insert(CPM.startpos, list, 11);
						CPS.name := "List"
					ELSIF (CPS.name = "LargeSet") OR (CPS.name = "Style") THEN
						Insert(CPM.startpos, selection, CPM.curpos - 1 - CPM.startpos);
						CPS.name := "Selection"
					ELSIF (CPS.name = "Date") OR (CPS.name = "Time") THEN
						Insert(p, dates, CPM.startpos - p);
						name := CPS.name; CPS.name := "Dates"; CPT.Find(CPS.name, obj);
						IF obj = NIL THEN CPT.Import(CPS.name, CPS.name, done) END;
						CPT.Find(CPS.name, obj); CPS.name := name
					END
				ELSIF (obj.name^ = "StdFolds") & (CPS.name = "Init") THEN
					CPS.name := "Expand"	(* wrong but avoids errors *)
				ELSIF (obj.name^ = "StdLinks") & ((CPS.name = "GetLink") OR (CPS.name = "GetTarget")) THEN
					CPS.name := "CreateLink"	(* wrong but avoids errors *)
				ELSIF formatters IN changes THEN
					IF (obj.name^ = "Sub") OR (obj.name^ = "StdLog") THEN
						IF CPS.name = "LReal" THEN Insert(CPM.startpos, procR, 5); CPS.name := "Real"
						ELSIF CPS.name = "LChar" THEN Insert(CPM.startpos, procC, 5); CPS.name := "Char"
						ELSIF CPS.name = "LString" THEN Insert(CPM.startpos, procS, 7); CPS.name := "String"
						END
					ELSIF obj.name^ = "Strings" THEN
						IF CPS.name = "LongRealToString" THEN Insert(CPM.startpos, procR, 8); CPS.name := "RealToString" END
					ELSIF (obj.name^ = "TextModels") OR (obj.name^ = "TextSetters") THEN
						IF CPS.name = "WriteableLChar" THEN Insert(CPM.startpos + 9, procC, 5); CPS.name := "WriteableChar"
						ELSIF CPS.name = "LongString" THEN Insert(CPM.startpos, procS, 10); CPS.name := "String"
						END
					ELSIF obj.name^ = "TextMappers" THEN
						IF CPS.name = "lstring" THEN Insert(CPM.startpos, conS, 7); CPS.name := "string"
						ELSIF CPS.name = "lchar" THEN Insert(CPM.startpos, conC, 5); CPS.name := "char"
						ELSIF CPS.name = "LongString" THEN Insert(CPM.startpos, procS, 10); CPS.name := "String"
						END
					ELSIF obj.name^ = "Controllers" THEN
						IF CPS.name = "pasteLChar" THEN Insert(CPM.startpos + 5, procC, 5); CPS.name := "pasteChar" END
					ELSIF obj.name^ = "StdCmds" THEN
						IF CPS.name = "PasteLCharGuard" THEN
							Insert(CPM.startpos + 5, procC, 5); CPS.name := "PasteCharGuard"
						END
					END
				END;
				mod := obj;
				IF mod # NIL THEN CPT.FindImport(CPS.name, mod, obj) END;
				IF obj = NIL THEN
					IF CPS.name = "LONGCHAR" THEN
						Insert(p, delete, CPM.startpos - p); obj := CPT.char16typ.strobj
					ELSIF (mod.name^ = "Dialog")
						& ((CPS.name = "Length") OR (CPS.name = "Size") OR (CPS.name = "Weight") OR (CPS.name = "Units")) THEN
						Insert(p, long2int, CPM.curpos - p - 1); obj := CPT.int32typ.strobj
					ELSIF (mod.name^ = "Dialog") & (CPS.name = "Interactor") THEN
						(* Insert(p, anyrec, CPM.curpos - p - 1); *) obj := CPT.anytyp.strobj
					ELSIF (mod.name^ = "SqlDB") & (CPS.name = "Par") THEN
						Insert(p, anyptr, CPM.curpos - p - 1); obj := CPT.anyptrtyp.strobj
					ELSIF (stores IN changes) & (mod.name^ = "Domains") & (CPS.name = "Message") THEN
						Insert(p, anyrec, CPM.curpos - p - 1); obj := CPT.anytyp.strobj
					ELSIF (mod.name^ = "Math") & (CPS.name = "Min") THEN
						Insert(p, min, CPM.curpos - p - 1); CPS.name := "MIN"; CPT.Find(CPS.name, obj)
					ELSIF (mod.name^ = "Math") & (CPS.name = "Max") THEN
						Insert(p, max, CPM.curpos - p - 1); CPS.name := "MAX"; CPT.Find(CPS.name, obj)
					ELSIF (mod.name^ = "Strings") & (CPS.name = "Len") THEN
						Insert(p, len, CPM.curpos - p - 1); obj := strLen
					ELSIF (mod.name^ = "Strings") & (CPS.name = "Append") THEN
						obj := strAppend
					ELSIF (mod.name^ = "Strings") & (CPS.name = "Concat") THEN
						obj := strConcat
					ELSIF ((mod.name^ = "TextModels") OR (mod.name^ = "TextSetters"))
							& ((CPS.name = "char") OR (CPS.name = "lchar") OR (CPS.name = "unicode")
								OR (CPS.name = "view") OR (CPS.name = "string") OR (CPS.name = "lstring"))
							OR ((mod.name^ = "TextMappers") & (CPS.name = "returnLChars"))
							OR ((mod.name^ = "StdStamps") & (CPS.name = "dmy")) THEN
						obj := CPT.NewObj(); obj.mode := Con; obj.conval := CPT.NewConst();
						IF CPS.name = "unicode" THEN obj.typ := CPT.char16typ ELSE obj.typ := CPT.int32typ END
					ELSIF (mod.name^ = "StdLinks") & (CPS.name = "NewLink") THEN
						Insert(CPM.startpos, dir, 0);
						CPS.name := "dir"; CPT.FindImport(CPS.name, mod, obj);
						CPS.name := "NewLink"; CPT.FindField(CPS.name, obj.typ, mod);
						obj := CPT.NewObj(); obj^ := mod^; obj.mode := XProc; obj.link := obj.link.link; obj.adr := -77
					ELSIF (mod.name^ = "StdLinks") & (CPS.name = "NewTarget") THEN
						Insert(CPM.startpos, dir, 0);
						CPS.name := "dir"; CPT.FindImport(CPS.name, mod, obj);
						CPS.name := "NewTarget"; CPT.FindField(CPS.name, obj.typ, mod);
						obj := CPT.NewObj(); obj^ := mod^; obj.mode := XProc; obj.link := obj.link.link; obj.adr := -77
					ELSIF (mod.name^ = "StdLinks") & ((CPS.name = "IsLink") OR (CPS.name = "IsTarget")) THEN
						obj := CPT.NewObj(); obj.mode := XProc; obj.typ := CPT.booltyp
					ELSIF (mod.name^ = "StdFolds") & ((CPS.name = "Flip") OR (CPS.name = "FlipNested")
							OR (CPS.name = "Matching") OR (CPS.name = "HiddenText")) THEN
						name := CPS.name;
						CPS.name := "Fold"; CPT.FindImport(CPS.name, mod, obj);
						IF name = "Matching" THEN CPS.name := "MatchingFold" ELSE CPS.name := name END;
						CPT.FindField(CPS.name, obj.typ.BaseTyp, mod);
						obj := CPT.NewObj(); obj^ := mod^; obj.mode := XProc
					ELSIF (CPS.name[0] = "P") & (CPS.name[1] = "t") & (CPS.name[2] = "r") THEN
						i := 3; WHILE CPS.name[i] # 0X DO CPS.name[i-3] := CPS.name[i]; INC(i) END;
						CPS.name[i-3] := 0X;
						CPT.FindImport(CPS.name, mod, obj);
						IF (obj # NIL) & (obj.mode = Typ) THEN
							IF obj.typ.form # Pointer THEN Insert(CPM.startpos, pointerTo, 3)
							ELSE Insert(CPM.startpos, delete, 3)
							END
						END
					ELSE
						i := 0; WHILE CPS.name[i] # 0X DO INC(i) END;
						IF (i > 5) & (CPS.name[i-4] = "D") & (CPS.name[i-3] = "e") & (CPS.name[i-2] = "s") & (CPS.name[i-1] = "c") THEN
							CPS.name[i-4] := 0X; CPT.FindImport(CPS.name, mod, obj);
							IF obj # NIL THEN
								Insert(CPM.startpos + i - 4, delete, 4)
							END
						ELSE
							obj := CPT.NewObj(); obj.mode := Var; obj.typ := CPT.int32typ
						END
					END
				END;
				CPS.Get(sym)
			ELSE err(ident); obj := NIL
			END
		END ;
		IF obj = NIL THEN err(0);
			obj := CPT.NewObj(); obj.mode := Var; obj.typ := CPT.undftyp; obj.adr := 0
		ELSE lev := obj.mnolev;
			IF (obj.mode IN {Var, VarPar}) & (lev # level) THEN
				obj.leaf := FALSE;
				IF lev > 0 THEN CPB.StaticLink(SHORT(SHORT(level-lev)), TRUE) END	(* !!! *)
			END
		END ;
		id := obj
	END qualident;

	PROCEDURE ConstExpression(VAR x: CPT.Node);
	BEGIN Expression(x);
		IF x.class # Nconst THEN
			err(50); x := CPB.NewIntConst(1) 
		END
	END ConstExpression;

	PROCEDURE CheckMark(obj: CPT.Object; baseVis: BYTE);	(* !!! *)
	BEGIN CPS.Get(sym);
		IF (sym = times) OR (sym = minus) THEN
(*
			IF (level > 0) OR ~(obj.mode IN {Var, Fld}) & (sym = minus) THEN err(47) END ;
*)
			IF sym = times THEN obj.vis := external ELSE obj.vis := externalR END ;
			IF (baseVis = externalR) & (obj.vis = external) THEN
				Insert(CPM.startpos, readonly, 1); obj.vis := externalR
			END;
			CPS.Get(sym)
		ELSE obj.vis := internal
		END;
		IF (obj.mode IN {LProc, XProc, Var, Typ}) & (sym = lbrak) THEN
			CPS.Get(sym);
			IF sym = string THEN
				IF obj.conval = NIL THEN obj.conval := CPT.NewConst() END;
				IF CPS.str^ # "" THEN obj.conval.ext := CPS.str END;
				CPS.Get(sym);
				IF sym = comma THEN
					CPS.Get(sym);
					IF sym = string THEN
						IF obj.conval.ext # NIL THEN
							obj.conval.link := CPT.NewConst(); obj.conval.link.ext := obj.conval.ext
						END;
						obj.conval.ext := NIL;
						IF CPS.str^ # "" THEN obj.conval.ext := CPS.str END;
						CPS.Get(sym);
					ELSE err(string)
					END
				END
			ELSE err(string)
			END;
			CheckSym(rbrak)
		END
	END CheckMark;

	PROCEDURE CheckSysFlag (VAR sysflag: SHORTINT;
										GetSF: PROCEDURE(id: ARRAY OF SHORTCHAR; num: SHORTINT; VAR flag: SHORTINT));
		VAR x: CPT.Object; i: SHORTINT;
	BEGIN
		sysflag := 0;
		IF sym = lbrak THEN
			CPS.Get(sym);
			WHILE (sym = number) OR (sym = ident) OR (sym = string) DO
				IF sym = number THEN
					IF CPS.numtyp = integer THEN
						i := SHORT(CPS.intval); GetSF("", i, sysflag)
					ELSE err(225)
					END
				ELSIF sym = ident THEN
					CPT.Find(CPS.name, x);
					IF (x # NIL) & (x.mode = Con) & (x.typ.form IN {Int8, Int16, Int32}) THEN 
						i := SHORT(x.conval.intval); GetSF("", i, sysflag)
					ELSE
						GetSF(CPS.name, 0, sysflag)
					END
				ELSE
					GetSF(CPS.str^, 0, sysflag)
				END;
				CPS.Get(sym);
				IF (sym = comma) OR (sym = plus) THEN CPS.Get(sym) END
			END;
			CheckSym(rbrak)
		END
	END CheckSysFlag;
	
	PROCEDURE CheckAlloc (VAR typ: CPT.Struct);
	BEGIN
(*
		IF typ.comp = DynArr THEN err(88); typ := CPT.undftyp
		ELSIF typ = CPT.anytyp THEN err(89); typ := CPT.undftyp
		ELSIF (typ.comp = Record) & ((typ.sysflag = interface) OR (typ.sysflag = som)) THEN err(163); typ := CPT.undftyp
		END
*)
	END CheckAlloc;

	PROCEDURE RecordType(VAR typ, banned: CPT.Struct);
		VAR fld, first, last, base: CPT.Object; pos: INTEGER; needSc: BOOLEAN; p: RecAttrPatch; p0: INTEGER;
			ftyp: CPT.Struct;
	BEGIN typ := CPT.NewStr(Comp, Record); typ.BaseTyp := NIL;
		IF attributes IN changes THEN
			NEW(p); p.next := patchList; patchList := p;
			p.pos := CPM.startpos; p.kind := attributes; p.len := 0; 
			p.typ := typ
		END;
		CPS.Get(sym);
		CheckSysFlag(typ.sysflag, CPM.GetRecordSysFlag);
		IF typ.sysflag = interface THEN
			IF CPS.str[0] = "{" THEN typ.ext := CPS.str END;
			typ.attribute := absAttr
		END;
		pos := CPM.errpos; needSc := FALSE;
		IF sym = lparen THEN
			p0 := CPM.startpos;
			CPS.Get(sym); (*record extension*)
			IF sym = ident THEN
				qualident(base);
				IF base.mode = Typ THEN
					ftyp := base.typ;
					IF ftyp.form = Pointer THEN ftyp := ftyp.BaseTyp END;
					IF ftyp = CPT.anytyp THEN
						Insert(p0, delete, CPM.curpos - 1 - p0)
					ELSIF (ftyp.comp = Record) & (ftyp # CPT.anytyp) THEN
						IF ftyp = banned THEN err(58)
						ELSE ftyp.pvused := TRUE;
							typ.BaseTyp := ftyp; typ.extlev := SHORT(SHORT(ftyp.extlev + 1));
							CPM.PropagateRecordSysFlag(ftyp.sysflag, typ.sysflag);
							IF (ftyp.mno = 0) & (ftyp.attribute # absAttr) & (ftyp.attribute # asgnAttr) THEN ftyp.attribute := extAttr END
						END
					END
				ELSE err(52)
				END
			ELSE err(ident)
			END ;
			CheckSym(rparen);
			pos := CPM.errpos;
		END ;
		first := NIL; last := NIL;
		LOOP
			IF sym = ident THEN
				LOOP
					IF sym = ident THEN
						IF typ.BaseTyp # NIL THEN
							CPT.FindBaseField(CPS.name, typ, fld);
							IF fld # NIL THEN err(1) END
						END ;
						CPT.InsertField(CPS.name, typ, fld);
						fld.mode := Fld; fld.link := NIL; fld.typ := CPT.undftyp;
						CheckMark(fld, 0);
						IF first = NIL THEN first := fld END ;
						IF last = NIL THEN typ.link := fld ELSE last.link := fld END ;
						last := fld
					ELSE err(ident)
					END ;
					IF sym = comma THEN CPS.Get(sym)
					ELSIF sym = ident THEN err(comma)
					ELSE EXIT
					END
				END ;
				CheckSym(colon); Type(ftyp, banned);
				ftyp.pvused := TRUE;
				CheckAlloc(ftyp);
				WHILE first # NIL DO
					first.typ := ftyp; first := first.link
				END;
				pos := CPM.errpos; needSc := TRUE
			END ;
			IF sym = semicolon THEN CPS.Get(sym);
				pos := CPM.errpos; needSc := FALSE
			ELSIF sym = ident THEN err(semicolon)
			ELSE EXIT
			END
		END ;
		typ.untagged := typ.sysflag # 0;
(*
		IF method IN changes THEN
			NEW(p); p.next := patchList; patchList := p;
			p.pos := pos; p.kind := method; p.len := 0; 
			p.typ := typ; p.needSc := needSc
		END
*)
	END RecordType;

	PROCEDURE ArrayType(VAR typ, banned: CPT.Struct);
		VAR x: CPT.Node; n: INTEGER; sysflag: SHORTINT;
	BEGIN CheckSysFlag(sysflag, CPM.GetArraySysFlag);
		IF sym = of THEN	(*dynamic array*)
			typ := CPT.NewStr(Comp, DynArr); typ.mno := 0; typ.sysflag := sysflag;
			CPS.Get(sym); Type(typ.BaseTyp, banned);
			typ.BaseTyp.pvused := TRUE;
			IF typ.BaseTyp.comp = DynArr THEN typ.n := typ.BaseTyp.n + 1
			ELSE typ.n := 0
			END
		ELSE
			typ := CPT.NewStr(Comp, Array); typ.sysflag := sysflag; ConstExpression(x);
			IF x.typ.form IN {Int8, Int16, Int32} THEN n := x.conval.intval;
				IF (n <= 0) OR (n > CPM.MaxIndex) THEN err(63); n := 1 END
			ELSE err(51); n := 1
			END ;
			typ.n := n;
			IF sym = of THEN
				CPS.Get(sym); Type(typ.BaseTyp, banned);
				typ.BaseTyp.pvused := TRUE
			ELSIF sym = comma THEN
				CPS.Get(sym); IF sym # of THEN ArrayType(typ.BaseTyp, banned) END
			ELSE err(35)
			END ;
			CheckAlloc(typ.BaseTyp)
		END;
		typ.untagged := typ.sysflag # 0
	END ArrayType;

	PROCEDURE PointerType(VAR typ: CPT.Struct);
		VAR id: CPT.Object;
	BEGIN typ := CPT.NewStr(Pointer, Basic); CheckSysFlag(typ.sysflag, CPM.GetPointerSysFlag);
		CheckSym(to);
		IF sym = ident THEN CPT.Find(CPS.name, id);
			IF id = NIL THEN
				IF nofFwdPtr < LEN(FwdPtr) THEN FwdPtr[nofFwdPtr] := typ; INC(nofFwdPtr)
				ELSE err(224)
				END ;
				typ.link := CPT.NewObj(); typ.link.name := CPT.NewName(CPS.name);
				typ.BaseTyp := CPT.undftyp; CPS.Get(sym) (*forward ref*)
			ELSE qualident(id);
				IF id.mode = Typ THEN
					IF id.typ.comp IN {Array, DynArr, Record} THEN typ.BaseTyp := id.typ
					ELSE typ.BaseTyp := CPT.undftyp; err(57)
					END
				ELSE typ.BaseTyp := CPT.undftyp; err(52)
				END
			END
		ELSE Type(typ.BaseTyp, CPT.notyp);
			IF ~(typ.BaseTyp.comp IN {Array, DynArr, Record}) THEN
				typ.BaseTyp := CPT.undftyp; err(57)
			END
		END;
		IF typ.BaseTyp.comp = Record THEN CPM.PropagateRecPtrSysFlag(typ.BaseTyp.sysflag, typ.sysflag)
		ELSIF typ.BaseTyp.comp IN {Array, DynArr} THEN CPM.PropagateArrPtrSysFlag(typ.BaseTyp.sysflag, typ.sysflag)
		END;
		typ.untagged := typ.sysflag # 0
	END PointerType;
	
	PROCEDURE FormalParameters(VAR firstPar: CPT.Object; VAR resTyp: CPT.Struct; base: CPT.Object);
		VAR mode, vis: BYTE; sys: SHORTINT;
				par, first, last, res, bpar: CPT.Object; typ: CPT.Struct; p: INTEGER;
	BEGIN first := NIL; last := firstPar;
		IF (sym = ident) OR (sym = var) OR (sym = in) OR (sym = out) THEN
			IF base # NIL THEN
				bpar := base.link;
				IF base.mode = TProc THEN bpar := bpar.link END;	(* skip receiver *)
			END;
			LOOP
				sys := 0; vis := 0; p := CPM.startpos;
				IF sym = var THEN CPS.Get(sym); mode := VarPar
				ELSIF sym = in THEN CPS.Get(sym); mode := VarPar; vis := inPar
				ELSIF sym = out THEN CPS.Get(sym); mode := VarPar; vis := outPar
				ELSE mode := Var
				END ;
				IF mode = VarPar THEN CheckSysFlag(sys, CPM.GetVarParSysFlag) END;
				IF vis = 0 THEN
					IF ODD(sys DIV newBit) THEN
						vis := outPar;
						IF varOut IN changes THEN Insert(p, varNew, CPM.errpos - p) END;
					ELSIF ODD(sys DIV iidBit) THEN
						vis := inPar;
						IF varIn IN changes THEN Insert(p, varIid, CPM.errpos - p) END
					ELSIF ODD(sys DIV inBit) OR (bpar # NIL) & (bpar.vis = inPar) THEN
						vis := inPar;
						IF ODD(sys DIV nilBit) THEN
							IF varIn IN changes THEN Insert(p, varInNil, CPM.errpos - p) END
						ELSE
							IF varIn IN changes THEN Insert(p, varIn, CPM.errpos - p) END
						END
					ELSIF ODD(sys DIV outBit) OR (bpar # NIL) & (bpar.vis = outPar) THEN
						vis := outPar;
						IF ODD(sys DIV nilBit) THEN
							IF varOut IN changes THEN Insert(p, varOutNil, CPM.errpos - p) END
						ELSE
							IF varOut IN changes THEN Insert(p, varOut, CPM.errpos - p) END
						END
					END;
					IF vis # 0 THEN mode := VarPar END
				END;
				LOOP
					IF sym = ident THEN
						CPT.Insert(CPS.name, par); CPS.Get(sym);
						par.mode := mode; par.link := NIL; par.vis := vis; par.sysflag := SHORT(sys);
						IF first = NIL THEN first := par END ;
						IF firstPar = NIL THEN firstPar := par ELSE last.link := par END ;
						last := par;
						IF bpar # NIL THEN bpar := bpar.link END
					ELSE err(ident)
					END ;
					IF sym = comma THEN CPS.Get(sym)
					ELSIF sym = ident THEN err(comma)
					ELSIF sym = var THEN err(comma); CPS.Get(sym)
					ELSE EXIT
					END
				END ;
				CheckSym(colon); Type(typ, CPT.notyp);
				IF (mode # VarPar) & (typ.comp # DynArr) THEN CheckAlloc(typ) END;
				IF mode = Var THEN typ.pvused := TRUE END ;
				(* typ.pbused is set when parameter type name is parsed *)
				WHILE first # NIL DO first.typ := typ; first := first.link END ;
				IF sym = semicolon THEN CPS.Get(sym)
				ELSIF sym = ident THEN err(semicolon)
				ELSE EXIT
				END
			END
		END ;
		CheckSym(rparen);
		IF sym = colon THEN
			CPS.Get(sym);
			Type(resTyp, CPT.notyp);
			IF resTyp.form = Comp THEN resTyp := CPT.undftyp; err(54) END
		ELSE resTyp := CPT.notyp
		END
	END FormalParameters;

	PROCEDURE TypeDecl(VAR typ, banned: CPT.Struct);
		VAR id: CPT.Object;
	BEGIN typ := CPT.undftyp;
		IF sym < lparen THEN err(12);
			REPEAT CPS.Get(sym) UNTIL sym >= lparen
		END ;
		IF sym = ident THEN qualident(id);
			IF id.mode = Typ THEN
				IF id.typ # banned THEN typ := id.typ ELSE err(58) END
			ELSE err(52)
			END
		ELSIF sym = array THEN
			CPS.Get(sym); ArrayType(typ, banned)
		ELSIF sym = record THEN
			RecordType(typ, banned);
			CPB.Inittd(TDinit, lastTDinit, typ); CheckSym(end)
		ELSIF sym = pointer THEN
			CPS.Get(sym); PointerType(typ)
		ELSIF sym = procedure THEN
			CPS.Get(sym); typ := CPT.NewStr(ProcTyp, Basic);
			IF sym = lparen THEN
				CPS.Get(sym); CPT.OpenScope(level, NIL);
				FormalParameters(typ.link, typ.BaseTyp, NIL); CPT.CloseScope
			ELSE typ.BaseTyp := CPT.notyp; typ.link := NIL
			END
		ELSE err(12)
		END ;
		LOOP
			IF (sym >= semicolon) & (sym <= else) OR (sym = rparen) OR (sym = eof) OR (sym = number) THEN EXIT END;
			err(15); IF sym = ident THEN EXIT END;
			CPS.Get(sym)
		END
	END TypeDecl;
	
	PROCEDURE Type(VAR typ, banned: CPT.Struct);
	BEGIN TypeDecl(typ, banned);
		IF (typ.form = Pointer) & (typ.BaseTyp = CPT.undftyp) & (typ.strobj = NIL) THEN err(0) END
	END Type;

	PROCEDURE ActualParameters(VAR aparlist: CPT.Node; fpar: CPT.Object; exchange: BOOLEAN);
		VAR apar, last: CPT.Node; n, p1, p2, p3, p4: INTEGER;
	BEGIN aparlist := NIL; last := NIL;
		IF sym # rparen THEN n := 0;
			LOOP
				IF n = 0 THEN p1 := CPM.startpos
				ELSIF n = 1 THEN p3 := CPM.startpos
				END;
				IF (fpar = NIL) OR (fpar.mode = VarPar) THEN Expression(apar)
				ELSE
					CheckedExpression(apar, fpar.typ);
					IF (fpar.typ.form = String16) & (apar.class # Nconst) THEN Insert(CPM.errpos, stringOp, 0) END;
					IF fpar.typ.comp = Record THEN fpar.typ.attribute := asgnAttr END
				END;
				IF fpar # NIL THEN
					IF (apar.typ.form = Pointer) & (fpar.typ.form = Comp) THEN CPB.DeRef(apar) END;
					(* CPB.Param(apar, fpar); *) CPB.Link(aparlist, last, apar);
					fpar := fpar.link;
				(* ELSE err(64) *)
				END ;
				IF n = 0 THEN p2 := CPM.errpos
				ELSIF n = 1 THEN p4 := CPM.errpos;
					IF exchange THEN Insert(p2, delete, p4 - p2) END
				END;
				IF sym = comma THEN CPS.Get(sym)
				ELSIF (lparen <= sym) & (sym <= ident) THEN err(comma)
				ELSE EXIT
				END;
				INC(n)
			END
		END ;
		(* IF fpar # NIL THEN err(65) END *)
	END ActualParameters;
	
	PROCEDURE ThisType (mod, name: ARRAY OF SHORTCHAR): CPT.Struct;
		VAR i: INTEGER; old: CPT.Name; obj: CPT.Object;
	BEGIN
		i := 0;
		WHILE (i < CPT.nofGmod) & (CPT.GlbMod[i].name^ # mod) DO INC(i) END;
		IF i < CPT.nofGmod THEN
			old := CPS.name$; CPS.name := name$;
			rootObj.scope := CPT.GlbMod[i] (* .right *);
			CPT.FindImport(CPS.name, rootObj, obj);
			CPS.name := old$;
			IF obj # NIL THEN
				ASSERT(obj.mode = Typ);
				RETURN obj.typ
			END
		END;
		RETURN CPT.undftyp
	END ThisType;

	PROCEDURE IsType (typ: CPT.Struct; mod, name: ARRAY OF SHORTCHAR): BOOLEAN;
		VAR btyp: CPT.Struct;
	BEGIN
		btyp := ThisType(mod, name);
		IF btyp.form = Pointer THEN btyp := btyp.BaseTyp END;
		IF btyp # CPT.undftyp THEN RETURN CPT.Extends(typ, btyp)
		ELSE RETURN FALSE
		END
	END IsType;
	
	PROCEDURE selector(VAR x: CPT.Node; VAR deleteIt: BOOLEAN; VAR perPos: INTEGER);
		VAR obj, proc, fpar: CPT.Object; y, apar: CPT.Node; typ: CPT.Struct; name: CPT.Name; p, p0: INTEGER;
	BEGIN
		deleteIt := FALSE; perPos := 0;
		LOOP
			IF sym = lbrak THEN CPS.Get(sym);
				LOOP
					IF (x.typ # NIL) & (x.typ.form = Pointer) THEN CPB.DeRef(x) END ;
					Expression(y); CPB.Index(x, y);
					IF sym = comma THEN CPS.Get(sym) ELSE EXIT END
				END ;
				CheckSym(rbrak);
				IF (sym # period) & IsType(x.typ, "TextRulers", "Tab") THEN
					Insert(CPM.errpos, stop, 0);
					name := "stop"; CPT.FindField(name, x.typ, obj);
					CPB.Field(x, obj)
				END
			ELSIF sym = period THEN p0 := CPM.startpos; perPos := p0; CPS.Get(sym);
				IF sym = ident THEN name := CPS.name; p := CPM.startpos; CPS.Get(sym);
					IF x.typ # NIL THEN
						IF x.typ.form = Pointer THEN CPB.DeRef(x) END ;
						IF x.typ.comp = Record THEN
							typ := x.typ;
							IF IsType(typ, "Models",  "Model") & (name = "CopyAllFrom") THEN
								Insert(p, copyFrom, 11); name := "CopyFrom"
							ELSIF IsType(typ, "Stores", "Store") & (name = "InitDomain") THEN
								IF sym = arrow THEN Insert(p, propagateDomain, 10) END;
								name := "PropagateDomain"
							ELSIF IsType(typ, "TextModels",  "Model") THEN
								IF name = "CopyFrom" THEN Insert(p, insertCopy, 8); name := "InsertCopy"
								ELSIF name = "MoveFrom" THEN Insert(p, insert, 8); name := "Insert"
								END
							ELSIF IsType(typ, "Ports", "Rider") & (name = "Set") THEN
								 Insert(p, setRect, 3); name := "SetRect"
							ELSIF IsType(typ, "Dialog", "Par") & (name = "string") THEN
								Insert(p, label, 6); name := "label"
							ELSIF IsType(typ, "Dialog", "Combo") & (name = "name") THEN
								Insert(p, item, 4); name := "item"
							ELSIF IsType(typ, "Dialog", "List") & (name = "val") THEN
								Insert(p, index, 3); name := "index"
							ELSIF IsType(typ, "StdFolds", "Fold") & (name = "kind") THEN
								Insert(p, leftSide, 4); name := "leftSide"
							ELSIF IsType(typ, "StdFolds", "Fold") & (name = "state") THEN
								Insert(p, collapsed, 5); name := "collapsed"
							ELSIF IsType(typ, "TextRulers", "Tab") & (name = "val") THEN
								Insert(p0, stop, p + 3 - p0); name := "stop"
							ELSIF IsType(typ, "Views", "View") THEN
								IF name = "Background" THEN name := "ThisModel"
								ELSIF name = "NewFrame" THEN name := "ThisModel"
								END
							ELSIF IsType(typ, "Models", "Context") & (name = "ThisDomain") THEN
								IF (x.class = Nderef) & (x.left.class = Nfield) & IsType(x.left.left.typ, "Stores", "Store") THEN
									IF sym = lparen THEN CPS.Get(sym);
										IF sym = rparen THEN CPS.Get(sym) END
									END;
									Insert(p0 - 7, domain, CPM.errpos - p0 + 7)
								END
							ELSIF IsType(typ, "TextRulers", "Attributes") THEN
								IF name = "tab" THEN
									Insert(p, tabsTab, 3); name := "tabs"; CPT.FindField(name, x.typ, obj);
									CPB.Field(x, obj); name := "tab"
								ELSIF (sym # period) & (name = "tabs") THEN
									Insert(CPM.errpos, lenFld, 0); CPT.FindField(name, x.typ, obj);
									CPB.Field(x, obj); name := "len"
								ELSIF name = "ModifyFrom" THEN
									name := "ModifyFromProp"
								END
							ELSIF IsType(typ, "TextModels", "Attributes") & (name = "ModifyFrom") THEN
								name := "ModifyFromProp"
							ELSIF (stores IN changes) & IsType(typ, "Stores", "Reader") THEN
								IF name = "ReadChar" THEN
									IF char2short IN changes THEN
										Insert(p + 4, procSC, 4); name := "ReadSChar"
									ELSIF ~(storesAuto IN changes) THEN
										Insert(p + 4, procXC, 4); name := "ReadXChar"
									END
								ELSIF name = "ReadLChar" THEN
									IF (long2char IN changes) OR ~(storesAuto IN changes) THEN
										Insert(p + 4, procC, 5); name := "ReadChar"
									END
								ELSIF name = "ReadSInt" THEN
									IF (short2byte IN changes) OR ~(storesAuto IN changes) THEN
										Insert(p + 4, procB, 4); name := "ReadByte"
									END
								ELSIF name = "ReadInt" THEN
									IF int2short IN changes THEN
										Insert(p + 4, procSI, 3); name := "ReadSInt"
									ELSIF ~(storesAuto IN changes) THEN
										Insert(p + 4, procXI, 3); name := "ReadXInt"
									END
								ELSIF name = "ReadLInt" THEN
									IF (long2int IN changes) OR ~(storesAuto IN changes) THEN
										Insert(p + 4, procI, 4); name := "ReadInt"
									END
								ELSIF name = "ReadReal" THEN
									IF real2short IN changes THEN
										Insert(p + 4, procSR, 4); name := "ReadSReal"
									ELSIF ~(storesAuto IN changes) THEN
										Insert(p + 4, procXR, 4); name := "ReadXReal"
									END
								ELSIF name = "ReadLReal" THEN
									IF (long2real IN changes) OR ~(storesAuto IN changes) THEN
										Insert(p + 4, procR, 5); name := "ReadReal"
									END
								ELSIF name = "ReadString" THEN
									IF char2short IN changes THEN
										Insert(p + 4, procSS, 6); name := "ReadSString"
									ELSIF ~(storesAuto IN changes) THEN
										Insert(p + 4, procXS, 6); name := "ReadXString"
									END
								ELSIF name = "ReadLString" THEN
									IF (long2char IN changes) OR ~(storesAuto IN changes) THEN
										Insert(p + 4, procS, 7); name := "ReadString"
									END
								END
							ELSIF (stores IN changes) & IsType(typ, "Stores", "Writer") THEN
								IF name = "WriteChar" THEN
									IF char2short IN changes THEN
										Insert(p + 5, procSC, 4); name := "WriteSChar"
									ELSIF ~(storesAuto IN changes) THEN
										Insert(p + 5, procXC, 4); name := "WriteXChar"
									END
								ELSIF name = "WriteLChar" THEN
									IF (long2char IN changes) OR ~(storesAuto IN changes) THEN
										Insert(p + 5, procC, 5); name := "WriteChar"
									END
								ELSIF name = "WriteSInt" THEN
									IF (short2byte IN changes) OR ~(storesAuto IN changes) THEN
										Insert(p + 5, procB, 4); name := "WriteByte"
									END
								ELSIF name = "WriteInt" THEN
									IF int2short IN changes THEN
										Insert(p + 5, procSI, 3); name := "WriteSInt"
									ELSIF ~(storesAuto IN changes) THEN
										Insert(p + 5, procXI, 3); name := "WriteXInt"
									END
								ELSIF name = "WriteLInt" THEN
									IF (long2int IN changes) OR ~(storesAuto IN changes) THEN
										Insert(p + 5, procI, 4); name := "WriteInt"
									END
								ELSIF name = "WriteReal" THEN
									IF real2short IN changes THEN
										Insert(p + 5, procSR, 4); name := "WriteSReal"
									ELSIF ~(storesAuto IN changes) THEN
										Insert(p + 5, procXR, 4); name := "WriteXReal"
									END
								ELSIF name = "WriteLReal" THEN
									IF (long2real IN changes) OR ~(storesAuto IN changes) THEN
										Insert(p + 5, procR, 5); name := "WriteReal"
									END
								ELSIF name = "WriteString" THEN
									IF char2short IN changes THEN
										Insert(p + 5, procSS, 6); name := "WriteSString"
									ELSIF ~(storesAuto IN changes) THEN
										Insert(p + 5, procXS, 6); name := "WriteXString"
									END
								ELSIF name = "WriteLString" THEN
									IF (long2char IN changes) OR ~(storesAuto IN changes) THEN
										Insert(p + 5, procS, 7); name := "WriteString"
									END
								END
							ELSIF formatters IN changes THEN
								IF IsType(typ, "TextMappers", "Formatter") THEN
									IF name = "WriteLChar" THEN Insert(p + 5, procC, 5); name := "WriteChar"
									ELSIF name = "WriteLString" THEN Insert(p + 5, procS, 7); name := "WriteString"
									ELSIF name = "WriteLReal" THEN Insert(p + 5, procR, 5); name := "WriteReal"
									ELSIF (char2short IN changes) & (name = "WriteString") THEN
										Insert(p + 5, procSS, 6); name := "WriteSString"
									END
								ELSIF IsType(typ, "TextMappers", "Scanner") THEN
									IF name = "lchar" THEN Insert(p, conC, 5); name := "char"
									ELSIF name = "lstring" THEN Insert(p, conS, 7); name := "string"
									END
								ELSIF IsType(typ, "TextModels", "Reader") THEN
									IF name = "ReadLChar" THEN Insert(p + 4, procC, 5); name := "ReadChar"
									ELSIF name = "ReadPrevLChar" THEN Insert(p + 8, procC, 5); name := "ReadPrevChar"
									ELSIF name = "lchar" THEN Insert(p, conC, 5); name := "char"
									END
								ELSIF IsType(typ, "TextModels", "Writer") THEN
									IF name = "WriteLChar" THEN Insert(p + 5, procC, 5); name := "WriteChar"
									ELSIF (char2short IN changes) & (name = "WriteChar") THEN
										Insert(p + 5, procSC, 4); name := "WriteSChar"
									END
								ELSIF IsType(typ, "TextSetters", "Reader") THEN
									IF name = "lstring" THEN Insert(p, conS, 7); name := "string"
									ELSIF (char2short IN changes) & (name = "string") THEN
										Insert(p, conSS, 6); name := "sString"
									END
								ELSIF IsType(typ, "Ports", "Rider") OR IsType(typ, "Ports", "Frame") THEN
									IF name = "DrawLString" THEN Insert(p + 4, procS, 7); name := "DrawString"
									ELSIF name = "LCharIndex" THEN Insert(p, procC, 5); name := "CharIndex"
									ELSIF name = "LCharPos" THEN Insert(p, procC, 5); name := "CharPos"
									END
								ELSIF IsType(typ, "Controllers", "Controller") THEN
									IF name = "PasteLChar" THEN Insert(p + 5, procC, 5); name := "PasteChar" END
								ELSIF IsType(typ, "Views", "CtrlMessage") THEN
									IF name = "lchar" THEN Insert(p, conC, 5); name := "char" END
								ELSIF IsType(typ, "Fonts", "Font") THEN
									IF name = "LStringWidth" THEN Insert(p, procS, 7); name := "StringWidth" END
								END
							END;
							CPT.FindField(name, x.typ, obj);
							IF obj = NIL THEN
								obj := CPT.NewObj(); obj.mode := Fld;
								IF name = "type" THEN obj.typ := CPT.settyp
								ELSIF name = "typeface" THEN obj.typ := ThisType("Fonts", "Typeface")
								ELSIF name = "style" THEN obj.typ := CPT.settyp
								ELSIF name = "ThisDomain" THEN obj.typ := ThisType("Stores", "Domain");
									IF sym = lparen THEN CPS.Get(sym);
										IF sym = rparen THEN CPS.Get(sym) END
									END
								ELSE obj.typ := CPT.int32typ
								END
							END;
							CPB.Field(x, obj);
							IF (obj # NIL) & (obj.mode = TProc) THEN
								IF sym = arrow THEN  (* super call *) CPS.Get(sym);
									y := x.left;
									IF y.class = Nderef THEN y := y.left END ;	(* y = record variable *)
									IF y.obj # NIL THEN
										proc := CPT.topScope;	(* find innermost scope which owner is a TProc *)
										WHILE (proc.link # NIL) & (proc.link.mode # TProc) DO proc := proc.left END ;
										IF (proc.link = NIL) OR (proc.link.link # y.obj) THEN err(75) END ;
										typ := y.obj.typ;
										IF typ.form = Pointer THEN typ := typ.BaseTyp END ;
										CPT.FindBaseField(x.obj.name^, typ, proc);
										IF proc # NIL THEN x.subcl := super;
											IF proc.conval.setval * {absAttr, empAttr} # {} THEN
												deleteIt := TRUE
											END
										ELSE err(74)
										END
									ELSE err(75)
									END
								END ;
								IF (obj.typ # CPT.notyp) & (sym # lparen) THEN err(lparen) END
							END
						ELSIF (x.typ.form <= Set) & (name = "val") THEN
							Insert(p0, delete, CPM.errpos - p0)
						ELSE err(53)
						END
					ELSE err(52)
					END
				ELSE err(ident)
				END
			ELSIF sym = arrow THEN CPS.Get(sym); CPB.DeRef(x)
			ELSIF sym = dollar THEN
				IF x.typ.form = Pointer THEN CPB.DeRef(x) END;
				CPS.Get(sym); CPB.StrDeref(x)
			ELSIF sym = lparen THEN
				IF (x.obj # NIL) & (x.obj.mode IN {XProc, LProc, CProc, TProc}) THEN typ := x.obj.typ
				ELSIF x.typ.form = ProcTyp THEN typ := x.typ.BaseTyp
				ELSIF x.class = Nproc THEN EXIT	(* standard procedure *)
				ELSE typ := NIL
				END;
				IF typ # CPT.notyp THEN
					CPS.Get(sym);
					IF typ = NIL THEN	(* type guard *)
						IF sym = ident THEN
							qualident(obj);
							IF obj.mode = Typ THEN CPB.TypTest(x, obj, TRUE)
							ELSE err(52)
							END
						ELSE err(ident)
						END
					ELSE	(* function call *)
						CPB.PrepCall(x, fpar);
						ActualParameters(apar, fpar, (x.obj # NIL) & (x.obj.adr = -77));
						CPB.Call(x, apar, fpar);
						IF level > 0 THEN CPT.topScope.link.leaf := FALSE END
					END;
					CheckSym(rparen)
				ELSE EXIT
				END
			ELSE EXIT
			END
		END
	END selector;

	PROCEDURE StandProcCall(VAR x: CPT.Node);
		VAR y: CPT.Node; m, f: BYTE; n: SHORTINT; p, p1, p2, p3, p4: INTEGER;
	BEGIN m := SHORT(SHORT(x.obj.adr)); n := 0;
		IF sym = lparen THEN CPS.Get(sym);
			IF sym # rparen THEN
				LOOP
					p := CPM.startpos;
					IF n = 0 THEN
						Expression(x);
						p1 := p; p2 := CPM.errpos;
						IF (m = longfn) & (x.class = Nconst) & (x.typ.form = Int32) THEN
							Insert(p, delete1, CPM.errpos - p)
						END;
						IF m >= 0 THEN CPB.StPar0(x, m) END;
						n := 1
					ELSIF n = 1 THEN
						Expression(y);
						IF (m = copyfn) & (copy IN changes) THEN
							Insert(p1, copy1, p2 - p1);
							Insert(p, copy2, CPM.errpos - p);
							x.typ := y.typ
						ELSIF m = -1 THEN
							Insert(p1, append1, p2 - p1);
							Insert(p, append2, CPM.errpos - p)
						ELSIF m = -2 THEN
							p3 := p; p4 := CPM.errpos;
						ELSIF ((m = putfn) OR (m = putrfn)) & (y.class = Nconst) & (y.typ.form = Int32) THEN
							IF (y.conval.intval >= -128) & (y.conval.intval <= 127) THEN f := SHORT(SHORT(Int8 + y.hint))
							ELSIF (y.conval.intval >= -32768) & (y.conval.intval <= 32767) THEN f := SHORT(SHORT(Int16 + y.hint))
							ELSE f := Int32
							END;
							IF f = Int8 THEN Insert(p, short, CPM.errpos - p); Insert(p, short, CPM.errpos - p);
							ELSIF f = Int16 THEN Insert(p, short, CPM.errpos - p)
							END
						END;
						IF m >= 0 THEN CPB.StPar1(x, y, m) END;
						n := 2
					ELSE
						Expression(y);
						IF (n = 2) & (m = -2) THEN
							Insert(p1, concat1, p2 - p1);
							Insert(p3, concat2, p4 - p3);
							Insert(p, concat3, CPM.errpos - p)
						END;
						IF m >= 0 THEN CPB.StParN(x, y, m, n) END;
						INC(n)
					END ;
					IF sym = comma THEN CPS.Get(sym)
					ELSIF (lparen <= sym) & (sym <= ident) THEN err(comma)
					ELSE EXIT
					END
				END ;
				CheckSym(rparen)
			ELSE CPS.Get(sym)
			END ;
			IF m >= 0 THEN CPB.StFct(x, m, n)
			ELSE x.typ := CPT.notyp
			END
		ELSE err(lparen)
		END ;
		IF (level > 0) & ((m = newfn) OR (m = sysnewfn)) THEN CPT.topScope.link.leaf := FALSE END
	END StandProcCall;
	
	PROCEDURE Element(VAR x: CPT.Node);
		VAR y: CPT.Node;
	BEGIN Expression(x);
		IF sym = upto THEN
			CPS.Get(sym); Expression(y); CPB.SetRange(x, y)
		ELSE CPB.SetElem(x)
		END
	END Element;

	PROCEDURE Sets(VAR x: CPT.Node);
		VAR y: CPT.Node;
	BEGIN
		IF sym # rbrace THEN
			Element(x);
			LOOP
				IF sym = comma THEN CPS.Get(sym)
				ELSIF (lparen <= sym) & (sym <= ident) THEN err(comma)
				ELSE EXIT
				END ;
				Element(y); CPB.Op(plus, x, y)
			END
		ELSE x := CPB.EmptySet()
		END ;
		CheckSym(rbrace)
	END Sets;
	
	PROCEDURE Factor(VAR x: CPT.Node);
		VAR fpar, id: CPT.Object; apar: CPT.Node; p, m: INTEGER; d: BOOLEAN;
	BEGIN
		IF sym < not THEN err(13);
			REPEAT CPS.Get(sym) UNTIL sym >= lparen
		END ;
		IF sym = ident THEN
			p := CPM.startpos;
			qualident(id); x := CPB.NewLeaf(id); selector(x, d, m);
			IF (x.class = Nproc) & (x.obj.mode = SProc) THEN
				m := x.obj.adr; StandProcCall(x);	(* x may be NIL *)
				IF m = entierfn THEN
					IF long2int IN changes THEN Insert(p, short, CPM.errpos - p); x.typ := CPT.int32typ END
				ELSIF m = chrfn THEN
					IF char2short IN changes THEN Insert(p, short, CPM.errpos - p); x.typ := CPT.char8typ END
				ELSIF (m = longfn) & (x.class = Nconst) & (x.typ.form = Int64) THEN
					IF (x.hint = 0) & (x.conval.intval >= -32768) & (x.conval.intval <= 32767) 
							OR (x.hint = 1) & (x.conval.intval >= -128) & (x.conval.intval <= 127) THEN
						Insert(p, delete, CPM.errpos - p); x.typ := CPT.int32typ; INC(x.hint)
					END
				END
			ELSIF (x.class = Ncall) & (x.left.class = Nproc) & (x.left.obj.mnolev < 0)
					& (CPT.GlbMod[-x.left.obj.mnolev].name^ = "Math")
					& ((x.left.obj.name^ = "Floor") OR (x.left.obj.name^ = "Ceiling")) THEN
				Insert(p, shortEntier, CPM.errpos - p); x.typ := CPT.int32typ
			END
		ELSIF sym = number THEN
			CASE CPS.numtyp OF
			   char:
				x := CPB.NewIntConst(CPS.intval); x.typ := CPT.char8typ;
				IF CPS.intval > 255 THEN x.typ := CPT.char16typ END
			| integer: x := CPB.NewIntConst(CPS.intval)
			| int64: x := CPB.NewLargeIntConst(CPS.intval, CPS.realval);
				IF CPM.searchpos > 0 THEN
					IF realExp IN changes THEN Insert(CPM.searchpos, delete, 1) END;
					CPM.searchpos := 0
				END
			| real: x := CPB.NewRealConst(CPS.realval, NIL);
				IF CPM.searchpos > 0 THEN
					IF realExp IN changes THEN Insert(CPM.searchpos, realExp, 1) END;
					CPM.searchpos := 0
				END
			END;
			CPS.Get(sym)
		ELSIF sym = string THEN
			x := CPB.NewString(CPS.str, CPS.lstr, CPS.intval);
			CPS.Get(sym)
		ELSIF sym = nil THEN
			x := CPB.Nil(); CPS.Get(sym)
		ELSIF sym = lparen THEN
			CPS.Get(sym); Expression(x); CheckSym(rparen)
		ELSIF sym = lbrak THEN
			CPS.Get(sym); err(lparen); Expression(x); CheckSym(rparen)
		ELSIF sym = lbrace THEN CPS.Get(sym); Sets(x)
		ELSIF sym = not THEN
			CPS.Get(sym); Factor(x); CPB.MOp(not, x)
		ELSE err(13); CPS.Get(sym); x := NIL
		END ;
		IF x = NIL THEN x := CPB.NewIntConst(1); x.typ := CPT.undftyp END
	END Factor;

	PROCEDURE Term(VAR x: CPT.Node);
		VAR y: CPT.Node; mulop: BYTE;
	BEGIN Factor(x);
		WHILE (times <= sym) & (sym <= and) DO
			mulop := sym; CPS.Get(sym);
			Factor(y); CPB.Op(mulop, x, y)
		END
	END Term;

	PROCEDURE SimpleExpression(VAR x: CPT.Node);
		VAR y: CPT.Node; addop: BYTE;
	BEGIN
		IF sym = minus THEN CPS.Get(sym); Term(x); CPB.MOp(minus, x)
		ELSIF sym = plus THEN CPS.Get(sym); Term(x); CPB.MOp(plus, x)
		ELSE Term(x)
		END ;
		WHILE (plus <= sym) & (sym <= or) DO
			addop := sym; CPS.Get(sym); Term(y); 
			IF (x.typ.comp IN {Array, DynArr}) & (x.typ.BaseTyp.form IN charSet) THEN CPB.StrDeref(x) END;
			IF (y.typ.comp IN {Array, DynArr}) & (y.typ.BaseTyp.form IN charSet) THEN CPB.StrDeref(y) END;
			CPB.Op(addop, x, y)
		END
	END SimpleExpression;

	PROCEDURE Expression(VAR x: CPT.Node);
		VAR y: CPT.Node; obj: CPT.Object; relation: BYTE;
	BEGIN SimpleExpression(x);
		IF (eql <= sym) & (sym <= geq) THEN
			relation := sym; CPS.Get(sym); SimpleExpression(y);
			IF (x.typ.comp IN {Array, DynArr}) & (x.typ.BaseTyp.form IN charSet) THEN CPB.StrDeref(x) END;
			IF (y.typ.comp IN {Array, DynArr}) & (y.typ.BaseTyp.form IN charSet) THEN CPB.StrDeref(y) END;
			CPB.Op(relation, x, y)
		ELSIF sym = in THEN
			CPS.Get(sym); SimpleExpression(y); CPB.In(x, y)
		ELSIF sym = is THEN
			CPS.Get(sym);
			IF sym = ident THEN
				qualident(obj);
				IF obj.mode = Typ THEN CPB.TypTest(x, obj, FALSE)
				ELSE err(52)
				END
			ELSE err(ident)
			END
		END
	END Expression;

	PROCEDURE Receiver(VAR mode, vis: BYTE; VAR name: CPT.Name; VAR typ, rec: CPT.Struct);
		VAR obj: CPT.Object;
	BEGIN typ := CPT.undftyp; rec := NIL; vis := 0;
		IF sym = var THEN CPS.Get(sym); mode := VarPar;
		ELSIF sym = in THEN CPS.Get(sym); mode := VarPar; vis := inPar
		ELSE mode := Var
		END ;
		name := CPS.name; CheckSym(ident); CheckSym(colon);
		IF sym = ident THEN CPT.Find(CPS.name, obj); CPS.Get(sym);
			IF obj = NIL THEN err(0)
			ELSIF obj.mode # Typ THEN err(72)
			ELSE typ := obj.typ; rec := typ;
				IF rec.form = Pointer THEN rec := rec.BaseTyp END ;
				IF ~((mode = Var) & (typ.form = Pointer) & (rec.comp = Record) OR
					(mode = VarPar) & (typ.comp = Record)) THEN err(70); rec := NIL END ;
				IF (rec # NIL) & (rec.mno # level) THEN err(72); rec := NIL END
			END
		ELSE err(ident)
		END ;
		CheckSym(rparen);
		IF rec = NIL THEN rec := CPT.NewStr(Comp, Record); rec.BaseTyp := NIL END
	END Receiver;
	
	PROCEDURE ProcedureDeclaration(VAR x: CPT.Node);
		VAR proc, fwd: CPT.Object;
			name: CPT.Name;
			mode: BYTE;
			forward: BOOLEAN;
			sys: SHORTINT;
			p: INTEGER;

		PROCEDURE GetCode;
			VAR ext: CPT.ConstExt; i, n, c: INTEGER; s: ARRAY 256 OF SHORTCHAR;
		BEGIN
			n := 0;
			LOOP
				IF sym = number THEN c := CPS.intval; INC(n);
					IF (c < 0) OR (c > 255) OR (n = 255) THEN
						err(64); c := 1; n := 1
					END ;
					CPS.Get(sym); s[n] := SHORT(CHR(c))
				END ;
				IF sym = comma THEN CPS.Get(sym)
				ELSIF sym = number THEN err(comma)
				ELSE s[0] := SHORT(CHR(n)); EXIT
				END
			END;
			NEW(ext, n + 1); proc.conval.ext := ext; i := 0;
			WHILE i <= n DO ext[i] := s[i]; INC(i) END;
			INCL(proc.conval.setval, hasBody)
		END GetCode;

		PROCEDURE GetParams (base: CPT.Object);
		BEGIN
			proc.mode := mode; proc.typ := CPT.notyp;
			proc.sysflag := SHORT(sys);
			proc.conval.setval := {};
			IF sym = lparen THEN
				IF base = NIL THEN base := fwd END;
				CPS.Get(sym); FormalParameters(proc.link, proc.typ, base)
			END ;
			IF fwd # NIL THEN
(*
				CPB.CheckParameters(proc.link, fwd.link, TRUE);
				IF ~CPT.EqualType(proc.typ, fwd.typ) THEN err(117) END ;
*)
				proc := fwd; CPT.topScope := proc.scope;
				IF mode = IProc THEN proc.mode := IProc END
			END
		END GetParams;

		PROCEDURE Body (VAR pos: INTEGER);
			VAR procdec, statseq: CPT.Node; c, e: INTEGER;
		BEGIN
			c := CPM.errpos;
			INCL(proc.conval.setval, hasBody);
			CheckSym(semicolon); pos := CPM.startpos; Block(procdec, statseq, e);
			CPB.Enter(procdec, statseq, proc); x := procdec;
			x.conval := CPT.NewConst(); x.conval.intval := c; x.conval.intval2 := e;
			IF sym = ident THEN
				IF (CPS.name = "CopyAllFrom") & (proc.name^ = "CopyFrom") THEN
					Insert(CPM.startpos, copyFrom, 11); name := "CopyFrom"
				ELSIF (CPS.name = "InitDomain") & (proc.name^ = "PropagateDomain") THEN
					Insert(CPM.startpos, propagateDomain, 10); name := "PropagateDomain"
				END;
(*
				IF CPS.name # proc.name^ THEN err(4) END ;
*)
				CPS.Get(sym)
			ELSE err(ident)
			END
		END Body;

		PROCEDURE TProcDecl;
			VAR baseProc, o, bo, oldSelf: CPT.Object;
				objTyp, recTyp: CPT.Struct;
				objMode, objVis: BYTE;
				objName: CPT.Name;
				p: ProcAttrPatch; pos: INTEGER;
		BEGIN
			CPS.Get(sym); mode := TProc;
			IF level > 0 THEN err(73) END ;
			Receiver(objMode, objVis, objName, objTyp, recTyp);
			IF sym = ident THEN
				name := CPS.name;
				IF (name = "CopyAllFrom") & IsType(recTyp, "Models", "Model") THEN
					Insert(CPM.startpos, copyFrom, 11); name := "CopyFrom"
				ELSIF (name = "Background") & IsType(recTyp, "Views", "View") THEN
					Insert(CPM.startpos, get, 0)
				ELSIF (name = "NewFrame") & IsType(recTyp, "Views", "View") THEN
					Insert(CPM.startpos, get, 0)
				ELSIF (name = "InitDomain") & IsType(recTyp, "Stores", "Store") THEN
					Insert(CPM.startpos, propagateDomain, 10); name := "PropagateDomain"
				END;
				CPT.FindField(name, recTyp, fwd);
				CPT.FindBaseField(name, recTyp, baseProc);
				IF (baseProc # NIL) & (baseProc.mode # TProc) THEN baseProc := NIL END ;
				IF fwd = baseProc THEN fwd := NIL END ;
				IF (fwd # NIL) & (fwd.mnolev # level) THEN fwd := NIL END ;
				IF (fwd # NIL) & (fwd.mode = TProc) & ~(hasBody IN fwd.conval.setval) THEN
					(* there exists a corresponding forward declaration *)
					proc := CPT.NewObj(); proc.leaf := TRUE;
					proc.mode := TProc; proc.conval := CPT.NewConst();
					IF baseProc # NIL THEN CheckMark(proc, baseProc.vis) ELSE CheckMark(proc, 0) END;
(*
					IF fwd.vis # proc.vis THEN err(118) END
*)
				ELSE
					IF fwd # NIL THEN err(1); fwd := NIL END ;
					CPT.InsertField(name, recTyp, proc);
					proc.mode := TProc; proc.conval := CPT.NewConst();
					IF baseProc # NIL THEN CheckMark(proc, baseProc.vis) ELSE CheckMark(proc, 0) END;
					IF recTyp.strobj = NIL THEN recTyp.strobj := CPT.NewObj() END;
					IF recTyp.strobj # NIL THEN
						o := recTyp.strobj.link;
						IF o = NIL THEN recTyp.strobj.link := proc
						ELSE
							WHILE o.nlink # NIL DO o := o.nlink END;
							o.nlink := proc
						END
					END
				END ;
				INC(level); CPT.OpenScope(level, proc);
				CPT.Insert(objName, proc.link); proc.link.mode := objMode; proc.link.vis := objVis; proc.link.typ := objTyp;
				GetParams(baseProc);
				IF baseProc # NIL THEN
					IF (objMode # baseProc.link.mode) OR (objVis # baseProc.link.vis)
						OR ~CPT.Extends(objTyp, baseProc.link.typ) THEN err(115) END ;
					o := proc.link; bo := baseProc.link;
					WHILE (o # NIL) & (bo # NIL) DO
						IF (bo.sysflag # 0) & (o.sysflag = 0)
								OR ODD(bo.sysflag DIV nilBit) & (bo.sysflag = o.sysflag + nilBit) THEN
							o.sysflag := bo.sysflag
						END;
						o := o.link; bo := bo.link
					END;
(*
					CPB.CheckParameters(proc.link.link, baseProc.link.link, FALSE);
					IF ~CPT.Extends(proc.typ, baseProc.typ) THEN err(117) END;
*)
					IF (baseProc.vis = external) & (proc.vis = internal) &
						(recTyp.strobj # NIL) & (recTyp.strobj.vis = external) THEN err(109)
					END ;
					INCL(proc.conval.setval, isRedef);
					IF baseProc.mnolev >= 0 THEN INCL(baseProc.conval.setval, extAttr) END
				ELSE INCL(proc.conval.setval, newAttr)
				END ;
				IF attributes IN changes THEN
					NEW(p); p.next := patchList; patchList := p;
					p.pos := CPM.errpos; p.kind := attributes; p.len := 0; 
					p.obj := proc
				END;
				IF (proc.vis # internal) & (recTyp.attribute # asgnAttr) THEN
					INCL(proc.conval.setval, extAttr);
					IF recTyp.attribute # absAttr THEN recTyp.attribute := extAttr END
				END;
				IF ~forward THEN
					oldSelf := self;
					IF (name = "PropagateDomain") & IsType(recTyp, "Views", "View") THEN self := proc.link END;
					Body(pos);
					self := oldSelf;
					IF x.right = NIL THEN	(* empty body *)
						IF recTyp.sysflag = interface THEN	(* COM interface *)
							INCL(proc.conval.setval, absAttr); Insert(pos, delete, CPM.curpos - pos - 1);
						ELSIF ((baseProc # NIL) OR (recTyp.attribute = absAttr) OR (recTyp.attribute = extAttr))
								& ((baseProc = NIL) OR (baseProc.conval.setval * {absAttr, empAttr} # {})) THEN
							INCL(proc.conval.setval, empAttr); Insert(pos, delete, CPM.curpos - pos - 1)
						END
					ELSIF (x.right.class = Ntrap) & (x.right.right.class = Nconst)
							& (x.right.right.conval.intval = 127) & (x.right.link = NIL) THEN	(* abstract *)
						IF (baseProc = NIL) OR (absAttr IN baseProc.conval.setval) THEN
							INCL(proc.conval.setval, absAttr); Insert(pos, delete, CPM.curpos - pos - 1);
							IF recTyp.attribute # asgnAttr THEN recTyp.attribute := absAttr END
						END
					END;
					IF (name = "ThisModel") & IsType(recTyp, "Views", "View") THEN
						NEW(p); p.next := patchList; patchList := p;
						p.pos := CPM.curpos - 1; p.kind := propagateMeth; p.len := 0; 
						p.obj := proc.link;
					END
				END ;
				DEC(level); CPT.CloseScope
			ELSE err(ident)
			END
		END TProcDecl;
	
	BEGIN proc := NIL; forward := FALSE; x := NIL; mode := LProc; sys := 0;
		IF (sym # ident) & (sym # lparen) THEN
			CheckSysFlag(sys, CPM.GetProcSysFlag);
			IF sys # 0 THEN
				IF sys = CPM.CProcFlag THEN mode := CProc END
			ELSE
				IF sym = times THEN	(* mode set later in CPB.CheckAssign *)
				ELSIF sym = arrow THEN forward := TRUE
				ELSE err(ident)
				END;
				CPS.Get(sym)
			END
		END ;
		IF sym = lparen THEN TProcDecl
		ELSIF sym = ident THEN CPT.Find(CPS.name, fwd);
			name := CPS.name;
			IF (fwd # NIL) & ((fwd.mnolev # level) OR (fwd.mode = SProc)) THEN fwd := NIL END ;
			IF (fwd # NIL) & (fwd.mode IN {LProc, XProc}) & ~(hasBody IN fwd.conval.setval) THEN
				(* there exists a corresponding forward declaration *)
				proc := CPT.NewObj(); proc.leaf := TRUE;
				proc.mode := mode; proc.conval := CPT.NewConst();
				CheckMark(proc, 0);
				IF fwd.vis # proc.vis THEN err(118) END
			ELSE
				IF fwd # NIL THEN err(1); fwd := NIL END ;
				CPT.Insert(name, proc);
				proc.mode := mode; proc.conval := CPT.NewConst();
				CheckMark(proc, 0);
			END ;
			IF (proc.vis # internal) & (mode = LProc) THEN mode := XProc END ;
			IF (mode # LProc) & (level > 0) THEN err(73) END ;
			INC(level); CPT.OpenScope(level, proc);
			proc.link := NIL; GetParams(NIL);
			IF mode = CProc THEN GetCode
			ELSIF ~forward THEN Body(p);
				IF CPM.interface IN CPM.options THEN
					Insert(p, delete, CPM.curpos - p - 1)
				END
			END ;
			DEC(level); CPT.CloseScope
		ELSE err(ident)
		END
	END ProcedureDeclaration;

	PROCEDURE CaseLabelList(VAR lab, root: CPT.Node; LabelForm: SHORTINT; VAR min, max: INTEGER);
		VAR x, y, lastlab: CPT.Node; i, f: SHORTINT; xval, yval: INTEGER;
		
		PROCEDURE Insert(VAR n: CPT.Node);	(* build binary tree of label ranges *)	(* !!! *)
		BEGIN
			IF n = NIL THEN
				IF x.hint # 1 THEN n := x END
			ELSIF yval < n.conval.intval THEN Insert(n.left)
			ELSIF xval > n.conval.intval2 THEN Insert(n.right)
			ELSE err(63)
			END
		END Insert;
		
	BEGIN lab := NIL; lastlab := NIL;
		LOOP ConstExpression(x); f := x.typ.form;
			IF f IN intSet + charSet THEN  xval := x.conval.intval
			ELSE err(61); xval := 1
			END ;
			IF (f IN intSet) # (LabelForm IN intSet) THEN err(60) END;
			IF sym = upto THEN
				CPS.Get(sym); ConstExpression(y); yval := y.conval.intval;
				IF (y.typ.form IN intSet) # (LabelForm IN intSet) THEN err(60) END;
				IF yval < xval THEN err(63); yval := xval END
			ELSE yval := xval
			END ;
			x.conval.intval2 := yval;
			IF xval < min THEN min := xval END;
			IF yval > max THEN max := yval END;
			IF lab = NIL THEN lab := x; Insert(root)
			ELSIF yval < lab.conval.intval - 1 THEN x.link := lab; lab := x; Insert(root)
			ELSIF yval = lab.conval.intval - 1 THEN x.hint := 1; Insert(root); lab.conval.intval := xval
			ELSIF xval = lab.conval.intval2 + 1 THEN x.hint := 1; Insert(root); lab.conval.intval2 := yval
			ELSE
				y := lab;
				WHILE (y.link # NIL) & (xval > y.link.conval.intval2 + 1) DO y := y.link END;
				IF y.link = NIL THEN y.link := x; Insert(root)
				ELSIF yval < y.link.conval.intval - 1 THEN x.link := y.link; y.link := x; Insert(root)
				ELSIF yval = y.link.conval.intval - 1 THEN x.hint := 1; Insert(root); y.link.conval.intval := xval
				ELSIF xval = y.link.conval.intval2 + 1 THEN x.hint := 1; Insert(root); y.link.conval.intval2 := yval
				END
			END;
			IF sym = comma THEN CPS.Get(sym)
			ELSIF (sym = number) OR (sym = ident) THEN err(comma)
			ELSE EXIT
			END
		END
	END CaseLabelList;
	
	PROCEDURE Evaluate (n: CPT.Node; VAR min, max, num, dist: INTEGER; VAR head: CPT.Node);
		VAR a, b: INTEGER;
	BEGIN
		a := MIN(INTEGER); b := MAX(INTEGER);
		IF n.left # NIL THEN
			a := MIN(INTEGER); Evaluate(n.left, min, a, num, dist, head);
			IF n.conval.intval - a > dist THEN dist := n.conval.intval - a; head := n END
		ELSIF n.conval.intval < min THEN
			min := n.conval.intval
		END;
		IF n.right # NIL THEN
			a := MAX(INTEGER); Evaluate(n.right, a, max, num, dist, head);
			IF a - n.conval.intval2 > dist THEN dist := a - n.conval.intval2; head := n END
		ELSIF n.conval.intval2 > max THEN
			max := n.conval.intval2
		END;
		INC(num);
		IF n.conval.intval < n.conval.intval2 THEN
			INC(num);
			IF n.conval.intval2 - n.conval.intval > dist THEN dist := n.conval.intval2 - n.conval.intval; head := n END
		END
	END Evaluate;
	
	PROCEDURE Rebuild (VAR root: CPT.Node; head: CPT.Node);
		VAR n: CPT.Node;
	BEGIN
		IF root # head THEN
			IF head.conval.intval2 < root.conval.intval THEN
				Rebuild(root.left, head);
				n := head; WHILE n.right # NIL DO n := n.right END;
				n.right := root; root.left := NIL; root := head
			ELSE
				Rebuild(root.right, head);
				n := head; WHILE n.left # NIL DO n := n.left END;
				n.left := root; root.right := NIL; root := head
			END
		END
	END Rebuild;
	
	PROCEDURE Optimize (VAR n: CPT.Node);
		VAR min, max, num, dist: INTEGER; head: CPT.Node;
	BEGIN
		IF n # NIL THEN
			min := MAX(INTEGER); max := MIN(INTEGER); num := 0; dist := 0; head := n;
			Evaluate(n, min, max, num, dist, head);
			IF (num > 4) & ((max - min < 100) OR (max - min < num * 6)) THEN
				INCL(n.conval.setval, useTable)
			ELSE
				IF num > 4 THEN Rebuild(n, head) END;
				INCL(n.conval.setval, useTree);
				Optimize(n.left);
				Optimize(n.right)
			END
		END
	END Optimize;

	PROCEDURE StatSeq(VAR stat: CPT.Node);
		VAR fpar, id, t, obj: CPT.Object; idtyp: CPT.Struct; e, del: BOOLEAN; rp: ProcAttrPatch;
				s, x, y, z, apar, last, lastif: CPT.Node; pos, pos1, p, m: INTEGER; name: CPT.Name;

		PROCEDURE CasePart(VAR x: CPT.Node);
			VAR low, high: INTEGER; e: BOOLEAN; cases, lab, y, lastcase, root: CPT.Node; 
		BEGIN
			Expression(x);
			IF (x.class = Ntype) OR (x.class = Nproc) THEN err(126)
			ELSIF x.typ.form = Int64 THEN err(260)
			ELSIF ~(x.typ.form IN intSet + charSet) THEN err(125)
			END ;
			CheckSym(of); cases := NIL; lastcase := NIL; root := NIL;
			low := MAX(INTEGER); high := MIN(INTEGER);
			LOOP
				IF sym < bar THEN
					CaseLabelList(lab, root, x.typ.form, low, high);
					CheckSym(colon); StatSeq(y);
					CPB.Construct(Ncasedo, lab, y); CPB.Link(cases, lastcase, lab)
				END ;
				IF sym = bar THEN CPS.Get(sym) ELSE EXIT END
			END;
			e := sym = else;
			IF e THEN CPS.Get(sym); StatSeq(y) ELSE y := NIL END ;
			CPB.Construct(Ncaselse, cases, y); CPB.Construct(Ncase, x, cases);
			cases.conval := CPT.NewConst();
			cases.conval.intval := low; cases.conval.intval2 := high;
			IF e THEN cases.conval.setval := {1} ELSE cases.conval.setval := {} END;
			Optimize(root); cases.link := root	(* !!! *)
		END CasePart;
		
		PROCEDURE SetPos(x: CPT.Node);
		BEGIN
			x.conval := CPT.NewConst(); x.conval.intval := pos
		END SetPos;

		PROCEDURE CheckBool(VAR x: CPT.Node);
		BEGIN
			IF (x.class = Ntype) OR (x.class = Nproc) THEN err(126); x := CPB.NewBoolConst(FALSE)
			ELSIF x.typ.form # Bool THEN err(120); x := CPB.NewBoolConst(FALSE)
			END
		END CheckBool;

	BEGIN stat := NIL; last := NIL;
		LOOP x := NIL;
			IF sym < ident THEN err(14);
				REPEAT CPS.Get(sym) UNTIL sym >= ident
			END ;
			pos := CPM.startpos;
			IF sym = ident THEN
				qualident(id); x := CPB.NewLeaf(id); selector(x, del, pos1);
				IF sym = becomes THEN
					CPS.Get(sym); CheckedExpression(y, x.typ);
					IF (y.typ.form = Pointer) & (x.typ.form = Comp) THEN CPB.DeRef(y) END;
					IF x.typ.comp = Record THEN x.typ.attribute := 0 END;
					CPB.Assign(x, y);
					IF x.typ.comp = Record THEN x.typ.attribute := asgnAttr END
				ELSIF sym = eql THEN
					err(becomes); CPS.Get(sym); CheckedExpression(y, x.typ); CPB.Assign(x, y)
				ELSIF (x.class = Nproc) & (x.obj.mode = SProc) THEN
					m := x.obj.adr; StandProcCall(x);
					IF (m = copyfn) & (copy IN changes) THEN
						IF x.right.class = Nconst THEN Insert(pos, ccopy, CPM.errpos - pos)
						ELSE Insert(pos, copy, CPM.errpos - pos)
						END
					ELSIF m = -1 THEN Insert(pos, append, CPM.errpos - pos)
					ELSIF m = -2 THEN Insert(pos, concat, CPM.errpos - pos)
					END;
					IF (x # NIL) & (x.typ # CPT.notyp) THEN err(55) END;
					IF (x # NIL) & (x.class = Nifelse) THEN	(* error pos for ASSERT *)
						SetPos(x.left); SetPos(x.left.right)
					END
				ELSE
					CPB.PrepCall(x, fpar);
					IF sym = lparen THEN
						IF (x.obj # NIL) & (x.obj.mode = TProc) & (x.obj.name^ = "PropagateDomain")
								& IsType(x.left.typ.BaseTyp, "Stores", "Store") & (x.subcl # super) THEN
							Insert(pos, storesInitDomain, 0);
							Insert(pos1, commaSp, CPM.startpos + 1 - pos1)
						END;
						CPS.Get(sym); ActualParameters(apar, fpar, (x.obj # NIL) & (x.obj.adr = -77)); CheckSym(rparen)
					ELSE apar := NIL;
						IF fpar # NIL THEN err(65) END
					END ;
					IF del THEN
						IF sym = semicolon THEN p := CPM.curpos - 1 ELSE p := CPM.errpos END;
						Insert(pos, delete, p - pos);
						IF self # NIL THEN
							NEW(rp); rp.next := patchList; patchList := rp;
							rp.pos := p; rp.kind := propagate; rp.len := 0; 
							rp.obj := self;
						END
					END;
					CPB.Call(x, apar, fpar);
					IF level > 0 THEN CPT.topScope.link.leaf := FALSE END;
					IF x.typ # CPT.notyp THEN err(55) END
				END
			ELSIF sym = if THEN
				CPS.Get(sym); pos := CPM.startpos; Expression(x); CheckBool(x); CheckSym(then); StatSeq(y);
				CPB.Construct(Nif, x, y); SetPos(x); lastif := x;
				WHILE sym = elsif DO
					CPS.Get(sym); pos := CPM.startpos; Expression(y); CheckBool(y); CheckSym(then); StatSeq(z);
					CPB.Construct(Nif, y, z); SetPos(y); CPB.Link(x, lastif, y)
				END ;
				pos := CPM.startpos;
				IF sym = else THEN CPS.Get(sym); StatSeq(y) ELSE y := NIL END ;
				CPB.Construct(Nifelse, x, y); CheckSym(end); CPB.OptIf(x);
			ELSIF sym = case THEN
				CPS.Get(sym); pos := CPM.startpos; CasePart(x); CheckSym(end)
			ELSIF sym = while THEN
				CPS.Get(sym); pos := CPM.startpos; Expression(x); CheckBool(x); CheckSym(do); StatSeq(y);
				CPB.Construct(Nwhile, x, y); CheckSym(end)
			ELSIF sym = repeat THEN
				CPS.Get(sym); StatSeq(x);
				IF sym = until THEN CPS.Get(sym); pos := CPM.startpos; Expression(y); CheckBool(y)
				ELSE err(until)
				END ;
				CPB.Construct(Nrepeat, x, y)
			ELSIF sym = for THEN
				CPS.Get(sym); pos := CPM.startpos;
				IF sym = ident THEN qualident(id);
					IF ~(id.typ.form IN intSet) THEN err(68) END ;
					CheckSym(becomes); CheckedExpression(y, id.typ);
					x := CPB.NewLeaf(id); CPB.Assign(x, y); SetPos(x);
					CheckSym(to); pos := CPM.startpos; CheckedExpression(y, id.typ);
					IF y.class # Nconst THEN
						name := "@@  "; CPT.Insert(name, t); t.name^ := "@for";	(* avoid err 1 *)
						t.mode := Var; t.typ := x.left.typ;
						obj := CPT.topScope.scope;
						IF obj = NIL THEN CPT.topScope.scope := t
						ELSE
							WHILE obj.link # NIL DO obj := obj.link END ;
							obj.link := t
						END ;
						z := CPB.NewLeaf(t); CPB.Assign(z, y); SetPos(z); CPB.Link(stat, last, z);
						y := CPB.NewLeaf(t)
					ELSIF (y.typ.form < Int8) OR (y.typ.form > x.left.typ.form) THEN err(113)
					END ;
					CPB.Link(stat, last, x);
					p := CPM.startpos;
					IF sym = by THEN CPS.Get(sym); ConstExpression(z) ELSE z := CPB.NewIntConst(1) END ;
					x := CPB.NewLeaf(id);
					IF z.conval.intval > 0 THEN CPB.Op(leq, x, y)
					ELSIF z.conval.intval < 0 THEN CPB.Op(geq, x, y)
					ELSE err(63); CPB.Op(geq, x, y)
					END ;
					CheckSym(do); StatSeq(s);
					y := CPB.NewLeaf(id); CPB.StPar1(y, z, incfn); pos := CPM.startpos; SetPos(y);
					IF s = NIL THEN s := y
					ELSE z := s;
						WHILE z.link # NIL DO z := z.link END ;
						z.link := y
					END ;
					CheckSym(end); CPB.Construct(Nwhile, x, s); pos := p
				ELSE err(ident)
				END
			ELSIF sym = loop THEN
				CPS.Get(sym); INC(LoopLevel); StatSeq(x); DEC(LoopLevel);
				CPB.Construct(Nloop, x, NIL); CheckSym(end)
			ELSIF sym = with THEN
				CPS.Get(sym); idtyp := NIL; x := NIL;
				LOOP
					pos := CPM.startpos;
					IF sym = ident THEN
						qualident(id); y := CPB.NewLeaf(id);
						IF (id # NIL) & (id.typ.form = Pointer) & ((id.mode = VarPar) OR ~id.leaf) THEN
							err(-302)	(* warning 302 *)
						END ;
						CheckSym(colon);
						IF sym = ident THEN qualident(t);
							IF t.mode = Typ THEN
								IF id # NIL THEN
									idtyp := id.typ; CPB.TypTest(y, t, FALSE); id.typ := t.typ;
									IF id.ptyp = NIL THEN id.ptyp := idtyp END
								ELSE err(130)
								END
							ELSE err(52)
							END
						ELSE err(ident)
						END
					ELSE err(ident)
					END ;
					CheckSym(do); StatSeq(s); CPB.Construct(Nif, y, s); SetPos(y);
					IF idtyp # NIL THEN
						IF id.ptyp = idtyp THEN id.ptyp := NIL END;
						id.typ := idtyp; idtyp := NIL
					END ;
					IF x = NIL THEN x := y; lastif := x ELSE CPB.Link(x, lastif, y) END ;
					IF sym = bar THEN CPS.Get(sym) ELSE EXIT END
				END;
				e := sym = else; pos := CPM.startpos;
				IF e THEN CPS.Get(sym); StatSeq(s) ELSE s := NIL END ;
				CPB.Construct(Nwith, x, s); CheckSym(end);
				IF e THEN x.subcl := 1 END
			ELSIF sym = exit THEN
				CPS.Get(sym);
				IF LoopLevel = 0 THEN err(46) END ;
				CPB.Construct(Nexit, x, NIL)
			ELSIF sym = return THEN CPS.Get(sym);
				IF sym < semicolon THEN
					IF CPT.topScope.link # NIL THEN CheckedExpression(x, CPT.topScope.link.typ)
					ELSE Expression(x)
					END
				END ;
				IF level > 0 THEN CPB.Return(x, CPT.topScope.link)
				ELSE (* not standard Oberon *) CPB.Return(x, NIL)
				END
			END ;
			IF x # NIL THEN SetPos(x); CPB.Link(stat, last, x) END ;
			IF sym = semicolon THEN CPS.Get(sym)
			ELSIF (sym <= ident) OR (if <= sym) & (sym <= return) THEN err(semicolon)
			ELSE EXIT
			END
		END
	END StatSeq;

	PROCEDURE Block(VAR procdec, statseq: CPT.Node; VAR endpos: INTEGER);
		VAR typ: CPT.Struct;
			obj, first, last: CPT.Object;
			x, lastdec: CPT.Node;
			i: SHORTINT;
			name: CPT.Name;

	BEGIN first := NIL; last := NIL; nofFwdPtr := 0;
		LOOP
			IF sym = const THEN
				CPS.Get(sym);
				WHILE sym = ident DO
					CPT.Insert(CPS.name, obj);
					obj.mode := Con; CheckMark(obj, 0);
					obj.typ := CPT.int8typ; obj.mode := Var;	(* Var to avoid recursive definition *)
					IF sym = eql THEN
						CPS.Get(sym); ConstExpression(x)
					ELSIF sym = becomes THEN
						err(eql); CPS.Get(sym); ConstExpression(x)
					ELSE err(eql); x := CPB.NewIntConst(1)
					END ;
					obj.mode := Con; obj.typ := x.typ; obj.conval := x.conval; (* ConstDesc ist not copied *)
					CheckSym(semicolon)
				END
			END ;
			IF sym = type THEN
				CPS.Get(sym);
				WHILE sym = ident DO
					CPT.Insert(CPS.name, obj); obj.mode := Typ; obj.typ := CPT.undftyp;
					CheckMark(obj, 0);
					IF sym = eql THEN
						CPS.Get(sym); TypeDecl(obj.typ, obj.typ)
					ELSIF (sym = becomes) OR (sym = colon) THEN
						err(eql); CPS.Get(sym); TypeDecl(obj.typ, obj.typ)
					ELSE err(eql)
					END ;
					IF obj.typ.form IN {Byte..Set, Char16, Int64} THEN	(* make alias structure *)
						typ := CPT.NewStr(obj.typ.form, Basic); i := typ.ref;
						typ^ := obj.typ^; typ.ref := i; typ.strobj := NIL; typ.mno := 0;
						typ.BaseTyp := obj.typ; obj.typ := typ;
					END;
					IF obj.typ.strobj = NIL THEN obj.typ.strobj := obj END ;
					IF obj.typ.comp IN {Record, Array, DynArr} THEN
						i := 0;
						WHILE i < nofFwdPtr DO typ := FwdPtr[i]; INC(i);
							IF typ.link.name^ = obj.name^ THEN
								typ.BaseTyp := obj.typ; typ.link.name := CPT.null;
								IF obj.typ.comp = Record THEN CPM.PropagateRecPtrSysFlag(obj.typ.sysflag, typ.sysflag)
								ELSE CPM.PropagateArrPtrSysFlag(obj.typ.sysflag, typ.sysflag)
								END;
								typ.untagged := typ.sysflag # 0
							END
						END
					ELSIF obj.typ.form = Pointer THEN	(* !!! *)
						typ := obj.typ.BaseTyp;
						IF (typ # NIL) & (typ.comp = Record) & (typ.strobj = NIL)
							& ((typ.sysflag = 0) OR (typ.sysflag = interface)) THEN
							(* pointer to unnamed record: name record as "pointerName^" *)
							name := obj.name^$; i := 0;
							WHILE name[i] # 0X DO INC(i) END;
							name[i] := "^"; name[i+1] := 0X;
							CPT.Insert(name, obj); obj.mode := Typ; obj.typ := typ; typ.strobj := obj
						END
					END ;
					IF obj.vis # internal THEN
						typ := obj.typ;
						IF typ.form = Pointer THEN typ := typ.BaseTyp END;
						IF (typ.comp = Record) & (typ.attribute # absAttr) THEN typ.attribute := extAttr END
					END;
					CheckSym(semicolon)
				END
			END ;
			IF sym = var THEN
				CPS.Get(sym);
				WHILE sym = ident DO
					LOOP
						IF sym = ident THEN
							CPT.Insert(CPS.name, obj);
							obj.mode := Var; obj.link := NIL; obj.leaf := obj.vis = internal; obj.typ := CPT.undftyp;
							CheckMark(obj, 0);
							IF first = NIL THEN first := obj END ;
							IF last = NIL THEN CPT.topScope.scope := obj ELSE last.link := obj END ;
							last := obj
						ELSE err(ident)
						END ;
						IF sym = comma THEN CPS.Get(sym)
						ELSIF sym = ident THEN err(comma)
						ELSE EXIT
						END
					END ;
					CheckSym(colon); Type(typ, CPT.notyp);
					typ.pvused := TRUE;
					CheckAlloc(typ);
					WHILE first # NIL DO first.typ := typ; first := first.link END ;
					CheckSym(semicolon)
				END
			END ;
			IF (sym < const) OR (sym > var) THEN EXIT END ;
		END ;
		i := 0;
		WHILE i < nofFwdPtr DO
			IF FwdPtr[i].link.name # CPT.null THEN CPM.Mark(128, FwdPtr[i].txtpos) END ;
			FwdPtr[i] := NIL;	(* garbage collection *)
			INC(i)
		END ;
		CPT.topScope.adr := CPM.errpos;
		procdec := NIL; lastdec := NIL;
		WHILE sym = procedure DO
			CPS.Get(sym); ProcedureDeclaration(x);
			IF x # NIL THEN
				IF lastdec = NIL THEN procdec := x ELSE lastdec.link := x END ;
				lastdec := x
			END ;
			CheckSym(semicolon)
		END ;
		IF sym = begin THEN CPS.Get(sym); StatSeq(statseq)
		ELSE statseq := NIL
		END ;
		IF (level = 0) & (TDinit # NIL) THEN
			lastTDinit.link := statseq; statseq := TDinit
		END ;
		IF level = 0 THEN
			CPS.name := "TERMINATE";
			CPT.Find(CPS.name, obj);
			IF (obj # NIL) & (obj.mode = XProc) THEN Insert(CPM.startpos, close, 0) END
		END;
		endpos := CPM.startpos;
		CheckSym(end)
	END Block;

	PROCEDURE Module*(min, max: SET);
		VAR impName, aliasName: CPT.Name;
				procdec, statseq, prog: CPT.Node;
				c, e, sf, p, pos: INTEGER; done: BOOLEAN;
	BEGIN
		changes := min; patchList := NIL;
		CPS.Init; LoopLevel := 0; level := 0; CPS.Get(sym);
		IF sym = module THEN CPS.Get(sym) ELSE err(16) END ;
		IF sym = ident THEN
			CPT.Open(CPS.name); CPS.Get(sym);
			CPT.libName := "";
			IF sym = lbrak THEN
				changes := max;
				INCL(CPM.options, CPM.interface); CPS.Get(sym);
				IF sym = string THEN CPT.libName := CPS.str^$; CPS.Get(sym)
				ELSE err(string)
				END;
				CheckSym(rbrak)
			END;
			CheckSym(semicolon);
			IF sym = import THEN CPS.Get(sym);
				LOOP
					IF sym = ident THEN
						pos := CPM.startpos; p := pos;
						aliasName := CPS.name$; impName := aliasName$; CPS.Get(sym);
						IF sym = becomes THEN CPS.Get(sym);
							IF sym = ident THEN
								impName := CPS.name$;
								p := CPM.startpos; CPS.Get(sym);
								IF (stores IN changes) & (impName = "Domains") THEN
									Insert(p, stores, CPM.errpos - p); impName := "Stores"
								ELSIF (formatters IN changes) & (impName = "DevLog") THEN
									Insert(p, stdLog, CPM.errpos - p); impName := "StdLog"
								END
							ELSE err(ident)
							END
						ELSE
							IF (stores IN changes) & (impName = "Domains") THEN
								Insert(CPM.errpos, become, 0); Insert(CPM.errpos, stores, 0);
								impName := "Stores"
							ELSIF (formatters IN changes) & (impName = "DevLog") THEN
								Insert(CPM.errpos, become, 0); Insert(CPM.errpos, stdLog, 0);
								impName := "StdLog"
							END
						END ;
						CPT.Import(aliasName, impName, done);
						IF impName = "SYSTEM" THEN changes := max END
					ELSE err(ident)
					END ;
					IF sym = comma THEN CPS.Get(sym)
					ELSIF sym = ident THEN err(comma)
					ELSE EXIT
					END
				END ;
				impPos := CPM.startpos;
				CheckSym(semicolon)
			END ;
			IF CPM.noerr THEN TDinit := NIL; lastTDinit := NIL; c := CPM.errpos;
				Block(procdec, statseq, e); CPB.Enter(procdec, statseq, NIL); prog := procdec;
				prog.conval := CPT.NewConst(); prog.conval.intval := c; prog.conval.intval2 := e;
				IF sym = ident THEN
					IF CPS.name # CPT.SelfName THEN err(4) END ;
					CPS.Get(sym)
				ELSE err(ident)
				END ;
				IF sym # period THEN err(period) END
			END
		ELSE err(ident)
		END ;
		TDinit := NIL; lastTDinit := NIL
	END Module;

BEGIN
	rootObj := CPT.NewObj();
	strAppend := CPT.NewObj(); strAppend.mode := SProc; strAppend.typ := CPT.notyp; strAppend.adr := -1;
	strConcat := CPT.NewObj(); strConcat.mode := SProc; strConcat.typ := CPT.notyp; strConcat.adr := -2;
	strLen := CPT.NewObj(); strLen.mode := XProc; strLen.typ := CPT.int32typ;
	strLen.link := CPT.NewObj(); strLen.link.mode := Var; strLen.link.typ := CPT.string16typ
END DevCPCP.
