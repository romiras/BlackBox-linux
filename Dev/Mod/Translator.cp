MODULE DevTranslator;

	(* bh 23.5.96 first version *)

	IMPORT
		Kernel,
		Files, Strings, Stores, Views, Converters, StdDialog,
		TextModels, TextViews, TextControllers, TextMappers, DevMarkers, StdLog;
	
	CONST
		(* filter *)
		outConstants = TRUE;
		outEnums = TRUE;
		outTypes = TRUE;
		outMethods = TRUE;
		outProcedures = TRUE;
		outMissingProc = TRUE;
		outAllProc = FALSE;
		outIntAsSet = FALSE;
		outEnumAsShort = FALSE;
		outEnumDynamic = FALSE;
		
		(* tokens *)
		null = 0; white = 1; eol = 2; pre1 = 3; pre2 = 4;
		number = 10; string = 12; ident = 13;
		lpar = 20; rpar = 21; lbrak = 22; rbrak = 23; lbrace = 24; rbrace = 25;
		point = 26; comma = 27; inv = 28; not = 29;
		minus = 30; plus = 31; mul = 32; div = 33; mod = 34; and = 35; or = 36; xor = 37;
		deref = 38;
		dblminus = 40; dblplus = 41; dbland = 42; dblor = 43; lshift = 44; rshift = 45;
		eql = 50; noteql = 51; gtr = 52; gtreql = 53; less = 54; lesseql = 55;
		question = 60; colon = 61; semicolon = 62; becomes = 63;
		typedef = 100; auto = 101; register = 102; static = 103; extern = 104; virtual = 105; public = 106;
		const = 110; volatile = 111; signed = 112; unsigned = 113; 
		void = 120; char = 121; short = 122; int = 123; long = 124; float = 125; double = 126;
		enum = 130; struct = 131; union = 132;
		sizeof = 140; switch = 141; 
		eof = 200;
		
		(* object kinds *)
		macro = 1; mPar = 2; hint = 3;
		constant = 10; procedure = 11; type = 12; field = 13; param = 14;
		library = 20; libEntry = 21;
		
		(* type forms *)
		basicTyp = 1; enumTyp = 3; recordTyp = 4; unionTyp = 5;
		arrayTyp = 6; pointerTyp = 7; procTyp = 8;
		
		(* param forms *)
		valPar = 1; inPar = 2; outPar = 3; varPar = 4; newPar = 5; iidPar = 6;
		
		(* options *)
		constOpt = 0; typedefOpt = 1; cdeclOpt = 2; voidOpt = 3; charOpt = 4; intOpt = 5; pvoidOpt = 6;
		ignoreOpt = 8; bodyOpt = 9; replacedOpt = 10; hasPtrOpt = 11; inlineOpt = 12;
		isConstOpt = 16; isTypeOpt = 17; isProcOpt = 18; isStringOpt = 19; isSetOpt = 20; isNewOpt = 21;
		inOpt = 24; outOpt = 25; uniqueOpt = 26; refOpt = 27; iidOpt = 28;
		listOpt = 29; doneOpt = 30; ptrDoneOpt = 31;
		
		(* name prefix *)
		plain = 0; ptr = 1; ret = 2; proc = 3;
		
		(* restrictions *)
		hashSize = 1024;
		maxIdentLen = 39;
		maxStk = 16;
	
	
	TYPE
		Name = ARRAY 256 OF CHAR;
		Module = POINTER TO RECORD
			next, link: Module;
			name, dll: Name;
			impSys: BOOLEAN
		END;
		Input = POINTER TO RECORD
			next, host: Input;
			view: TextViews.View;
			name: Files.Name;
			mod: Module;
			errors: INTEGER
		END;
		Object = POINTER TO ObjDesc;
		Type = POINTER TO RECORD
			base: Type;
			baseObj, sub: Object;
			form, align, min, max: INTEGER;
			size: INTEGER;
			opts: SET
		END;
		ObjDesc = RECORD
			next, base, typObj, link: Object;
			typ: Type;
			name: Name;
			kind (* , form *): INTEGER;
			opts: SET;
			value: INTEGER;
			inp: Input;
			beg, end: INTEGER
		END;
		Context = POINTER TO RECORD
			pos, end: INTEGER;
			act, ahd: CHAR;
			inp: Input;
			par: Object;
			next: Context
		END;
		Scanner = POINTER TO RECORD
			rd: TextModels.Reader;
			end: INTEGER;
			par: Object;	(* actual parameters *)
			context: Context;
			sym: INTEGER;
			act, ahd: CHAR;
			ident: Name;
			num: INTEGER;
			level, skipLev: INTEGER;
			inIncl: BOOLEAN;	(* scanning argument of #include *)
			inShort: BOOLEAN;	(* scanning second argument of shortcut *)
			inDef: BOOLEAN;	(* scanning argument of #define (no errors) *)
			inPre: BOOLEAN;	(* scanning preprocessor expression *)
			error: BOOLEAN;
			hints: SET;
			inp: Input
		END;
		
	
	VAR
		table: ARRAY hashSize OF Object;
		firstObj, lastObj: Object;
		libList: Object;
		inpList: Input;
		modList, impList, actMod: Module;
		alignStack: ARRAY maxStk OF INTEGER;
		alignIdx: INTEGER;
		out: TextMappers.Formatter;
		path, libPath: Files.Locator;
		conv: Converters.Converter;
		errors: INTEGER;
		incLevel: INTEGER;
		baseType: ARRAY double - signed + 1 OF Object;
	
	
	(* ---------- file handling ---------- *)
	
	PROCEDURE OpenFile (VAR name: Name; host: Input; VAR inp: Input);
		VAR s: Stores.Store; t: TextModels.Model; v: TextViews.View; n: Files.Name; i: Input; l: INTEGER;
	BEGIN
		l := incLevel;
		WHILE l > 0 DO StdLog.String("  "); DEC(l) END;
		n := name$; i := inpList;
		WHILE (i # NIL) & (i.name # n) DO i := i.next END;
		IF i # NIL THEN
			StdLog.String("reopen ");
			inp := i;
			IF inp.host = NIL THEN inp.host := host END
		ELSE
			StdLog.String("open ");
			s := Views.Old(FALSE, path, n, conv);
			IF (s # NIL) & (s IS TextViews.View) THEN
				v := s(TextViews.View); t := v.ThisModel();
				DevMarkers.Unmark(t);
				NEW(inp); inp.view := v; inp.name := n; inp.host := host; inp.next := inpList; inpList := inp;
			ELSE
				inp := NIL
			END
		END;
		StdLog.String(name);
		IF inp = NIL THEN StdLog.String(" not found") END;
		StdLog.Ln
	END OpenFile;
	
	
	(* ---------- table handler ---------- *)
	
	PROCEDURE EmptyTable;
		VAR i: INTEGER;
	BEGIN
		i := 0; firstObj := NIL; lastObj := NIL;
		WHILE i < LEN(table) DO table[i] := NIL; INC(i) END
	END EmptyTable;
	
	PROCEDURE HashIndex (IN name: ARRAY OF CHAR): INTEGER;
		VAR i, h: INTEGER;
	BEGIN
		RETURN ((ORD(name[0]) MOD 16) * (ORD(name[1]) MOD 16) * (ORD(name[0]) MOD 16)) MOD hashSize;
(*
		i := 0; h := 0;
		WHILE name[i] # 0X DO INC(h, ORD(name[i])); INC(i) END;
		RETURN h MOD hashSize
*)
	END HashIndex;
	
	PROCEDURE Find (IN name: ARRAY OF CHAR; VAR obj: Object);
		VAR o: Object;
	BEGIN
		o := table[HashIndex(name)];
		WHILE (o # NIL) & ((o.kind = libEntry) OR (o.name # name)) DO o := o.next END;
		obj := o
	END Find;
	
	PROCEDURE FindPrefix (IN name: Name; from: Object; VAR obj: Object);
		VAR ch: CHAR; i: INTEGER;
	BEGIN
		IF from # NIL THEN from := from.next
		ELSE from := table[HashIndex(name)]
		END;
		i := 0; WHILE name[i] # 0X DO INC(i) END;
		WHILE from # NIL DO
			IF from.kind # libEntry THEN
				ch := from.name[i]; from.name[i] := 0X;
				IF name = from.name THEN from.name[i] := ch; obj := from; RETURN
				ELSE from.name[i] := ch
				END
			END;
			from := from.next
		END;
		obj := NIL
	END FindPrefix;
	
	PROCEDURE FindEntry (VAR name: Name; VAR obj: Object);
		VAR o: Object;
	BEGIN
		o := table[HashIndex(name)];
		WHILE (o # NIL) & ((o.kind # libEntry) OR (o.name # name)) DO o := o.next END;
		obj := o
	END FindEntry;
	
	PROCEDURE Insert (VAR name: Name; kind: INTEGER; inp: Input; VAR obj: Object);
		VAR o: Object; i: INTEGER;
	BEGIN
		NEW(obj); obj.name := name$; obj.kind := kind; obj.inp := inp;
		i := HashIndex(name); obj.next := table[i]; table[i] := obj
	END Insert;
	
	PROCEDURE ListInsert (obj: Object);
	BEGIN
		IF obj.opts * {listOpt, ignoreOpt, replacedOpt} = {} THEN
			IF lastObj = NIL THEN firstObj := obj ELSE lastObj.link := obj END;
			lastObj := obj; INCL(obj.opts, listOpt)
		END
	END ListInsert;
	
	PROCEDURE Remove (VAR name: Name);
		VAR o, l: Object; i: INTEGER;
	BEGIN
		l := NIL; i := HashIndex(name); o := table[i];
		WHILE (o # NIL) & (o.name # name) DO l := o; o := o.next END;
		IF o # NIL THEN
(*
			ASSERT(o.kind = macro, 100);
*)
			INCL(o.opts, ignoreOpt);
			IF l = NIL THEN table[i] := o.next ELSE l.next := o.next END
		END
	END Remove;
	
	PROCEDURE InsertIn (VAR first, last: Object; IN name: ARRAY OF CHAR; kind: INTEGER; VAR par: Object);
	BEGIN
		NEW(par); par.kind := kind; par.name := name$;
		IF last = NIL THEN first := par ELSE last.next := par END;
		last := par
	END InsertIn;
	
	
	(* ---------- helper functions ---------- *)
	
	PROCEDURE Append (VAR listObj: Object; VAR list: Type; elemObj: Object; elem: Type);
		VAR t: Type;
	BEGIN
		IF list = NIL THEN listObj := elemObj; list := elem
		ELSE
			t := list;
			WHILE t.base # NIL DO t := t.base END;
			t.baseObj := elemObj; t.base := elem;
			IF (t.form = pointerTyp) & (elem # NIL) THEN INCL(elem.opts, hasPtrOpt) END
		END
	END Append;
	
	PROCEDURE Alignment (t: Type): INTEGER;
		VAR f: Object; a: INTEGER;
	BEGIN
		WHILE t.form = arrayTyp DO t := t.base END;
		IF t.form = basicTyp THEN ASSERT(t.align > 0, 101); RETURN t.align
		ELSIF (t.form = recordTyp) OR (t.form = unionTyp) THEN
			IF t.size = 0 THEN
				f := t.sub;
				WHILE f # NIL DO
					a := Alignment(f.typ);
					IF a > t.size THEN t.size := a END;
					f := f.next
				END
			END;
			IF t.size > t.align THEN RETURN t.align ELSE RETURN t.size END
		ELSIF t.form = procTyp THEN HALT(20)
		ELSE RETURN 4
		END
	END Alignment;
	
	PROCEDURE ChangeToSet (typ: Type; IN name: Name);
		VAR o: Object; t: Type;
	BEGIN
		CASE typ.form OF
		| recordTyp, unionTyp, procTyp:
			o := typ.sub;
			WHILE o # NIL DO
				t := o.typ;
				IF (typ.form = procTyp) & (t.form = pointerTyp) THEN t := t.base END;
				IF (intOpt IN t.opts) & (o.name = name) THEN
					INCL(o.opts, isSetOpt)
				ELSIF o.typObj = NIL THEN
					ChangeToSet(o.typ, name)
				END;
				o := o.next
			END
		| arrayTyp, pointerTyp:
			IF typ.baseObj = NIL THEN 
				ChangeToSet(typ.base, name)
			END
		ELSE
		END
	END ChangeToSet;
	
	
	(* ---------- output formatter ---------- *)
	
	PROCEDURE WriteName (obj: Object; pre: INTEGER);
		VAR mod, m: Module; i, j, n: INTEGER; name: Name;
	BEGIN
		ASSERT(obj # NIL, 20);
		WHILE replacedOpt IN obj.opts DO obj := obj.base END;
		IF obj.inp # NIL THEN
			mod := obj.inp.mod;
			IF mod # actMod THEN
				IF (mod # NIL) & (mod.name # "") THEN
					out.WriteString(mod.name);
					out.WriteChar("."); m := impList;
					WHILE (m # NIL) & (m # mod) DO m := m.link END;
					IF m = NIL THEN mod.link := impList; impList := mod END
				ELSE out.WriteString("UNKNOWN.")
				END
			END
		END;
		name := obj.name$; n := maxIdentLen;
		IF pre = ptr THEN out.WriteString("Ptr"); DEC(n, 3)
		ELSIF pre = ret THEN out.WriteString("Ret"); DEC(n, 3)
		ELSIF pre = proc THEN	(* remove "P" or "LP" *)
			IF name[0] = "P" THEN i := 1
			ELSIF (name[0] = "L") & (name[1] = "P") THEN i := 2
			ELSE i := 0
			END;
			j := 0;
			WHILE name[i] # 0X DO name[j] := name[i]; INC(i); INC(j) END;
			name[j] := 0X
		END;
		i := 0; WHILE (name[i] # 0X) DO INC(i) END;
		IF i > n THEN	(* cut name *)
			name[n] := 0X;
			out.WriteString(name); out.WriteString(" (*...*)")
		ELSE
			out.WriteString(name)
		END
	END WriteName;
	
	PROCEDURE^ WriteType (typObj: Object; typ: Type; lev: INTEGER; hint: Object);
	
	PROCEDURE WriteParList (typ: Type);
		VAR n: INTEGER; par, to, o: Object; t: Type; hasiid, hasnew: BOOLEAN;
	BEGIN
		ASSERT(typ.form = procTyp, 100);
		par := typ.sub; hasiid := FALSE; hasnew := FALSE;
		WHILE par # NIL DO
			IF par.typ.form = pointerTyp THEN
				t := par.typ.base;
				IF (refOpt IN par.opts) & (iidOpt IN par.opts) THEN hasiid := TRUE
				ELSIF (t.form = pointerTyp) & (voidOpt IN t.base.opts) OR (pvoidOpt IN t.opts) THEN hasnew := TRUE
				END
			END;
			par := par.next
		END;
		out.WriteChar("("); n := 0; par := typ.sub;
		WHILE par # NIL DO
			ASSERT(par.kind = param, 101);
			IF ~(voidOpt IN par.typ.opts) THEN
				t := par.typ; to := par.typObj; o := t.baseObj;
				WHILE (o # NIL) & (replacedOpt IN o.opts) DO o := o.base END;
				IF (t.form = pointerTyp) & (t.base.form # procTyp) (* & ~(uniqueOpt IN par.opts) *)
						& ~(charOpt IN t.base.opts) & ~(voidOpt IN t.base.opts)
						& ((o = NIL) OR (o.typ.form = pointerTyp) OR (doneOpt IN o.opts) OR (o.inp = NIL) OR (o.inp.mod # actMod))
						& ~((to # NIL) & (o = NIL)) 
						& ~((t.base.form = recordTyp) & (t.base.sub # NIL) & (t.base.sub.typ.form = procTyp)) THEN
					out.WriteString("VAR "); to := t.baseObj; t := t.base;
					IF (refOpt IN par.opts) & (iidOpt IN par.opts) & hasnew THEN out.WriteString("[iid] ")
					ELSIF ((t.form = pointerTyp) & (voidOpt IN t.base.opts) OR (pvoidOpt IN t.opts)) & hasiid THEN
						out.WriteString("[new] "); INCL(par.opts, isNewOpt)
					ELSIF (inOpt IN par.opts) & ~(outOpt IN par.opts) THEN out.WriteString("[in, nil] ")
					ELSIF (outOpt IN par.opts) & ~(inOpt IN par.opts) THEN out.WriteString("[out, nil] ")
					ELSIF refOpt IN par.opts THEN out.WriteString("[in, nil] ")
					ELSE out.WriteString("[nil] ")
					END
				END;
				IF par.name = "" THEN
					out.WriteChar("p"); out.WriteIntForm(n, 10, 1, " ", FALSE)
				ELSE
					out.WriteString(par.name)
				END;
				out.WriteString(": ");
				IF isSetOpt IN par.opts THEN out.WriteString("SET")
				ELSIF isNewOpt IN par.opts THEN out.WriteString("COM.PtrIUnknown")
				ELSE WriteType(to, t, 3, NIL)
				END;
				IF par.next # NIL THEN out.WriteString("; ") END
			END;
			par := par.next; INC(n)
		END;
		out.WriteChar(")");
		IF (typ.baseObj # NIL) & (typ.baseObj.kind = procedure) THEN
			out.WriteString(": "); WriteName(typ.baseObj, ret)
		ELSIF ~(voidOpt IN typ.base.opts) THEN
			out.WriteString(": "); WriteType(typ.baseObj, typ.base, 3, NIL)
		END
	END WriteParList;
	
	PROCEDURE WriteType (typObj: Object; typ: Type; lev: INTEGER; hint: Object);
		VAR o, b: Object; i, n, k, align: INTEGER; name: Name;
	BEGIN
		IF typ.form = pointerTyp THEN
			IF typ.base.form = procTyp THEN
				IF typ.baseObj # NIL THEN WriteName(typ.baseObj, plain)
				ELSIF typObj # NIL THEN WriteName(typObj, proc)
				ELSE out.WriteString("PROCEDURE "); WriteParList(typ.base)
				END
			ELSE
				o := typ.baseObj;
				WHILE (o # NIL) & (replacedOpt IN o.opts) DO o := o.base END;
				IF (typObj # NIL) & (o = NIL) THEN
					WriteName(typObj, plain)
				ELSIF (o # NIL) & ((ptrDoneOpt IN o.opts) OR (o.inp # NIL) & (o.inp.mod # actMod) & (o.typ.form # basicTyp)) THEN
					WriteName(o, ptr)
				ELSIF voidOpt IN typ.base.opts THEN
					IF (o # NIL) & (isTypeOpt IN o.opts) THEN
						WriteName(o, ptr)
					ELSE
						Find("PVOID", o);
						IF o # NIL THEN WriteName(o, plain) ELSE out.WriteString("PtrVoid") END
					END
				ELSIF charOpt IN typ.base.opts THEN
					IF typ.base.align = 1 THEN
						Find("PSTR", o);
						IF o # NIL THEN WriteName(o, plain) ELSE out.WriteString("PtrChar") END
					ELSE
						Find("PWSTR", o);
						IF o # NIL THEN WriteName(o, plain) ELSE out.WriteString("PtrWChar") END
					END
				ELSIF (typ.base.form = basicTyp) OR (typ.base.form = enumTyp) OR (typ.base.form = pointerTyp) THEN
					out.WriteString("POINTER TO (*?*) ARRAY [untagged] OF "); WriteType(typ.baseObj, typ.base, lev, NIL)
				ELSE
					out.WriteString("POINTER TO "); WriteType(typ.baseObj, typ.base, lev, NIL)
				END
			END
		ELSIF typObj # NIL THEN
			WriteName(typObj, plain)
		ELSE
			CASE typ.form OF
			| basicTyp: HALT(100)
			| enumTyp: 
				IF outEnumDynamic THEN
					IF (typ.min >= -128) & (typ.max < 256) THEN out.WriteString("BYTE")
					ELSIF (typ.min >= -32768) & (typ.max < 65536) THEN out.WriteString("SHORTINT")
					ELSE out.WriteString("INTEGER")
					END
				ELSIF outEnumAsShort THEN out.WriteString("SHORTINT")
				ELSE out.WriteString("INTEGER")
				END
			| recordTyp, unionTyp:
				IF typ.form = recordTyp THEN
					IF (typ.sub # NIL) & (typ.sub.typ.form = procTyp) THEN
						IF hint # NIL THEN
							name := "IID_"; name := name + hint.name;
							Find(name, o)
						ELSE o := NIL
						END;
						IF (o # NIL) & (o.kind = constant) & (iidOpt IN o.opts) THEN
							out.WriteString('RECORD ["');
							out.WriteString(o.base.name);
							out.WriteString('"]')
						ELSE
							out.WriteString("RECORD [interface]")
						END
					ELSE
						align := Alignment(typ);
						IF align = 8 THEN
							out.WriteString("RECORD [align8]")
						ELSIF (typ.size <= typ.align) THEN	(* max(field align) <= pack *)
							out.WriteString("RECORD [untagged]")
						ELSIF align = 2 THEN
							out.WriteString("RECORD [align2]")
						ELSIF align = 1 THEN
							out.WriteString("RECORD [noalign]")
						ELSE
							out.WriteString("RECORD [untagged]")
						END
					END;
					IF typ.base # NIL THEN
						ASSERT(typ.baseObj # NIL, 102);
						out.WriteString(" ("); WriteName(typ.baseObj, plain); out.WriteChar(")")
					END
				ELSE out.WriteString("RECORD [union]")
				END;
				out.WriteLn; o := typ.sub; k := 0; n := 0;
				WHILE o # NIL DO
					ASSERT(o.kind = field, 103);
					IF o.typ.form # procTyp THEN
						IF o.value > n THEN	(* bit field *)
							i := 0; WHILE i <= lev DO out.WriteTab; INC(i) END;
							out.WriteString("fBits"); out.WriteInt(k); out.WriteString("*: "); INC(k);
							IF o.typ.align = 1 THEN out.WriteString("BYTE"); n := 8
							ELSIF o.typ.align = 2 THEN out.WriteString("SHORTINT"); n := 16
							ELSIF o.typ.align = 4 THEN out.WriteString("SET"); n := 32
							ELSE out.WriteString("UNKNOWN")
							END;
							out.WriteChar(";"); out.WriteLn
						END;
						i := 0; WHILE i <= lev DO out.WriteTab; INC(i) END;
						IF o.value > 0 THEN out.WriteString("(* "); DEC(n, o.value) END;
						IF o.name = "" THEN
							IF o.typ.form = recordTyp THEN out.WriteChar("r") ELSE out.WriteChar("u") END;
							IF k > 0 THEN out.WriteInt(k) END;
							INC(k)
						ELSE out.WriteString(o.name);
						END;
						out.WriteString("*: ");
						IF isSetOpt IN o.opts THEN out.WriteString("SET")
						ELSE WriteType(o.typObj, o.typ, lev + 1, NIL)
						END;
						out.WriteChar(";");
						IF o.value > 0 THEN out.WriteString(" ("); out.WriteInt(o.value); out.WriteString(" bits) *)") END;
						out.WriteLn
					END;
					o := o.next
				END;
				i := 0; WHILE i < lev DO out.WriteTab; INC(i) END;
				out.WriteString("END")
			| arrayTyp:
				out.WriteString("ARRAY [untagged] ");
				IF typ.size > 0 THEN out.WriteInt(typ.size) END;
				out.WriteString(" OF "); WriteType(typ.baseObj, typ.base, lev, NIL)
			| procTyp:
				out.WriteString("PROCEDURE"); WriteParList(typ)
			END
		END
	END WriteType;
	
	PROCEDURE WritePointerDecl (typObj: Object; typ: Type);
		VAR o: Object;
	BEGIN
		CASE typ.form OF
		| recordTyp, unionTyp:
			o := typ.sub;
			WHILE o # NIL DO WritePointerDecl(o.typObj, o.typ); o := o.next END
		| arrayTyp:
			WritePointerDecl(typ.baseObj, typ.base)
		| pointerTyp:
			IF (typ.baseObj = NIL) OR (typ.base.form = procTyp) OR (typ.base.form = pointerTyp) THEN
				WritePointerDecl(typ.baseObj, typ.base)
			ELSIF (typ.base.form >= recordTyp) & (typ.base.form <= unionTyp) THEN
				o := typ.baseObj;
				WHILE replacedOpt IN o.opts DO o := o.base END;
				IF ~(ptrDoneOpt IN o.opts) & (o.inp.mod = actMod) THEN
					IF ~(bodyOpt IN typ.base.opts) THEN
						out.WriteTab; out.WriteTab; WriteName(typ.baseObj, plain);
						out.WriteString("* = RECORD [untagged] (*i*) END; "); out.WriteLn
					END;
					out.WriteTab; out.WriteTab; WriteName(typ.baseObj, ptr);
					out.WriteString("* = POINTER TO "); WriteName(typ.baseObj, plain);
					out.WriteChar(";"); out.WriteLn;
					INCL(o.opts, ptrDoneOpt)
				END
			END
		| procTyp:
			o := typ.sub;
			WHILE o # NIL DO WritePointerDecl(o.typObj, o.typ); o := o.next END;
			WritePointerDecl(typ.baseObj, typ.base)
		ELSE
		END
	END WritePointerDecl;
	
	PROCEDURE WriteModule (mod: Module);
		VAR i, imp: INTEGER; o, p, f, b: Object; u: Type; t: TextModels.Model; m: Module; fn: Files.Name; loc: Files.Locator;
	BEGIN
		actMod := mod; impList := NIL;
		t := TextModels.dir.New(); out.ConnectTo(t); out.SetPos(0);
		out.WriteString("MODULE "); out.WriteString(mod.name);
		out.WriteString(' ["'); out.WriteString(mod.dll); out.WriteString('"];'); out.WriteLn;
		imp := out.rider.Pos();
		IF outConstants THEN
			out.WriteLn; out.WriteTab; out.WriteString("CONST (* macros *)"); out.WriteLn;
			o := firstObj;
			WHILE o # NIL DO
				IF ~(ignoreOpt IN o.opts) & (o.inp.mod = mod) & (o.kind = macro) & (isConstOpt IN o.opts) THEN
					out.WriteTab; out.WriteTab; WriteName(o, plain); out.WriteString("* = ");
					IF isStringOpt IN o.opts THEN
						out.WriteChar('"'); out.WriteString(o.base.name); out.WriteChar('"')
					ELSIF isSetOpt IN o.opts THEN out.WriteSet(BITS(o.value))
					ELSIF o.value = MIN(INTEGER) THEN out.WriteString("80000000H")
					ELSE out.WriteInt(o.value);
						IF outIntAsSet THEN
							out.WriteString(" (* "); out.WriteSet(BITS(o.value)); out.WriteString(" *)")
						END
					END;
					out.WriteChar(";"); out.WriteLn
				END;
				o := o.link
			END
		END;
		IF outEnums THEN
			out.WriteLn; out.WriteTab; out.WriteString("CONST (* enumerations *)"); out.WriteLn;
			o := firstObj;
			WHILE o # NIL DO
				IF ~(ignoreOpt IN o.opts) & (o.inp.mod = mod) & (o.kind = constant) & ~(iidOpt IN o.opts) THEN
					out.WriteTab; out.WriteTab; WriteName(o, plain); out.WriteString("* = ");
					IF isStringOpt IN o.opts THEN
						out.WriteChar('"'); out.WriteString(o.base.name); out.WriteChar('"')
					ELSIF isSetOpt IN o.opts THEN out.WriteSet(BITS(o.value))
					ELSIF o.value = MIN(INTEGER) THEN out.WriteString("80000000H")
					ELSE out.WriteInt(o.value)
					END;
					out.WriteChar(";"); out.WriteLn
				END;
				o := o.link
			END
		END;
		IF outTypes THEN
			out.WriteLn; out.WriteTab; out.WriteString("TYPE"); out.WriteLn;
			o := firstObj;
			WHILE o # NIL DO
				IF ~(ignoreOpt IN o.opts) & (o.inp.mod = mod) THEN
					IF (isTypeOpt IN o.opts) & (o.base.typ.form # pointerTyp) THEN
						out.WriteTab; out.WriteTab; WriteName(o, plain); 
						IF voidOpt IN o.base.typ.opts THEN	(* alias of void *)
							out.WriteString("* = RECORD [untagged] END;"); out.WriteLn;
							out.WriteTab; out.WriteTab; WriteName(o, ptr);
							out.WriteString("* = POINTER TO "); WriteName(o, plain);
							out.WriteChar(";"); out.WriteLn;
							INCL(o.opts, doneOpt);
						ELSE
							out.WriteString("* = "); WriteName(o.base, plain); out.WriteChar(";");
							IF o.kind = macro THEN out.WriteString(" (*m*)") END;
							out.WriteLn;
							INCL(o.opts, doneOpt);
							IF (o.base.typ.form >= recordTyp) & (o.base.typ.form <= arrayTyp)
									& ~(ptrDoneOpt IN o.opts) & (hasPtrOpt IN o.base.typ.opts) THEN
								out.WriteTab; out.WriteTab; WriteName(o, ptr);
								out.WriteString("* = "); WriteName(o.base, ptr);
								out.WriteChar(";"); out.WriteLn;
								INCL(o.opts, ptrDoneOpt)
							END
						END
					ELSIF (o.kind = type) & ((o.typ.form # pointerTyp)
									OR (o.typ.base.form IN {procTyp, recordTyp, unionTyp, pointerTyp}) & (o.typ.baseObj = NIL)) THEN
						WritePointerDecl(o.typObj, o.typ);
						out.WriteTab; out.WriteTab;
						IF (o.typ.form = pointerTyp) & (o.typ.base.form = procTyp) THEN WriteName(o, proc)
						ELSE WriteName(o, plain)
						END;
						out.WriteString("* = "); WriteType(NIL, o.typ, 2, o);
						out.WriteChar(";"); out.WriteLn;
						INCL(o.opts, doneOpt);
						IF (o.typ.form >= recordTyp) & (o.typ.form <= arrayTyp)
								& ~(ptrDoneOpt IN o.opts) & (hasPtrOpt IN o.typ.opts) THEN
							out.WriteTab; out.WriteTab; WriteName(o, ptr);
							out.WriteString("* = POINTER TO "); WriteName(o, plain);
							out.WriteChar(";"); out.WriteLn;
							INCL(o.opts, ptrDoneOpt)
						END
					END
				END;
				o := o.link
			END
		END;
		IF outMethods THEN
			o := firstObj;
			WHILE o # NIL DO
				IF ~(ignoreOpt IN o.opts) & (o.inp.mod = mod) & (o.kind = type) & (o.typ.form = recordTyp) THEN
					f := o.typ.sub;
					WHILE f # NIL DO
						IF (f.typ.form = procTyp)
								& (f.name # "QueryInterface") & (f.name # "AddRef") & (f.name # "Release") THEN
							u := o.typ.base; b := NIL;
							WHILE (u # NIL) & (b = NIL) DO	(* search name in base types *)
								b := u.sub;
								WHILE (b # NIL) & (b.name # f.name) DO b := b.next END;
								u := u.base
							END;
							IF b = NIL THEN
								out.WriteLn; out.WriteTab;
								out.WriteString("PROCEDURE (this: "); WriteName(o, ptr);
								out.WriteString(") "); out.WriteString(f.name);
								out.WriteString("* "); WriteParList(f.typ);
								out.WriteChar(";"); out.WriteLn;
(*
								out.WriteTab; out.WriteString("END "); out.WriteString(f.name);
								out.WriteChar(";"); out.WriteLn
*)
							END
						END;
						f := f.next
					END
				END;
				o := o.link
			END
		END;
		IF outProcedures THEN
			o := firstObj;
			WHILE o # NIL DO
				IF ~(ignoreOpt IN o.opts) & (o.inp.mod = mod) & (o.kind = procedure) & (o.typ.base.form = pointerTyp)
						& ((o.typ.base.base.form <= enumTyp) OR (o.typ.base.base.form = pointerTyp))
						& ~(charOpt IN o.typ.base.base.opts) & ~(voidOpt IN o.typ.base.base.opts) THEN
					FindEntry(o.name, b);
					IF b # NIL THEN	(* declare return type *)
						out.WriteTab; out.WriteTab; WriteName(o, ret); 
						out.WriteString("* = "); WriteType(o.typ.baseObj, o.typ.base, 2, NIL);
						out.WriteChar(";"); out.WriteLn; o.typ.baseObj := o
					END
				END;
				o := o.link
			END;
			o := firstObj;
			WHILE o # NIL DO
				IF ~(ignoreOpt IN o.opts) & (o.inp.mod = mod)
						& ((o.kind = procedure) OR (o.kind = macro) & (isProcOpt IN o.opts)) THEN
					IF o.kind = macro THEN p := o.base ELSE p := o END;
					ASSERT(p.kind = procedure, 100);
					ASSERT(p.typ.form = procTyp, 101);
					IF inlineOpt IN o.opts THEN
						out.WriteLn; out.WriteTab; out.WriteString("PROCEDURE [code] ");
						mod.impSys := TRUE;
						WriteName(o, plain); out.WriteString("* ");
						WriteParList(p.typ);
						out.WriteChar(" ");
						out.WriteInt(o.beg DIV 256 MOD 256);
						out.WriteString(", ");
						out.WriteInt(o.beg MOD 256);
						IF o.end # -1 THEN
							out.WriteString(", ");
							out.WriteInt(o.end DIV 256 MOD 256);
							out.WriteString(", ");
							out.WriteInt(o.end MOD 256)
						END;
						out.WriteChar(";"); out.WriteLn;
					ELSE
						FindEntry(p.name, b);
						IF b # NIL THEN
							INC(b.base.value);
							out.WriteLn; out.WriteTab; out.WriteString("PROCEDURE ");
							IF b.value < 0 THEN out.WriteString("[ccall] "); mod.impSys := TRUE END;
							WriteName(o, plain); out.WriteString("* ");
							IF b.base.name # actMod.dll THEN
								out.WriteString('["'); out.WriteString(b.base.name); out.WriteString('", "');
								IF o # p THEN WriteName(p, plain) END;
								out.WriteString('"] ')
							ELSIF o # p THEN
								out.WriteString('["'); WriteName(p, plain); out.WriteString('"] ')
							END;
							WriteParList(p.typ);
							out.WriteChar(";"); out.WriteLn;
						ELSIF outAllProc THEN
							out.WriteLn; out.WriteTab; out.WriteString("PROCEDURE ");
							WriteName(o, plain); out.WriteString("* ");
							IF o # p THEN
								out.WriteString('["'); WriteName(p, plain); out.WriteString('"] ')
							END;
							WriteParList(p.typ);
							out.WriteChar(";"); out.WriteLn
						ELSIF outMissingProc THEN
							StdLog.String(p.name); StdLog.String(" not found in libraries"); StdLog.Ln
						END
					END
				END;
				o := o.link
			END
		END;
		out.WriteLn; out.WriteString("END "); out.WriteString(mod.name); out.WriteChar("."); out.WriteLn;
		IF (impList # NIL) OR mod.impSys THEN
			out.SetPos(imp);
			out.WriteTab; out.WriteString("IMPORT ");
			IF mod.impSys THEN
				out.WriteString("SYSTEM");
				IF impList # NIL THEN out.WriteString(", ") END
			END;
			m := impList;
			WHILE m # NIL DO
				out.WriteString(m.name);
				IF m.link # NIL THEN out.WriteString(", ") END;
				m := m.link
			END;
			out.WriteChar(";"); out.WriteLn
		END;
		out.ConnectTo(NIL);
		StdDialog.GetSubLoc(mod.name, "Mod", loc, fn);
		loc.res := 77;
		Views.Open(TextViews.dir.New(t), loc, fn, NIL)
	END WriteModule;
	
	PROCEDURE WriteOutput;
		VAR m: Module; i, j: Input;
	BEGIN
		i := inpList;
		WHILE i # NIL DO
			j := i;
			WHILE (j # NIL) & (j.mod = NIL) DO j := j.host END;
			IF j # NIL THEN i.mod := j.mod END;
			i := i.next;
		END;
		m := modList;
		WHILE m # NIL DO
			IF m.name # "" THEN WriteModule(m) END;
			m := m.next
		END
	END WriteOutput;
	
	PROCEDURE (s: Scanner) WriteSym, NEW ;
	BEGIN
		CASE s.sym OF
		| null: StdLog.String(" <null>")
		| pre1: StdLog.String(" #")
		| pre2: StdLog.String(" ##")
		| number: StdLog.Int(s.num)
		| string: StdLog.String(' "'); StdLog.String(s.ident); out.WriteChar('"')
		| ident: StdLog.Char(" "); StdLog.String(s.ident)
		| lpar: StdLog.String(" (")
		| rpar: StdLog.String(" )")
		| lbrak: StdLog.String(" [")
		| rbrak: StdLog.String(" ]")
		| lbrace: StdLog.String(" {")
		| rbrace: StdLog.String(" }")
		| point: StdLog.String(" .")
		| comma: StdLog.String(" ,")
		| inv: StdLog.String(" ~")
		| not: StdLog.String(" !")
		| minus: StdLog.String(" -")
		| plus: StdLog.String(" +")
		| mul: StdLog.String(" *")
		| div: StdLog.String(" /")
		| mod: StdLog.String(" %")
		| and: StdLog.String(" &")
		| or: StdLog.String(" |")
		| xor: StdLog.String(" ^")
		| dblminus: StdLog.String(" --")
		| dblplus: StdLog.String(" ++")
		| dbland: StdLog.String(" &&")
		| dblor: StdLog.String(" ||")
		| lshift: StdLog.String(" <<")
		| rshift: StdLog.String(" >>")
		| eql: StdLog.String(" ==")
		| noteql: StdLog.String(" !=")
		| gtr: StdLog.String(" >")
		| gtreql: StdLog.String(" >=")
		| less: StdLog.String(" <")
		| lesseql: StdLog.String(" <=")
		| question: StdLog.String(" ?")
		| colon: StdLog.String(" :")
		| semicolon: StdLog.String(" ;")
		| becomes: StdLog.String(" =")
		| auto: StdLog.String(" auto")
		| char: StdLog.String(" char")
		| const: StdLog.String(" const")
		| double: StdLog.String(" double")
		| enum: StdLog.String(" enum")
		| extern: StdLog.String(" extern")
		| float: StdLog.String(" float")
		| int: StdLog.String(" int")
		| long: StdLog.String(" long")
		| register: StdLog.String(" register")
		| short: StdLog.String(" short")
		| signed: StdLog.String(" signed")
		| sizeof: StdLog.String(" sizeof")
		| static: StdLog.String(" static")
		| struct: StdLog.String(" struct")
		| switch: StdLog.String(" switch")
		| typedef: StdLog.String(" typedef")
		| union: StdLog.String(" union")
		| unsigned: StdLog.String(" unsigned")
		| virtual: StdLog.String(" virtual")
		| public: StdLog.String(" public")
		| void: StdLog.String(" void")
		| volatile: StdLog.String(" volatile")
		| eof: StdLog.String(" <eof>")
		END
	END WriteSym;
	
	
	(* ---------- scanner ---------- *)
	
	PROCEDURE (s: Scanner) Pos(): INTEGER, NEW;
		VAR p: INTEGER;
	BEGIN
		p := s.rd.Pos();
		IF s.act = 0X THEN RETURN p
		ELSIF s.ahd = 0X THEN RETURN p - 1
		ELSE RETURN p - 2
		END
	END Pos;
	
	PROCEDURE (s: Scanner) Read, NEW;
		VAR t: TextModels.Model; c: Context;
	BEGIN
		REPEAT
			s.act := s.ahd; s.rd.ReadChar(s.ahd);
			WHILE ((s.act = 0X) OR (s.Pos() - 1 >= s.end)) & (s.context # NIL) DO
				IF s.end = MAX(INTEGER) THEN DEC(incLevel) END;
				c := s.context;
				s.end := c.end; s.inp := c.inp; s.act := c.act; s.ahd := c.ahd; s.par := c.par;
				t := s.inp.view.ThisModel(); s.rd := t.NewReader(s.rd); s.rd.SetPos(c.pos);
				s.context := c.next;
			END;
			IF s.Pos() = s.end THEN s.act := 1FX
			ELSIF (s.act = "\") & (s.ahd = 0DX) THEN s.act := 7FX; s.ahd := 7FX
			END
		UNTIL s.act # 7FX
	END Read;
	
	PROCEDURE (s: Scanner) Include (first: CHAR; par: Object; beg, end: INTEGER; inp: Input), NEW;
		VAR c: Context; t: TextModels.Model;
	BEGIN
		NEW(c); c.pos := s.rd.Pos(); c.act := s.act; c.ahd := s.ahd; c.par := s.par;
		c.end := s.end; c.inp := s.inp; c.next := s.context; s.context := c;
		t := inp.view.ThisModel(); s.rd := t.NewReader(s.rd); s.rd.SetPos(beg);
		s.end := end; s.inp := inp; s.par := par;
		s.act := " "; s.ahd := first;
		IF end = MAX(INTEGER) THEN INC(incLevel) END
	END Include;
	
	PROCEDURE (s: Scanner) ConnectTo (t: TextModels.Model; beg, end: INTEGER), NEW;
	BEGIN
		s.rd := t.NewReader(NIL); s.rd.SetPos(beg); s.ahd := 0DX; s.end := end; s.Read;
	END ConnectTo;
	
	PROCEDURE (s: Scanner) Mark (msg: ARRAY OF CHAR), NEW;
		VAR c: Context;
	BEGIN
		IF s.inDef THEN
			s.error := TRUE
		ELSIF (s.context = NIL) OR (s.end = MAX(INTEGER)) THEN
			DevMarkers.Insert(s.rd.Base(), s.Pos(), DevMarkers.dir.NewMsg(msg));
			s.rd.SetPos(s.rd.Pos() + 1); INC(s.inp.errors); INC(errors)
		ELSE
			c := s.context;
			WHILE (c.next # NIL) & (c.end < MAX(INTEGER)) DO c := c.next END;
			DevMarkers.Insert(c.inp.view.ThisModel(), c.pos - 2, DevMarkers.dir.NewMsg(msg));
			INC(c.pos); INC(c.inp.errors); INC(errors)
		END
	END Mark;
	
	PROCEDURE (s: Scanner) Identifier, NEW;
		VAR i: INTEGER;
	BEGIN
		i := 0;
		REPEAT
			s.ident[i] := s.act; INC(i); s.Read
		UNTIL (s.act < "0") OR (s.act > "9") & (CAP(s.act) < "A") OR (CAP(s.act) > "Z") & (s.act # "_");
		s.ident[i] := 0X; s.sym := ident
	END Identifier;
	
	PROCEDURE (s: Scanner) Comment, NEW;
	BEGIN
		IF s.act = "*" THEN
			s.Read;
			REPEAT
				WHILE s.act # "*" DO
					IF s.act = "[" THEN
						s.Read;
						IF (s.act >= "a") & (s.act <= "z") THEN
							s.Identifier;
							IF s.act = "]" THEN
								IF s.ident = "in" THEN INCL(s.hints, inOpt)
								ELSIF s.ident = "out" THEN INCL(s.hints, outOpt)
								ELSIF s.ident = "unique" THEN INCL(s.hints, uniqueOpt)
								END;
								s.Read
							END
						END
					ELSE s.Read
					END
				END;
				s.Read
			UNTIL s.act = "/";
			s.Read
		ELSE
			REPEAT s.Read UNTIL (s.act = 0DX) OR (s.act = 1FX)
		END
	END Comment;
	
	PROCEDURE (s: Scanner) Number, NEW;
		VAR fact, n: INTEGER;
	BEGIN
		n := 0; fact := 10; s.sym := number;
		IF s.act = "0" THEN
			s.Read;
			IF CAP(s.act) = "X" THEN s.Read; fact := 16
			ELSE fact := 8
			END
		END;
		WHILE (s.act >= "0") & ((s.act <= "7") OR (fact >= 10) & (s.act <= "9"))
				OR (fact = 16) & (CAP(s.act) >= "A") & (CAP(s.act) <= "F") DO
			IF s.act <= "9" THEN n := n * fact + ORD(s.act) - ORD("0")
			ELSE n := n * 16 + ORD(CAP(s.act)) - ORD("A") + 10
			END;
			s.Read
		END;
		 IF CAP(s.act) = "U" THEN s.Read END;
		 IF CAP(s.act) = "L" THEN s.Read END;
		s.num := n
	END Number;
	
	PROCEDURE (s: Scanner) String (term: CHAR), NEW;
		VAR i: INTEGER; n: INTEGER; ch: CHAR;
	BEGIN
		i := 0;
		WHILE (s.act # 0X) & (s.act # term) DO
			ch := s.act; s.Read;
			IF ch = "\" THEN
				ch := s.act; s.Read;
				IF ch = "n" THEN ch := 0AX
				ELSIF ch = "t" THEN ch := 09X
				ELSIF ch = "v" THEN ch := 0BX
				ELSIF ch = "b" THEN ch := 08X
				ELSIF ch = "r" THEN ch := 0DX
				ELSIF ch = "f" THEN ch := 0CX
				ELSIF ch = "a" THEN ch := 07X
				ELSIF ch = "0" THEN
					n := 0;
					WHILE (s.act >= "0") & (s.act <= "7") DO
						n := n * 8 + ORD(s.act) - ORD("0");
						s.Read
					END;
					ch := CHR(n)
				ELSIF ch = "x" THEN 
					n := 0;
					WHILE (s.act >= "0") & (s.act <= "9") OR (CAP(s.act) >= "A") & (CAP(s.act) <= "F") DO
						IF s.act <= "9" THEN n := n * 16 + ORD(s.act) - ORD("0")
						ELSE n := n * 16 + ORD(CAP(s.act)) - ORD("A") + 10
						END;
						s.Read
					END;
					ch := CHR(n)
				END
			END;
			IF i < LEN(s.ident) - 1 THEN s.ident[i] := ch; INC(i) END
		END;
		s.Read;
		IF term = "'" THEN
			s.num := ORD(s.ident[0]);
			IF i >= 2 THEN s.num := 256 * s.num + ORD(s.ident[1]) END;
			IF i >= 3 THEN s.num := 256 * s.num + ORD(s.ident[2]) END;
			IF i = 4 THEN s.num := 256 * s.num + ORD(s.ident[3]) END;
			s.sym := number;
			IF (i > 4) & (s.skipLev = 0) THEN s.Mark("wrong char constant") END
		ELSE
			s.ident[i] := 0X; s.sym := string
		END
	END String;
	
	PROCEDURE (s: Scanner) GetLnSym, NEW;
	BEGIN
		REPEAT
			CASE s.act OF
			| 0X: s.sym := eof
			| 1X..0CX: s.Read; s.sym := white
			| 0DX: s.Read; s.sym := eol
			| 0EX.." ": s.Read; s.sym := white
			| "'": s.Read; s.String("'")
			| '"': s.Read; s.String('"')
			| "#": s.Read;
				IF s.act = "#" THEN s.Read; s.sym := pre2 ELSE s.sym := pre1 END
			| "(": s.Read; s.sym := lpar
			| ")": s.Read; s.sym := rpar
			| "[": s.Read; s.sym := lbrak
			| "]": s.Read; s.sym := rbrak
			| "{": s.Read; s.sym := lbrace
			| "}": s.Read; s.sym := rbrace
			| ".": s.Read; s.sym := point
			| ",": s.Read; s.sym := comma
			| "?": s.Read; s.sym := question
			| ":": s.Read; s.sym := colon
			| ";": s.Read; s.sym := semicolon
			| "-": s.Read;
				IF s.act = ">" THEN s.Read; s.sym := deref
				ELSIF s.act = "-" THEN s.Read; s.sym := dblminus
				ELSE s.sym := minus
				END
			| "+": s.Read; IF s.act = "+" THEN s.Read; s.sym := dblplus ELSE s.sym := plus END
			| "*": s.Read; s.sym := mul
			| "/": s.Read; IF (s.act = "*") OR (s.act = "/") THEN s.Comment; s.sym := white ELSE s.sym := div END
			| "%": s.Read; s.sym := mod
			| "&": s.Read; IF s.act = "&" THEN s.Read; s.sym := dbland ELSE s.sym := and END
			| "|": s.Read; IF s.act = "|" THEN s.Read; s.sym := dblor ELSE s.sym := or END
			| "^": s.Read; s.sym := xor
			| "~": s.Read; s.sym := inv
			| "!": s.Read; IF s.act = "=" THEN s.Read; s.sym := noteql ELSE s.sym := not END
			| "=": s.Read; IF s.act = "=" THEN s.Read; s.sym := eql ELSE s.sym := becomes END
			| ">": s.Read;
				IF s.act = "=" THEN s.Read; s.sym := gtreql
				ELSIF s.act = ">" THEN s.Read; s.sym := rshift
				ELSE s.sym := gtr
				END
			| "<": s.Read;
				IF s.act = "=" THEN s.Read; s.sym := lesseql
				ELSIF s.act = "<" THEN s.Read; s.sym := lshift
				ELSIF s.inIncl THEN s.String(">")
				ELSE s.sym := less
				END
			| "\": s.Read; IF s.act = 0DX THEN s.sym := white ELSE s.sym := null END
			| "0".."9": s.Number
			| "a": s.Identifier; IF s.ident = "auto" THEN s.sym := auto END
			| "c": s.Identifier; IF s.ident = "char" THEN s.sym := char ELSIF s.ident = "const" THEN s.sym := const END
			| "d": s.Identifier; IF s.ident = "double" THEN s.sym := double END
			| "e": s.Identifier; IF s.ident = "enum" THEN s.sym := enum ELSIF s.ident = "extern" THEN s.sym := extern END
			| "f": s.Identifier; IF s.ident = "float" THEN s.sym := float END
			| "i": s.Identifier; IF s.ident = "int" THEN s.sym := int END
			| "l": s.Identifier; IF s.ident = "long" THEN s.sym := long END
			| "r": s.Identifier; IF s.ident = "register" THEN s.sym := register END
			| "s": s.Identifier;
				IF s.ident = "short" THEN s.sym := short
				ELSIF s.ident = "signed" THEN s.sym := signed
				ELSIF s.ident = "sizeof" THEN s.sym := sizeof
				ELSIF s.ident = "static" THEN s.sym := static
				ELSIF s.ident = "struct" THEN s.sym := struct
				ELSIF s.ident = "switch" THEN s.sym := switch
				END
			| "p": s.Identifier; IF s.ident = "public" THEN s.sym := public END
			| "t": s.Identifier; IF s.ident = "typedef" THEN s.sym := typedef END
			| "u": s.Identifier; IF s.ident = "union" THEN s.sym := union ELSIF s.ident = "unsigned" THEN s.sym := unsigned END
			| "v": s.Identifier;
				IF s.ident = "virtual" THEN s.sym := virtual
				ELSIF s.ident = "void" THEN s.sym := void
				ELSIF s.ident = "volatile" THEN s.sym := volatile
				END
			| "L":
				IF s.ahd = '"' THEN s.Read; s.Read; s.String('"')
				ELSIF s.ahd = "'" THEN s.Read; s.Read; s.String("'")
				ELSE s.Identifier
				END
			| "b", "g", "h", "j", "k", "m".."o", "q", "w".."z", "A".."K", "M".."Z", "_": s.Identifier
			ELSE s.Read; s.sym := null
			END;
		UNTIL s.sym # white
	END GetLnSym;
	
	PROCEDURE (s: Scanner) GetSubSym, NEW;
		VAR first, last, fp, ap, o: Object; nest, i: INTEGER; rd: TextModels.Reader; t: TextModels.Model;
	BEGIN
		REPEAT
			s.GetLnSym;
			IF (s.sym = pre1) & (s.par # NIL) & (s.skipLev = 0) THEN	(* literal parameter *)
				s.GetLnSym;
				IF s.sym = ident THEN
					o := s.par;
					WHILE (o # NIL) & (o.name # s.ident) DO o := o.next END;
					IF o # NIL THEN
						t := o.inp.view.ThisModel(); rd := t.NewReader(NIL);
						rd.SetPos(o.beg); i := 0;
						WHILE (i < LEN(s.ident) - 1) & (rd.Pos() < o.end) DO
							rd.ReadChar(s.ident[i]); INC(i)
						END;
						s.ident[i] :=0X; s.sym := string
					ELSE s.Mark("illegal use of '#'")
					END
				ELSE s.Mark("illegal use of '#'")
				END
			ELSIF (s.sym = ident) & (s.skipLev = 0) THEN
				o := s.par;
				WHILE (o # NIL) & (o.name # s.ident) DO o := o.next END;
				IF o # NIL THEN	(* substitute parameter *)
					s.Include(" ", o.typObj, o.beg, o.end, o.inp);
					s.sym := white
				ELSE
					IF s.ident = "REFIID" THEN INCL(s.hints, refOpt); INCL(s.hints, iidOpt)
					ELSIF s.ident = "REFGUID" THEN INCL(s.hints, refOpt)
					ELSIF s.ident = "REFCLSID" THEN INCL(s.hints, refOpt)
					END;
					Find(s.ident, o);
					IF s.ident = "DEFINE_GUID" THEN o := NIL END;
					IF (o # NIL) & (o.kind = macro) THEN	(* substitute macro *)
						IF o.typObj # NIL THEN	(* parameterized macro *)
							WHILE (s.act = " ") OR (s.act = 09X) OR (s.act = 1FX) DO s.Read END;
							IF s.act = "(" THEN
								s.GetLnSym; ASSERT(s.sym = lpar, 100);
								first := NIL; last := NIL; fp := o.typObj; nest := 1;
								REPEAT
									IF fp # NIL THEN
										InsertIn(first, last, fp.name, mPar, ap);
										fp := fp.next
									ELSE s.Mark("too many parameters")
									END;
									ap.inp := s.inp; ap.beg := s.Pos(); ap.typObj := s.par;
									REPEAT
										s.GetLnSym;
										IF s.sym = lpar THEN INC(nest)
										ELSIF s.sym = rpar THEN DEC(nest)
										END;
										IF (* (s.sym = eol) OR *) (s.sym = eof) THEN nest := 0; s.Mark("missing ')'") END
									UNTIL (nest = 0) OR (nest = 1) & (s.sym = comma);
									ap.end := s.Pos() - 1;
								UNTIL nest = 0;
								IF fp # NIL THEN s.Mark("too few parameters") END;
								s.Include(" ", first, o.beg, o.end, o.inp);
								s.sym := white
							END
						ELSE
							s.Include(" ", NIL, o.beg, o.end, o.inp);
							s.sym := white
						END
					END
				END
			END
		UNTIL s.sym # white
	END GetSubSym;
	
	PROCEDURE (s: Scanner) EvalDef (VAR x: INTEGER), NEW;
		VAR sym: INTEGER; o: Object;
	BEGIN
		s.GetLnSym;
		sym := null; x := 0;
		IF s.sym = lpar THEN sym := lpar; s.GetLnSym END;
		IF s.sym = ident THEN
			Find(s.ident, o);
			IF (o # NIL) & (o.kind = macro) THEN x := 1 END
		ELSE s.Mark("identifier missing")
		END;
		s.GetSubSym;
		IF sym = lpar THEN
			IF s.sym = rpar THEN s.GetSubSym ELSE s.Mark("')' missing") END;
		END
	END EvalDef;
	
	PROCEDURE^ ConstExp (s: Scanner; VAR x: INTEGER);
	PROCEDURE^ ReadIdlFile (IN libName: Name);
	PROCEDURE^ ReadLibFile (IN libName: Name);

	PROCEDURE (s: Scanner) HandlePreProc, NEW;
		VAR x: INTEGER; o, p, old, last: Object; i: Input; name: Name; m: Module;
	BEGIN
		s.GetSubSym;
		IF s.sym = ident THEN
			IF s.ident = "if" THEN
				INC(s.level);
				IF s.skipLev = 0 THEN
					s.GetSubSym; s.inPre := TRUE; ConstExp(s, x); s.inPre := FALSE;
					IF x = 0 THEN s.skipLev := s.level END
				END
			ELSIF s.ident = "ifdef" THEN
				INC(s.level);
				IF s.skipLev = 0 THEN
					s.EvalDef(x);
					IF x = 0 THEN s.skipLev := s.level END
				END
			ELSIF s.ident = "ifndef" THEN
				INC(s.level);
				IF s.skipLev = 0 THEN
					s.EvalDef(x);
					IF x # 0 THEN s.skipLev := s.level END
				END
			ELSIF s.ident = "elif" THEN
				IF s.skipLev = 0 THEN s.skipLev := -s.level
				ELSIF s.skipLev = s.level THEN
					s.skipLev := 0;
					s.GetSubSym; s.inPre := TRUE; ConstExp(s, x); s.inPre := FALSE;
					IF x = 0 THEN s.skipLev := s.level END
				END
			ELSIF s.ident = "else" THEN
				IF s.skipLev = 0 THEN s.skipLev := -s.level
				ELSIF s.skipLev = s.level THEN s.skipLev := 0
				END
			ELSIF s.ident = "endif" THEN
				IF s.level = ABS(s.skipLev) THEN s.skipLev := 0 END;
				IF s.level > 0 THEN DEC(s.level)
				ELSE s.Mark("unbalanced endif")
				END
			ELSIF s.skipLev = 0 THEN
				IF s.ident  = "define" THEN
					s.GetLnSym;
					IF s.sym = ident THEN
						name := s.ident$;
						Find(name, old);
						IF (old # NIL) & (old.kind = macro) THEN o := old ELSE Insert(name, macro, s.inp, o) END;
						IF s.act = "(" THEN
							s.GetLnSym; ASSERT(s.sym = lpar, 100);
							s.GetLnSym; last := NIL;
							WHILE s.sym = ident DO
								InsertIn(o.typObj, last, s.ident, mPar, p);
								s.GetLnSym;
								IF s.sym = comma THEN s.GetLnSym
								ELSIF s.sym # rpar THEN s.Mark("missing ','")
								END
							END;
							IF s.sym # rpar THEN s.Mark("missing ')'") END;
						END;
						o.inp := s.inp; o.beg := s.Pos();
						s.inDef := TRUE; s.error := FALSE;
						s.GetLnSym;
						IF (old = NIL) & (s.sym # eol) & (o.typObj = NIL) THEN ListInsert(o) END;
(*
						s.GetSubSym;
						IF (old = NIL) & (s.sym # eol) & (o.typObj = NIL) THEN
							IF s.sym = string THEN
								NEW(p); COPY(s.ident, p.name); s.GetSubSym;
								 IF s.sym = eol THEN
									INCL(o.opts, isStringOpt); INCL(o.opts, isConstOpt);
									o.base := p; ListInsert(o)
								END
							ELSE
								IF s.sym = ident THEN Find(s.ident, p) ELSE p := NIL END;
								IF (p # NIL) & ((p.kind = type) OR (p.kind = procedure)) THEN
									s.GetSubSym;
									IF s.sym = eol THEN
										IF p.kind = type THEN INCL(o.opts, isTypeOpt)
										ELSE INCL(o.opts, isProcOpt)
										END;
										o.base := p; ListInsert(o)
									END
								ELSE
									s.inPre := TRUE; ConstExp(s, x); s.inPre := FALSE;
									IF ~s.error & (s.sym = eol) THEN
										o.value := x; INCL(o.opts, isConstOpt); ListInsert(o)
									END
								END
							END
						END;
*)
						WHILE (s.sym # eol) & (s.sym # eof) DO s.GetLnSym END;
						o.end := s.Pos() - 1; s.inDef := FALSE
					ELSE s.Mark("missing identifier")
					END
				ELSIF s.ident = "undef" THEN
					s.GetLnSym;
					IF s.sym = ident THEN Remove(s.ident)
					ELSE s.Mark("missing identifier")
					END
				ELSIF s.ident = "include" THEN
					s.inIncl := TRUE; s.GetSubSym; s.inIncl := FALSE;
					IF s.sym = string THEN
						OpenFile(s.ident, s.inp, i);
						IF i # NIL THEN
							s.Include(0DX, NIL, 0, MAX(INTEGER), i);
							s.GetSubSym;
							RETURN
						ELSE s.Mark("file not found")
						END
					ELSE s.Mark("illegal file name")
					END
				ELSIF s.ident = "pragma" THEN
					s.GetLnSym;
					IF (s.sym = ident) & (s.ident = "pack") THEN
						s.GetLnSym;
						IF s.sym = lpar THEN
							s.GetLnSym;
							IF s.sym = number THEN
								IF (s.num = 1) OR (s.num = 2) OR (s.num = 4) OR (s.num = 8) THEN
									alignStack[alignIdx] := s.num
								ELSE s.Mark("illegal value")
								END;
								s.GetLnSym
							ELSIF s.sym = ident THEN
								IF s.ident = "push" THEN
									INC(alignIdx); s.GetLnSym;
									IF s.sym = comma THEN
										s.GetLnSym;
										IF s.sym = number THEN
											IF (s.num = 1) OR (s.num = 2) OR (s.num = 4) OR (s.num = 8) THEN
												alignStack[alignIdx] := s.num
											ELSE s.Mark("illegal value")
											END;
											s.GetLnSym
										ELSE s.Mark("illegal parameter")
										END
									ELSE alignStack[alignIdx] := alignStack[alignIdx - 1]
									END
								ELSIF s.ident = "pop" THEN
									DEC(alignIdx);
									IF alignIdx < 0 THEN s.Mark("align stack underflow") END;
									s.GetLnSym
								ELSE s.Mark("illegal parameter")
								END
							ELSIF s.sym = rpar THEN
								alignStack[alignIdx] := 8	(* use default *)
							ELSE s.Mark("illegal parameter")
							END;
							IF s.sym # rpar THEN s.Mark("missing ')'") END
						ELSE s.Mark("missing '('")
						END
					END
				ELSIF s.ident = "PATH" THEN
					s.GetSubSym;
					IF s.sym = string THEN
						path := Files.dir.This(s.ident)
					ELSE s.Mark("illegal path")
					END
				ELSIF s.ident = "LIBPATH" THEN
					s.GetSubSym;
					IF s.sym = string THEN
						libPath := Files.dir.This(s.ident)
					ELSE s.Mark("illegal path")
					END
				ELSIF s.ident = "PRINT" THEN
					s.GetSubSym;
					WHILE (s.sym # eol) & (s.sym # eof) DO
						s.WriteSym;
						s.GetSubSym
					END;
					StdLog.Ln
				ELSIF s.ident = "LIB" THEN
					IF errors = 0 THEN
						s.GetSubSym;
						WHILE s.sym = string DO
							ReadLibFile(s.ident); s.GetSubSym
						END
					END
				ELSIF s.ident = "IDL" THEN
					IF errors = 0 THEN
						s.GetSubSym;
						WHILE s.sym = string DO
							ReadIdlFile(s.ident); s.GetSubSym
						END
					END
				ELSIF s.ident = "USE" THEN
					s.GetSubSym;
					IF s.sym = ident THEN
						Insert(s.ident, type, NIL, o); s.GetSubSym;
						IF s.sym = ident THEN
							NEW(p); p.kind := type; p.name := s.ident$;
							NEW(p.typ); p.typ.form := basicTyp; o.typ := p.typ;
							o.base := p; INCL(o.opts, ignoreOpt); INCL(o.opts, replacedOpt);
							INCL(p.opts, listOpt);	(* prevents from being inserted in list *)
							s.GetSubSym;
							IF s.sym = point THEN
								s.GetSubSym;
								IF s.sym = ident THEN
									m := modList;
									WHILE (m # NIL) & (m.name # p.name) DO m := m.next END;
									IF m = NIL THEN
										NEW(m); m.name := p.name$; m.next := modList; modList := m
									END;
									NEW(i); i.mod := m; p.inp := i; p.name := s.ident$;
									s.GetSubSym
								ELSE s.Mark("missing identifier")
								END
							END;
							IF s.sym = number THEN
								IF (s.num = 1) OR (s.num = 2) OR (s.num = 4) OR (s.num = 8) THEN
									p.typ.align := SHORT(s.num); s.GetSubSym;
									IF s.sym = number THEN	(* option *)
										INCL(p.typ.opts, s.num)
									END
								ELSE s.Mark("illegal alignment"); p.value := 1
								END
							ELSE s.Mark("missing alignment")
							END
						ELSE s.Mark("missing identifier")
						END
					ELSE s.Mark("missing identifier")
					END
				ELSIF s.ident = "USESET" THEN
					s.GetSubSym;
					WHILE s.sym = string DO
						name := s.ident$; s.GetSubSym;
						IF s.sym = plus THEN
							s.GetSubSym; o := firstObj;
							WHILE o # NIL DO
								IF (o.kind = procedure) OR (o.kind = type) THEN
									ChangeToSet(o.typ, name)
								END;
								o := o.link
							END
						ELSIF s.sym = mul THEN
							s.GetSubSym; FindPrefix(name, NIL, o);
							WHILE o # NIL DO INCL(o.opts, isSetOpt); FindPrefix(name, o, o) END
						ELSIF s.sym = not THEN
							s.GetSubSym; FindPrefix(name, NIL, o);
							WHILE o # NIL DO EXCL(o.opts, isSetOpt); FindPrefix(name, o, o) END
						ELSE
							Find(name, o);
							IF o # NIL THEN INCL(o.opts, isSetOpt) END
						END
					END
				ELSIF s.ident = "OUTPUT" THEN
					s.GetSubSym;
					IF s.sym = string THEN
						IF s.ident  = "" THEN i := s.inp
						ELSE
							i := inpList;
							WHILE (i # NIL) & (i.name # s.ident) DO i := i.next END
						END;
						IF i # NIL THEN
							s.GetSubSym;
							IF s.sym = string THEN
								m := modList;
								WHILE (m # NIL) & (m.name # s.ident) DO m := m.next END;
								IF m = NIL THEN
									NEW(m); m.name := s.ident$; m.next := modList; modList := m
								END;
								i.mod := m; s.GetSubSym;
								IF (s.sym = string) & (m.dll = "") THEN m.dll := s.ident$ END
							ELSE s.Mark("missing module name")
							END
						ELSE s.Mark("unknown file name")
						END
					ELSE s.Mark("missing file name")
					END
				ELSE s.Mark("illegal directive")
				END
			END
		ELSIF (s.sym # eol) & (s.skipLev = 0) THEN s.Mark("illegal directive") 
		END;
		WHILE (s.sym # eof) & (s.sym # eol) DO s.GetSubSym END
	END HandlePreProc;
	
	PROCEDURE (s: Scanner) GetSym, NEW;
	BEGIN
		REPEAT
			s.GetSubSym;
			WHILE s.sym = eol DO
				s.GetSubSym;
				IF s.sym = pre1 THEN s.HandlePreProc END
			END;
			IF s.sym = pre1 THEN s.Mark("illegal use of '#'")
			ELSIF s.sym = pre2 THEN s.Mark("illegal use of '##'")
			END;
		UNTIL (s.skipLev = 0) & (s.sym # pre1) & (s.sym # pre2) OR (s.sym = eof)
	END GetSym;
	
	PROCEDURE (s: Scanner) GetESym, NEW;
	BEGIN
		IF s.inPre THEN s.GetSubSym ELSE s.GetSym END
	END GetESym;
	
	
	(* ---------- idl parser ---------- *)
	
	PROCEDURE ReadIdlFile (IN libName: Name);
		VAR st: Stores.Store; t: TextModels.Model; fn: Files.Name; s: Scanner; str, n: Name; i: INTEGER; o: Object;
	BEGIN
		StdLog.String("open "); fn := libName$;
		st := Views.Old(FALSE, path, fn, conv);
		StdLog.String(libName);
		IF st = NIL THEN StdLog.String(" not found") END;
		StdLog.Ln;
		IF (st # NIL) & (st IS TextViews.View) THEN
			t := st(TextViews.View).ThisModel();
			NEW(s); s.ConnectTo(t, 0, MAX(INTEGER));
			s.GetLnSym; str := "";
			WHILE s.sym # eof DO
				IF s.sym = lbrak THEN
					s.GetLnSym; str := "";
					WHILE (s.sym # eof) & (s.sym # rbrak) DO
						IF (s.sym = ident) & (s.ident = "uuid") THEN
							s.GetLnSym;
							IF s.sym = lpar THEN
								str[0] := "{"; i := 1;
								WHILE s.act # ")" DO str[i] := s.act; INC(i); s.Read END;
								str[i] := "}"; INC(i); str[i] := 0X
							END
						END;
						s.GetLnSym
					END;
				ELSIF (s.sym = ident) & (s.ident = "interface") THEN
					s.GetLnSym;
					IF (s.sym = ident) & (str # "") THEN
						n := "IID_"; n := n + s.ident;
						Insert(n, constant, NIL, o);
						INCL(o.opts, ignoreOpt); INCL(o.opts, isStringOpt); INCL(o.opts, iidOpt);
						NEW(o.base); o.base.name := str$
					END;
					str := ""
				END;
				s.GetLnSym
			END
		END
	END ReadIdlFile;
	
	
	(* ---------- library parser ---------- *)
	
	PROCEDURE ReadLibFile (IN libName: Name);
		VAR f: Files.File; r: Files.Reader; fn: Files.Name; name: Name;
			ch, t: CHAR; n, i, s: INTEGER; lib, o, p: Object; b: BYTE;
	BEGIN
		fn := libName$; fn := fn + ".lib";
		StdLog.String("open "); StdLog.String(fn); StdLog.Ln;
		f := Files.dir.Old(libPath, fn, FALSE);
		r := f.NewReader(NIL); r.SetPos(46H);
		r.ReadByte(b); n := b MOD 256;
		r.ReadByte(b); n := n * 256 + b MOD 256;
		r.SetPos(r.Pos() + 4 * n);
		NEW(lib); lib.kind := library; lib.next := libList; libList := lib;
		lib.name := libName$; lib.name := lib.name + ".dll";
		WHILE n > 0 DO
			r.ReadByte(b); t := CHR(b MOD 256);
			r.ReadByte(b); ch := CHR(b MOD 256); i := 0;
			WHILE (ch # 0X) & (ch # "@") DO name[i] := ch; INC(i); r.ReadByte(b); ch := CHR(b MOD 256) END;
			name[i] := 0X;
			IF ch = "@"THEN
				r.ReadByte(b); ch := CHR(b MOD 256); s := 0;
				WHILE ch # 0X DO s := 10 * s + ORD(ch) - ORD("0"); r.ReadByte(b); ch := CHR(b MOD 256) END
			ELSE s := -1
			END;
			IF t = "_" THEN
				FindEntry(name, o);
				IF o = NIL THEN
					Insert(name, libEntry, NIL, o);
					o.value := s; o.base := lib
				END
			END;
			DEC(n)
		END;
		f.Close
	END ReadLibFile;
	
	PROCEDURE WriteLibList;
		VAR o: Object;
	BEGIN
		o := libList;
		WHILE o # NIL DO
			StdLog.String(o.name); StdLog.Int(o.value); StdLog.Ln;
			o := o.next
		END
	END WriteLibList;
	
	
	(* ---------- parser ---------- *)
	
	PROCEDURE CExp11 (s: Scanner; VAR x: INTEGER);
		VAR sym: INTEGER; o: Object;
	BEGIN
		x := 0;
		IF (s.sym = plus) OR (s.sym = minus) OR (s.sym = inv) OR (s.sym = not) THEN
			sym := s.sym; s.GetESym; CExp11(s, x);
			IF sym = minus THEN x := -x
			ELSIF sym = inv THEN x := -1 - x
			ELSIF sym = not THEN
				IF x # 0 THEN x := 0 ELSE x := 1 END
			END
		ELSIF s.sym = ident THEN
			IF s.inPre & (s.ident = "defined") THEN
				s.EvalDef(x)
			ELSE
				Find(s.ident, o);
				IF (o # NIL) & (o.kind = constant) & ~(isStringOpt IN o.opts) THEN
					x := o.value; s.GetESym
				ELSE
					s.GetESym;
					IF ~s.inShort THEN s.Mark("undefined value") END
				END
			END
		ELSIF s.sym = number THEN x := s.num; s.GetESym
		ELSIF s.sym = lpar THEN
			s.GetESym;
			IF s.sym = ident THEN Find(s.ident, o) ELSE o := NIL END;
			IF (s.sym >= signed) & (s.sym <= long) OR (o # NIL) & (o.kind = type) & (replacedOpt IN o.opts) THEN
				sym := s.sym; s.GetESym;
				IF ((sym = signed) OR (sym = unsigned)) & (s.sym >= void) & (s.sym <= double) THEN
					sym := s.sym; s.GetESym
				END;
				IF ((sym = short) OR (sym = long)) & (s.sym = int) THEN
					sym := s.sym; s.GetESym
				END;
				IF s.sym = rpar THEN s.GetESym; CExp11(s, x)
				ELSE s.Mark("illegal cast")
				END
			ELSE
				ConstExp(s, x);
				IF s.sym = rpar THEN s.GetESym ELSE s.Mark("')' missing") END
			END
		ELSE s.Mark("illegal primary");
		END
	END CExp11;
	
	PROCEDURE CExp10 (s: Scanner; VAR x: INTEGER);
		VAR y: INTEGER; sym: INTEGER;
	BEGIN
		CExp11(s, x);
		WHILE (s.sym = mul) OR (s.sym = div) OR (s.sym = mod) DO
			sym := s.sym; s.GetESym; CExp11(s, y);
			IF sym = mul THEN x := x * y
			ELSIF y = 0 THEN x := 0
			ELSIF sym = div THEN x := x DIV y
			ELSE x := x MOD y
			END
		END
	END CExp10;
	
	PROCEDURE CExp9 (s: Scanner; VAR x: INTEGER);
		VAR y: INTEGER; sym: INTEGER;
	BEGIN
		CExp10(s, x);
		WHILE (s.sym = plus) OR (s.sym = minus) DO
			sym := s.sym; s.GetESym; CExp10(s, y);
			IF sym = plus THEN x := x + y
			ELSE x := x - y
			END
		END
	END CExp9;
	
	PROCEDURE CExp8 (s: Scanner; VAR x: INTEGER);
		VAR y: INTEGER; sym: INTEGER;
	BEGIN
		CExp9(s, x);
		WHILE (s.sym = lshift) OR (s.sym = rshift) DO
			sym := s.sym; s.GetESym; CExp9(s, y);
			IF sym = lshift THEN x := ASH(x, y)
			ELSE x := ASH(x, -y)
			END
		END
	END CExp8;
	
	PROCEDURE CExp7 (s: Scanner; VAR x: INTEGER);
		VAR y: INTEGER; sym: INTEGER;
	BEGIN
		CExp8(s, x);
		WHILE (s.sym = gtr) OR (s.sym = gtreql) OR (s.sym = less) OR (s.sym = lesseql) DO
			sym := s.sym; s.GetESym; CExp8(s, y);
			IF (sym = gtr) & (x > y)
				OR (sym = gtreql) & (x >= y)
				OR (sym = less) & (x < y)
				OR (sym = lesseql) & (x <= y) THEN x := 1 ELSE x := 0 END
		END
	END CExp7;
	
	PROCEDURE CExp6 (s: Scanner; VAR x: INTEGER);
		VAR y: INTEGER; sym: INTEGER;
	BEGIN
		CExp7(s, x);
		WHILE (s.sym = eql) OR (s.sym = noteql) DO
			sym := s.sym; s.GetESym; CExp7(s, y);
			IF (x = y) = (sym = eql) THEN x := 1 ELSE x := 0 END
		END
	END CExp6;
	
	PROCEDURE CExp5 (s: Scanner; VAR x: INTEGER);
		VAR y: INTEGER;
	BEGIN
		CExp6(s, x);
		WHILE s.sym = and DO
			s.GetESym; CExp6(s, y);
			x := ORD(BITS(x) * BITS(y))
		END
	END CExp5;
	
	PROCEDURE CExp4 (s: Scanner; VAR x: INTEGER);
		VAR y: INTEGER;
	BEGIN
		CExp5(s, x);
		WHILE s.sym = xor DO
			s.GetESym; CExp5(s, y);
			x := ORD(BITS(x) / BITS(y))
		END
	END CExp4;
	
	PROCEDURE CExp3 (s: Scanner; VAR x: INTEGER);
		VAR y: INTEGER;
	BEGIN
		CExp4(s, x);
		WHILE s.sym = or DO
			s.GetESym; CExp4(s, y);
			x := ORD(BITS(x) + BITS(y))
		END
	END CExp3;
	
	PROCEDURE CExp2 (s: Scanner; VAR x: INTEGER);
		VAR y: INTEGER; b: BOOLEAN;
	BEGIN
		b := s.inShort; CExp3(s, x);
		WHILE s.sym = dbland DO
			IF x = 0 THEN s.inShort := TRUE END;
			s.GetESym; CExp3(s, y);
			IF (x # 0) & (y # 0) THEN x := 1 ELSE x := 0 END
		END;
		s.inShort := b
	END CExp2;
	
	PROCEDURE CExp1 (s: Scanner; VAR x: INTEGER);
		VAR y: INTEGER; b: BOOLEAN;
	BEGIN
		b := s.inShort; CExp2(s, x);
		WHILE s.sym = dblor DO
			IF x # 0 THEN s.inShort := TRUE END;
			s.GetESym; CExp2(s, y);
			IF (x # 0) OR (y # 0) THEN x := 1 ELSE x := 0 END
		END;
		s.inShort := b
	END CExp1;
	
	PROCEDURE ConstExp (s: Scanner; VAR x: INTEGER);
		VAR y, z: INTEGER;
	BEGIN
		CExp1(s, x);
		IF s.sym = question THEN
			s.GetESym; ConstExp(s, y);
			IF s.sym = colon THEN
				s.GetESym; ConstExp(s, z);
				IF x # 0 THEN x := y ELSE x := z END
			ELSE s.Mark("':' missing")
			END
		END
	END ConstExp;
	
	PROCEDURE Attributes (s: Scanner; VAR opts: SET);
	BEGIN
		WHILE (s.sym >= typedef) & (s.sym <= volatile) 
		OR (s.sym = ident) & (
			(s.ident = "__asm") OR (s.ident = "__fastcall") OR (s.ident = "__loadds")
			OR (s.ident = "__segname") OR (s.ident = "__based") OR (s.ident = "__fortran")
			OR (s.ident = "__near") OR (s.ident = "__self") OR (s.ident = "__cdecl")
			OR (s.ident = "__huge") OR (s.ident = "__pascal") OR (s.ident = "__export")
			OR (s.ident = "__inline") OR (s.ident = "__saveregs") OR (s.ident = "__far")
			OR (s.ident = "__interrupt") OR (s.ident = "__segment") OR (s.ident = "__stdcall")
		) DO
			IF s.sym = const THEN
				INCL(opts, constOpt); s.GetSym
			ELSIF s.sym = typedef THEN
				INCL(opts, typedefOpt); s.GetSym
			ELSIF s.sym = public THEN
				s.GetSym;
				IF s.sym = colon THEN s.GetSym END
			ELSIF s.sym = ident THEN
				IF s.ident = "__cdecl" THEN INCL(opts, cdeclOpt)
				ELSIF (s.ident = "__asm") OR (s.ident = "__inline") OR (s.ident = "__fastcall") THEN INCL(opts, ignoreOpt)
				END;
				s.GetSym
			ELSE s.GetSym
			END
		END
	END Attributes;
	
	PROCEDURE^ TypeSpec (s: Scanner; VAR opts: SET; VAR typObj: Object; VAR typ: Type);

	PROCEDURE Declarator (s:Scanner; abstract: BOOLEAN; VAR opts: SET; VAR name: Name; VAR typ: Type);
		VAR x: INTEGER; popts: SET; ptyp, btyp, t, t1: Type; last, o, o1: Object; n: Name;
	BEGIN
		btyp := NIL; typ := NIL;
		WHILE (s.sym = mul) OR (s.sym = and) DO
			s.GetSym;
			IF btyp # NIL THEN INCL(btyp.opts, hasPtrOpt) END;
			NEW(t); t.form := pointerTyp; t.base := btyp; btyp := t;
			Attributes(s, t.opts); opts := opts + (t.opts - {constOpt})
		END;
		IF s.sym = ident THEN
			name := s.ident$;
			IF s.ident = "operator" THEN
				s.GetSym; INCL(opts, ignoreOpt);
				IF (s.sym >= inv) & (s.sym <= lesseql) THEN s.GetSym END
			ELSE
				s.GetSym
			END
		ELSIF s.sym = lpar THEN
			s.GetSym;Attributes(s, opts);
			Declarator(s, abstract, opts, name, typ);
			IF s.sym = rpar THEN s.GetSym ELSE s.Mark("missing ')'") END
		ELSE
			name := "";
			IF ~abstract THEN s.Mark("illegal declarator") END
		END;
		WHILE (s.sym = lpar) OR (s.sym = lbrak) DO
			IF s.sym = lbrak THEN	(* array *)
				s.GetSym; x := 0;
				IF s.sym # rbrak THEN ConstExp(s, x) END;
				NEW(t); t.form := arrayTyp; t.size := x; Append(o, typ, NIL, t);
				IF s.sym = rbrak THEN s.GetSym ELSE s.Mark("missing ']'") END
			ELSE	(* parameter list *)
				NEW(ptyp); ptyp.form := procTyp; Append(o, typ, NIL, ptyp);
				s.hints := {}; s.GetSym; last := NIL;
				WHILE (s.sym >= auto) & (s.sym <= union) OR (s.sym = ident) DO
					TypeSpec(s, popts, o1, t1);
					Declarator(s, TRUE, popts, n, t);
					InsertIn(ptyp.sub, last, n, param, o);
					o.typ := t; Append(o.typObj, o.typ, o1, t1);
					o.opts := popts + s.hints; s.hints := {};
					IF s.sym = comma THEN s.GetSym END
				END;
				IF s.sym = point THEN
					s.GetSym;
					IF s.sym = point THEN
						s.GetSym;
						IF s.sym = point THEN s.GetSym END
					END;
					INCL(opts, cdeclOpt)
				END;
				IF s.sym = rpar THEN s.GetSym ELSE s.Mark("missing ')'") END
			END
		END;
		Append(o, typ, NIL, btyp)
	END Declarator;
	
	PROCEDURE TypeSpec (s: Scanner; VAR opts: SET; VAR typObj: Object; VAR typ: Type);
		VAR x: INTEGER; found : BOOLEAN; fopts: SET; name: Name; ftyp, t: Type; last, o, to: Object;
	BEGIN
		opts := {}; Attributes(s, opts);
		IF (s.sym = struct) OR (s.sym = union) OR (s.sym = enum) THEN
			typObj := NIL; typ := NIL; x := s.sym; s.GetSym;
			IF s.sym = ident THEN
				Find(s.ident, typObj);
				IF typObj # NIL THEN
					ASSERT(typObj.kind = type, 100);
					typ := typObj.typ
				ELSE
					Insert(s.ident, type, s.inp, typObj)
				END;
				s.GetSym;
			END;
			IF typ = NIL THEN NEW(typ) END;
			IF typObj # NIL THEN typObj.typ := typ END;
			typ.opts := typ.opts + opts; typ.align := SHORT(alignStack[alignIdx]);
			IF x = struct THEN
				typ.form := recordTyp;
				IF s.sym = colon THEN
					s.GetSym;
					TypeSpec(s, fopts, typ.baseObj, typ.base);
				END
			ELSIF x = union THEN typ.form := unionTyp
			ELSE typ.form := enumTyp
			END;
			IF s.sym = lbrace THEN
				s.GetSym; INCL(opts, bodyOpt); INCL(typ.opts, bodyOpt);
				IF x = enum THEN
					x := 0;
					WHILE s.sym = ident DO
						name := s.ident$; s.GetSym;
						IF s.sym = becomes THEN s.GetSym; ConstExp(s, x) END;
						Insert(name, constant, s.inp, o); o.value := x;
						typ.min := MIN(typ.min, x);
						typ.max := MAX(typ.max, x);
						ListInsert(o);
						IF s.sym = comma THEN s.GetSym END;
						INC(x)
					END
				ELSE
					last := NIL; x := 0;
					WHILE (s.sym >= auto) & (s.sym <= union) OR (s.sym = ident) DO
						TypeSpec(s, fopts, to, t);
						IF (s.sym = semicolon) & (bodyOpt IN fopts) THEN	(* nameless local struct or union *)
							InsertIn(typ.sub, last, "", field, o);
							o.typ := t; o.typObj := to; o.opts := fopts
						ELSE
							WHILE (s.sym = mul) OR (s.sym = lpar) OR (s.sym = ident) DO
								Declarator(s, FALSE, fopts, name, ftyp);
								InsertIn(typ.sub, last, name, field, o); o.typ := ftyp;
								Append(o.typObj, o.typ, to, t); o.opts := fopts;
								IF s.sym = colon THEN s.GetSym; ConstExp(s, o.value) (* ; INCL(opts, ignoreOpt) *)
								ELSIF s.sym = becomes THEN s.GetSym; ConstExp(s, x)
								END;
								IF s.sym = comma THEN s.GetSym END
							END
						END;
						IF s.sym = semicolon THEN s.GetSym ELSE s.Mark("missing ';'") END
					END
				END;
				IF s.sym = rbrace THEN s.GetSym ELSE s.Mark("missing '}'") END;
				IF typObj # NIL THEN
					IF replacedOpt IN typObj.opts THEN ListInsert(typObj.base)
					ELSE ListInsert(typObj)
					END
				END
			END;
		ELSIF (s.sym >= signed) & (s.sym <= double) THEN
			x := s.sym; s.GetSym;
			IF (x <= unsigned) & (s.sym >= char) & (s.sym <= double) THEN x := s.sym; s.GetSym END;
			IF ((x = short) OR (x = long)) & (s.sym = int) THEN x := s.sym; s.GetSym END;
			typObj := baseType[x - signed]; typ := typObj.typ
		ELSIF s.sym = ident THEN
			Find(s.ident, o);
			IF (o # NIL) & (o.kind = type) THEN typObj := o; typ := o.typ
			ELSE
				name := s.ident$; name := name + " is not a type";
				s.Mark(name)
			END;
			s.GetSym
		ELSE s.Mark("missing type specifier")
		END;
		Attributes(s, opts);
		ASSERT((typ # NIL) OR (errors > 0));
		IF typ = NIL THEN typ := baseType[int - signed].typ END
	END TypeSpec;
	
	PROCEDURE GuidDeclaration (s: Scanner);
		VAR o: Object; i, x: INTEGER; str, ns: Name;
	BEGIN
		s.GetSym;
		IF s.sym = lpar THEN
			s.GetSym;
			IF s.sym = ident THEN
				Insert(s.ident, constant, s.inp, o); ListInsert(o);
				s.GetSym; i := 0; str := "{";
				WHILE s.sym = comma DO
					s.GetSym; ConstExp(s, x);
					IF i = 0 THEN
						Strings.IntToStringForm(x, Strings.hexadecimal, 8, "0", FALSE, ns);
						str := str + ns
					ELSIF i < 3 THEN
						Strings.IntToStringForm(x, Strings.hexadecimal, 4, "0", FALSE, ns);
						str := str + "-"; str := str + ns
					ELSE
						Strings.IntToStringForm(x, Strings.hexadecimal, 2, "0", FALSE, ns);
						IF (i = 3) OR (i = 5) THEN str := str + "-" END;
						str := str + ns
					END;
					INC(i)
				END;
				str := str + "}";
				NEW(o.base); o.base.name := str$; INCL(o.opts, isStringOpt);
				str := o.name$; str[4] := 0X;
				IF str = "IID_" THEN INCL(o.opts, iidOpt) END;
				IF i # 11 THEN s.Mark("wrong argument") END;
				IF s.sym = rpar THEN s.GetSym ELSE s.Mark("missing ')'") END
			ELSE s.Mark("missing ident")
			END
		ELSE s.Mark("missing '('")
		END
	END GuidDeclaration;
	
	PROCEDURE Declaration (s: Scanner);
		VAR lev, i, x: INTEGER; opts: SET; t0, t: Type; o0, o, no: Object; name: Name; inp: Input;
	BEGIN
		IF (s.sym = ident) & (s.ident = "DEFINE_GUID") THEN
			GuidDeclaration(s)
		ELSE
			TypeSpec(s, opts, o0, t0); no := o0;
			WHILE (s.sym = mul) OR (s.sym = lpar) OR (s.sym = ident) DO
				inp := s.inp;
				Declarator(s, FALSE, opts, name, t);
				IF typedefOpt IN opts THEN
					Find(name, o);
					IF (o # NIL) & (o.kind = type) & (replacedOpt IN o.opts) THEN	(* USE declared *)
					ELSIF t # NIL THEN	(* type declaration *)
						Insert(name, type, inp, o); ListInsert(o); o.typ := t;
						Append(o.typObj, o.typ, no, t0); o.opts := opts
					ELSIF o0 = NIL THEN	(* use as name *)
						Insert(name, type, inp, o); ListInsert(o);
						o.typ := t0; o.opts := opts;
						IF no # NIL THEN INCL(o.opts, isTypeOpt); o.base := no
						ELSE no := o
						END
					ELSIF ~(replacedOpt IN o0.opts) & (t0.form >= enumTyp) & (t0.form <= unionTyp)
							& ((bodyOpt IN opts) OR ~(bodyOpt IN t0.opts)) THEN	(* replace tag by name *)
						Insert(name, type, inp, o);
						IF bodyOpt IN t0.opts THEN ListInsert(o) END;
						o.typ := t0; o.opts := opts;
						INCL(o0.opts, ignoreOpt); INCL(o0.opts, replacedOpt); o0.base := o; no := o
					ELSIF (name # o0.name) OR (t0.form = basicTyp) THEN	(* alias declaration *)
						Insert(name, type, inp, o); ListInsert(o);
						o.typ := t0; o.opts := opts;
						INCL(o.opts, isTypeOpt); o.base := no
					END
				ELSIF (t # NIL) & (t.form = procTyp) THEN
					Insert(name, procedure, inp, o); ListInsert(o); o.typ := t;
					Append(o.typObj, o.typ, o0, t0); o.opts := opts;
				END;
				IF s.sym = becomes THEN
					IF (o # NIL) & (o.kind = procedure) THEN
						s.GetSym; INCL(o.opts, inlineOpt);
						IF s.sym = lbrace THEN
							s.GetSym; o.end := -1;
							ConstExp(s, o.beg);
							IF s.sym = comma THEN
								s.GetSym;
								ConstExp(s, o.end);
							END;
							IF s.sym = rbrace THEN s.GetSym
							ELSE s.Mark("missing '}'")
							END
						ELSE
							o.end := -1;
							ConstExp(s, o.beg)
						END
					ELSE s.Mark("unexpected initializer")
					END
				END;
				IF s.sym = comma THEN s.GetSym
				ELSIF (s.sym = mul) OR (s.sym = lpar) OR (s.sym = ident) THEN s.Mark("missing ','")
				END
			END
		END;
		IF s.sym = lbrace THEN
			lev := 0;
			REPEAT
				IF s.sym = lbrace THEN INC(lev)
				ELSIF s.sym = rbrace THEN DEC(lev)
				END;
				s.GetSym
			UNTIL (s.sym = eof) OR (lev = 0)
		ELSIF s.sym = semicolon THEN s.GetSym
		ELSE s.Mark("missing ';'")
		END
	END Declaration;
	
	PROCEDURE Declarations (s: Scanner);
	BEGIN
		WHILE (s.sym # eof) & (s.sym # rbrace) & (errors < 20) DO
			IF s.sym = extern THEN
				s.GetSym;
				IF (s.sym = string) & (s.ident = "C") THEN
					s.GetSym;
					IF s.sym = lbrace THEN
						s.GetSym; 
						Declarations(s);
						IF s.sym = rbrace THEN s.GetSym ELSE s.Mark("missing '}'") END
					ELSE Declaration(s)
					END
				ELSE Declaration(s)
				END
			ELSIF (s.sym >= typedef) & (s.sym <= union) OR (s.sym = ident) THEN
				Declaration(s)
			ELSE
				s.Mark("illegal declaration");
				REPEAT s.GetSym UNTIL (s.sym >= typedef) & (s.sym <= union) OR (s.sym = ident) OR (s.sym = rbrace)
			END
		END;
	END Declarations;
	
	PROCEDURE CheckMacros (s: Scanner);
		VAR o, p: Object; x: INTEGER; name: Name;
n, i: INTEGER;
	BEGIN
n := 0;
o := firstObj;
WHILE o # NIL DO INC(n); o := o.link END;
		o := firstObj; s.inDef := TRUE; 
i := 0;
		WHILE o # NIL DO
			IF ~(ignoreOpt IN o.opts) & (o.kind = macro) & (o.typObj = NIL) THEN
				s.Include(" ", NIL, o.beg, o.end, o.inp);
				s.inDef := TRUE; s.error := FALSE;
				s.GetSubSym;
				IF s.sym = string THEN
					name := s.ident$; s.GetSubSym;
					IF s.sym = eof THEN
						NEW(p); p.name := name$;
						INCL(o.opts, isStringOpt); INCL(o.opts, isConstOpt);
						o.base := p
					END
				ELSE
					IF s.sym = ident THEN Find(s.ident, p) ELSE p := NIL END;
					IF (p # NIL) & ((p.kind = type) OR (p.kind = procedure)) THEN
						s.GetSubSym;
						IF s.sym = eof THEN
							IF p.kind = type THEN INCL(o.opts, isTypeOpt)
							ELSE INCL(o.opts, isProcOpt)
							END;
							o.base := p
						END
					ELSE
						s.inPre := TRUE; ConstExp(s, x); s.inPre := FALSE;
						IF ~s.error & (s.sym = eof) THEN
							o.value := x; INCL(o.opts, isConstOpt)
						END
					END;
					s.inDef := FALSE
				END
			END;
INC(i);
IF i > n THEN HALT(9) END;
			o := o.link
		END
	END CheckMacros;
	
	PROCEDURE Parse (v: TextViews.View);
		VAR s: Scanner; t: TextModels.Model;
	BEGIN
		alignIdx := 0; alignStack[0] := 8;	(* default alignment *)
		t := v.ThisModel(); DevMarkers.Unmark(t);
		NEW(s); s.ConnectTo(t, 0, MAX(INTEGER));
		NEW(s.inp); s.inp.view := v; 
		s.GetSym;
		Declarations(s);
		IF s.level > 0 THEN s.Mark("unbalanced if") END;
		IF errors = 0 THEN CheckMacros(s) END
	END Parse;
	
	
	(* ---------- commands ---------- *)
	
	PROCEDURE Translate*;
		VAR v: TextViews.View; i: Input;
	BEGIN
		errors := 0; inpList := NIL; libList := NIL; modList := NIL; incLevel := 0; EmptyTable;
		v := TextViews.Focus();
		IF v # NIL THEN
			Parse(v);
			IF errors = 0 THEN
				WriteOutput;
			ELSE
				StdLog.Int(errors); StdLog.String(" errors"); StdLog.Ln;
				i := inpList;
				WHILE i # NIL DO
					IF i.errors > 0 THEN
						Views.Open(i.view, path, i.name, conv);
						DevMarkers.ShowFirstError(i.view.ThisModel(), TextViews.any)
					END;
					i := i.next
				END
			END
		END;
		inpList := NIL; libList := NIL; modList := NIL; impList := NIL; actMod := NIL;
		(* EmptyTable; *)
		Kernel.Cleanup
	END Translate;
	
	PROCEDURE Search*;
		VAR name: Name; obj: Object; t: TextModels.Model; r: TextModels.Reader; ch: CHAR;
			c: TextControllers.Controller; beg, end, i: INTEGER;
	BEGIN
		c := TextControllers.Focus();
		IF c # NIL THEN
			IF c.HasSelection() THEN
				c.GetSelection(beg, end);
				r := c.text.NewReader(NIL); r.SetPos(beg); i := 0;
				WHILE r.Pos() < end DO r.ReadChar(name[i]); INC(i) END;
				name[i] := 0X;
				Find(name, obj);
				IF (obj # NIL) THEN
					CASE obj.kind OF
					| macro:
						StdLog.String("macro: ");
						t := obj.inp.view.ThisModel();
						r := t.NewReader(NIL); r.SetPos(obj.beg);
						WHILE r.Pos() < obj.end DO
							r.ReadChar(ch);
							IF ch = 09X THEN ch := " " END;
							StdLog.Char(ch)
						END
					| constant:
						StdLog.String("costant: "); StdLog.Int(obj.value)
					| procedure:
						StdLog.String("procedure ");
						IF inlineOpt IN obj.opts THEN StdLog.String("inline") END
					| type:
						StdLog.String("type: ");
						CASE obj.typ.form OF
						| basicTyp: StdLog.String("basic")
						| enumTyp: StdLog.String("enum")
						| recordTyp: StdLog.String("record")
						| unionTyp: StdLog.String("union")
						| arrayTyp: StdLog.String("array")
						| pointerTyp: StdLog.String("pointer")
						| procTyp: StdLog.String("procedure")
						END
					END;
					StdLog.Ln
				END
			END
		END
	END Search;
	
	PROCEDURE Evaluate*;
		VAR name: Name; obj: Object; t: TextModels.Model; r: TextModels.Reader; ch: CHAR;
			c: TextControllers.Controller; beg, end, i, x: INTEGER; s: Scanner;
	BEGIN
		c := TextControllers.Focus();
		IF c # NIL THEN
			IF c.HasSelection() THEN
				c.GetSelection(beg, end);
				NEW(s); s.ConnectTo(c.text, beg, end);
				NEW(s.inp); s.inp.view := c.view; 
				s.GetSym;
				ConstExp(s, x);
				StdLog.Int(x)
(*				
				r := c.text.NewReader(NIL); r.SetPos(beg); i := 0;
				WHILE r.Pos() < end DO r.ReadChar(name[i]); INC(i) END;
				name[i] := 0X;
				Find(name, obj);
				IF (obj # NIL) THEN
					IF obj.kind = macro THEN
						t := obj.inp.view.ThisModel();
						NEW(s); s.ConnectTo(t, obj.beg, obj.end); s.inp := obj.inp;
						s.GetSym;
						ConstExp(s, x);
						StdLog.Int(x)
					ELSIF obj.kind = constant THEN StdLog.Int(obj.value)
					END;
					StdLog.Ln
				END
*)
			END
		END
	END Evaluate;
	
	PROCEDURE ShowLib* (path, name: ARRAY OF CHAR);
		VAR p, n: Files.Name; loc: Files.Locator; f: Files.File; r: Files.Reader;
			ch: CHAR; x: INTEGER; b: BYTE;
	BEGIN
		p := path$; n := name$;
		loc := Files.dir.This(p);
		f := Files.dir.Old(loc, n, FALSE);
		r := f.NewReader(NIL); r.SetPos(46H);
		r.ReadByte(b); x := b MOD 256;
		r.ReadByte(b); x := x * 256 + b MOD 256;
		r.SetPos(r.Pos() + 4 * x);
		WHILE x > 0 DO
			r.ReadByte(b); ch := CHR(b MOD 256);
			WHILE ch # 0X DO StdLog.Char(ch); r.ReadByte(b); ch := CHR(b MOD 256) END;
			StdLog.Ln; DEC(x)
		END;
	END ShowLib;
	
	
	PROCEDURE InitType(name: ARRAY OF CHAR; align: INTEGER; VAR o: Object);
	BEGIN
		NEW(o); o.name := name$; o.kind := type;
		NEW(o.typ); o.typ.form := basicTyp; o.typ.align := align
	END InitType;
	
	
BEGIN
	path := Files.dir.This("");
	libPath := Files.dir.This("");
	conv := Converters.list;
	WHILE conv.imp # "HostTextConv.ImportText" DO conv := conv.next END;
	InitType("INTEGER", 4, baseType[signed - signed]); INCL(baseType[signed - signed].typ.opts, intOpt);
	InitType("INTEGER", 4, baseType[unsigned - signed]); INCL(baseType[unsigned - signed].typ.opts, intOpt);
	InitType("VOID", 1, baseType[void - signed]); INCL(baseType[void - signed].typ.opts, voidOpt);
	InitType("SHORTCHAR", 1, baseType[char - signed]); INCL(baseType[char - signed].typ.opts, charOpt);
	InitType("SHORTINT", 2, baseType[short - signed]);
	InitType("INTEGER", 4, baseType[int - signed]); INCL(baseType[int - signed].typ.opts, intOpt);
	InitType("INTEGER", 4, baseType[long - signed]); INCL(baseType[long - signed].typ.opts, intOpt);
	InitType("SHORTREAL", 4, baseType[float - signed]);
	InitType("REAL", 8, baseType[double - signed])
END DevTranslator.


Probleme:



welche Konstanten als Set?	hint!

welche Parameter als VAR Par?

identifier too long !!!
MINSHORT* = 32768; !!!
