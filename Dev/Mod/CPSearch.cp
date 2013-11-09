MODULE DevCPSearch;
(* rc 2.7.92 / mf 8.1.93 / eo 4.2.93 / mf 4.2.93 / cp 23 Sep 93 / bh 19.6.94 *)

(* Component Pascal version, bh, 03 Mar 1999 *)

	IMPORT Kernel,
		Files, Stores, Sequencers, Models, Views, Dialog, Documents, Controls,
		TextModels, TextMappers, TextViews, TextControllers, StdLog,
		StdDialog,
		DevMarkers, DevCommanders,
		DevCPM, DevCPS, DevCPT, DevCPB, DevCPP;

	CONST
		(* compiler options: *)
		checks = 0; allchecks = 1; assert = 2; obj = 3; ref = 4; allref = 5; srcpos = 6; reallib = 7;
		hint = 29; oberon = 30; errorTrap = 31;
		defopt = {checks, assert, obj, ref, allref, srcpos};

		(* object modes *)
		Var = 1; VarPar = 2; Con = 3; Fld = 4; Typ = 5; LProc = 6; XProc = 7;
		SProc = 8; CProc = 9; IProc = 10; Mod = 11; Head = 12; TProc = 13; Attr = 20;

		(* symbol values and ops *)
		times = 1; slash = 2; div = 3; mod = 4;
		and = 5; plus = 6; minus = 7; or = 8; eql = 9;
		neq = 10; lss = 11; leq = 12; gtr = 13; geq = 14;
		in = 15; is = 16; ash = 17; msk = 18; len = 19;
		conv = 20; abs = 21; cap = 22; odd = 23; not = 33;
		adr = 24; cc = 25; bit = 26; lsh = 27; rot = 28; val = 29;
		min = 34; max = 35; typfn = 36;
		thisrecfn = 45; thisarrfn = 46;
		shl = 50; shr = 51; lshr = 52; xor = 53;

		(* structure forms *)
		Undef = 0; Byte = 1; Bool = 2; Char8 = 3; Int8 = 4; Int16 = 5; Int32 = 6;
		Real32 = 7; Real64 = 8; Set = 9; String8 = 10; NilTyp = 11; NoTyp = 12;
		Pointer = 13; ProcTyp = 14; Comp = 15;
		AnyPtr = 14; AnyRec = 15;	(* sym file only *)
		Char16 = 16; String16 = 17; Int64 = 18;
		Res = 20; IUnk = 21; PUnk = 22; Guid = 23;
		
		(* composite structure forms *)
		Basic = 1; Array = 2; DynArr = 3; Record = 4;

		(* nodes classes *)
		Nvar = 0; Nvarpar = 1; Nfield = 2; Nderef = 3; Nindex = 4; Nguard = 5; Neguard = 6;
		Nconst = 7; Ntype = 8; Nproc = 9; Nupto = 10; Nmop = 11; Ndop = 12; Ncall = 13;
		Ninittd = 14; Nif = 15; Ncaselse = 16; Ncasedo = 17; Nenter = 18; Nassign = 19;
		Nifelse =20; Ncase = 21; Nwhile = 22; Nrepeat = 23; Nloop = 24; Nexit = 25;
		Nreturn = 26; Nwith = 27; Ntrap = 28; Ncomp = 30;
		Ndrop = 50; Nlabel = 51; Ngoto = 52; Njsr = 53; Nret = 54; Ncmp = 55;
		
	VAR
		sourceR: TextModels.Reader;
		s: TextMappers.Scanner;
		str: Dialog.String;
		found: BOOLEAN;	(* DevComDebug was found -> DTC *)
		rootObj: DevCPT.Object;
		
	PROCEDURE ThisType (mod, name: ARRAY OF SHORTCHAR): DevCPT.Struct;
		VAR i: INTEGER; n: DevCPT.Name; old, obj: DevCPT.Object;
	BEGIN
		i := 0;
		WHILE (i < DevCPT.nofGmod) & (DevCPT.GlbMod[i].name^ # mod) DO INC(i) END;
		IF i < DevCPT.nofGmod THEN
			old := DevCPT.topScope; DevCPT.topScope := DevCPT.GlbMod[i];
			n := name$; DevCPT.Find(n, obj);
			DevCPT.topScope := old;
			IF obj # NIL THEN
				ASSERT(obj.mode = Typ);
				RETURN obj.typ
			END
		END;
		RETURN DevCPT.undftyp
	END ThisType;

	PROCEDURE IsType (typ: DevCPT.Struct; mod, name: ARRAY OF SHORTCHAR): BOOLEAN;
		VAR btyp: DevCPT.Struct;
	BEGIN
		IF typ.form = Pointer THEN typ := typ.BaseTyp END;
		btyp := ThisType(mod, name);
		IF btyp.form = Pointer THEN btyp := btyp.BaseTyp END;
		IF btyp # DevCPT.undftyp THEN RETURN DevCPT.Extends(typ, btyp)
		ELSE RETURN FALSE
		END
	END IsType;
	
	PROCEDURE Traverse (n: DevCPT.Node; pos: INTEGER);
		VAR p: INTEGER;
	BEGIN
		WHILE n # NIL DO
			IF n.conval # NIL THEN p := n.conval.intval ELSE p := pos END;
			CASE n.class OF
			| Nguard:
				Traverse(n.left, pos);
				IF IsType(n.typ, "Views", "View") THEN
					DevCPM.Mark(-700, pos);
				END
			| Nmop:
				Traverse(n.left, pos);
				IF (n.subcl = is) & IsType(n.obj.typ, "Views", "View") THEN
					DevCPM.Mark(-700, pos)
				END
			| Ncase:
				Traverse(n.left, p); Traverse(n.right.left, p); Traverse(n.right.right, p)
			| Ncasedo:
				Traverse(n.right, p)
			| Ngoto, Ndrop, Nloop, Nreturn:
				Traverse(n.left, p)
			| Nfield, Nderef:
				Traverse(n.left, pos)
			| Nenter, Nassign, Ncomp, Ncall, Nifelse, Nif, Nwhile, Nrepeat, Nwith:
				Traverse(n.left, p); Traverse(n.right, p)
			| Ndop, Nupto, Nindex:
				Traverse(n.left, pos); Traverse(n.right, pos)
			| Njsr, Nret, Nlabel, Ntrap, Nexit, Ninittd, Ntype, Nproc, Nconst, Nvar, Nvarpar:
			END;
			n := n.link
		END
	END Traverse;
	
	PROCEDURE TypeSize (typ: DevCPT.Struct);
	BEGIN
		typ.size := 1
	END TypeSize;
	
	PROCEDURE Module (source: TextModels.Reader; opt: SET; log: TextModels.Model; VAR error: BOOLEAN);
		VAR ext, new, clean: BOOLEAN; p: DevCPT.Node;
	BEGIN
		DevCPM.Init(source, log);
		IF found THEN INCL(DevCPM.options, DevCPM.comAware) END;
		IF errorTrap IN opt THEN INCL(DevCPM.options, DevCPM.trap) END;
		IF oberon IN opt THEN INCL(DevCPM.options, DevCPM.oberon) END;
		DevCPT.Init(opt);
		DevCPB.typSize := TypeSize;
		DevCPT.processor := 0;
		DevCPP.Module(p);
		IF DevCPM.noerr THEN
			Traverse(p, 0)
		END;
		DevCPT.Close;
		error := TRUE;
		DevCPM.Close;
		p := NIL;
		Kernel.FastCollect;
		DevCPM.InsertMarks(source.Base());
		DevCPM.LogWLn;
	END Module;


	PROCEDURE Do (source, log: TextModels.Model; beg: INTEGER; opt: SET; VAR error: BOOLEAN);
		VAR r: TextMappers.Scanner;
	BEGIN
		Dialog.MapString("#Dev:Compiling", str);
		StdLog.String(str); StdLog.Char(" ");
		r.ConnectTo(source); r.SetPos(beg); r.Scan;
		IF (r.type = TextMappers.string) & (r.string = "MODULE") THEN
			r.Scan;
			IF r.type = TextMappers.string THEN
				StdLog.Char('"'); StdLog.String(r.string); StdLog.Char('"')
			END
		END;
		sourceR := source.NewReader(NIL); sourceR.SetPos(beg);
		Module(sourceR, opt, log, error);
	END Do;

	
	PROCEDURE Open;
	BEGIN
		Dialog.ShowStatus("#Dev:Compiling");
		StdLog.buf.Delete(0, StdLog.buf.Length());
	END Open;
	
	PROCEDURE Close;
	BEGIN
		StdLog.text.Append(StdLog.buf);
		IF DevCPM.noerr THEN Dialog.ShowStatus("#Dev:Ok")
		END;
		sourceR := NIL;
		Kernel.Cleanup;
	END Close;

	PROCEDURE Compile*;
		VAR t: TextModels.Model; error: BOOLEAN;
	BEGIN
		Open;
		t := TextViews.FocusText();
		IF t # NIL THEN
			Do(t, StdLog.text, 0, defopt, error);
			IF error THEN DevMarkers.ShowFirstError(t, TextViews.focusOnly) END
		ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
		END;
		Close
	END Compile;

	PROCEDURE CompileOpt* (opt: ARRAY OF CHAR);
		VAR t: TextModels.Model; error: BOOLEAN; i: INTEGER; opts: SET;
	BEGIN
		i := 0; opts := defopt;
		WHILE opt[i] # 0X DO
			IF opt[i] = "-" THEN
				IF srcpos IN opts THEN EXCL(opts, srcpos)
				ELSIF allref IN opts THEN EXCL(opts, allref)
				ELSIF ref IN opts THEN EXCL(opts, ref)
				ELSE EXCL(opts, obj)
				END
			ELSIF opt[i] = "!" THEN
				IF assert IN opts THEN EXCL(opts, assert)
				ELSE EXCL(opts, checks)
				END
			ELSIF opt[i] = "+" THEN INCL(opts, allchecks)
			ELSIF opt[i] = "?" THEN INCL(opts, hint)
			ELSIF opt[i] = "@" THEN INCL(opts, errorTrap)
			ELSIF opt[i] = "$" THEN INCL(opts, oberon)
			END;
			INC(i)
		END;
		Open;
		t := TextViews.FocusText();
		IF t # NIL THEN
			Do(t, StdLog.text, 0, opts, error);
			IF error THEN DevMarkers.ShowFirstError(t, TextViews.focusOnly) END
		ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
		END;
		Close
	END CompileOpt;

	PROCEDURE CompileText* (text: TextModels.Model; beg: INTEGER; VAR error: BOOLEAN);
	BEGIN
		ASSERT(text # NIL, 20); ASSERT((beg >= 0) & (beg < text.Length()), 21);
		Open;
		Do(text, StdLog.text, beg, defopt, error);
		IF error THEN DevMarkers.ShowFirstError(text, TextViews.focusOnly) END;
		Close
	END CompileText;

	PROCEDURE CompileAndUnload*;
		VAR t: TextModels.Model; error: BOOLEAN; mod: Kernel.Module; n: ARRAY 256 OF CHAR;
	BEGIN
		Open;
		t := TextViews.FocusText();
		IF t # NIL THEN
			Do(t, StdLog.text, 0, defopt, error);
			IF error THEN DevMarkers.ShowFirstError(t, TextViews.focusOnly)
			ELSE
				mod := Kernel.ThisLoadedMod(DevCPT.SelfName);
				IF mod # NIL THEN
					Kernel.UnloadMod(mod);
					n := DevCPT.SelfName$;
					IF mod.refcnt < 0 THEN
						Dialog.MapParamString("#Dev:Unloaded", n, "", "", str);
						StdLog.String(str); StdLog.Ln;
						Controls.Relink
					ELSE
						Dialog.MapParamString("#Dev:UnloadingFailed", n, "", "", str);
						StdLog.String(str); StdLog.Ln
					END
				END
			END
		ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
		END;
		Close
	END CompileAndUnload;

	PROCEDURE CompileSelection*;
		VAR c: TextControllers.Controller; t: TextModels.Model; beg, end: INTEGER; error: BOOLEAN;
	BEGIN
		Open;
		c := TextControllers.Focus();
		IF c # NIL THEN
			t := c.text;
			IF c.HasSelection() THEN
				c.GetSelection(beg, end); Do(t, StdLog.text, beg, defopt, error);
				IF error THEN DevMarkers.ShowFirstError(t, TextViews.focusOnly) END
			ELSE Dialog.ShowMsg("#Dev:NoSelectionFound")
			END
		ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
		END;
		Close
	END CompileSelection;
	
	PROCEDURE CompileList (beg, end: INTEGER; c: TextControllers.Controller);
		VAR v: Views.View; i: INTEGER; error, one: BOOLEAN; name: Files.Name; loc: Files.Locator;
			t: TextModels.Model; opts: SET;
	BEGIN
		s.SetPos(beg); s.Scan; one := FALSE;
		WHILE (s.start < end) & (s.type = TextMappers.string) & (s.len < LEN(name)) DO
			s.Scan; one := TRUE;
			WHILE (s.type = TextMappers.char) &
				((s.char = "-") OR (s.char = "+") OR
				(s.char = "!") OR (s.char = "*") OR (s.char = "?") OR (s.char = "^")) DO s.Scan END
		END;
		IF one & ((s.start >= end) OR (s.type = TextMappers.eot)) THEN
			s.SetPos(beg); s.Scan; error := FALSE;
			WHILE (s.start < end) & (s.type = TextMappers.string) & ~error DO
				i := 0; WHILE i < LEN(name) DO name[i] := 0X; INC(i) END;
				StdDialog.GetSubLoc(s.string, "Mod", loc, name);
				s.Scan; opts := defopt;
				WHILE s.type = TextMappers.char DO
					IF s.char = "-" THEN
						IF srcpos IN opts THEN EXCL(opts, srcpos)
						ELSIF allref IN opts THEN EXCL(opts, allref)
						ELSIF ref IN opts THEN EXCL(opts, ref)
						ELSE EXCL(opts, obj)
						END
					ELSIF s.char = "!" THEN
						IF assert IN opts THEN EXCL(opts, assert)
						ELSE EXCL(opts, checks)
						END
					ELSIF s.char = "+" THEN INCL(opts, allchecks)
					ELSIF s.char = "?" THEN INCL(opts, hint)
					ELSIF s.char = "@" THEN INCL(opts, errorTrap)
					ELSIF s.char = "$" THEN INCL(opts, oberon)
					END;
					s.Scan
				END;
				IF loc # NIL THEN
					v := Views.OldView(loc, name);
					IF v # NIL THEN
						WITH v: TextViews.View DO t := v.ThisModel() ELSE t := NIL END;
						IF t # NIL THEN
							i := 0; one := FALSE;
							Do(t, StdLog.text, 0, opts, error)
						ELSE
							Dialog.ShowParamMsg("#Dev:NoTextFileFound", name, "", ""); error := TRUE
						END
					ELSE
						Dialog.ShowParamMsg("#Dev:CannotOpenFile", name, "", ""); error := TRUE
					END
				ELSE Dialog.ShowParamMsg("#System:FileNotFound", name, "", ""); error := TRUE
				END
			END
		ELSE Dialog.ShowMsg("#Dev:NotOnlyFileNames")
		END;
		s.ConnectTo(NIL);
		IF error & (c # NIL) & c.HasSelection() & (s.start < end) THEN
			c.SetSelection(s.start, end)
		END;
		IF error & (v # NIL) THEN
			Views.Open(v, loc, name, NIL);
			DevMarkers.ShowFirstError(t, TextViews.any)
		END
	END CompileList;

	PROCEDURE CompileModuleList*;
		VAR c: TextControllers.Controller; beg, end: INTEGER;
	BEGIN
		Open;
		c := TextControllers.Focus();
		IF c # NIL THEN
			s.ConnectTo(c.text);
			IF c.HasSelection() THEN c.GetSelection(beg, end)
			ELSE beg := 0; end := c.text.Length()
			END;
			CompileList(beg, end, c);
		ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
		END;
		Close
	END CompileModuleList;

	PROCEDURE CompileThis*;
		VAR p: DevCommanders.Par; beg, end: INTEGER;
	BEGIN
		Open;
		p := DevCommanders.par;
		IF p # NIL THEN
			DevCommanders.par := NIL;
			s.ConnectTo(p.text); beg := p.beg; end := p.end;
			CompileList(beg, end, NIL);
		ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
		END;
		Close
	END CompileThis;
	
	PROCEDURE Init;
		VAR loc: Files.Locator;
	BEGIN
		loc := Files.dir.This("Dev"); loc := loc.This("Code");
		found := Files.dir.Old(loc, "ComDebug.ocf", TRUE) # NIL
	END Init;
	
BEGIN
	rootObj := DevCPT.NewObj();
	Init
END DevCPSearch.
