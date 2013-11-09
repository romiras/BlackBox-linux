MODULE DevCPConv;

	(* Oberon/L to ComPas Converter, bh, 31 Mar 1999 *)
	
	IMPORT Kernel,
		Files, Fonts, Ports, Stores, Sequencers, Models, Views, Dialog, Documents,
		TextModels, TextMappers, TextViews, TextControllers, StdLog,
		StdDialog, DevMarkers,
		CPM := DevCPM, CPT := DevCPT, CPB := DevCPB, CPP := DevCPCP, DevCompiler;
		
	CONST
		no = 1; all = 2; auto = 3;
		strict = 0;
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
	
	VAR
		dialog*: RECORD
			long2int*: INTEGER;
			int2short*: INTEGER;
			short2byte*: INTEGER;
			char2short*: INTEGER;
			long2real*: INTEGER;
			real2short*: INTEGER;
(*
			meta*: INTEGER;
			math*: INTEGER;
			stores*: INTEGER;
			formatters*: INTEGER;
			attributes*: INTEGER;
*)
			compile*: BOOLEAN;
			bold*, italic*, underline*: BOOLEAN;
			color*: Dialog.Color
		END;
		sourceR: TextModels.Reader;
		s: TextMappers.Scanner;
		str: Dialog.String;
		
		
	PROCEDURE TypeSize (typ: CPT.Struct);
	BEGIN
		typ.size := 1
	END TypeSize;
	
	PROCEDURE WriteAttributes (p: CPP.Patch; VAR w: TextMappers.Formatter);
	BEGIN
		WITH p: CPP.RecAttrPatch DO
			IF p.typ.attribute = 17 THEN w.WriteString("ABSTRACT ")
			ELSIF p.typ.attribute = 20 THEN w.WriteString("EXTENSIBLE ")
			END
		| p: CPP.ProcAttrPatch DO
			IF 16 IN p.obj.conval.setval THEN w.WriteString(", NEW") END;
			IF 17 IN p.obj.conval.setval THEN w.WriteString(", ABSTRACT")
			ELSIF 19 IN p.obj.conval.setval THEN w.WriteString(", EMPTY")
			ELSIF 20 IN p.obj.conval.setval THEN w.WriteString(", EXTENSIBLE")
			END
		END
	END WriteAttributes;
	
	PROCEDURE InsertPropagate (p: CPP.Patch; VAR w: TextMappers.Formatter);
		VAR obj, base: CPT.Object; rec: CPT.Struct; name: CPT.Name;
	BEGIN
		WITH p: CPP.ProcAttrPatch DO
			rec := p.obj.typ.BaseTyp; name := "ThisModel";
			CPT.FindField(name, rec, obj);
			CPT.FindBaseField(name, rec, base);
			IF (base # NIL) & (obj # base) THEN
				w.WriteLn; w.WriteTab; w.WriteTab;
				w.WriteString("IF "); w.WriteString(p.obj.name$);
				w.WriteString(".ThisModel() # NIL THEN Stores.InitDomain("); w.WriteString(p.obj.name$);
				w.WriteString(".ThisModel(), "); w.WriteString(p.obj.name$);
				w.WriteString(".domain) END;")
			END
		END
	END InsertPropagate;
	
	PROCEDURE InsertPropagateMeth (p: CPP.Patch; VAR w: TextMappers.Formatter);
		VAR obj, base: CPT.Object; rec: CPT.Struct; name: CPT.Name;
	BEGIN
		WITH p: CPP.ProcAttrPatch DO
			rec := p.obj.typ.BaseTyp; name := "PropagateDomain";
			CPT.FindField(name, rec, obj);
			IF (obj # NIL) & (obj.mode = 13) & (19 IN obj.conval.setval) THEN
				w.WriteLn; w.WriteLn; w.WriteTab;
				w.WriteString("PROCEDURE (v: "); w.WriteString(p.obj.typ.strobj.name$);
				w.WriteString(") PropagateDomain");
				IF p.obj.typ.strobj.vis # 0 THEN w.WriteChar("-") END;
				w.WriteString(";");
				w.WriteLn; w.WriteTab; w.WriteString("BEGIN");
				w.WriteLn; w.WriteTab; w.WriteTab;
				w.WriteString("IF v.ThisModel() # NIL THEN Stores.InitDomain(v.ThisModel(), v.domain) END");
				w.WriteLn; w.WriteTab; w.WriteString("END PropagateDomain;");
			END
		END
	END InsertPropagateMeth;
	
	PROCEDURE^ Change (text: TextModels.Model; offs: INTEGER; VAR patch: CPP.Patch);
	
	PROCEDURE Exchange (VAR w: TextMappers.Formatter; offs, as, ae, bs, be, cs, ce: INTEGER;
									VAR p: CPP.Patch; sep, sep2: ARRAY OF CHAR);
		VAR p0, p1: CPP.Patch; text: TextModels.Model;
	BEGIN
		text := w.rider.Base();
		text.Insert(offs + ce, text, offs + as, offs + be);
		p0 := p;
		WHILE (p # NIL) & (p.pos >= cs) DO p := p.next END;
		p1 := p;
		IF ae # bs THEN
			WHILE (p # NIL) & (p.pos >= bs) DO Change(text, offs + ce - be, p) END;
			text.Delete(offs + ae + ce - be, offs + bs + ce - be);
			w.SetPos(offs + ae + ce - be);
			w.WriteString(sep2)
		END;
		WHILE (p # NIL) & (p.pos >= as) DO Change(text, offs + ce - be, p) END;
		w.SetPos(offs + ce - (be - as));
		w.WriteString(sep);
		WHILE p0 # p1 DO Change(text, offs - (be - as), p0) END;
		text.Delete(offs + as, offs + cs - (be - as))
	END Exchange;
	
	PROCEDURE Change (text: TextModels.Model; offs: INTEGER; VAR patch: CPP.Patch);
		VAR p, a, b, c, p0: CPP.Patch; w: TextMappers.Formatter; r: TextModels.Reader; attr: TextModels.Attributes;
			face: Fonts.Typeface; size, weight: INTEGER; style: SET;
	BEGIN
		p := patch; patch := patch.next;
		IF p.kind < copy1 THEN
			r := text.NewReader(r); r.SetPos(offs + p.pos); r.Read; attr := r.attr;
			face := attr.font.typeface; size := attr.font.size; style := attr.font.style; weight := attr.font.weight;
			IF dialog.bold THEN attr := TextModels.NewWeight(attr, Fonts.bold) END;
			IF dialog.italic THEN INCL(style, Fonts.italic); attr := TextModels.NewStyle(attr, style) END;
			IF dialog.underline THEN INCL(style, Fonts.underline); attr := TextModels.NewStyle(attr, style) END;
			IF dialog.color.val # Ports.white THEN attr := TextModels.NewColor(attr, dialog.color.val) END;
			w.ConnectTo(text); w.rider.SetAttr(attr);
			IF (p.kind = copy) OR (p.kind = ccopy) THEN
				a := p.next.next; b := p.next;
				ASSERT((b.kind = copy2) & (a.kind = copy1));
				patch := patch.next.next;
				text.Delete(offs + b.pos + b.len, offs + p.pos + p.len);
				IF p.kind = copy THEN
					w.SetPos(offs + b.pos + b.len);
					w.WriteChar("$")
				END;
				Exchange(w, offs, a.pos, 0, 0, a.pos + a.len, b.pos, b.pos + b.len, patch, " := ", "");
				text.Delete(offs + p.pos, offs + a.pos)
			ELSIF p.kind = append THEN
				a := p.next.next; b := p.next;
				ASSERT((b.kind = append2) & (a.kind = append1));
				patch := patch.next.next;
				text.Delete(offs + b.pos + b.len, offs + p.pos + p.len);
				WHILE (patch # NIL) & (patch.pos >= b.pos) DO Change(text, offs, patch) END;
				text.Delete(offs + a.pos + a.len, offs + b.pos);
				w.SetPos(offs + a.pos + a.len);
				w.WriteString(" + ");
				text.InsertCopy(offs + a.pos + a.len, text, offs + a.pos, offs + a.pos + a.len);
				p0 := patch;
				WHILE (patch # NIL) & (patch.pos >= a.pos) DO Change(text, offs + a.len, patch) END;
				w.SetPos(offs + a.pos + a.len);
				w.WriteString(" := ");
				WHILE p0 # patch DO Change(text, offs, p0) END;
				text.Delete(offs + p.pos, offs + a.pos)
			ELSIF p.kind = concat THEN
				c := p.next; b := c.next; a := b.next;
				ASSERT((c.kind = concat3) & (b.kind = concat2) & (a.kind = concat1));
				patch := patch.next.next.next;
				text.Delete(offs + c.pos + c.len, offs + p.pos + p.len);
				Exchange(w, offs, a.pos, a.pos + a.len, b.pos, b.pos + b.len, c.pos, c.pos + c.len, patch, " := ", " + ");
				text.Delete(offs + p.pos, offs + a.pos)
			ELSIF p.kind = swap THEN
				a := p.next;
				ASSERT(a.kind = swap1);
				patch := patch.next;
				Exchange(w, offs, a.pos, 0, 0, a.pos + a.len, p.pos, p.pos + p.len, patch, ", ", "");
			ELSIF p.kind = delete THEN
				IF (p.next # NIL) & (p.next.kind = delete1) THEN
					text.Delete(offs + p.next.pos + p.next.len, offs + p.pos + p.len);
					WHILE (patch # NIL) & (patch.pos >= p.pos) DO Change(text, offs, patch) END;
					w.SetPos(offs + p.next.pos); w.WriteString(" *)");
					w.SetPos(offs + p.pos); w.WriteString("(* ")
				ELSE
					w.SetPos(offs + p.pos + p.len); w.WriteString("*)");
					WHILE (patch # NIL) & (patch.pos >= p.pos) DO Change(text, offs, patch) END;
					w.SetPos(offs + p.pos); w.WriteString("(*")
				END
			ELSIF p.kind = short THEN
				w.SetPos(offs + p.pos + p.len); w.WriteChar(")");
				WHILE (patch # NIL) & (patch.pos >= p.pos) DO Change(text, offs, patch) END
			ELSIF p.kind = shortEntier THEN
				w.SetPos(offs + p.pos + p.len); w.WriteString("))");
				WHILE (patch # NIL) & (patch.pos >= p.pos) DO Change(text, offs, patch) END
			ELSIF p.len > 0 THEN
				text.Delete(offs + p.pos, offs + p.pos + p.len)
			END;
			w.SetPos(offs + p.pos);
			CASE p.kind OF
			| large2long: w.WriteString("LONGINT")
			| long2int: w.WriteString("INTEGER")
			| int2short: w.WriteString("SHORTINT")
			| short2byte: w.WriteString("BYTE")
			| long2char: w.WriteString("CHAR")
			| char2short: w.WriteString("SHORTCHAR")
			| real2short: w.WriteString("SHORTREAL")
			| long2real: w.WriteString("REAL")
			| realExp: w.WriteChar("E")
			| lchr: w.WriteString("CHR")
			| lentier: w.WriteString("ENTIER")
			| anyrec: w.WriteString("ANYREC")
			| anyptr: w.WriteString("ANYPTR")
			| varIn: w.WriteString("IN"); IF p.len <= 0 THEN w.WriteChar(" ") END
			| varOut: w.WriteString("OUT"); IF p.len <= 0 THEN w.WriteChar(" ") END
			| varInNil: w.WriteString("IN [nil]"); IF p.len <= 0 THEN w.WriteChar(" ") END
			| varOutNil: w.WriteString("OUT [nil]"); IF p.len <= 0 THEN w.WriteChar(" ") END
			| varNew: w.WriteString("OUT [new]"); IF p.len <= 0 THEN w.WriteChar(" ") END
			| varIid: w.WriteString("IN [iid]"); IF p.len <= 0 THEN w.WriteChar(" ") END
			| pointerTo: w.WriteString("POINTER TO ")
			| short: w.WriteString("SHORT(")
			| shortEntier: w.WriteString("SHORT(ENTIER(")
			| conSC: w.WriteString("sChar")
			| conC: w.WriteString("char")
			| conB: w.WriteString("byte")
			| conSI: w.WriteString("sInt")
			| conI: w.WriteString("int")
			| conSR: w.WriteString("sReal")
			| conR: w.WriteString("real")
			| conSS: w.WriteString("sString")
			| conS: w.WriteString("string")
			| procSC: w.WriteString("SChar")
			| procXC: w.WriteString("XChar")
			| procC: w.WriteString("Char")
			| procB: w.WriteString("Byte")
			| procSI: w.WriteString("SInt")
			| procXI: w.WriteString("XInt")
			| procI: w.WriteString("Int")
			| procSR: w.WriteString("SReal")
			| procXR: w.WriteString("XReal")
			| procR: w.WriteString("Real")
			| procSS: w.WriteString("SString")
			| procXS: w.WriteString("XString")
			| procS: w.WriteString("String")
			| services: w.WriteString("Services.")
			| stores: w.WriteString("Stores")
			| stdLog: w.WriteString("StdLog")
			| copyFrom: w.WriteString("CopyFrom")
			| readonly: w.WriteChar("-")
			| become: w.WriteString(" := ")
			| setRect: w.WriteString("SetRect")
			| thisDomain: w.WriteString("ThisDomain()")
			| insertCopy: w.WriteString("InsertCopy")
			| label: w.WriteString("label")
			| notifierProc: w.WriteString("NotifierProc")
			| min: w.WriteString("MIN")
			| max: w.WriteString("MAX")
			| len: w.WriteString("LEN")
			| stringOp: w.WriteChar("$")
			| item: w.WriteString("item")
			| index: w.WriteString("index")
			| dates: w.WriteString("Dates")
			| selection: w.WriteString("Selection")
			| list: w.WriteString("List")
			| leftSide: w.WriteString("leftSide")
			| rightSide: w.WriteString("rightSide")
			| collapsed: w.WriteString("collapsed")
			| dir: w.WriteString("dir.")
			| stop: w.WriteString(".stop")
			| tabsTab: w.WriteString("tabs.tab")
			| lenFld: w.WriteString(".len")
			| copyOf: w.WriteString("CopyOf")
			| store: w.WriteString("Store")
			| insert: w.WriteString("Insert")
			| storesInitDomain: w.WriteString("Stores.InitDomain(")
			| commaSp: w.WriteString(", ")
			| domain: w.WriteString("domain")
			| cloneOf: w.WriteString("CloneOf")
			| get: w.WriteString("Get")
			| propagateDomain: w.WriteString("PropagateDomain")
			| attributes: WriteAttributes(p, w)
			| propagate: InsertPropagate(p, w)
			| propagateMeth: InsertPropagateMeth(p, w)
			| close: w.WriteString("CLOSE"); w.WriteLn; w.WriteTab; w.WriteString("TERMINATE"); w.WriteLn
			| copy, ccopy, append, concat, swap, delete:
			END
		END
	END Change;
	
	PROCEDURE MakeChanges (text: TextModels.Model);
		VAR p: CPP.Patch; script: Stores.Operation;
	BEGIN
		Models.BeginScript(text, "#Dev:Conversion", script);
		p := CPP.patchList;
		WHILE p # NIL DO Change(text, 0, p) END;
		Models.EndScript(text, script)
	END MakeChanges;


	PROCEDURE Module (source: TextModels.Reader; min, max: SET; log: TextModels.Model; VAR error: BOOLEAN);
		VAR ext, new, clean: BOOLEAN; p: CPT.Node;
	BEGIN
		CPM.Init(source, log);
		CPM.options := CPM.options + {CPM.comAware, CPM.somAware, CPM.oberon, 10, 20};
		CPT.Init({});
		CPB.typSize := TypeSize;
		CPT.processor := 0;
		CPP.Module(min, max);
		IF CPM.noerr THEN MakeChanges(source.Base()) ELSE CPM.InsertMarks(source.Base()) END;
		CPT.Close;
		error := ~CPM.noerr;
		CPM.Close;
		p := NIL;
		Kernel.FastCollect;
		IF error THEN
			CPM.LogWLn; CPM.LogWStr(" ");
			IF CPM.errors = 1 THEN
				Dialog.MapString("#Dev:OneErrorDetected", str);
			ELSE
				CPM.LogWNum(CPM.errors, 0); Dialog.MapString("#Dev:ErrorsDetected", str);
			END;
			StdLog.String(str);
		END;
		CPM.LogWLn
	END Module;
	
	PROCEDURE Do (source, log: TextModels.Model; beg: INTEGER; opt: SET; VAR error: BOOLEAN);
		VAR r: TextMappers.Scanner; min, max: SET;
	BEGIN
		Dialog.MapString("#Dev:Converting", str);
		StdLog.String(str); StdLog.Char(" ");
		r.ConnectTo(source); r.SetPos(beg); r.Scan;
		IF (r.type = TextMappers.string) & (r.string = "MODULE") THEN
			r.Scan;
			IF r.type = TextMappers.string THEN
				StdLog.Char('"'); StdLog.String(r.string); StdLog.Char('"')
			END
		END;
		sourceR := source.NewReader(NIL); sourceR.SetPos(beg);
		min := {large2long, long2char, long2real, realExp, lchr, lentier, varIn, varOut, anyrec, anyptr, copy, short};
		min := min + {stores, meta, math, formatters, attributes, metaAuto};
(*
		IF dialog.stores # no THEN INCL(min, stores) END;
		IF dialog.stores = auto THEN INCL(min, storesAuto) END;
		IF dialog.meta # no THEN INCL(min, meta) END;
		IF dialog.meta = auto THEN INCL(min, metaAuto) END;
		IF dialog.math # no THEN INCL(min, math) END;
		IF dialog.formatters # no THEN INCL(min, formatters) END;
		IF dialog.attributes # no THEN INCL(min, attributes) END;
*)
		max := min;
		IF dialog.long2int # no THEN INCL(max, long2int) END;
		IF dialog.long2int = all THEN INCL(min, long2int) END;
		IF dialog.int2short # no THEN INCL(max, int2short) END;
		IF dialog.int2short = all THEN INCL(min, int2short) END;
		IF dialog.short2byte # no THEN INCL(max, short2byte) END;
		IF dialog.short2byte = all THEN INCL(min, short2byte) END;
		IF dialog.char2short # no THEN INCL(max, char2short) END;
		IF dialog.char2short = all THEN INCL(min, char2short) END;
		IF dialog.long2real # no THEN INCL(max, long2real) END;
		IF dialog.long2real = all THEN INCL(min, long2real) END;
		IF dialog.real2short # no THEN INCL(max, real2short) END;
		IF dialog.real2short = all THEN INCL(min, real2short) END;
		IF strict IN opt THEN min := max END;
		Module(sourceR, min, max, log, error);
		CPP.patchList := NIL;
		Kernel.FastCollect
	END Do;

	
	PROCEDURE Open;
	BEGIN
		Dialog.ShowStatus("#Dev:Converting");
		StdLog.buf.Delete(0, StdLog.buf.Length());
	END Open;
	
	PROCEDURE Close;
	BEGIN
		StdLog.text.Append(StdLog.buf);
		IF CPM.noerr THEN Dialog.ShowStatus("#Dev:Ok")
		END;
		sourceR := NIL;
		Kernel.Cleanup;
	END Close;

	PROCEDURE Convert*;
		VAR t: TextModels.Model; error: BOOLEAN;
	BEGIN
		Open;
		t := TextViews.FocusText();
		IF t # NIL THEN
			Do(t, StdLog.text, 0, {}, error);
			IF error THEN
				DevMarkers.ShowFirstError(t, TextViews.focusOnly)
			ELSIF dialog.compile THEN
				DevCompiler.CompileText(t, 0, error)
			END
		ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
		END;
		Close
	END Convert;
	
	PROCEDURE ConvertList (beg, end: INTEGER);
		VAR v: Views.View; i: INTEGER; error, one: BOOLEAN; name: Files.Name; loc: Files.Locator;
			t: TextModels.Model; opts: SET;
	BEGIN
		s.SetPos(beg); s.Scan; one := FALSE;
		WHILE (s.start < end) & (s.type = TextMappers.string) & (s.len < LEN(name)) DO
			s.Scan; one := TRUE;
			WHILE (s.type = TextMappers.char) & (s.char = "!") DO s.Scan END
		END;
		IF one & ((s.start >= end) OR (s.type = TextMappers.eot)) THEN
			s.SetPos(beg); s.Scan;
			WHILE (s.start < end) & (s.type = TextMappers.string) & ~error DO
				i := 0; WHILE i < LEN(name) DO name[i] := 0X; INC(i) END;
				StdDialog.GetSubLoc(s.string, "Mod", loc, name);
				s.Scan; opts := {};
				WHILE s.type = TextMappers.char DO
					IF s.char = "!" THEN INCL(opts, strict) END;
					s.Scan; error := TRUE
				END;
				IF loc # NIL THEN
					v := Views.OldView(loc, name);
					IF v # NIL THEN
						WITH v: TextViews.View DO t := v.ThisModel() ELSE t := NIL END;
						IF t # NIL THEN
							i := 0; one := FALSE;
							Do(t, StdLog.text, 0, opts, error);
							IF ~error THEN Views.RegisterView(v, loc, name) END;
							IF ~error & dialog.compile THEN
								DevCompiler.CompileText(t, 0, error)
							END
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
		IF error & (v # NIL) THEN
			Views.Open(v, loc, name, NIL);
			DevMarkers.ShowFirstError(t, TextViews.any)
		END
	END ConvertList;

	PROCEDURE ConvertModuleList*;
		VAR c: TextControllers.Controller; beg, end: INTEGER;
	BEGIN
		Open;
		c := TextControllers.Focus();
		IF c # NIL THEN
			s.ConnectTo(c.text);
			IF c.HasSelection() THEN c.GetSelection(beg, end)
			ELSE beg := 0; end := c.text.Length()
			END;
			ConvertList(beg, end);
		ELSE Dialog.ShowMsg("#Dev:NoTextViewFound")
		END;
		Close
	END ConvertModuleList;
	
	PROCEDURE Default*;
	BEGIN
		dialog.long2int := all;
		dialog.int2short := auto;
		dialog.short2byte := all;
		dialog.char2short := auto;
		dialog.long2real := all;
		dialog.real2short := auto;
(*
		dialog.meta := auto;
		dialog.math := all;
		dialog.stores := all;
		dialog.formatters := all;
		dialog.attributes := all;
*)
		Dialog.Update(dialog)
	END Default;
	
	PROCEDURE Strict*;
	BEGIN
		dialog.long2int := all;
		dialog.int2short := all;
		dialog.short2byte := all;
		dialog.char2short := all;
		dialog.long2real := all;
		dialog.real2short := all;
(*
		dialog.meta := all;
		dialog.math := all;
		dialog.stores := all;
		dialog.formatters := all;
		dialog.attributes := all;
*)
		Dialog.Update(dialog)
	END Strict;
	
BEGIN
	Default;
	dialog.compile := TRUE;
	dialog.bold := FALSE;
	dialog.italic := FALSE;
	dialog.underline := TRUE;
	dialog.color.val := Ports.white
END DevCPConv.
