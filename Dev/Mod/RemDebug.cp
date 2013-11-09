MODULE DevRemDebug;	(* remote debugger *)
(* bh 6 May 1999 *)

	(* dg	06.05.99	changes due to interface change of BeginModification/EndModification *)
	(* dg	04.01.99	changed references to Controllers.Controller to Containers.Controller *)
	(* dg	02.12.98	changes due to renaming of Views.CopyFrom => Views.CopyFromSimpleView *)

	IMPORT SYSTEM, WinApi,
		Kernel, Strings, Dates, Files, Services, Fonts, Ports, Stores, Converters,
		Models, Views, Controllers, Properties, Dialog, Containers,
		Windows, StdDialog, StdFolds, StdLinks,
		TextModels, TextMappers, TextControllers, TextViews, TextRulers, StdLog,
		HostFonts, HostCmds, HostWindows, HostMenus;
	
	CONST
		mm = Ports.mm; pt = Ports.point;
		refViewSize = 9 * Ports.point;
		
		heap = 1; source = 2; module = 3; modules = 4;	(* RefView types *)
		open = 1; undo = 2; update = 3;	(* RefView commands *)
		
		(* additional scanner types *)
		import = 100; smodule = 101; semicolon = 102; becomes  = 103; stop = 104; comEnd = 105; 
		
		running = 1000;

		logOutput = FALSE;
		stackFrames = 100;
		showStack = TRUE;
		

	TYPE
		Name = Kernel.Name;
		
		Type = INTEGER;
		Module = INTEGER;
		Object= INTEGER;
		ArrayPtr = INTEGER;
		Cluster = INTEGER;
		
		RefView = POINTER TO RECORD (Views.View)
			type: SHORTINT;
			command: SHORTINT;
			back: RefView;
			adr: INTEGER;
			desc: Type;
			ptr: ArrayPtr;
			name: Name
		END;
		
		Action = POINTER TO RECORD (Services.Action) END;
		
		DAction = POINTER TO RECORD (Services.Action) END;
		
		BrkPt* = RECORD
			path*: Name;
			adr*, size*: INTEGER
		END;
		
		
	VAR
		breakpoints*: ARRAY 3 OF BrkPt;
		
		stateText: TextModels.Model;
		out: TextMappers.Formatter;
		path: ARRAY 4 OF Ports.Point;
		empty: Name;
		
		err, pc, sp, fp, stack, val: INTEGER;
		procHandle, threadHandle: WinApi.HANDLE;
		procId, threadId: INTEGER;
		kernelAdr: INTEGER;
		
		skipCalls: BOOLEAN;	(* step over is running *)
		skipMode: INTEGER;	(* # -1: delayed ContinueExec(200, skipMode) *)
		
		framePc, frameFp, frameCnt: INTEGER;
		
		cache: ARRAY 256 OF SHORTCHAR;
		cacheLine: INTEGER;
		modCache: Module;
		
	
	(* remote memory access *)
	
	PROCEDURE  IsReadable (from, to: INTEGER): BOOLEAN;
		VAR data: ARRAY 100 OF SHORTCHAR; res: INTEGER;
	BEGIN
		res := WinApi.ReadProcessMemory(procHandle, from, SYSTEM.ADR(data), to - from, NIL);
		RETURN res # 0
	END IsReadable;
	
	PROCEDURE GetByte (adr: INTEGER; VAR x: SHORTCHAR);
		VAR res: INTEGER;
	BEGIN
		IF adr DIV LEN(cache) = cacheLine THEN
			x := cache[adr MOD LEN(cache)]
		ELSE
			res := WinApi.ReadProcessMemory(procHandle, adr - adr MOD LEN(cache),
															SYSTEM.ADR(cache), LEN(cache), NIL);
			IF res # 0 THEN
				x := cache[adr MOD LEN(cache)];
				cacheLine := adr DIV LEN(cache)
			ELSE
				cacheLine := 0;
				res := WinApi.ReadProcessMemory(procHandle, adr, SYSTEM.ADR(x), 1, NIL);
				IF res = 0 THEN x := 0X END
			END
		END
	END GetByte;
	
	PROCEDURE PutByte (adr: INTEGER; x: SHORTCHAR);
		VAR res: INTEGER;
	BEGIN
		res := WinApi.WriteProcessMemory(procHandle, adr, SYSTEM.ADR(x), 1, NIL);
		res := WinApi.FlushInstructionCache(procHandle, adr, 1)
	END PutByte;
	
	PROCEDURE GetShort (adr: INTEGER; VAR x: SHORTINT);
		VAR res: INTEGER;
	BEGIN
		IF ODD(adr) THEN
			res := WinApi.ReadProcessMemory(procHandle, adr, SYSTEM.ADR(x), 2, NIL);
			IF res = 0 THEN x := 0 END
		ELSIF adr DIV LEN(cache) = cacheLine THEN
			SYSTEM.GET(SYSTEM.ADR(cache) + adr MOD LEN(cache), x)
		ELSE
			res := WinApi.ReadProcessMemory(procHandle, adr - adr MOD LEN(cache),
															SYSTEM.ADR(cache), LEN(cache), NIL);
			IF res # 0 THEN
				SYSTEM.GET(SYSTEM.ADR(cache) + adr MOD LEN(cache), x);
				cacheLine := adr DIV LEN(cache)
			ELSE
				cacheLine := 0;
				res := WinApi.ReadProcessMemory(procHandle, adr, SYSTEM.ADR(x), 2, NIL);
				IF res = 0 THEN x := 0 END
			END
		END
	END GetShort;

	PROCEDURE GetInt (adr: INTEGER; VAR x: INTEGER);
		VAR res: INTEGER;
	BEGIN
		IF adr MOD 4 # 0 THEN
			res := WinApi.ReadProcessMemory(procHandle, adr, SYSTEM.ADR(x), 4, NIL);
			IF res = 0 THEN x := 0 END
		ELSIF adr DIV LEN(cache) = cacheLine THEN
			SYSTEM.GET(SYSTEM.ADR(cache) + adr MOD LEN(cache), x)
		ELSE
			res := WinApi.ReadProcessMemory(procHandle, adr - adr MOD LEN(cache),
															SYSTEM.ADR(cache), LEN(cache), NIL);
			IF res # 0 THEN
				SYSTEM.GET(SYSTEM.ADR(cache) + adr MOD LEN(cache), x);
				cacheLine := adr DIV LEN(cache)
			ELSE
				cacheLine := 0;
				res := WinApi.ReadProcessMemory(procHandle, adr, SYSTEM.ADR(x), 4, NIL);
				IF res = 0 THEN x := 0 END
			END
		END
	END GetInt;
	
	PROCEDURE GetReal (adr: INTEGER; VAR x: REAL);
		VAR res: INTEGER;
	BEGIN
		res := WinApi.ReadProcessMemory(procHandle, adr, SYSTEM.ADR(x), 8, NIL);
		IF res = 0 THEN x := 0 END
	END GetReal;
	
	PROCEDURE GetName (adr: INTEGER; VAR name: Name);
		VAR res: INTEGER;
	BEGIN
		res := WinApi.ReadProcessMemory(procHandle, adr, SYSTEM.ADR(name), LEN(name), NIL);
		IF res = 0 THEN name := "" END
	END GetName;
	
	PROCEDURE ModList (): Module;
		VAR m: Module;
	BEGIN
		ASSERT(kernelAdr # 0);
		GetInt(kernelAdr, m); 
		RETURN m
	END ModList;
	
	PROCEDURE Root (): Cluster;
		VAR c: Cluster;
	BEGIN
		ASSERT(kernelAdr # 0);
		GetInt(kernelAdr + 4, c); RETURN c
	END Root;
	
	PROCEDURE Stack (): INTEGER;
		VAR stk: INTEGER;
	BEGIN
		ASSERT(kernelAdr # 0);
		GetInt(kernelAdr + 8, stk); RETURN stk
	END Stack;
	
	PROCEDURE ModNext (m: Module): Module;
		VAR next: Module;
	BEGIN
		GetInt(m + 0, next); RETURN next
	END ModNext;
	
	PROCEDURE ModCnt (m: Module): INTEGER;
		VAR cnt: INTEGER;
	BEGIN
		GetInt(m + 8, cnt); RETURN cnt
	END ModCnt;
	
	PROCEDURE ModCode (m: Module): INTEGER;
		VAR code: INTEGER;
	BEGIN
		GetInt(m + 64, code); RETURN code
	END ModCode;
	
	PROCEDURE ModCSize (m: Module): INTEGER;
		VAR csize: INTEGER;
	BEGIN
		GetInt(m + 52, csize); RETURN csize
	END ModCSize;
	
	PROCEDURE ThisModS (name: ARRAY OF SHORTCHAR): Module;	(* loaded modules only *)
		VAR m: Module; n: Name;
	BEGIN
		m := ModList(); GetName(m + 100, n);
		WHILE (m # 0) & ((n # name) OR (ModCnt(m) < 0)) DO m := ModNext(m); GetName(m + 100, n) END;
		RETURN m
	END ThisModS;
	
	PROCEDURE ThisMod (name: ARRAY OF CHAR): Module;	(* loaded modules only *)
		VAR m: Module; n: Name;
	BEGIN
		m := ModList(); GetName(m + 100, n);
		WHILE (m # 0) & ((n # name) OR (ModCnt(m) < 0)) DO m := ModNext(m); GetName(m + 100, n) END;
		RETURN m
	END ThisMod;
	
	PROCEDURE TypeSize (t: Type): INTEGER;
		VAR size: INTEGER;
	BEGIN
		GetInt(t + 0, size); RETURN size
	END TypeSize;
	
	PROCEDURE TypeMod (t: Type): Module;
		VAR mod: Module;
	BEGIN
		GetInt(t + 4, mod); RETURN mod
	END TypeMod;
	
	PROCEDURE TypeId (t: Type): INTEGER;
		VAR id: INTEGER;
	BEGIN
		GetInt(t + 8, id); RETURN id
	END TypeId;
	
	PROCEDURE TypeBase (t: Type; i: INTEGER): Type;
		VAR base: Type;
	BEGIN
		GetInt(t + 12 + i * 4, base); RETURN base
	END TypeBase;
	
	PROCEDURE GetTypeName (t: Type; VAR name: Name);
		VAR names: INTEGER;
	BEGIN
		GetInt(TypeMod(t) + 84, names);	(* t.mod.names *)
		GetName(names + TypeId(t) DIV 256, name)
	END GetTypeName;
	
	PROCEDURE GetObjName (mod: Module; obj: Object; VAR name: Name);
		VAR names, id: INTEGER;
	BEGIN
		GetInt(mod + 84, names);	(* mod.names *)
		GetInt(obj + 8, id);	
		GetName(names + id DIV 256, name)
	END GetObjName;
	
	PROCEDURE RefCh (VAR ref: INTEGER; VAR ch: SHORTCHAR);
	BEGIN
		GetByte(ref, ch); INC(ref)
	END RefCh;
	
	PROCEDURE RefNum (VAR ref: INTEGER; VAR x: INTEGER);
		VAR s, n: INTEGER; ch: SHORTCHAR;
	BEGIN
		s := 0; n := 0; RefCh(ref, ch);
		WHILE ORD(ch) >= 128 DO INC(n, ASH(ORD(ch) - 128, s) ); INC(s, 7); RefCh(ref, ch) END;
		x := n + ASH(ORD(ch) MOD 64 - ORD(ch) DIV 64 * 64, s)
	END RefNum;
	
	PROCEDURE RefName (VAR ref: INTEGER; VAR n: Name);
		VAR i: INTEGER; ch: SHORTCHAR;
	BEGIN
		i := 0; RefCh(ref, ch);
		WHILE ch # 0X DO n[i] := ch; INC(i); RefCh(ref, ch) END;
		n[i] := 0X
	END RefName;
	
	PROCEDURE GetSourcePos (mod: Module; codePos: INTEGER; VAR cadr, spos: INTEGER);
		VAR ref, pos, ad, d: INTEGER; ch: SHORTCHAR; name: Name;
	BEGIN
		GetInt(mod + 72, ref);	(* mod.refs *)
		pos := 0; ad := 0; GetByte(ref, ch);
		WHILE ch # 0X DO
			WHILE (ch > 0X) & (ch < 0FCX) DO
				INC(ad, LONG(ORD(ch))); INC(ref); RefNum(ref, d);
				IF ad > codePos THEN cadr := ad; spos := pos; RETURN END;
				INC(pos, d); GetByte(ref, ch)
			END;
			IF ch = 0FCX THEN
				INC(ref); RefNum(ref, d); RefName(ref, name); GetByte(ref, ch);
				IF (d > codePos) & (pos > 0) THEN cadr := d + 1; spos := pos; RETURN END;
			END;
			WHILE ch >= 0FDX DO	(* skip variables *)
				INC(ref); RefCh(ref, ch);
				IF ch = 10X THEN INC(ref, 4) END;
				RefNum(ref, d); RefName(ref, name); GetByte(ref, ch)
			END
		END;
		cadr := -1; spos := -1
	END GetSourcePos;
	
	PROCEDURE GetCodePos (mod: Module; srcPos: INTEGER; VAR codePos: INTEGER);
		VAR ref, pos, ad, d, min: INTEGER; ch: SHORTCHAR; name: Name;
	BEGIN
		GetInt(mod + 72, ref);	(* mod.refs *)
		pos := 0; ad := 0; min := MAX(INTEGER); codePos := 0; GetByte(ref, ch);
		WHILE ch # 0X DO
			WHILE (ch > 0X) & (ch < 0FCX) DO
				INC(ad, LONG(ORD(ch))); INC(ref); RefNum(ref, d); INC(pos, d);
				IF (pos >= srcPos) & (pos < min) THEN
					min := pos; codePos := ad
				END;
				GetByte(ref, ch) 
			END;
			IF ch = 0FCX THEN INC(ref); RefNum(ref, d); RefName(ref, name); GetByte(ref, ch) END;
			WHILE ch >= 0FDX DO	(* skip variables *)
				INC(ref); RefCh(ref, ch);
				IF ch = 10X THEN INC(ref, 4) END;
				RefNum(ref, d); RefName(ref, name); GetByte(ref, ch)
			END
		END
	END GetCodePos;
	
	PROCEDURE  GetRefProc (VAR ref, adr: INTEGER; VAR name: Name);
		VAR ch: SHORTCHAR;
	BEGIN
		GetByte(ref, ch);
		WHILE ch >= 0FDX DO	(* skip variables *)
			INC(ref); RefCh(ref, ch);
			IF ch = 10X THEN INC(ref, 4) END;
			RefNum(ref, adr); RefName(ref, name); GetByte(ref, ch)
		END;
		WHILE (ch > 0X) & (ch < 0FCX) DO	(* skip source refs *)
			INC(ref); RefNum(ref, adr); GetByte(ref, ch)
		END;
		IF ch = 0FCX THEN INC(ref); RefNum(ref, adr); RefName(ref, name);
		ELSE adr := 0
		END
	END GetRefProc;
	
	PROCEDURE  GetRefVar (VAR ref: INTEGER; VAR mode, form: SHORTCHAR; VAR desc: Type; VAR adr: INTEGER; VAR name: Name);
	BEGIN
		GetByte(ref, mode); desc := 0;
		IF mode >= 0FDX THEN
			mode := SHORT(CHR(ORD(mode) - 0FCH));
			INC(ref); RefCh(ref, form);
			IF form = 10X THEN
				GetInt(ref, desc); INC(ref, 4); form := SHORT(CHR(16 + TypeId(desc) MOD 4))
			END;
			RefNum(ref, adr); RefName(ref, name)
		ELSE
			mode := 0X; form := 0X; adr := 0
		END
	END GetRefVar;
	
	PROCEDURE  SearchProcVar (var: INTEGER; VAR m: Module; VAR adr: INTEGER);
	BEGIN
		adr := var; m := 0;
		IF var # 0 THEN
			m := ModList();
			WHILE (m # 0) & ((adr < ModCode(m)) OR (adr >= ModCode(m) + ModCSize(m))) DO
				m := ModNext(m)
			END;
			IF m # 0 THEN DEC(adr, ModCode(m)) END
		END
	END SearchProcVar;
	
	PROCEDURE Valid (): BOOLEAN;
		VAR res, code: INTEGER;
	BEGIN
		res := WinApi.GetExitCodeProcess(procHandle, code);
		RETURN code = WinApi.STILL_ACTIVE
	END Valid;
	
	PROCEDURE Suspend;
		VAR res: INTEGER;
	BEGIN
		res := WinApi.SuspendThread(threadHandle);
	END Suspend;
	
	PROCEDURE Resume;
		VAR res: INTEGER;
	BEGIN
		res := WinApi.ResumeThread(threadHandle);
	END Resume;

	(* ------------------------------- *)

	PROCEDURE NewRuler (): TextRulers.Ruler;
		CONST mm = Ports.mm; pt = Ports.point;
		VAR r: TextRulers.Ruler;
	BEGIN
		r := TextRulers.dir.New(NIL);
		TextRulers.SetRight(r, 140 * mm);
		TextRulers.AddTab(r, 4 * mm); TextRulers.AddTab(r, 34 * mm); TextRulers.AddTab(r, 80 * mm);
		RETURN r
	END NewRuler;

	PROCEDURE NewModRuler (): TextRulers.Ruler;
		CONST mm = Ports.mm; pt = Ports.point;
		VAR r: TextRulers.Ruler;
	BEGIN
		r := TextRulers.dir.New(NIL);
		IF Dialog.platform DIV 10 = 2 THEN	(* mac *)
			TextRulers.SetRight(r, 154 * mm);
			TextRulers.AddTab(r, 48 * mm); TextRulers.MakeRightTab(r);
			TextRulers.AddTab(r, 64 * mm); TextRulers.MakeRightTab(r);
			TextRulers.AddTab(r, 76 * mm); TextRulers.AddTab(r, 115 * mm)
		ELSE
			TextRulers.SetRight(r, 144 * mm);
			TextRulers.AddTab(r, 48 * mm); TextRulers.MakeRightTab(r);
			TextRulers.AddTab(r, 64 * mm); TextRulers.MakeRightTab(r);
			TextRulers.AddTab(r, 76 * mm); TextRulers.AddTab(r, 110 * mm)
		END;
		RETURN r
	END NewModRuler;

	PROCEDURE OpenViewer (t: TextModels.Model; title: Views.Title; ruler:TextRulers.Ruler);
		VAR v: TextViews.View; c: Containers.Controller;
	BEGIN
		Dialog.MapString(title, title);
		v := TextViews.dir.New(t);
		v.SetDefaults(ruler, TextViews.dir.defAttr);
		c := v.ThisController();
		IF c # NIL THEN
			c.SetOpts(c.opts - {Containers.noFocus, Containers.noSelection} + {Containers.noCaret})
		END;
		Views.OpenAux(v, title)
	END OpenViewer;
	
	PROCEDURE OpenFold (hidden: ARRAY OF CHAR);
		VAR fold: StdFolds.Fold; t: TextModels.Model; w: TextMappers.Formatter;
	BEGIN
		Dialog.MapString(hidden, hidden);
		t := TextModels.CloneOf(StdLog.buf);
		w.ConnectTo(t); w.WriteString(hidden);
		fold := StdFolds.dir.New(StdFolds.expanded, "", t);
		out.WriteView(fold)
	END OpenFold;
	
	PROCEDURE CloseFold (collaps: BOOLEAN);
		VAR fold: StdFolds.Fold; m: TextModels.Model;
	BEGIN
		fold := StdFolds.dir.New(StdFolds.expanded, "", NIL);
		out.WriteView(fold);
		IF collaps THEN fold.Flip(); m := out.rider.Base(); out.SetPos(m.Length()) END
	END CloseFold;
	
	PROCEDURE Scan (VAR s: TextMappers.Scanner);
	BEGIN
		s.Scan;
		IF s.type = TextMappers.string THEN
			IF s.string = "IMPORT" THEN s.type := import
			ELSIF s.string = "MODULE" THEN s.type := smodule
			ELSIF s.string = "THEN" THEN s.type := stop
			ELSIF s.string = "OF" THEN s.type := stop
			ELSIF s.string = "DO" THEN s.type := stop
			ELSIF s.string = "END" THEN s.type := stop
			ELSIF s.string = "ELSE" THEN s.type := stop
			ELSIF s.string = "ELSIF" THEN s.type := stop
			ELSIF s.string = "UNTIL" THEN s.type := stop
			ELSIF s.string = "TO" THEN s.type := stop
			ELSIF s.string = "BY" THEN s.type := stop
			END
		ELSIF s.type = TextMappers.char THEN
			IF s.char = ";" THEN s.type := semicolon
			ELSIF s.char = "|" THEN s.type := stop
			ELSIF s.char = ":" THEN
				IF s.rider.char = "=" THEN s.rider.Read; s.type := becomes END
			ELSIF s.char = "(" THEN
				IF s.rider.char = "*" THEN
					s.rider.Read;
					REPEAT Scan(s) UNTIL (s.type = TextMappers.eot) OR (s.type = comEnd);
					Scan(s)
				END
			ELSIF s.char = "*" THEN
				IF s.rider.char = ")" THEN s.rider.Read; s.type := comEnd END
			END
		END	
	END Scan;
	
	PROCEDURE WriteHex (n: INTEGER);
	BEGIN
		out.WriteIntForm(n, TextMappers.hexadecimal, 9, "0", TextMappers.showBase)
	END WriteHex;
	
	PROCEDURE WriteString (adr, len, base: INTEGER; zterm: BOOLEAN);
		CONST beg = 0; char = 1; code = 2;
		VAR ch: SHORTCHAR; val, mode: SHORTINT; str: ARRAY 16 OF CHAR;
	BEGIN
		mode := beg;
		IF base = 2 THEN GetShort(adr, val) ELSE GetByte(adr, ch); val := ORD(ch) END;
		IF zterm & (val = 0) THEN out.WriteSString('""')
		ELSE
			REPEAT
				IF (val >= ORD(" ")) & (val < 7FH) OR (val > 0A0H) & (val < 256) THEN
					IF mode # char THEN
						IF mode = code THEN out.WriteSString(", ") END;
						out.WriteChar(22X); mode := char
					END;
					out.WriteChar(SHORT(CHR(val)))
				ELSE
					IF mode = char THEN out.WriteChar(22X) END;
					IF mode # beg THEN out.WriteSString(", ") END;
					mode := code; Strings.IntToStringForm(val, Strings.hexadecimal, 1, "0", FALSE, str);
					IF str[0] > "9" THEN out.WriteChar("0") END;
					out.WriteString(str); out.WriteChar("X")
				END;
				INC(adr, base); DEC(len);
				IF base = 2 THEN GetShort(adr, val) ELSE GetByte(adr, ch); val := ORD(ch) END
			UNTIL (len = 0) OR zterm & (val = 0)
		END;
		IF mode = char THEN out.WriteChar(22X) END
	END WriteString;
	
	PROCEDURE IsIdent (s: ARRAY OF CHAR): BOOLEAN;
		VAR i: SHORTINT; ch: CHAR;
	BEGIN
		ch := s[0];
		IF ("A" <= CAP(ch)) & (CAP(ch) <= "Z") THEN
			i := 1; ch := s[1];
			WHILE ("A" <= CAP(ch)) & (CAP(ch) <= "Z") OR ("0" <= ch) & (ch <= "9") DO
				INC(i); ch := s[i]
			END;
			RETURN (s[i] = 0X) & (i < 256)
		ELSE RETURN FALSE
		END
	END IsIdent;
	
	PROCEDURE OutString (s: ARRAY OF CHAR);
		VAR str: Dialog.String;
	BEGIN
		Dialog.MapString(s, str);
		out.WriteString(str);
	END OutString;

	(* -------------------  variable display ------------------- *)
	
	PROCEDURE FormOf (t: Type): SHORTCHAR;
	BEGIN
		IF t DIV 256 = 0 THEN
			RETURN SHORT(CHR(t))
		ELSE
			RETURN SHORT(CHR(16 + TypeId(t) MOD 4))
		END
	END FormOf;
	
	PROCEDURE LenOf (t: Type; ptr: ArrayPtr): INTEGER;
		VAR size, len: INTEGER;
	BEGIN
		size := TypeSize(t);
		IF size # 0 THEN RETURN size
		ELSIF ptr # 0 THEN
			GetInt(ptr + 12 + (TypeId(t) DIV 16 MOD 16 - 1) * 4, len);	(* ptr.len[id DIV 16 MOD 16 - 1] *)
			RETURN len
		ELSE RETURN 0
		END
	END LenOf;
	
	PROCEDURE SizeOf (t: Type; ptr: ArrayPtr): INTEGER;
	BEGIN
		CASE FormOf(t) OF
		| 0BX: RETURN 0
		| 1X, 2X, 4X: RETURN 1
		| 3X, 5X: RETURN 2
		| 8X, 0AX: RETURN 8
		| 11X: RETURN TypeSize(t)
		| 12X: RETURN LenOf(t, ptr) * SizeOf(TypeBase(t, 0), ptr)
		ELSE RETURN 4
		END
	END SizeOf;

	PROCEDURE WriteName (t: Type; ptr: ArrayPtr);
		VAR name, mname: Name; f: SHORTCHAR; mod: Module;
	BEGIN
		f := FormOf(t);
		CASE f OF
		| 0X: OutString("#Dev:Unknown")
		| 1X: out.WriteSString("BOOLEAN")
		| 2X: out.WriteSString("SHORTCHAR")
		| 3X: out.WriteSString("CHAR")
		| 4X: out.WriteSString("BYTE")
		| 5X: out.WriteSString("SHORTINT")
		| 6X: out.WriteSString("INTEGER")
		| 7X: out.WriteSString("SHORTREAL")
		| 8X: out.WriteSString("REAL")
		| 9X: out.WriteSString("SET")
		| 0AX: out.WriteSString("LONGINT")
		| 0BX: out.WriteSString("ANYREC")
		| 0CX: out.WriteSString("ANYPTR")
		| 0DX: out.WriteSString("POINTER")
		| 0EX: out.WriteSString("PROCEDURE")
		| 0FX: out.WriteSString("STRING")
		| 10X..13X:
			mod := TypeMod(t);
			IF (mod # 0) & (TypeId(t) DIV 256 # 0) & (ModCnt(mod) >= 0) THEN
				GetTypeName(t, name);
				IF name = "!" THEN
					IF f = 11X THEN out.WriteSString("RECORD")
					ELSIF f = 12X THEN out.WriteSString("ARRAY")
					ELSE OutString("#Dev:Unknown")
					END
				ELSE
					GetName(mod + 100, mname);
					out.WriteSString(mname); out.WriteChar("."); out.WriteSString(name)
				END
			ELSIF f = 11X THEN
				IF mod # 0 THEN GetName(mod + 100, mname); out.WriteSString(mname); out.WriteChar(".") END;
				out.WriteSString("RECORD");
			ELSIF f = 12X THEN
				out.WriteSString("ARRAY "); out.WriteInt(LenOf(t, ptr)); t := TypeBase(t, 0);
				WHILE (FormOf(t) = 12X) & ((TypeId(t) DIV 256 = 0) OR (mod = 0) OR (ModCnt(mod) < 0)) DO
					out.WriteSString(", "); out.WriteInt(LenOf(t, ptr)); t := TypeBase(t, 0)
				END;
				out.WriteSString(" OF "); WriteName(t, ptr)
			ELSIF f = 13X THEN
				out.WriteSString("POINTER")
			ELSE
				out.WriteSString("PROCEDURE")
			END
		| 20X: out.WriteSString("COM.IUnknown")
		| 21X: out.WriteSString("COM.GUID")
		| 22X: out.WriteSString("COM.RESULT")
		ELSE OutString("#Dev:UnknownFormat"); out.WriteInt(ORD(f))
		END
	END WriteName;
	
	PROCEDURE WriteGuid (a: INTEGER);
	
		PROCEDURE Hex (a: INTEGER);
			VAR x: SHORTCHAR;
		BEGIN
			GetByte(a, x);
			out.WriteIntForm(ORD(x), TextMappers.hexadecimal, 2, "0", FALSE)
		END Hex;
		
	BEGIN
		out.WriteChar("{");
		Hex(a + 3); Hex(a + 2); Hex(a + 1); Hex(a);
		out.WriteChar("-");
		Hex(a + 5); Hex(a + 4);
		out.WriteChar("-");
		Hex(a + 7); Hex(a + 6);
		out.WriteChar("-");
		Hex(a + 8);
		Hex(a + 9);
		out.WriteChar("-");
		Hex(a + 10);
		Hex(a + 11);
		Hex(a + 12);
		Hex(a + 13);
		Hex(a + 14);
		Hex(a + 15);
		out.WriteChar("}")
	END WriteGuid;
	
	PROCEDURE^ ShowVar (ad, ind: INTEGER; f, c: SHORTCHAR; desc: Type; ptr: ArrayPtr; back: RefView; VAR name, sel: Name);
	
	PROCEDURE^ NewRefView (type, command: SHORTINT; adr: INTEGER; back: RefView;
												desc: Type; ptr: ArrayPtr; name: Name): RefView;

	PROCEDURE^ InsertRefView (type, command: SHORTINT; adr: INTEGER; back: RefView;
												desc: Type; ptr: ArrayPtr; name: Name);

	PROCEDURE ShowRecord (a, ind: INTEGER; desc: Type; back: RefView; VAR sel: Name);
		VAR name: Name; i, j, n, dir, num, offs: INTEGER; base, struct: Type; obj: Object;
	BEGIN
		WriteName(desc, 0); out.WriteTab;
		IF ModCnt(TypeMod(desc)) >= 0 THEN
			OpenFold("#Dev:Fields");
			n := TypeId(desc) DIV 16 MOD 16; j := 0;
			WHILE j <= n DO
				base := TypeBase(desc, j);
				IF base # 0 THEN
					GetInt(base + 76, dir);	(* base.fields *)
					GetInt(dir, num);	(* dir.num *)
					i := 0;
					WHILE i < num DO
						obj := dir + 4 + i * 16;	(* ADR(dir.obj[i]) *)
						GetObjName(TypeMod(base), obj, name);
						GetInt(obj + 4, offs);	(* obj.offs *)
						GetInt(obj + 12, struct);	(* obj.struct *)
						ShowVar(a + offs, ind, FormOf(struct), 1X, struct, 0, back, name, sel);
						INC(i)
					END
				END;
				INC(j)
			END;
			out.WriteSString("   "); CloseFold((ind > 1) OR (sel # ""))
		ELSE
			OutString("#Dev:Unloaded")
		END
	END ShowRecord;
	
	PROCEDURE ShowArray (a, ind: INTEGER; desc: Type; ptr: ArrayPtr; back: RefView; VAR sel: Name);
		VAR f: SHORTCHAR; i, n, m, size, len: INTEGER; name: Name; eltyp, t: Type; vi: SHORTINT; vs: SHORTCHAR; str: Dialog.String;
	BEGIN
		WriteName(desc, ptr); out.WriteTab;
		len := LenOf(desc, ptr); eltyp := TypeBase(desc, 0); f := FormOf(eltyp); size := SizeOf(eltyp, ptr);
		IF (f = 2X) OR (f = 3X) THEN	(* string *)
			n := 0; m := len;
			IF f = 2X THEN
				REPEAT GetByte(a + n, vs); INC(n) UNTIL (n = 32) OR (n = len) OR (vs = 0X);
				REPEAT DEC(m); GetByte(a + m, vs) UNTIL (m = 0) OR (vs # 0X)
			ELSE
				REPEAT GetShort(a + n * 2, vi); INC(n) UNTIL (n = 32) OR (n = len) OR (vi = 0);
				REPEAT DEC(m); GetShort(a + m * 2, vi) UNTIL (m = 0) OR (vi # 0)
			END;
			WriteString(a, n, size, TRUE);
			INC(m, 2);
			IF m > len THEN m := len END;
			IF m > n THEN
				out.WriteSString("   "); OpenFold("...");
				out.WriteLn;
				WriteString(a, m, size, FALSE);
				IF m < len THEN out.WriteSString(", ..., 0X") END;
				out.WriteSString("   "); CloseFold(TRUE)
			END
		ELSE
			t := eltyp;
			WHILE FormOf(t) = 12X DO t := TypeBase(t, 0) END;
			IF FormOf(t) # 0X THEN
				OpenFold("#Dev:Elements");
				i := 0;
				WHILE i < len DO
					Strings.IntToString(i, str);
					name := "[" + SHORT(str$) + "]";
					ShowVar(a, ind, f, 1X, eltyp, ptr, back, name, sel);
					INC(i); INC(a, size)
				END;
				out.WriteSString("   "); CloseFold(TRUE)
			END
		END
	END ShowArray;
	
	PROCEDURE ShowProcVar (a: INTEGER);
		VAR vli, n, ref: INTEGER; m: Module; name, mname: Name;
	BEGIN
		GetInt(a, vli);
		SearchProcVar(vli, m, vli);
		IF m = 0 THEN
			IF vli = 0 THEN out.WriteSString("NIL")
			ELSE WriteHex(vli)
			END
		ELSE
			IF ModCnt(m) >= 0 THEN
				GetName(m + 100, mname);
				out.WriteSString(mname); GetInt(m + 72, ref);	(* m.refs *)
				REPEAT GetRefProc(ref, n, name) UNTIL (n = 0) OR (vli < n);
				IF vli < n THEN out.WriteChar("."); out.WriteSString(name) END
			ELSE
				OutString("#Dev:ProcInUnloadedMod");
				GetName(m + 100, mname);
				out.WriteSString(mname); out.WriteSString(" !!!")
			END
		END
	END ShowProcVar;

	PROCEDURE ShowPointer (a: INTEGER; f: SHORTCHAR; desc: Type; back: RefView; VAR sel: Name);
		VAR adr, x, size: INTEGER; ptr: ArrayPtr; c: Cluster; btyp: Type;
	BEGIN
		GetInt(a, adr);
		IF f = 13X THEN btyp := TypeBase(desc, 0) ELSE btyp := 0 END;
		IF adr = 0 THEN out.WriteSString("NIL")
		ELSIF f = 20X THEN
			out.WriteChar("["); WriteHex(adr); out.WriteChar("]");
			out.WriteChar(" "); c := Root(); GetInt(c + 0, size);	(* c.size *)
			WHILE (c # 0) & ((adr < c) OR (adr >= c + size)) DO GetInt(c + 4, c); (* c.next *) GetInt(c + 0, size) END;
			IF c # 0 THEN
				ptr := adr;
				InsertRefView(heap, open, adr, back, btyp, ptr, sel)
			END
		ELSE
			IF (f = 13X) OR (f = 0CX) THEN x := adr - 4 ELSE x := adr END;
			IF IsReadable(x, adr + 16) THEN
				out.WriteChar("["); WriteHex(adr); out.WriteChar("]");
				IF (f = 13X) OR (f = 0CX) THEN
					out.WriteChar(" "); c := Root(); GetInt(c + 0, size);	(* c.size *)
					WHILE (c # 0) & ((adr < c) OR (adr >= c + size)) DO GetInt(c + 4, c); (* c.next *) GetInt(c + 0, size) END;
					IF c # 0 THEN
						ptr := adr;
						IF (f = 13X) & (FormOf(btyp) = 12X) THEN	(* array *)
							adr := ptr + 12 + (TypeId(btyp) DIV 16 MOD 16) * 4;	(* ADR(ptr.len[btyp.id DIV 16 MOD 16]) *)
						END;
						InsertRefView(heap, open, adr, back, btyp, ptr, sel)
					ELSE OutString("#Dev:IllegalPointer");
					END
				END
			ELSE OutString("#Dev:IllegalAddress"); WriteHex(adr)
			END
		END
	END ShowPointer;
	
	PROCEDURE ShowSelector (ref: RefView);
		VAR b: RefView; n: SHORTINT; a, a0: TextModels.Attributes;
	BEGIN
		b := ref.back; n := 1;
		IF b # NIL THEN
			WHILE (b.name = ref.name) & (b.back # NIL) DO INC(n); b := b.back END;
			ShowSelector(b);
			IF n > 1 THEN out.WriteChar("(") END;
			out.WriteChar(".")
		END;
		out.WriteSString(ref.name);
		IF ref.type = heap THEN out.WriteChar("^") END;
		IF n > 1 THEN
			out.WriteChar(")");
			a0 := out.rider.attr; a := TextModels.NewOffset(a0, 2 * Ports.point);
			out.rider.SetAttr(a);
			out.WriteInt(n); out.rider.SetAttr(a0)
		END;
	END ShowSelector;
	
	PROCEDURE ShowVar (ad, ind: INTEGER; f, c: SHORTCHAR; desc: Type; ptr: ArrayPtr; back: RefView; VAR name, sel: Name);
		VAR i, j, vli, a, ref: INTEGER; tsel: Name; a0: TextModels.Attributes;
			vc: SHORTCHAR; vsi: BYTE; vi: SHORTINT; vr: SHORTREAL; vlr: REAL; vs: SET;
	BEGIN
		out.WriteLn; out.WriteTab; i := 0;
		WHILE i < ind DO out.WriteSString("  "); INC(i) END;
		a := ad; i := 0; j := 0;
		IF sel # "" THEN
			WHILE sel[i] # 0X DO tsel[i] := sel[i]; INC(i) END;
			IF (tsel[i-1] # ":") & (name[0] # "[") THEN tsel[i] := "."; INC(i) END
		END;
		WHILE name[j] # 0X DO tsel[i] := name[j]; INC(i); INC(j) END;
		tsel[i] := 0X;
		a0 := out.rider.attr;
		IF c = 3X THEN	(* varpar *)
			GetInt(ad, a);
			out.rider.SetAttr(TextModels.NewStyle(a0, {Fonts.italic}))
		END;
		IF name[0] # "[" THEN out.WriteChar(".") END;
		out.WriteSString(name);
		out.rider.SetAttr(a0); out.WriteTab;
		IF (c = 3X) & (a >= 0) & (a < 65536) THEN 
			out.WriteTab; out.WriteSString("NIL VARPAR");
		ELSIF f = 11X THEN
			GetTypeName(desc, name);
			IF (c = 3X) & (name[0] # "!") THEN GetInt(ad + 4, desc) END;	(* dynamic type *)
			ShowRecord(a, ind + 1, desc, back, tsel)
		ELSIF (c = 3X) & (f = 0BX) THEN	(* VAR anyrecord *)
			GetInt(ad + 4, desc);
			ShowRecord(a, ind + 1, desc, back, tsel)
		ELSIF f = 12X THEN
			IF (TypeSize(desc) = 0) & (ptr = 0) THEN GetInt(ad, a) END;	(* dyn array val par *)
			IF ptr = 0 THEN ptr := ad - 8 END;
			ShowArray(a, ind + 1, desc, ptr, back, tsel)
		ELSE
			IF desc = 0 THEN desc := ORD(f) END;
			WriteName(desc, 0); out.WriteTab;
			CASE f OF
			| 0X: (* GetInt(a, vli); WriteHex(vli) *)
			| 1X: GetByte(a, vc); 
				IF vc = 0X THEN out.WriteSString("FALSE")
				ELSIF vc = 1X THEN out.WriteSString("TRUE")
				ELSE OutString("#Dev:Undefined"); out.WriteInt(ORD(vc))
				END
			| 2X: WriteString(a, 1, 1, FALSE)
			| 3X: WriteString(a, 1, 2, FALSE)
			| 4X: GetByte(a, vc); out.WriteInt(SYSTEM.VAL(BYTE, vc))
			| 5X: GetShort(a, vi); out.WriteInt(vi)
			| 6X: GetInt(a, vli); out.WriteInt(vli)
			| 7X: GetInt(a, vli); out.WriteReal(SYSTEM.VAL(SHORTREAL, vli))
			| 8X: GetReal(a, vlr); out.WriteReal(vlr)
			| 9X: GetInt(a, vli); out.WriteSet(SYSTEM.VAL(SET, vli))
			| 0AX: GetInt(a, vli); GetInt(a + 4, i);
				IF (vli >= 0) & (i = 0) OR (vli < 0) & (i = -1) THEN out.WriteInt(vli)
				ELSE out.WriteIntForm(i, TextMappers.hexadecimal, 8, "0", TextMappers.hideBase); WriteHex(vli)
				END
			| 0CX, 0DX, 13X, 20X: ShowPointer(a, f, desc, back, tsel)
			| 0EX, 10X: ShowProcVar(a)
			| 0FX: WriteString(a, 256, 1, TRUE)
			| 21X: WriteGuid(a)
			| 22X: GetInt(a, vli); WriteHex(vli)
			ELSE 
			END
		END
	END ShowVar;
	
	PROCEDURE WriteTimeStamp (adr: INTEGER);
		VAR d: Dates.Date; t: Dates.Time; x: SHORTINT; str: ARRAY 64 OF CHAR;
	BEGIN
		GetShort(adr, x); d.year := x;
		IF d.year = 0 THEN
			out.WriteSString("      "); OutString("#Dev:Linked")
		ELSE
			GetShort(adr + 2, x); d.month := x;
			GetShort(adr + 4, x); d.day := x;
			GetShort(adr + 6, x); t.hour := x;
			GetShort(adr + 8, x); t.minute := x;
			GetShort(adr + 10, x); t.second := x;
			Dates.DateToString(d, Dates.short, str);
			out.WriteString(str); out.WriteString("  ");
			Dates.TimeToString(t, str);
			out.WriteString(str); 
		END
	END WriteTimeStamp;

	PROCEDURE ShowModules;
		VAR m, m1: Module; a0: TextModels.Attributes; n, h, t, h1: ARRAY 256 OF CHAR; name: Name; x, y: INTEGER;
	BEGIN
		cacheLine := 0;
		a0 := out.rider.attr;
		out.rider.SetAttr(TextModels.NewStyle(a0, {Fonts.italic}));
		OutString("#Dev:ModuleName"); out.WriteTab;
		OutString("#Dev:BytesUsed"); out.WriteTab;
		OutString("#Dev:Clients"); out.WriteTab;
		OutString("#Dev:Compiled"); out.WriteTab;
		OutString("#Dev:Loaded");
		out.rider.SetAttr(a0); out.WriteTab; out.WriteTab;
		out.rider.SetAttr(TextModels.NewStyle(out.rider.attr, {Fonts.underline}));
		out.rider.SetAttr(TextModels.NewColor(out.rider.attr, Ports.blue));
		out.WriteView(StdLinks.dir.NewLink("DevRemDebug.UpdateModules"));
		OutString("#Dev:Update");
		out.WriteView(StdLinks.dir.NewLink(""));
		out.rider.SetAttr(a0); out.WriteLn;
		m := ModList();
		WHILE m # 0 DO
			IF ModCnt(m) >= 0 THEN
				GetName(m + 100, name);	(* m.name *)
				n := name$; Kernel.SplitName(n, h, t);
				m1 := ModList(); h1 := "*";
				WHILE (m1 # m) & (h1 # h) DO
					IF ModCnt(m1) >= 0 THEN GetName(m1 + 100, name); n := name$; Kernel.SplitName(n, h1, t) END;
					m1 := ModNext(m1)
				END;
				IF h1 # h THEN
					out.WriteLn;
					m1 := m;
					WHILE m1 # 0 DO
						GetName(m1 + 100, name);	(* m1.name *)
						n := name$; Kernel.SplitName(n, h1, t);
						IF (h1 = h) & (ModCnt(m1) >= 0) THEN
							out.WriteSString(name); out.WriteTab;
							GetInt(m1 + 52, x);	(* m1.csize *)
							GetInt(m1 + 56, y); x := x + y;	(* m1.dsize *)
							GetInt(m1 + 60, y); x := x + y;	(* m1.rsize *)
							out.WriteIntForm(x, 10, 6, TextModels.digitspace, TextMappers.hideBase);
							out.WriteTab;
							out.WriteIntForm(ModCnt(m1), 10, 3, TextModels.digitspace, TextMappers.hideBase);
							out.WriteTab;
							WriteTimeStamp(m1 + 12);	(* m1.compTime *)
							out.WriteTab;
							WriteTimeStamp(m1 + 24);	(* m1.loadTime *)
							out.WriteLn
						END;
						m1 := ModNext(m1)
					END
				END
			END;
			m := ModNext(m)
		END
	END ShowModules;
	
	PROCEDURE ShowGlobals (mod: Module);
		VAR ref, x, data: INTEGER; m, f: SHORTCHAR; name: ARRAY 256 OF CHAR; mname, vname: Name;
			d: Type; v: RefView; a0: TextModels.Attributes;
	BEGIN
		IF mod # 0 THEN
			cacheLine := 0;
			GetName(mod + 100, mname);	(* mod.name *)
			out.WriteSString(mname);
			out.WriteTab; out.WriteTab; out.WriteTab;
			a0 := out.rider.attr;
			out.rider.SetAttr(TextModels.NewStyle(out.rider.attr, {Fonts.underline}));
			out.rider.SetAttr(TextModels.NewColor(out.rider.attr, Ports.blue));
			name := "DevRemDebug.UpdateGlobals('" + mname + "')";
			out.WriteView(StdLinks.dir.NewLink(name));
			OutString("#Dev:Update");
			out.WriteView(StdLinks.dir.NewLink(""));
			out.rider.SetAttr(a0); out.WriteLn;
			GetInt(mod + 72, ref); GetRefProc(ref, x, vname);	(* get body *)
			IF x # 0 THEN
				v := NewRefView (module, open, 0, NIL, 0, 0, mname);
				GetRefVar(ref, m, f, d, x, vname);
				WHILE m = 1X DO
					GetInt(mod + 68, data);	(* mod.data *)
					ShowVar(data + x, 0, f, m, d, 0, v, vname, empty);
					GetRefVar(ref, m, f, d, x, vname)
				END
			END;
			out.WriteLn
		END
	END ShowGlobals;
	
	PROCEDURE ShowObject (adr: INTEGER);
		VAR eltyp: Type; ptr: ArrayPtr; desc: ARRAY 64 OF INTEGER; i, n, lev, elsize, fields, first: INTEGER; name: Name;
	BEGIN
		GetInt(adr - 4, eltyp);
		IF ODD(eltyp DIV 2) THEN
(*
			DEC(eltyp, 2); ptr := adr;
			elsize := TypeSize(eltyp);
			GetName(TypeMod(eltyp) + 100, name);
			IF name = "Kernel" THEN
				GetInt(eltyp + 76, fields);	(* eltyp.fields *)
				GetInt(fields, n);	(* fields.num *)
				IF n = 1 THEN
					GetInt(fields + 4 + 12, eltyp)	(* fields.obj[0].struct *)
				END
			END;
			GetInt(ptr + 8, first);	(* ptr.first *)
			GetInt(ptr + 0, n);	(* ptr.last *)
			n := (n - first) DIV elsize + 1;
			lev := (first - adr - 12) DIV 4; i := 0;
			WHILE lev > 0 DO	(* dynamic levels *)
				DEC(lev);
				GetInt(ptr + 12 + lev * 4, desc[i]);	(* ptr.len[lev] *)	(* size *)
				n := n DIV desc[i]; INC(i);
				desc[i] := 0; INC(i);	(* module *)
				desc[i] := 2; INC(i);	(* id *)
				desc[i] := SYSTEM.ADR(desc[i+1]); INC(i)	(* desc *)
			END;
			IF n > 1 THEN	(* static level *)
				desc[i] := n; INC(i);	(* size *)
				desc[i] := 0; INC(i);	(* module *)
				desc[i] := 2; INC(i);	(* id *)
			ELSE DEC(i)
			END;
			desc[i] := eltyp;	(* desc *)
			ShowArray(first, 1, SYSTEM.ADR(desc), ptr, NIL, empty);
			out.WriteLn
*)
		ELSE ShowRecord(adr, 1, eltyp, NIL, empty)
		END;
	END ShowObject;
	
	PROCEDURE ShowPtrDeref (ref: RefView);
		VAR b: RefView; typ: Type;
	BEGIN
		ShowSelector(ref); b := ref.back;
		IF b # NIL THEN
			out.WriteChar(" ");
			InsertRefView(b.type, undo, b.adr, b.back, b.desc, b.ptr, b.name)
		END;
		out.WriteLn; out.WriteLn;
		out.WriteChar("["); WriteHex(ref.adr); out.WriteChar("]"); out.WriteTab;
		IF ref.desc = 0 THEN
			ShowObject(ref.adr)
		ELSIF FormOf(ref.desc) = 12X THEN
			ShowArray(ref.adr, 1, ref.desc, ref.ptr, ref, empty)
		ELSE
			GetInt(ref.ptr - 4, typ);	(* TypeOf(ref.ptr *)
			ShowRecord(ref.adr, 1, typ, ref, empty)
		END;
		out.WriteLn
	END ShowPtrDeref;
	
	PROCEDURE ShowSourcePos (name: Name; adr: INTEGER; auto: BOOLEAN);
		VAR loc: Files.Locator; fname: Files.Name; v: Views.View; m: Models.Model; conv: Converters.Converter;
			c: Containers.Controller; a, beg, end: INTEGER; s: TextMappers.Scanner; w, fw: Windows.Window;
			n: ARRAY 256 OF CHAR;
	BEGIN
		(* search source by name heuristic *)
		n := name$; StdDialog.GetSubLoc(n, "Mod", loc, fname); m := NIL;
		IF auto THEN
			w := Windows.dir.First(); fw := w; a := 0; 
			WHILE (w # NIL) & ((w.loc = NIL) OR (w.name = "") OR (w.loc.res = 77) OR
									~Files.dir.SameFile(loc, fname, w.loc, w.name) OR (w.conv # Converters.list)) DO
				w := Windows.dir.Next(w); INC(a)
			END;
			IF w # NIL THEN
				v := w.doc.ThisView();
				m := v.ThisModel();
				IF ~(m IS TextModels.Model) THEN m := NIL END;
				IF m # NIL THEN
					IF fw.doc.ThisView().ThisModel() = stateText THEN
						IF a > 1 THEN
							Windows.dir.Select(w, Windows.eager);
							Windows.dir.Select(fw, Windows.eager)
						END
					ELSE Windows.dir.Select(w, Windows.eager)
					END
				END
			END
		END;
		IF m = NIL THEN
			v := Views.OldView(loc, fname);
			IF v # NIL THEN
				Views.Open(v, loc, fname, NIL);
				m := v.ThisModel();
				IF ~(m IS TextModels.Model) THEN m := NIL END
			END
		END;
		IF m = NIL THEN
			(* search in open windows *)
			w := Windows.dir.First();
			WHILE (w # NIL) & (m = NIL) DO
				v := w.doc.ThisView();
				m := v.ThisModel();
				IF m # NIL THEN
					WITH m: TextModels.Model DO
						s.ConnectTo(m); s.SetPos(0);
						REPEAT
							REPEAT s.Scan UNTIL s.rider.eot OR (s.type = TextMappers.string) & (s.string = "MODULE");
							s.Scan;
						UNTIL s.rider.eot OR (s.type = TextMappers.string) & (s.string = name);
						IF ~s.rider.eot THEN Windows.dir.Select(w, Windows.eager)
						ELSE m := NIL
						END
					ELSE m := NIL
					END
				END;
				w := Windows.dir.Next(w)
			END
		END;
		IF (m = NIL) & ~auto THEN
			(* ask user for source file *)
			conv := NIL; v := Views.Old(Views.ask, loc, fname, conv);
			IF v # NIL THEN
				Views.Open(v, loc, fname, conv);
				m := v.ThisModel();
				IF ~(m IS TextModels.Model) THEN m := NIL END
			END
		END;
		IF m # NIL THEN
			(* mark error position in text *)
			WITH m: TextModels.Model DO
				GetSourcePos(ThisModS(name), adr, a, beg);
				IF beg >= 0 THEN
					IF beg > m.Length() THEN beg := m.Length() - 10 END;
					s.ConnectTo(m); s.SetPos(beg);
					Scan(s); beg := s.start; end := beg + 3;
					IF s.type = stop THEN end := s.Pos() - 1
					ELSE
						WHILE (s.type # TextMappers.eot) & (s.type # stop) & (s.type # semicolon) DO
							end := s.Pos() - 1; Scan(s)
						END
					END;
					c := v(TextViews.View).ThisController();
					v(TextViews.View).ShowRange(beg, end, TextViews.any);
					c(TextControllers.Controller).SetSelection(beg, end);
				END
			END
		ELSIF ~auto THEN
			Dialog.ShowParamMsg("#Dev:SourcefileNotFound", n, "", "")
		END
	END ShowSourcePos;
			
	(* -------------------  RefView ------------------- *)
	
	PROCEDURE (v: RefView) Internalize (VAR rd: Stores.Reader);
		VAR s: Stores.Store; thisVersion: INTEGER;
	BEGIN
		v.Internalize^(rd); IF rd.cancelled THEN RETURN END;
		rd.ReadVersion(0, 0, thisVersion); IF rd.cancelled THEN RETURN END;
		v.command := open;
		rd.ReadSInt(v.type);
		IF v.type = source THEN
			rd.ReadInt(v.adr);
			rd.ReadSString(v.name)
		ELSIF v.type = module THEN
			rd.ReadSString(v.name)
		ELSIF v.type # modules THEN
			v.type := 0
		END
	END Internalize;

	PROCEDURE (v: RefView) Externalize (VAR wr: Stores.Writer);
		VAR t: SHORTINT;
	BEGIN
		v.Externalize^(wr);
		wr.WriteVersion(0);
		t := v.type;
		IF v.command # open THEN t := 0 END;
		wr.WriteSInt(t);
		IF t = source THEN
			wr.WriteInt(v.adr);
			wr.WriteSString(v.name)
		ELSIF t = module THEN
			wr.WriteSString(v.name)
		END
	END Externalize;

	PROCEDURE (v: RefView) CopyFromSimpleView (source: Views.View);
	BEGIN
		(* v.CopyFrom^(source); *)
		WITH source: RefView DO
			v.type := source.type; v.command := source.command; v.adr := source.adr; v.back := source.back;
			v.desc := source.desc; v.ptr := source.ptr; v.name := source.name$;
		END
	END CopyFromSimpleView;

	PROCEDURE (v: RefView) Restore (f: Views.Frame; l, t, r, b: INTEGER);
	BEGIN
		f.DrawPath(path, 4, Ports.fill, 008000H, Ports.closedPoly)
	END Restore;
	
	PROCEDURE (v: RefView) GetBackground (VAR color: Ports.Color);
	BEGIN
		color := Ports.background
	END GetBackground;

	PROCEDURE (v: RefView) HandleCtrlMsg (f: Views.Frame; VAR msg: Controllers.Message; VAR focus: Views.View);
		VAR t, t0: TextModels.Model; m: Models.Model; x, y: INTEGER;
			isDown, new: BOOLEAN; mo: SET; script: Stores.Operation;
	BEGIN
		WITH msg: Controllers.TrackMsg DO
			IF v.type > 0 THEN
				REPEAT
					f.MarkRect(0, 0, refViewSize, refViewSize, Ports.fill, Ports.hilite, Ports.show);
					IF v.command = undo THEN Dialog.ShowStatus("#Dev:ShowPrecedingObject")
					ELSIF v.command = update THEN Dialog.ShowStatus("#Dev:UpdateWindow")
					ELSIF v.type = module THEN Dialog.ShowStatus("#Dev:ShowGlobalVariables")
					ELSIF v.type = source THEN Dialog.ShowStatus("#Dev:ShowSourcePosition")
					ELSIF v.type = heap THEN Dialog.ShowStatus("#Dev:ShowReferencedObject")
					END;
					REPEAT
						f.Input(x, y, mo, isDown)
					UNTIL (x < 0) OR (x > refViewSize) OR (y < 0) OR (y > refViewSize) OR ~isDown;
					f.MarkRect(0, 0, refViewSize, refViewSize, Ports.fill, Ports.hilite, Ports.hide);
					Dialog.ShowStatus("");
					WHILE isDown & ((x < 0) OR (x > refViewSize) OR (y < 0) OR (y > refViewSize)) DO
						f.Input(x, y, mo, isDown)
					END
				UNTIL ~isDown;
				IF (x >= 0) & (x <= refViewSize) & (y >= 0) & (y <= refViewSize) THEN
					IF v.type = source THEN ShowSourcePos(v.name, v.adr, FALSE)
					ELSIF Valid() THEN
						Suspend;
						m := v.context.ThisModel();
						new := (v.command = open) & (v.back = NIL)
							OR (Controllers.modify IN msg.modifiers) & (v.command # update)
							OR ~(m IS TextModels.Model) ;
						IF new THEN
							t := TextModels.CloneOf(StdLog.buf); t0 := NIL
						ELSE
							t0 := m(TextModels.Model); t := TextModels.CloneOf(t0);
						END;
						out.ConnectTo(t);
						IF v.type = heap THEN  ShowPtrDeref(v)
						ELSIF v.type = module THEN ShowGlobals(ThisModS(v.name))
						ELSIF v.type = modules THEN ShowModules
						END;
						out.ConnectTo(NIL);
						IF new THEN
							OpenViewer(t, "#Dev:RemoteVariables", NewRuler())
						ELSE
							Models.BeginScript(t0, "#Dev:Change", script);
							t0.Delete(0, t0.Length()); t0.Insert(0, t, 0, t.Length());
							Models.EndScript(t0, script)
						END;
						Resume
					END
				END
			END
		| msg: Controllers.PollCursorMsg DO
			msg.cursor := Ports.refCursor
		ELSE
		END
	END HandleCtrlMsg;
	
	PROCEDURE (v: RefView) HandlePropMsg (VAR msg: Properties.Message);
	BEGIN
		WITH msg: Properties.Preference DO
			WITH msg: Properties.ResizePref DO msg.fixed := TRUE
			| msg: Properties.SizePref DO msg.w := refViewSize; msg.h := refViewSize
			| msg: Properties.FocusPref DO msg.hotFocus := TRUE
			ELSE
			END
		ELSE
		END
	END HandlePropMsg;
	
	PROCEDURE NewRefView (type, command: SHORTINT; adr: INTEGER; back: RefView;
												desc: Type; ptr: ArrayPtr; name: Name): RefView;
		VAR v: RefView;
	BEGIN
		NEW(v); v.type := type; v.command := command; v.adr := adr; v.back := back;
		v.desc := desc; v.ptr := ptr; v.name := name$;
		RETURN v
	END NewRefView;

	PROCEDURE InsertRefView (type, command: SHORTINT; adr: INTEGER; back: RefView;
												desc: Type; ptr: ArrayPtr; name: Name);
		VAR v: RefView; a0: TextModels.Attributes;
	BEGIN
		v := NewRefView(type, command, adr, back, desc, ptr, name);
		a0 := out.rider.attr;
		out.rider.SetAttr(TextModels.NewOffset(a0, Ports.point));
		out.WriteView(v);
		out.rider.SetAttr(a0)
	END InsertRefView;
	
	(* ----------------------------------------- *)

	PROCEDURE GetMod (VAR mod: Module);
		VAR c: TextControllers.Controller; s: TextMappers.Scanner; beg, end: INTEGER;
	BEGIN
		mod := 0;
		c := TextControllers.Focus();
		IF (c # NIL) & c.HasSelection() THEN
			c.GetSelection(beg, end);
			s.ConnectTo(c.text); s.SetPos(beg); s.Scan;
			IF s.type = TextMappers.string THEN
				mod := ThisMod(s.string);
				IF mod = 0 THEN
					Dialog.ShowParamMsg("#Dev:ModuleNotFound", s.string, "", "")
				END				
			ELSE Dialog.ShowMsg("#Dev:NoModuleNameSelected")
			END
		ELSE Dialog.ShowMsg("#Dev:NoSelectionFound")
		END
	END GetMod;

	PROCEDURE ShowStackFrame (first: BOOLEAN);
		VAR ref, end, i, j, x: INTEGER; m, f: SHORTCHAR; mod: Module; name, sel, mname: Name; d: Type;
	BEGIN
		out.ConnectTo(stateText); out.SetPos(stateText.Length());
		mod := ModList();
		WHILE (mod # 0) & ((framePc < ModCode(mod)) OR (framePc >= ModCode(mod) + ModCSize(mod))) DO
			mod := ModNext(mod)
		END;
		IF mod # 0 THEN
			DEC(framePc, ModCode(mod));
			GetName(mod + 100, mname);	(* mod.name *)
			IF ModCnt(mod) >= 0 THEN
				InsertRefView(module, open, 0, NIL, 0, 0, mname);
				out.WriteChar(" "); out.WriteSString(mname); GetInt(mod + 72, ref);	(* mod.refs *)
				REPEAT GetRefProc(ref, end, name) UNTIL (end = 0) OR (framePc < end);
				IF framePc < end THEN
					out.WriteChar("."); out.WriteSString(name);
					sel := mname$; i := 0;
					WHILE sel[i] # 0X DO INC(i) END;
					sel[i] := "."; INC(i); j := 0;
					WHILE name[j] # 0X DO sel[i] := name[j]; INC(i); INC(j) END;
					sel[i] := ":"; sel[i+1] := 0X;
					out.WriteSString("   ["); WriteHex(framePc);
					out.WriteSString("] ");
					GetSourcePos(mod, 0, j, i);
					IF i >= 0 THEN
						InsertRefView(source, open, framePc, NIL, 0, 0, mname);
						IF first THEN ShowSourcePos(mname, framePc, TRUE) END
					END;
					IF showStack & (name # "$$") THEN
						GetRefVar(ref, m, f, d, x, name);
						WHILE m # 0X DO
							IF name[0] # "@" THEN ShowVar(frameFp + x, 0, f, m, d, 0, NIL, name, sel) END;
							GetRefVar(ref, m, f, d, x, name);
						END
					END
				ELSE out.WriteSString(".???")
				END;
				out.WriteLn
			ELSE
				out.WriteChar("("); out.WriteSString(mname);
				out.WriteSString(")   (pc="); WriteHex(framePc);
				out.WriteSString(",  fp="); WriteHex(frameFp); out.WriteChar(")");
				out.WriteLn
			END
		ELSE
			out.WriteSString("<system>   (pc="); WriteHex(framePc);
			out.WriteSString(",  fp="); WriteHex(frameFp); out.WriteChar(")");
			out.WriteLn
		END;
		IF (frameFp >= fp) & (frameFp < stack) THEN
			GetInt(frameFp+4, framePc);	(* stacked pc *)
			GetInt(frameFp, frameFp);	(* dynamic link *)
			DEC(framePc); DEC(frameCnt)
		ELSE frameCnt := 0
		END;
		out.ConnectTo(NIL)
	END ShowStackFrame;
	
	PROCEDURE Trap;
		VAR a0: TextModels.Attributes; prop: Properties.StdProp; t: TextModels.Model; script: Stores.Operation;
	BEGIN
		cacheLine := 0;
		t := TextModels.CloneOf(stateText);
		out.ConnectTo(t);
		a0 := out.rider.attr;
		out.rider.SetAttr(TextModels.NewWeight(a0, Fonts.bold));
		IF err = 129 THEN out.WriteSString("invalid with")
		ELSIF err = 130 THEN out.WriteSString("invalid case")
		ELSIF err = 131 THEN out.WriteSString("function without return")
		ELSIF err = 132 THEN out.WriteSString("type guard")
		ELSIF err = 133 THEN out.WriteSString("implied type guard")
		ELSIF err = 134 THEN out.WriteSString("value out of range")
		ELSIF err = 135 THEN out.WriteSString("index out of range")
		ELSIF err = 136 THEN out.WriteSString("incompatible copy")
		ELSIF err = 137 THEN out.WriteSString("stack overflow")
		ELSIF err = 138 THEN out.WriteSString("integer overflow")
		ELSIF err = 139 THEN out.WriteSString("division by zero")
		ELSIF err = 140 THEN out.WriteSString("infinite real result")
		ELSIF err = 141 THEN out.WriteSString("real underflow")
		ELSIF err = 142 THEN out.WriteSString("real overflow")
		ELSIF err = 143 THEN out.WriteSString("undefined real result")
		ELSIF err = 144 THEN out.WriteSString("not a number")
		ELSIF err = 200 THEN
			IF val = 0 THEN out.WriteSString("stopped")
			ELSIF val > 0 THEN out.WriteSString("breakpoint "); out.WriteInt(val)
			ELSE out.WriteSString("data breakpoint "); out.WriteInt(-val)
			END
		ELSIF err = 201 THEN out.WriteSString("NIL dereference")
		ELSIF err = 202 THEN
			out.WriteSString("illegal instruction: ");
			out.WriteIntForm(val, TextMappers.hexadecimal, 5, "0", TextMappers.showBase)
		ELSIF err = 203 THEN
			IF (val >= -4) & (val < 65536) THEN out.WriteSString("NIL dereference (read)")
			ELSE out.WriteSString("illegal memory read (ad = "); WriteHex(val); out.WriteChar(")")
			END
		ELSIF err = 204 THEN
			IF (val >= -4) & (val < 65536) THEN out.WriteSString("NIL dereference (write)")
			ELSE out.WriteSString("illegal memory write (ad = "); WriteHex(val); out.WriteChar(")")
			END
		ELSIF err = 205 THEN
			IF (val >= -4) & (val < 65536) THEN out.WriteSString("NIL procedure call")
			ELSE out.WriteSString("illegal execution (ad = "); WriteHex(val); out.WriteChar(")")
			END
		ELSIF err = 207 THEN out.WriteSString("int 3")
		ELSIF err = 257 THEN out.WriteSString("out of memory")
		ELSIF err = 10001H THEN out.WriteSString("bus error")
		ELSIF err = 10002H THEN out.WriteSString("address error")
		ELSIF err = 10007H THEN out.WriteSString("fpu error")
		ELSIF err < 0 THEN
			out.WriteSString("Exception "); out.WriteIntForm(-err, TextMappers.hexadecimal, 3, "0", TextMappers.showBase)
		ELSE
			out.WriteSString("TRAP "); out.WriteInt(err);
			IF err = 127 THEN out.WriteSString("  (abstract method called)")
			ELSIF err = 126 THEN out.WriteSString("  (not yet implemented)")
			ELSIF err = 125 THEN out.WriteSString("  (call of obsolete procedure)")
			ELSIF err >= 100 THEN out.WriteSString("  (invariant violated)")
			ELSIF err >= 60 THEN out.WriteSString("  (postcondition violated)")
			ELSIF err >= 20 THEN out.WriteSString("  (precondition violated)")
			END
		END;
		out.WriteLn; out.rider.SetAttr(a0);
		out.WriteLn;
		framePc := pc; frameFp := fp; frameCnt := stackFrames;
		Models.BeginScript(stateText, "#Dev:Change", script);
		stateText.Delete(0, stateText.Length());
		Models.BeginModification(Models.notUndoable, stateText);	(* remove undo buffers *)
		Models.EndModification(Models.notUndoable, stateText);
		stateText.Insert(0, t, 0, t.Length());
		out.ConnectTo(NIL);
		ShowStackFrame(TRUE);
		Models.EndScript(stateText, script);
		Windows.dir.Update(NIL)
	END Trap;
	
	PROCEDURE HandleException (VAR event: WinApi.DEBUG_EVENT);
		VAR res: INTEGER; context: WinApi.CONTEXT; s: SET;
	BEGIN
		IF event.u.Exception.dwFirstChance # 0 THEN END;
		context.ContextFlags := WinApi.CONTEXT_CONTROL + WinApi.CONTEXT_DEBUG_REGISTERS;
		res := WinApi.GetThreadContext(threadHandle, context); ASSERT(res # 0);
		pc := context.Eip;
		sp := context.Esp;
		fp := context.Ebp;
		IF kernelAdr # 0 THEN stack := Stack() ELSE stack := 0 END;
		err := -(event.u.Exception.ExceptionRecord.ExceptionCode MOD 256);
(*
StdLog.Int(-err); StdLog.Char(" ");
StdLog.IntForm(event.u.Exception.ExceptionRecord.ExceptionAddress, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
StdLog.IntForm(pc, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
StdLog.IntForm(sp, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
StdLog.IntForm(fp, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
StdLog.IntForm(stack, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Ln;
*)
		IF err = -3 THEN err := 207;	(* breakpoint *)
		ELSIF err = -4 THEN err := 200;	(* keyboard interrupt *)
			s := SYSTEM.VAL(SET, context.Dr6);
			IF 0 IN s THEN val := 1
			ELSIF 1 IN s THEN val := 2
			ELSIF 2 IN s THEN val := 3
			ELSE val := 0
			END;
			IF (val > 0) & (breakpoints[val-1].size # 0) THEN val := -val; DEC(pc) END	(* watchpoint *)
		ELSIF err = -5 THEN
			val := event.u.Exception.ExceptionRecord.ExceptionInformation[1];
			IF val = pc THEN	(* call to undef adr *)
				err := 205; GetInt(sp, pc); INC(sp, 4); DEC(pc)
			ELSIF event.u.Exception.ExceptionRecord.ExceptionInformation[0] = 0 THEN	(* illegal read *)
				err := 203
			ELSE	(* illegal write *)
				err := 204
			END
		ELSIF (err = -29) OR (err = -30) THEN	(* illegal instruction *)
			err := 202; val := 0;
			GetInt(event.u.Exception.ExceptionRecord.ExceptionAddress, val);
			IF val MOD 100H = 8DH THEN	(* lea reg,reg *)
				IF val DIV 100H MOD 100H = 0F0H THEN	(* trap *)
					err := val DIV 10000H MOD 100H
				ELSIF val DIV 1000H MOD 10H = 0EH THEN	(* run time error *)
					err := 128 + val DIV 100H MOD 10H
				END
			END
		ELSIF err = -142 THEN DEC(pc); err := 140	(* fpu: div by zero *)
		ELSIF err = -144 THEN DEC(pc); err := 143	(* fpu: invalid op *)
		ELSIF err = -145 THEN DEC(pc); err := 142	(* fpu: overflow *)
		ELSIF err = -147 THEN DEC(pc); err := 141	(* fpu: underflow *)
		ELSIF err = -148 THEN err := 139	(* division by zero *)
		ELSIF err = -149 THEN err := 138	(* integer overflow *)
		ELSIF (err = -1) OR (err = -253) THEN err := 137	(* stack overflow *)
		END;
		res := WinApi.SetForegroundWindow(HostWindows.main);
		IF res = 0 THEN res := WinApi.SetActiveWindow(HostWindows.main) END;	(* win32s *)			
		Trap
	END HandleException;
	
	PROCEDURE SetRunning;
	BEGIN
		stateText.Delete(0, stateText.Length());
		out.ConnectTo(stateText);
		out.WriteSString("running"); out.WriteLn;
		out.ConnectTo(NIL);
		Windows.dir.Update(NIL);
		err := running;
		frameCnt := 0
	END SetRunning;
	
	PROCEDURE SetBP (VAR bp: BrkPt; n: INTEGER; VAR context: WinApi.CONTEXT);
		VAR ctrl: SET;
	BEGIN
		ctrl := SYSTEM.VAL(SET, context.Dr7);
		IF bp.adr # 0 THEN
			IF n = 0 THEN context.Dr0 := bp.adr
			ELSIF n = 1 THEN context.Dr1 := bp.adr
			ELSE context.Dr2 := bp.adr
			END;
			INCL(ctrl, 2 * n);
			IF bp.size # 0 THEN INCL(ctrl, 16 + 4 * n); INCL(ctrl, 8) END;
			IF bp.size > 0 THEN INCL(ctrl, 17 + 4 * n) END;
			IF ABS(bp.size) > 1 THEN INCL(ctrl, 18 + 4 * n) END;
			IF ABS(bp.size) > 2 THEN INCL(ctrl, 19 + 4 * n) END;
		ELSE EXCL(ctrl, 2 * n)
		END;
		context.Dr7 := SYSTEM.VAL(INTEGER, ctrl)
	END SetBP;
	
	PROCEDURE ContinueExec (err, adr: INTEGER);	(* adr = -1: step; adr = 0: continue; adr > 0: goto *)
		VAR res, bp: INTEGER; context: WinApi.CONTEXT; b0, b1: SHORTCHAR;
	BEGIN
		context.ContextFlags := WinApi.CONTEXT_CONTROL + WinApi.CONTEXT_DEBUG_REGISTERS;
		res := WinApi.GetThreadContext(threadHandle, context); ASSERT(res # 0);
		IF (err >= 0) & (err <= 127) THEN INC(context.Eip, 3) END;	(* correct trap pc *)
		EXCL(SYSTEM.VAL(SET, context.EFlags), 8);	(* reset trace flag *)
		context.Dr6 := 0;	(* clear debug status register *)
		context.Dr7 := 0;	(* clear debug control register *)
		skipMode := -1;
		IF breakpoints[0].adr = context.Eip THEN skipMode := adr ELSE SetBP(breakpoints[0], 0, context) END;
		IF breakpoints[1].adr = context.Eip THEN skipMode := adr ELSE SetBP(breakpoints[1], 1, context) END;
		IF breakpoints[2].adr = context.Eip THEN skipMode := adr ELSE SetBP(breakpoints[2], 2, context) END;
		IF adr > 0 THEN
			context.Dr3 := adr; INCL(SYSTEM.VAL(SET, context.Dr7), 6)	(* set breakpoint 3 *)
		ELSIF adr = -1 THEN
			GetByte(context.Eip, b0); GetByte(context.Eip + 1, b1);
			IF skipCalls & ((b0 = 0E8X) OR (b0 = 0FFX) & (ORD(b1) DIV 8 MOD 8 = 2)) THEN	(* place breakpoint after call *)
				IF b0 = 0E8X THEN	(* call direct *)
					bp := context.Eip + 5
				ELSIF ORD(b1) DIV 64 = 0 THEN	(* call indirect (no offset) *)
					IF b1 = 14X THEN	(* s-i-b byte present *)
						GetByte(context.Eip + 2, b1);
						IF ORD(b1) MOD 8 = 5 THEN bp := context.Eip + 7	(* absolute *)
						ELSE bp := context.Eip + 3
						END
					ELSIF b1 = 15X THEN bp := context.Eip + 6	(* absolute *)
					ELSE bp := context.Eip + 2
					END
				ELSIF ORD(b1) DIV 64 = 1 THEN	(* call indirect (8 bit offset) *)
					IF b1 = 54X THEN bp := context.Eip + 4	(* s-i-b byte present *)
					ELSE bp := context.Eip + 3
					END
				ELSIF ORD(b1) DIV 64 = 2 THEN	(* call indirect (32 bit offset) *)
					IF b1 = 94X THEN bp := context.Eip + 7	(* s-i-b byte present *)
					ELSE bp := context.Eip + 6
					END
				ELSE	(* call indirect (register) *)
					bp := context.Eip + 2
				END;
				context.Dr3 := bp; INCL(SYSTEM.VAL(SET, context.Dr7), 6)	(* set breakpoint 3 *)
			ELSE
				INCL(SYSTEM.VAL(SET, context.EFlags), 8)	(* set trace flag *)
			END
		END;
		IF skipMode # -1 THEN INCL(SYSTEM.VAL(SET, context.EFlags), 8) END;	(* set trace flag *)
(*
		INCL(SYSTEM.VAL(SET, context.EFlags), 16);	(* set resume flag (supresses a breakpoint at start address) *)
*)
		res := WinApi.SetThreadContext(threadHandle, context); ASSERT(res # 0);
(*
StdLog.IntForm(context.Dr0, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
StdLog.IntForm(context.Dr7, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
res := WinApi.GetThreadContext(threadHandle, context); ASSERT(res # 0);
StdLog.IntForm(context.Dr0, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
StdLog.IntForm(context.Dr7, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
StdLog.IntForm(context.EFlags, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Ln;
*)
		res := WinApi.ContinueDebugEvent(procId, threadId, WinApi.DBG_CONTINUE)
	END ContinueExec;
	
	PROCEDURE IsBadSourcePos (adr: INTEGER): BOOLEAN;
		VAR ad, p: INTEGER; mod: Module;
	BEGIN
		IF (modCache # 0) & (ModCnt(modCache) >= 0)
				& (adr >= ModCode(modCache)) & (adr < ModCode(modCache) + ModCSize(modCache)) THEN
			mod := modCache
		ELSE
			mod := ModList();
			WHILE (mod # 0) & ((adr < ModCode(mod)) OR (adr >= ModCode(mod) + ModCSize(mod))) DO
				mod := ModNext(mod)
			END
		END;
		IF mod # 0 THEN
			modCache := mod;
			DEC(adr, ModCode(mod));
			GetSourcePos(mod, adr - 1, ad, p);
			RETURN ad > adr
		ELSE
			RETURN FALSE
		END
	END IsBadSourcePos;
	
	PROCEDURE HandleEvent (VAR event: WinApi.DEBUG_EVENT);
		VAR res, ex, adr: INTEGER; string: Name; context: WinApi.CONTEXT; str: ARRAY 256 OF CHAR;
	BEGIN
		IF event.dwDebugEventCode = WinApi.EXCEPTION_DEBUG_EVENT THEN
			IF (threadId = event.dwThreadId) & (procId = event.dwProcessId) THEN
				ex := event.u.Exception.ExceptionRecord.ExceptionCode MOD 256;
				adr := event.u.Exception.ExceptionRecord.ExceptionAddress;
IF logOutput THEN
	StdLog.Int(ex); StdLog.Char(" ");
	StdLog.IntForm(adr, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
	context.ContextFlags := WinApi.CONTEXT_DEBUG_REGISTERS + WinApi.CONTEXT_CONTROL;
	res := WinApi.GetThreadContext(threadHandle, context); ASSERT(res # 0);
	StdLog.IntForm(context.Dr6, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
	StdLog.IntForm(context.Dr7, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
	StdLog.IntForm(context.EFlags, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Ln;
END;
(*
				IF breakpoint # 0 THEN	(* remove breakpoint *)
					PutByte(breakpoint, oldCode); breakpoint := 0
				END;
*)
				IF (ex = 3) (* & (kernelAdr = 0) *) THEN	(* breakpoint trap *)
					res := WinApi.ContinueDebugEvent(event.dwProcessId, event.dwThreadId, WinApi.DBG_CONTINUE)
				ELSIF ex = 4 THEN	(* trace trap *)
					context.ContextFlags := WinApi.CONTEXT_DEBUG_REGISTERS;
					res := WinApi.GetThreadContext(threadHandle, context); ASSERT(res # 0);
					IF (context.Dr6 MOD 8 = 0) & ((skipMode # -1) OR IsBadSourcePos(adr)) THEN
						ContinueExec(200, skipMode)
					ELSE
						HandleException(event)
					END
				ELSE
					HandleException(event)
				END
			ELSE
				res := WinApi.ContinueDebugEvent(event.dwProcessId, event.dwThreadId,
																WinApi.DBG_EXCEPTION_NOT_HANDLED)
			END
		ELSE
			IF event.dwDebugEventCode = WinApi.CREATE_PROCESS_DEBUG_EVENT THEN
IF logOutput THEN
	StdLog.String("create process event ");
	StdLog.Int(event.u.CreateProcessInfo.hProcess);
	StdLog.Int(event.u.CreateProcessInfo.hThread);
END;
				IF event.dwProcessId = procId THEN procHandle := event.u.CreateProcessInfo.hProcess END;
				IF event.dwThreadId = threadId THEN threadHandle := event.u.CreateProcessInfo.hThread END;
				context.ContextFlags := WinApi.CONTEXT_DEBUG_REGISTERS;
				res := WinApi.GetThreadContext(threadHandle, context); ASSERT(res # 0);
				context.Dr6 := 0; context.Dr7 := 0;
				res := WinApi.SetThreadContext(threadHandle, context); ASSERT(res # 0)
			ELSIF event.dwDebugEventCode = WinApi.CREATE_THREAD_DEBUG_EVENT THEN
IF logOutput THEN
	StdLog.String("create thread event ");
	StdLog.Int(event.u.CreateThread.hThread);
END;
				IF event.dwThreadId = threadId THEN threadHandle := event.u.CreateThread.hThread END;
				context.ContextFlags := WinApi.CONTEXT_DEBUG_REGISTERS;
				res := WinApi.GetThreadContext(threadHandle, context); ASSERT(res # 0);
				context.Dr6 := 0; context.Dr7 := 0;
				res := WinApi.SetThreadContext(threadHandle, context); ASSERT(res # 0)
			ELSIF event.dwDebugEventCode = WinApi.EXCEPTION_DEBUG_EVENT THEN
IF logOutput THEN StdLog.String("exception event ") END
			ELSIF event.dwDebugEventCode = WinApi.EXIT_PROCESS_DEBUG_EVENT THEN
IF logOutput THEN StdLog.String("exit process event ") END
			ELSIF event.dwDebugEventCode = WinApi.EXIT_THREAD_DEBUG_EVENT THEN
IF logOutput THEN StdLog.String("exit thread event ") END
			ELSIF event.dwDebugEventCode = WinApi.LOAD_DLL_DEBUG_EVENT THEN
IF logOutput THEN StdLog.String("load dll event ") END
			ELSIF event.dwDebugEventCode = WinApi.UNLOAD_DLL_DEBUG_EVENT THEN
IF logOutput THEN StdLog.String("unload dll event ") END
			ELSIF event.dwDebugEventCode = WinApi.OUTPUT_DEBUG_STRING_EVENT THEN
IF logOutput THEN StdLog.String("debug string: ") END;
				IF (event.u.DebugString.fUnicode = 0) & (event.dwProcessId = procId) THEN
					GetName(SYSTEM.VAL(INTEGER, event.u.DebugString.lpDebugStringData), string);
IF logOutput THEN str := string$; StdLog.String(str) END;
					IF string = "BlackBox started" THEN
						context.ContextFlags := WinApi.CONTEXT_INTEGER;
						res := WinApi.GetThreadContext(threadHandle, context); ASSERT(res # 0);
IF logOutput THEN StdLog.Char(" "); StdLog.IntForm(context.Ebx, StdLog.hexadecimal, 8, "0", FALSE) END;
						kernelAdr := context.Ebx;
IF logOutput THEN StdLog.Char(" "); StdLog.IntForm(Stack(), StdLog.hexadecimal, 8, "0", FALSE) END
					END
				END
			ELSE StdLog.String("unknown event ")
			END;
IF logOutput THEN			
	StdLog.Int(event.dwProcessId); StdLog.Int(event.dwThreadId); 
	StdLog.Ln;
END;
			res := WinApi.ContinueDebugEvent(event.dwProcessId, event.dwThreadId, WinApi.DBG_CONTINUE)
		END
	END HandleEvent;
	

	(* ---------- Action ---------- *)
	
	PROCEDURE (a: Action) Do;
		VAR res: INTEGER; event: WinApi.DEBUG_EVENT;
	BEGIN
		IF Valid() THEN
			res := WinApi.WaitForDebugEvent(event, 0);
			IF res # 0 THEN HandleEvent(event) END;
			IF frameCnt > 0 THEN ShowStackFrame(FALSE) END;
			Services.DoLater(a, 200)
		ELSE stateText := NIL
		END
	END Do;
	
	
	(* ---------- DAction ---------- *)
	
	PROCEDURE (a: DAction) Do;
	BEGIN
		HostMenus.disablePipe := FALSE
	END Do;
	
	
	(* ---------- breakpoints ---------- *)
	
	PROCEDURE GetBreakPos (VAR adr, size: INTEGER);
		VAR c: TextControllers.Controller; s: TextMappers.Scanner; beg, end, ref, x, data: INTEGER;
			m, f: SHORTCHAR; name: Name; d: Type; mod: Module;
	BEGIN
		adr := 0; size := 0;
		c := TextControllers.Focus();
		IF c # NIL THEN
			IF c.HasSelection() THEN
				c.GetSelection(beg, end);
				s.ConnectTo(c.text); s.SetOpts(s.opts - {TextMappers.returnQualIdents});
				s.SetPos(beg); s.Scan;
				IF s.type = TextMappers.string THEN
					mod := ThisMod(s.string);
					IF mod # 0 THEN
						s.Scan;
						IF (s.type = TextMappers.char) & (s.char = ".") THEN
							s.Scan;
							IF s.type = TextMappers.string THEN
								GetInt(mod + 72, ref); GetRefProc(ref, x, name);	(* get body *)
								IF x # 0 THEN
									REPEAT
										GetRefVar(ref, m, f, d, x, name)
									UNTIL (m # 1X) OR (name = s.string);
									IF m = 1X THEN
										IF (f = 1X) OR (f = 2X) OR (f = 4X) THEN size := 1
										ELSIF (f = 3X) OR (f = 5X) THEN size := 2
										ELSIF (f # 0FX) & (f # 11X) & (f # 12X) THEN size := 4
										END;
										IF size > 0 THEN
											GetInt(mod + 68, data);	(* mod.data *)
											adr := data + x;
											s.Scan;
											IF (s.type = TextMappers.char) & (s.char = "-") THEN size := -size END
										ELSE Dialog.ShowParamMsg("#Dev:NoBasicType", s.string, "", "")
										END
									ELSE Dialog.ShowParamMsg("#Dev:NoVariable", s.string, "", "")
									END
								ELSE Dialog.ShowParamMsg("#Dev:NoVariable", s.string, "", "")
								END
							ELSE Dialog.ShowMsg("#Dev:NoVarName")
							END
						ELSE Dialog.ShowMsg("#Dev:NoQualident")
						END
					ELSE Dialog.ShowParamMsg("#Dev:NoModule", s.string, "", "")
					END
				ELSE Dialog.ShowMsg("#Dev:NoModuleName")
				END
			ELSIF c.HasCaret() THEN
				beg := c.CaretPos();
				s.ConnectTo(c.text); s.SetPos(0); s.Scan;
				WHILE ~s.rider.eot & ((s.type # TextMappers.string) OR (s.string # "MODULE")) DO s.Scan END;
				s.Scan; mod := 0;
				IF s.type = TextMappers.string THEN
					mod := ThisMod(s.string);
					IF mod # 0 THEN
						GetCodePos(mod, beg, adr);
						IF adr # 0 THEN
							INC(adr, ModCode(mod))
						ELSE Dialog.ShowParamMsg("#Dev:NoSourcePosInfoIn", s.string, "", "")
						END
					ELSE Dialog.ShowParamMsg("#Dev:NoModule", s.string, "", "")
					END
				ELSE Dialog.ShowMsg("#Dev:NoModuleName")
				END
			ELSE Dialog.ShowMsg("#Dev:NoSelectionOrCaret")
			END
		ELSE Dialog.ShowMsg("#Dev:NoText")
		END
	END GetBreakPos;
	
	PROCEDURE Breakpoint* (n: INTEGER);
		VAR adr, size, res: INTEGER; context: WinApi.CONTEXT;
	BEGIN
		ASSERT((n >= 0) & (n < 3), 20);
		IF Valid() THEN
			IF err = running THEN
				Suspend;
				context.ContextFlags := WinApi.CONTEXT_DEBUG_REGISTERS;
				res := WinApi.GetThreadContext(threadHandle, context)
			END;
			IF breakpoints[n].adr = 0 THEN
				GetBreakPos(adr, size);
				IF adr # 0 THEN
					breakpoints[n].adr := adr;
					breakpoints[n].size := size
				END
			ELSE
				breakpoints[n].adr := 0;
				breakpoints[n].size := 0
			END;
			IF err = running THEN
				SetBP(breakpoints[n], n, context);
				res := WinApi.SetThreadContext(threadHandle, context);
				Resume
			END
		END
	END Breakpoint;
	
	PROCEDURE BreakpointGuard* (n: INTEGER; VAR par: Dialog.Par);
	BEGIN
		ASSERT((n >= 0) & (n < 3), 20);
		IF ~Valid() THEN par.disabled := TRUE END;
		IF breakpoints[n].adr # 0 THEN par.checked := TRUE END
	END BreakpointGuard;
	

	(* ---------- commands ---------- *)
	
	PROCEDURE ShowLoadedModules*;
	BEGIN
		IF Valid() THEN
			Suspend;
			out.ConnectTo(TextModels.CloneOf(StdLog.buf));
			ShowModules;
			OpenViewer(out.rider.Base(), "#Dev:RemoteModules", NewModRuler());
			out.ConnectTo(NIL);
			Resume
		END
	END ShowLoadedModules;
	
	PROCEDURE UpdateModules*;
		VAR t, t0: TextModels.Model; script: Stores.Operation;
	BEGIN
		IF Valid() THEN
			Suspend;
			t0 := TextViews.FocusText();
			Models.BeginScript(t0, "#Dev:Change", script);
			t := TextModels.CloneOf(t0);
			out.ConnectTo(t);
			ShowModules;
			(*Stores.InitDomain(t, t0.domain);*) Stores.Join(t, t0);	(* not efficient to init domain before writing *)
			t0.Delete(0, t0.Length()); t0.Insert(0, t, 0, t.Length());
			Models.EndScript(t0, script);
			out.ConnectTo(NIL);
			Resume
		END
	END UpdateModules;

	PROCEDURE ShowGlobalVariables*;
		VAR mod: Module;
	BEGIN
		IF Valid() THEN
			Suspend;
			GetMod(mod);
			IF mod # 0 THEN
				out.ConnectTo(TextModels.CloneOf(StdLog.buf));
				ShowGlobals(mod);
				OpenViewer(out.rider.Base(), "#Dev:RemoteVariables", NewRuler());
				out.ConnectTo(NIL)
			END;
			Resume
		END
	END ShowGlobalVariables;
	
	PROCEDURE UpdateGlobals* (name: ARRAY OF SHORTCHAR);
		VAR t, t0: TextModels.Model; script: Stores.Operation; mod: Module;
	BEGIN
		IF Valid() THEN
			Suspend;
			mod := ThisModS(name);
			IF mod # 0 THEN
				t0 := TextViews.FocusText();
				Models.BeginScript(t0, "#Dev:Change", script);
				t := TextModels.CloneOf(t0);
				out.ConnectTo(t);
				ShowGlobals(mod);
				(*Stores.InitDomain(t, t0.domain);*) Stores.Join(t, t0);	(* not efficient to init domain before writing *)
				t0.Delete(0, t0.Length()); t0.Insert(0, t, 0, t.Length());
				Models.EndScript(t0, script);
				out.ConnectTo(NIL)
			END;
			Resume
		END
	END UpdateGlobals;
	
	PROCEDURE Handle*;
		VAR res: INTEGER;
	BEGIN
		IF Valid() & (err # running) & (err # 200) & (err # 207) THEN
			res := WinApi.ContinueDebugEvent(procId, threadId, WinApi.DBG_EXCEPTION_NOT_HANDLED);
			SetRunning
		END
	END Handle;
	
	PROCEDURE Terminate*;
		VAR res: INTEGER;
	BEGIN
		IF Valid() THEN
			res := WinApi.TerminateProcess(procHandle, 1);
			IF res = 0 THEN
				res := WinApi.GetLastError();
				HALT(100)
			END
		END
	END Terminate;
	
	PROCEDURE Continue*;
	BEGIN
		IF Valid() & ((err >= 0) & (err <= 127) OR (err = 200) OR (err = 207)) THEN
			ContinueExec(err, 0);
			SetRunning
		END
	END Continue;
	
	PROCEDURE StepInto*;
	BEGIN
		IF Valid() & ((err >= 0) & (err <= 127) OR (err = 200) OR (err = 207)) THEN
			skipCalls := FALSE;
			ContinueExec(err, -1);
			SetRunning
		END
	END StepInto;
	
	PROCEDURE StepOver*;
	BEGIN
		IF Valid() & ((err >= 0) & (err <= 127) OR (err = 200) OR (err = 207)) THEN
			skipCalls := TRUE;
			ContinueExec(err, -1);
			SetRunning
		END
	END StepOver;
	
	PROCEDURE ContinueTo*;
		VAR adr, size: INTEGER;
	BEGIN
		IF Valid() & ((err >= 0) & (err <= 127) OR (err = 200) OR (err = 207)) THEN
			GetBreakPos(adr, size);
			IF (adr # 0) & (size = 0) THEN
				ContinueExec(err, adr);
				SetRunning
			END
		END
	END ContinueTo;
	
	PROCEDURE Stop*;
		VAR res: INTEGER; context: WinApi.CONTEXT;
	BEGIN
		IF Valid() & (err = running) THEN
			Suspend;
			context.ContextFlags := WinApi.CONTEXT_CONTROL;
			res := WinApi.GetThreadContext(threadHandle, context); ASSERT(res # 0);
IF logOutput THEN
	StdLog.IntForm(context.Eip, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
	StdLog.IntForm(context.Esp, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
	StdLog.IntForm(context.Ebp, StdLog.hexadecimal, 8, "0", FALSE); StdLog.Ln;
END;
			INCL(SYSTEM.VAL(SET, context.EFlags), 8);	(* set trace flag *)
			res := WinApi.SetThreadContext(threadHandle, context); ASSERT(res # 0);
			Resume
		END
	END Stop;
	
	PROCEDURE Open*;
	BEGIN
		OpenViewer(stateText, "#Dev:RemoteState", NewRuler());
	END Open;
	
	PROCEDURE Start* (commandLine: ARRAY OF CHAR);
		VAR res: INTEGER; info: WinApi.STARTUPINFO; action: Action; process: WinApi.PROCESS_INFORMATION;
			cl: ARRAY 256 OF SHORTCHAR; da: DAction;
	BEGIN
		IF ~Valid() THEN
			kernelAdr := 0; modCache := 0;
			WinApi.GetStartupInfo(info);
			HostMenus.disablePipe := TRUE;
			IF commandLine = "" THEN
				res := WinApi.CreateProcess(NIL, WinApi.GetCommandLine(), NIL, NIL, 0,
												WinApi.DEBUG_PROCESS, 0, NIL, info, process);
			ELSE
				cl := SHORT(commandLine$);
				res := WinApi.CreateProcess(NIL, cl, NIL, NIL, 0,
												WinApi.DEBUG_PROCESS, 0, NIL, info, process);
			END;
			IF res # 0 THEN
IF logOutput THEN
	StdLog.String("start "); StdLog.Int(process.hProcess); StdLog.Int(process.hThread);
	StdLog.Int(process.dwProcessId); StdLog.Int(process.dwThreadId); StdLog.Ln;
END;
				procId := process.dwProcessId;
				threadId := process.dwThreadId;
				procHandle := process.hProcess;
				threadHandle := process.hThread;
				stateText := TextModels.dir.New();
				SetRunning;
				Open
			ELSE
				res := WinApi.GetLastError();
				StdLog.Int(res); StdLog.Ln
			END;
			NEW(action);
			Services.DoLater(action, Services.now);
			NEW(da);
			Services.DoLater(da, Services.Ticks() + 3000);
		END
	END Start;
	
	PROCEDURE ContinueGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~Valid() OR (err < 0) OR (err > 127) & (err # 200) & (err # 207) THEN par.disabled := TRUE END
	END ContinueGuard;
	
	PROCEDURE HandleGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~Valid() OR (err = running) OR (err = 200) OR (err = 207) THEN par.disabled := TRUE END
	END HandleGuard;
	
	PROCEDURE ValidGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~Valid() THEN par.disabled := TRUE END
	END ValidGuard;
	
	PROCEDURE StopGuard* (VAR par: Dialog.Par);
	BEGIN
		IF ~Valid() OR (err # running) THEN par.disabled := TRUE END
	END StopGuard;
	
	PROCEDURE StartGuard* (VAR par: Dialog.Par);
	BEGIN
		IF Valid() THEN par.disabled := TRUE END
	END StartGuard;
	
	
	PROCEDURE ShowModList*;
		VAR mod: Module; name: Name; n: ARRAY 256 OF CHAR;
	BEGIN
		cacheLine := 0;
		mod := ModList();
		WHILE mod # 0 DO
			GetName(mod + 100, name);
			n := name$; StdLog.String(n); StdLog.Char(" ");
			StdLog.IntForm(ModCode(mod), StdLog.hexadecimal, 8, "0", FALSE); StdLog.Char(" ");
			StdLog.IntForm(ModCode(mod) + ModCSize(mod), StdLog.hexadecimal, 8, "0", FALSE); StdLog.Ln;
			mod := ModNext(mod);
		END
	END ShowModList;

BEGIN
	empty := "";
	path[0].x := refViewSize DIV 2; path[0].y := 0;
	path[1].x := refViewSize; path[1].y := refViewSize DIV 2;
	path[2].x := refViewSize DIV 2; path[2].y := refViewSize;
	path[3].x := 0; path[3].y := refViewSize DIV 2;
END DevRemDebug.




	PROCEDURE ShowMod*;	(* (!)DevRemDebug.ShowMod *)
	BEGIN
		cacheLine := 0;
		StdLog.Int(ModList()); StdLog.Ln
	END ShowMod;
	
	
	PROCEDURE ShowPos*;
		VAR c: TextControllers.Controller; s: TextMappers.Scanner; beg, end: LONGINT; mod: Module;
			ref, pos, ad, d, num, i, f: LONGINT; ch: CHAR; name, n: Name; ptab, ctab, ftab: ARRAY 1000 OF LONGINT;
	BEGIN
		cacheLine := 0;
		pos := 0;
		c := TextControllers.Focus();
		IF c # NIL THEN
			s.ConnectTo(c.text); s.SetPos(0); s.Scan;
			WHILE ~s.rider.eot & ((s.type # TextMappers.string) OR (s.string # "MODULE")) DO s.Scan END;
			s.Scan;
			IF s.type = TextMappers.string THEN
				mod := ThisMod(s.string);
				IF mod # 0 THEN
					GetInt(mod + 72, ref);	(* mod.refs *)
					pos := 0; ad := 0; num := 0; f := 0; GetByte(ref, ch);
					WHILE ch # 0X DO
						WHILE (ch > 0X) & (ch < 0FCX) DO
							INC(ad, LONG(ORD(ch))); INC(ref); RefNum(ref, d); INC(pos, d);
							IF (d # 0) & (num < LEN(ptab)) THEN
								i := num; INC(num);
								WHILE (i > 0) & (ptab[i-1] > pos) DO
									ptab[i] := ptab[i-1]; ctab[i] := ctab[i-1]; ftab[i] := ftab[i-1]; DEC(i)
								END;
								ptab[i] := pos; ctab[i] := ad; ftab[i] := f
							END;
							GetByte(ref, ch) 
						END;
						IF ch = 0FCX THEN INC(ref); RefNum(ref, d); RefName(ref, name); GetByte(ref, ch) END;
						WHILE ch >= 0FDX DO	(* skip variables *)
							INC(ref); RefCh(ref, ch);
							IF ch = 10X THEN INC(ref, 4) END;
							RefNum(ref, d); RefName(ref, name); GetByte(ref, ch)
						END;
						INC(f)
					END;
					WHILE num > 0 DO
						DEC(num);
						Strings.IntToString(ftab[num], name); Strings.Append(name, ",");
						Strings.IntToString(ctab[num], n); Strings.Append(name, n);
						DevMarkers.Insert(c.text, ptab[num], DevMarkers.dir.NewMsg(name))
					END
				ELSE
					Dialog.ShowParamMsg("#Dev:NoModule", s.string, "", "")
				END
			ELSE
				Dialog.ShowMsg("#Dev:NoModuleName")
			END
		ELSE
			Dialog.ShowMsg("#Dev:NoText")
		END
	END ShowPos;
	
