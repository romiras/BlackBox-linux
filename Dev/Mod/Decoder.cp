MODULE DevDecoder;
(* bh 6 Dec 1999 *)

	IMPORT S := SYSTEM,
		Files, Ports, Views, Dialog, StdDialog,
		DevDecBase, DevCommanders,
		TextModels, TextRulers, TextViews, TextMappers;
	
	CONST
		(* meta interface consts *)
		mConst = 1; mTyp = 2; mVar = 3; mProc = 4; mField = 5;
		mBool = 1; mChar = 2; mLChar = 3; mSInt = 4; mInt = 5; mLInt = 6;
		mReal = 7; mLReal = 8; mSet = 9; mLargeInt = 10; mAnyRec = 11; mAnyPtr = 12; mSysPtr = 13;
		mProctyp = 0; mRecord = 1; mArray = 2; mPointer = 3;
		mInternal = 1; mReadonly = 2; mPrivate = 3; mExported = 4;

		(* fixup types *)
		absolute = 100; relative = 101; copy = 102; table = 103; tableend = 104; deref = 105; halfword = 106;
	
		nlen = 256;
		
		proc386 = 10;
		proc68k = 20;
		procPPC = 30;
		procARM = 40;
		procSH3 = 50;
		OFtag = 6F4F4346H;

	TYPE
		Name = ARRAY nlen OF CHAR;
		StrPtr = POINTER TO ARRAY [untagged] 40 OF SHORTCHAR;	(* untagged array !!! *)
		Struct = POINTER TO RECORD
			link: Struct;
			adr: INTEGER
		END;
		
	VAR
		out: TextMappers.Formatter;
		inp: Files.Reader;
		headsize, metasize, descsize, codesize, datasize, codepos, pc, names: INTEGER;
		strList, strLast: Struct;
		code, data: POINTER TO ARRAY OF BYTE;
		bigEndian: BOOLEAN;
		mode: INTEGER;

	PROCEDURE WriteHex(x, n: INTEGER);
		VAR
	BEGIN
		IF n > 1 THEN WriteHex(x DIV 16, n-1) END;
		x := x MOD 16;
		IF x <= 9 THEN out.WriteChar(CHR(x + ORD("0")))
		ELSE out.WriteChar(CHR(x -10 + ORD("A")))
		END
	END WriteHex;

	PROCEDURE WriteInt (x: INTEGER);
	BEGIN
		IF x < 0 THEN out.WriteChar("-"); WriteInt(-x)
		ELSE
			IF x > 9 THEN WriteInt(x DIV 10) END;
			out.WriteChar(CHR(x MOD 10 + ORD("0")))
		END
	END WriteInt;
	
	PROCEDURE WriteChar (ch: CHAR);
	BEGIN
		IF (ch >= " ") & (ch < 7FX) THEN out.WriteChar(ch)
		ELSE out.WriteChar("·")
		END
	END WriteChar;
	
	PROCEDURE WriteString (s: ARRAY OF CHAR);
		VAR i: INTEGER;
	BEGIN
		i := 0;
		WHILE s[i] # 0X DO WriteChar(s[i]); INC(i) END
	END WriteString;
	
	PROCEDURE Read4 (VAR x: INTEGER);
		VAR b: BYTE; y: INTEGER;
	BEGIN
		inp.ReadByte(b); y := b MOD 256;
		inp.ReadByte(b); y := y + 100H * (b MOD 256);
		inp.ReadByte(b); y := y + 10000H * (b MOD 256);
		inp.ReadByte(b); x := y + 1000000H * b
	END Read4;
	
	PROCEDURE ReadName (VAR name: Name);
		VAR i: INTEGER; b: BYTE;
	BEGIN i := 0;
		REPEAT
			inp.ReadByte(b); name[i] := CHR(b MOD 256); INC(i)
		UNTIL b = 0
	END ReadName;
	
	PROCEDURE RNum (VAR x: INTEGER);
		VAR b: BYTE; s, y: INTEGER;
	BEGIN
		s := 0; y := 0; inp.ReadByte(b);
		WHILE b < 0 DO INC(y, ASH(b + 128, s)); INC(s, 7); inp.ReadByte(b) END;
		x := ASH((b + 64) MOD 128 - 64, s) + y
	END RNum;
	
	PROCEDURE ReadIn;
		VAR fprint, refsize, datasize, lastEnd, procpc, saver, savef, fsize, carea, k: INTEGER;
			linkadr, consize, nofnewmeth, noflocptr, nofmod: INTEGER;
			nofexp, nofdesc, nofcom, nofptr, i, j: INTEGER;
			modname, name, str: Name;
			mods: ARRAY 128 OF Name;
			ch, n0: CHAR; r: REAL; lr: REAL; b: BYTE;

		PROCEDURE Fixup (adr: INTEGER);
			VAR offset, link, linkadr, t, n, x, i, low, hi: INTEGER;
		BEGIN
			WriteHex(adr, 8);
			RNum(link);
			WHILE link # 0 DO
				RNum(offset);
				out.WriteString("  ");
				IF offset >= 0 THEN out.WriteChar("+") END;
				WriteInt(offset);
				out.WriteString(": ");
				IF link < 0 THEN out.WriteChar("d"); WriteHex(-link, 6)
				ELSE out.WriteChar("c"); WriteHex(link, 6)
				END;
				WHILE link # 0 DO
					IF link > 0 THEN
						IF bigEndian THEN
							n := code[link+3] MOD 256 + (code[link+2] MOD 256) * 256 + code[link+1] * 65536;
							t := code[link]; linkadr := 0F2000000H + link
						ELSE
							n := code[link] MOD 256 + (code[link+1] MOD 256) * 256 + code[link+2] * 65536;
							t := code[link+3]; linkadr := 0F2000000H + link
						END
					ELSE
						IF bigEndian THEN
							n := data[-link+3] MOD 256 + (data[-link+2] MOD 256) * 256 + data[-link+1] * 65536;
							t := data[-link]; linkadr := 0F1000000H - link
						ELSE
							n := data[-link] MOD 256 + (data[-link+1] MOD 256) * 256 + data[-link+2] * 65536;
							t := data[-link+3]; linkadr := 0F1000000H - link
						END
					END;
					IF t = absolute THEN x := adr + offset
					ELSIF t = relative THEN x := adr + offset; linkadr := -(linkadr + 4);
						i := SHORT(x DIV 65536 + linkadr DIV 65536);
						x := x MOD 65536 + linkadr MOD 65536;
						IF x >= 65536 THEN INC(i); DEC(x, 65536) END;
						x := x + 65536 * i
					ELSIF t = copy THEN x := 70000000H + (adr + offset) MOD 10000000H
					ELSIF t = table THEN x := adr + n; n := link + 4
					ELSIF t = tableend THEN x := adr + n; n := 0
					ELSIF t = deref THEN x := 70000000H + (adr + 2) MOD 10000000H; INC(x, offset);
					ELSIF t = halfword THEN
						x := adr + offset;
						low := (x + 8000H) MOD 10000H - 8000H;
						hi := (x - low) DIV 10000H;
						IF link > 0 THEN
							IF bigEndian THEN
								x := code[link+7] MOD 256 + code[link+6] * 256;
								code[link+6] := SHORT(SHORT(low DIV 256));
								code[link+7] := SHORT(SHORT(low))
							ELSE
								x := code[link+4] MOD 256 + code[link+5] * 256;
								code[link+5] := SHORT(SHORT(low DIV 256));
								code[link+4] := SHORT(SHORT(low))
							END
						ELSE
							IF bigEndian THEN
								x := data[-link+7] MOD 256 + data[-link+6] * 256;
								data[-link+6] := SHORT(SHORT(low DIV 256));
								data[-link+7] := SHORT(SHORT(low))
							ELSE
								x := data[-link+4] MOD 256 + data[-link+5] * 256;
								data[-link+5] := SHORT(SHORT(low DIV 256));
								data[-link+4] := SHORT(SHORT(low))
							END
						END;
						x := x * 10000H + hi MOD 10000H
					ELSE
						out.WriteString(" Wrong Link !!! : ");
						WriteHex(t, 2); WriteHex(n, 6); out.WriteString(" @ "); WriteHex(linkadr, 8);
						n := 0; link := 0
					END;
					IF link > 0 THEN
						IF bigEndian THEN
							code[link+3] := SHORT(SHORT(x));
							code[link+2] := SHORT(SHORT(x DIV 100H));
							code[link+1] := SHORT(SHORT(x DIV 10000H));
							code[link] := SHORT(SHORT(x DIV 1000000H))
						ELSE
							code[link] := SHORT(SHORT(x));
							code[link+1] := SHORT(SHORT(x DIV 100H));
							code[link+2] := SHORT(SHORT(x DIV 10000H));
							code[link+3] := SHORT(SHORT(x DIV 1000000H))
						END
					ELSIF link < 0 THEN
						link := -link;
						IF bigEndian THEN
							data[link+3] := SHORT(SHORT(x));
							data[link+2] := SHORT(SHORT(x DIV 100H));
							data[link+1] := SHORT(SHORT(x DIV 10000H));
							data[link] := SHORT(SHORT(x DIV 1000000H))
						ELSE
							data[link] := SHORT(SHORT(x));
							data[link+1] := SHORT(SHORT(x DIV 100H));
							data[link+2] := SHORT(SHORT(x DIV 10000H));
							data[link+3] := SHORT(SHORT(x DIV 1000000H))
						END
					END;
					link := n
				END;
				RNum(link)
			END
		END Fixup;
	
	BEGIN
	(* Header *)
		out.WriteString("Header: ");
		Read4(headsize); Read4(metasize); Read4(descsize); Read4(codesize); Read4(datasize); RNum(nofmod);
		ReadName(modname); out.WriteString(modname); out.WriteLn;
		out.WriteString("header size: "); WriteInt(headsize); out.WriteLn;
		out.WriteString("meta size: "); WriteInt(metasize); out.WriteLn;
		out.WriteString("desc size: "); WriteInt(descsize); out.WriteLn;
		out.WriteString("code size: "); WriteInt(codesize); out.WriteLn;
		out.WriteString("data size: "); WriteInt(datasize); out.WriteLn;
		WriteInt(nofmod); out.WriteString(" imports: "); out.WriteLn; i := 1;
		WHILE i <= nofmod DO
			ReadName(mods[i]); out.WriteString(mods[i]); out.WriteLn; INC(i)
		END;
		WHILE inp.Pos() < headsize DO inp.ReadByte(b) END;
	(* Data *)
		NEW(data, metasize + descsize); i := 0;
		WHILE i < metasize + descsize DO inp.ReadByte(data[i]); INC(i) END;
	(* Code *)
		NEW(code, codesize); i := 0;
		WHILE i < codesize DO inp.ReadByte(code[i]); INC(i) END;
	(* Fixups *)
		out.WriteLn; out.WriteString("fixups: "); out.WriteLn;
		Fixup(0FA000000H); out.WriteLn;
		Fixup(0FB000000H); out.WriteLn;
		Fixup(0F1000000H); out.WriteLn;
		Fixup(0F1000000H + metasize); out.WriteLn;
		Fixup(0F2000000H); out.WriteLn;
		Fixup(0F0000000H); out.WriteLn;
	(* UseBlk *)
		out.WriteLn; out.WriteString("imports: "); out.WriteLn;
		i := 0; linkadr := 1000000H;
		WHILE i < nofmod DO
			INC(i); out.WriteString("from "); out.WriteString(mods[i]); out.WriteChar(":"); out.WriteLn;
			RNum(k);
			WHILE k # 0 DO
				ReadName(name); RNum(fprint); j := 0;
				CASE k OF
				| mConst: out.WriteString("  const     ")
				| mTyp: out.WriteString("  type      "); RNum(j)
				| mVar: out.WriteString("  var       ")
				| mProc: out.WriteString("  proc      ")
				END;
				WriteString(name); out.WriteString("  ");
				IF j = 1 THEN out.WriteString("P ")
				ELSIF j = 2 THEN out.WriteString("* ")
				ELSIF j = 3 THEN out.WriteString("P* ")
				END;
				out.WriteChar("("); WriteHex(fprint, 8); out.WriteChar(")");
				IF k # mConst THEN
					out.WriteString(" adr="); Fixup(linkadr); INC(linkadr, 1000000H)
				END;
				out.WriteLn; RNum(k)
			END
		END;
		out.WriteLn
	END ReadIn;
	
	PROCEDURE Word (a: INTEGER): INTEGER;
	BEGIN
		IF bigEndian THEN
			RETURN ((data[a] * 256 + data[a+1] MOD 256) * 256 + data[a+2] MOD 256) * 256 + data[a+3] MOD 256
		ELSE
			RETURN ((data[a+3] * 256 + data[a+2] MOD 256) * 256 + data[a+1] MOD 256) * 256 + data[a] MOD 256
		END
	END Word;
	
	PROCEDURE ShowHex (name: ARRAY OF CHAR; a: INTEGER);
	BEGIN
		out.WriteString(name);
		WriteHex(Word(a), 8); out.WriteLn
	END ShowHex;
	
	PROCEDURE ShowInt (name: ARRAY OF CHAR; a: INTEGER);
	BEGIN
		out.WriteString(name);
		WriteInt(Word(a)); out.WriteLn
	END ShowInt;
	
	PROCEDURE ShowName (name: ARRAY OF CHAR; idx: INTEGER);
		VAR p: StrPtr;
	BEGIN
		out.WriteString(name);
		p := S.VAL(StrPtr, S.ADR(data[idx]));
		out.WriteSString(p^)
	END ShowName;
	
	PROCEDURE ShowIndName (a: INTEGER);
	BEGIN
		ShowName("", names + Word(a) DIV 256)
	END ShowIndName;
	
	PROCEDURE ShowStruct (x: INTEGER);
		VAR a: INTEGER; s: Struct;
	BEGIN
		IF (x >= 0) & (x < 256) THEN
			CASE x OF
			| 0: out.WriteChar("-")
			| 1: out.WriteString("BOOLEAN")
			| 2: out.WriteString("SHORTCHAR")
			| 3: out.WriteString("CHAR")
			| 4: out.WriteString("BYTE")
			| 5: out.WriteString("SHORTINT")
			| 6: out.WriteString("INTEGER")
			| 7: out.WriteString("SHORTREAL")
			| 8: out.WriteString("REAL")
			| 9: out.WriteString("SET")
			| 10: out.WriteString("LONGINT")
			| 11: out.WriteString("ANYREC")
			| 12: out.WriteString("ANYPTR")
			| 13: out.WriteString("POINTER")
			| 14: out.WriteString("PROCEDURE")
			| 15: out.WriteString("STRING")
			| 32: out.WriteString("PtrInterface")
			| 33: out.WriteString("RESULT")
			| 34: out.WriteString("GUID")
			ELSE out.WriteString("(Form: "); out.WriteInt(x); out.WriteChar(")")
			END
		ELSE
			out.WriteChar("["); WriteHex(x, 8); out.WriteString("]  ");
			IF (x >= 0F1000000H) & (x < 0F2000000H) THEN
				a := x - 0F1000000H;
				ShowIndName(a + 8);
				IF strList # NIL THEN
					s := strList.link;
					WHILE (s # NIL) & (s.adr # a) DO s := s.link END;
					IF s = NIL THEN
						NEW(s); s.adr := a; s.link := NIL; strLast.link := s; strLast := s
					END
				END
			END
		END
	END ShowStruct;
	
	PROCEDURE ShowObj (a: INTEGER);
		VAR x, m: INTEGER;
	BEGIN
		m := Word(a + 8) MOD 16;
		CASE m OF
		| mConst: out.WriteString("const ")
		| mTyp: out.WriteString("typ ")
		| mVar: out.WriteString("var ")
		| mProc: out.WriteString("proc ")
		| mField: out.WriteString("field ")
		END;
		ShowIndName(a + 8); x := Word(a + 8) DIV 16 MOD 16;
		IF x = mExported THEN out.WriteChar("*")
		ELSIF x = mReadonly THEN out.WriteChar("-")
		ELSIF x = mPrivate THEN out.WriteChar("+")
		END;
		out.WriteString("  "); x := Word(a + 4);
		IF m = mField THEN WriteInt(x)
		ELSIF m = mProc THEN WriteHex(0F2000000H + x, 8)
		ELSIF m = mVar THEN WriteHex(0F0000000H + x, 8)
		ELSIF m = mTyp THEN out.WriteChar("("); WriteHex(x, 8); out.WriteChar(")")
		ELSE WriteHex(x, 8)
		END;
		out.WriteString("  ("); WriteHex(Word(a), 8);
		out.WriteString(")  "); ShowStruct(Word(a + 12));
		out.WriteLn
	END ShowObj;
	
	PROCEDURE ShowDesc (str: Struct);
		VAR a, b, x, i: INTEGER;
	BEGIN
		a := str.adr;
		out.WriteChar("["); WriteHex(a + 0F1000000H, 8); out.WriteChar("]"); out.WriteLn;
		IF Word(a + 4) # 0F1000000H + metasize THEN
			out.WriteString("mod = "); WriteHex(Word(a + 4), 8);
			out.WriteString(" !!! "); out.WriteLn
		END;
		ShowIndName(a + 8);
		out.WriteString(" = "); x := Word(a + 8) MOD 4;
		IF x = mArray THEN
			out.WriteString("ARRAY "); x := Word(a);
			IF x # 0 THEN WriteInt(x) END;
			out.WriteString(" OF "); ShowStruct(Word(a + 12));
			x := Word(a + 8) DIV 16 MOD 16;
			out.WriteString("  lev: "); WriteInt(x)
		ELSIF x = mPointer THEN
			out.WriteString("POINTER TO "); ShowStruct(Word(a + 12))
		ELSIF x = mProctyp THEN
			out.WriteString("PROCEDURE (sig = ");
			WriteHex(Word(a), 8); out.WriteChar(")")
		ELSE
			x := Word(a + 8) DIV 4 MOD 4;
			IF x = 1 THEN out.WriteString("EXTENSIBLE ")
			ELSIF x = 2 THEN out.WriteString("LIMITED ")
			ELSIF x = 3 THEN out.WriteString("ABSTRACT ")
			END;
			out.WriteString("RECORD"); out.WriteLn;
			x := Word(a + 8) DIV 16 MOD 16;
			out.WriteString("  extlev: "); WriteInt(x); out.WriteLn;
			out.WriteString("  size: "); WriteInt(Word(a)); out.WriteLn;
			x := Word(a + 76);
			IF (x >= 0F1000000H) & (x < 0F2000000H) THEN
				b := x - 0F1000000H; x := Word(b); INC(b, 4); i := 0;
				WHILE i < x DO
					out.WriteString("  "); ShowObj(b); INC(i); INC(b, 16)
				END
			ELSE
				WriteHex(x, 8); out.WriteString(" bad field list pointer !!! "); out.WriteLn
			END;
			b := a - 4; i := 0; x := Word(b);
			WHILE x # -1 DO
				out.WriteString("  meth "); WriteInt(i); out.WriteString(":  ");
				WriteHex(x, 8); out.WriteLn; INC(i); DEC(b, 4); x := Word(b)
			END;
			out.WriteString("  base types:"); out.WriteLn; b := a + 12; i := 0;
			WHILE i < 16 DO
				x := Word(b);
				IF x # 0 THEN
					out.WriteString("  base "); WriteInt(i); out.WriteString(":  ");
					ShowStruct(x); out.WriteLn
				END;
				INC(i); INC(b, 4)
			END;
			out.WriteString("  pointers:"); out.WriteLn;
			out.WriteString("  "); b := a + 80; x := Word(b); i := 0;
			WHILE x >= 0 DO 
				WriteInt(x); out.WriteChar(" ");
				INC(b, 4); INC(i); x := Word(b)
			END;
			IF b + x # a + 76 THEN
				out.WriteLn; WriteInt(x); out.WriteString(" bad sentinel !!! ")
			END;
			INC(b, 4); i := b;
			WHILE (i < metasize + descsize) & (Word(i) DIV 1000000H = 0) DO INC(i, 4) END;
			IF (i < metasize + descsize) & (Word(i) = -1) THEN
				out.WriteString("/ "); i := b; x := Word(i);
				WHILE x # -1 DO WriteInt(x); out.WriteChar(" "); INC(i, 4); x := Word(i) END
			END
		END;
		out.WriteLn
	END ShowDesc;
	
	PROCEDURE RefCh (VAR ref: INTEGER; VAR ch: SHORTCHAR);
	BEGIN
		ch := SHORT(CHR(data[ref])); INC(ref)
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
(*	
	PROCEDURE ShowReference;
		VAR ref, adr, desc, p, a, d: INTEGER; ch, f: SHORTCHAR; name: Name;
	BEGIN
		out.WriteLn;
		out.WriteString("Reference:"); out.WriteLn;
		ref := Word(metasize + 72) - 0F1000000H;
		RefCh(ref, ch); p := 0; a := 0F2000000H;
		WHILE ch # 0X DO
			IF ch < 0FCX THEN	(* source pos *)
				INC(a, ORD(ch)); RefNum(ref, d); INC(p, d);
				out.WriteString("   SourcePos: "); out.WriteInt(p);
				out.WriteString(" adr: "); WriteHex(a, 8); out.WriteLn
			ELSIF ch = 0FCX THEN	(* procedure *)
				RefNum(ref, adr); RefName(ref, name);
				out.WriteString("PROCEDURE "); out.WriteString(name);
				out.WriteString(" end: "); WriteHex(0F2000000H + adr, 8); out.WriteLn
			ELSE	(* variable *)
				RefCh(ref, f);
				IF f = 10X THEN desc := Word(ref); INC(ref, 4) ELSE desc := ORD(f) END;
				RefNum(ref, adr); RefName(ref, name);
				IF ch = 0FDX THEN out.WriteString("   VAR ")
				ELSIF ch = 0FFX THEN out.WriteString("   VARPAR ")
				ELSE out.WriteString("   ??? ")
				END;
				out.WriteString(name); out.WriteString(": "); ShowStruct(desc);
				out.WriteString(" adr: "); out.WriteInt(adr); out.WriteLn
			END;
			RefCh(ref, ch)
		END
	END ShowReference;
*)	
	PROCEDURE ShowMeta;
		VAR a, i, j, nptrs, ptrs, imps, exp: INTEGER; s: Struct; ch: CHAR;
	BEGIN
		NEW(strList); strList.adr := -1; strList.link := NIL; strLast := strList;
		
		out.WriteString("Constants:");
		a := Word(metasize + 84) DIV 16 * 16;
		i := a - 0F1000000H;
(*
		WHILE i < metasize DO
			IF i MOD 16 = 0 THEN
				out.WriteLn; WriteHex(a, 8); out.WriteString(":  ")
			END;
			WriteHex(data[i], 2); out.WriteChar(" ");
			INC(a); INC(i)
		END;
*)
		WHILE i < metasize DO
			out.WriteLn; WriteHex(a, 8); out.WriteString(":  "); j := 0;
			WHILE j < 16 DO
				WriteHex(data[i + j], 2); out.WriteChar(" "); INC(j)
			END;
			out.WriteString("   "); j := 0;
			WHILE j < 16 DO
				ch := CHR(data[i + j]); INC(j);
				IF (ch >= " ") & (ch < 07FX) OR (ch >= 0A0X) THEN out.WriteChar(ch)
				ELSE out.WriteChar(".")
				END
			END;
			INC(a, 16); INC(i, 16)
		END;
		out.WriteLn; out.WriteLn;

		out.WriteString("module descriptor:"); out.WriteLn; a := metasize;
		ShowHex("link: ", a); INC(a, 4);
		ShowHex("opts: ", a); INC(a, 4);
		ShowInt("refcnt: ", a); INC(a, 4);
		ShowHex("comp time: ", a); INC(a, 4);
		ShowHex(" ", a); INC(a, 4);
		ShowHex(" ", a); INC(a, 4);
		ShowHex("load time: ", a); INC(a, 4);
		ShowHex(" ", a); INC(a, 4);
		ShowHex(" ", a); INC(a, 4);
		ShowHex("ext: ", a); INC(a, 4);
		ShowHex("term: ", a); INC(a, 4);
		ShowInt("nofimps: ", a); INC(a, 4);
		ShowInt("nofptrs: ", a); nptrs := Word(a); INC(a, 4);
		ShowInt("csize: ", a); INC(a, 4);
		ShowInt("dsize: ", a); INC(a, 4);
		ShowInt("rsize: ", a); INC(a, 4);
		ShowHex("code: ", a); INC(a, 4);
		ShowHex("data: ", a); INC(a, 4);
		ShowHex("refs: ", a); INC(a, 4);
		ShowHex("proc base: ", a); INC(a, 4);
		ShowHex("var base: ", a); INC(a, 4);
		names := Word(a) - 0F1000000H; INC(a, 4);
		ptrs := Word(a) - 0F1000000H; INC(a, 4);
		imps := Word(a) - 0F1000000H; INC(a, 4);
		exp := Word(a) - 0F1000000H; INC(a, 4);
		ShowName("name: ", a); out.WriteLn;
		out.WriteLn; a := exp; i := Word(a); INC(a, 4);
		WriteInt(i); out.WriteString(" exports:"); out.WriteLn;
		WHILE i > 0 DO ShowObj(a); INC(a, 16); DEC(i) END;
		out.WriteLn; out.WriteString("pointers:"); out.WriteLn; a := ptrs; i := 0;
		WHILE i < nptrs DO
			WriteInt(Word(a)); out.WriteChar(" "); INC(a, 4); INC(i)
		END;
		i := Word(metasize + 4);
		IF ODD(i DIV 40000000H) THEN
			i := Word(a);
			IF i # -1 THEN out.WriteString("/ ") END;
			WHILE (a < imps) & (i # -1) DO
				WriteInt(i); out.WriteChar(" "); INC(a, 4); i := Word(a)
			END;
			IF a >= imps THEN out.WriteLn; out.WriteString(" bad sentinel !!! ") END
		END;
		out.WriteLn; out.WriteLn; s := strList.link;
		WHILE s # NIL DO ShowDesc(s); s := s.link END;
(*
		ShowReference;
*)
		strList := NIL; strLast := NIL
	END ShowMeta;
	
	PROCEDURE GetChar(OUT b: BYTE);
	BEGIN
		IF pc < LEN(code) THEN b := code[pc] ELSE b := 0 END;
		INC(pc)
	END GetChar;
	
	PROCEDURE PutChar(ch: CHAR);
	BEGIN
		out.rider.WriteChar(ch)
	END PutChar;
	
	PROCEDURE GetDecoder (proc: INTEGER; OUT decode: DevDecBase.DecodeProc);
	BEGIN
		decode := NIL;
		IF proc = proc386 THEN
			bigEndian := FALSE; mode := 0;
			DevDecBase.GetDecode("DevDec486", decode)
		ELSIF proc = proc386 + 1 THEN
			bigEndian := FALSE; mode := 1;
			DevDecBase.GetDecode("DevDec486", decode)
		ELSIF proc = proc68k THEN
			bigEndian := TRUE; mode := 0;
			DevDecBase.GetDecode("DevDec68k", decode)
		ELSIF proc = procPPC THEN
			bigEndian := TRUE; mode := 1;
			DevDecBase.GetDecode("DevDecPPC", decode)
		ELSIF proc = procARM THEN
			bigEndian := TRUE; mode := 0;
			DevDecBase.GetDecode("DevDecARM", decode)
		ELSIF proc = procSH3 THEN
			bigEndian := FALSE; mode := 0;
			DevDecBase.GetDecode("DevDecSH3", decode)
		END
	END GetDecoder;
	
	PROCEDURE ShowRefProc (VAR ref: INTEGER; VAR end: INTEGER);
		VAR ch, f: SHORTCHAR; name: Name; desc, adr: INTEGER; global: BOOLEAN;
	BEGIN
		ch := SHORT(CHR(data[ref]));
		WHILE (ch > 0X) & (ch < 0FCX) DO	(* skip source refs *)
			INC(ref); RefNum(ref, adr); ch := SHORT(CHR(data[ref]))
		END;
		IF ch = 0FCX THEN
			INC(ref); RefNum(ref, end); RefName(ref, name);
			out.WriteLn;
			IF name = "$$" THEN out.WriteString("Module Body"); global := TRUE
			ELSE out.WriteString("PROCEDURE "); out.WriteString(name); global := FALSE
			END;
			out.WriteChar(";"); out.WriteLn;
			ch := SHORT(CHR(data[ref]))
		ELSE end := 0
		END;
		WHILE ch >= 0FDX DO	(* show variables *)
			INC(ref); RefCh(ref, f);
			IF f = 10X THEN desc := Word(ref); INC(ref, 4) ELSE desc := ORD(f) END;
			RefNum(ref, adr); RefName(ref, name);
			IF ch = 0FDX THEN out.WriteString("      VAR ")
			ELSIF ch = 0FFX THEN out.WriteString("      VARPAR ")
			ELSE out.WriteString("   ??? ")
			END;
			out.WriteString(name); out.WriteString(": "); ShowStruct(desc); out.WriteString(" adr: ");
			IF global THEN WriteHex(0F0000000H + adr, 8) ELSE out.WriteInt(adr) END;
			out.WriteLn; ch := SHORT(CHR(data[ref]))
		END
	END ShowRefProc;
	
	PROCEDURE GetSourcePos (VAR ref, spos, cpos: INTEGER; codePos: INTEGER; OUT pos: INTEGER);
		VAR d: INTEGER; ch: SHORTCHAR; name: Name;
	BEGIN
		ch := SHORT(CHR(data[ref]));
		WHILE (ch # 0X) & (cpos < codePos) DO
			IF ch < 0FCX THEN
				INC(cpos, ORD(ch)); INC(ref); RefNum(ref, d); INC(spos, d)
			ELSIF ch = 0FCX THEN	(* skip procedure *)
				INC(ref); RefNum(ref, d); RefName(ref, name)
			ELSE	(* skip variables *)
				INC(ref); RefCh(ref, ch);
				IF ch = 10X THEN INC(ref, 4) END;
				RefNum(ref, d); RefName(ref, name)
			END;
			ch := SHORT(CHR(data[ref]))
		END;
		IF cpos = codePos THEN pos := spos ELSE pos := 0 END
	END GetSourcePos;
	
	PROCEDURE DecodeObj (decode: DevDecBase.DecodeProc; adr, from, to: INTEGER);
		VAR end, ref, sref, spos, cpos, pos: INTEGER; name: Name;
	BEGIN
		IF decode # NIL THEN
			pc := from; ref := 0;
			sref := 0; spos := 0; cpos := 0;
			WHILE pc < to DO
				IF data = NIL THEN end := to
				ELSE
					ShowRefProc(ref, end);
					IF end = 0 THEN end := to END
				END;
				WHILE pc < end DO
					IF data = NIL THEN pos := 0
					ELSE GetSourcePos(sref, spos, cpos, pc, pos)
					END;
					decode (adr, PutChar, GetChar, mode);
					IF pos # 0 THEN out.WriteTab; out.WriteChar("("); out.WriteInt(pos); out.WriteChar(")") END;
					out.WriteLn
				END;
				IF pc > end THEN pc := end; adr := 0F2000000H + pc END
			END
		ELSE
			out.WriteString("no disassembler available"); out.WriteLn
		END
	END DecodeObj;

	PROCEDURE Decode*;
		VAR s: TextMappers.Scanner; p: DevCommanders.Par; f: Files.File; loc: Files.Locator; rp: TextRulers.Prop;
			tag, proc: INTEGER; name: Files.Name; t: TextModels.Model; v: TextViews.View; title: Views.Title;
			dec: DevDecBase.DecodeProc; codeDir: ARRAY 16 OF CHAR;
	BEGIN
		p := DevCommanders.par;
		IF p # NIL THEN
			DevCommanders.par := NIL; codeDir := "Code";
			s.ConnectTo(p.text); s.SetPos(p.beg); s.Scan;
			IF (s.type = TextMappers.char) & (s.char = "(") THEN
				s.Scan;
				IF s.type = TextMappers.string THEN
					codeDir := codeDir + s.string; s.Scan
				END;
				IF (s.type = TextMappers.char) & (s.char = ")") THEN s.Scan END
			END;
			IF s.type = TextMappers.string THEN
				StdDialog.GetSubLoc(s.string, codeDir$, loc, name);
				title := s.string$;
				f := Files.dir.Old(loc, name, Files.shared);
				IF f # NIL THEN
					inp := f.NewReader(NIL); inp.SetPos(0);
					t := TextModels.dir.New();
					out.ConnectTo(t); out.SetPos(0);
					Read4(tag); Read4(proc);
					IF tag = OFtag THEN
						GetDecoder(proc, dec);
						ReadIn;
						ShowMeta;
						DecodeObj(dec, 0F2000000H, 0, codesize);
						out.WriteLn;
						v := TextViews.dir.New(t);
						NEW(rp);
						rp.valid := {TextRulers.tabs};
						rp.tabs.len := 3;
						rp.tabs.tab[0].stop := 40 * Ports.mm;
						rp.tabs.tab[1].stop := 90 * Ports.mm;
						rp.tabs.tab[2].stop := 140 * Ports.mm;
						v.SetDefaults(TextRulers.dir.NewFromProp(rp), TextViews.dir.defAttr);
						Views.OpenAux(v, title)
					ELSE
						Dialog.ShowMsg("not an object file")
					END;
					out.ConnectTo(NIL);
					inp := NIL
				ELSE
					Dialog.ShowMsg("code file not found")
				END
			ELSE
				Dialog.ShowMsg("module name missing")
			END
		END;
		data := NIL; code := NIL
	END Decode;
	
	PROCEDURE Open* (name: ARRAY OF CHAR);
		VAR t: TextModels.Model;
	BEGIN
		t := TextModels.dir.New();
		out.ConnectTo(t); out.SetPos(0);
		out.WriteString(name); out.WriteLn; out.WriteLn
	END Open;
	
	PROCEDURE Close*;
		VAR rp: TextRulers.Prop; v: TextViews.View;
	BEGIN
		v := TextViews.dir.New(out.rider.Base());
		NEW(rp);
		rp.valid := {TextRulers.tabs};
		rp.tabs.len := 3;
		rp.tabs.tab[0].stop := 40 * Ports.mm;
		rp.tabs.tab[1].stop := 90 * Ports.mm;
		rp.tabs.tab[2].stop := 140 * Ports.mm;
		v.SetDefaults(TextRulers.dir.NewFromProp(rp), TextViews.dir.defAttr);
		Views.OpenAux(v, "Decode");
		out.ConnectTo(NIL)
	END Close;

	PROCEDURE DecodeThis* (IN bytes: ARRAY OF BYTE; adr, length, processor: INTEGER; name: ARRAY OF CHAR);
		VAR rp: TextRulers.Prop; decoder: DevDecBase.DecodeProc;
			i, proc: INTEGER; t: TextModels.Model; v: TextViews.View;
	BEGIN
		codesize := length; datasize := 0;
		NEW(code, codesize); i := 0;
		WHILE i < codesize DO code[i] := bytes[i]; INC(i) END;
		out.WriteString(name); out.WriteLn;
		GetDecoder(processor, decoder);
		DecodeObj(decoder, adr, 0, codesize);
		out.WriteLn; out.WriteLn; 
		code := NIL
	END DecodeThis;

	PROCEDURE DecodeTo* (bytes: POINTER TO ARRAY OF BYTE;
									adr, from, to, processor: INTEGER;
									VAR wr: TextMappers.Formatter);
		VAR decoder: DevDecBase.DecodeProc;
	BEGIN
		S.PUT(S.ADR(out.rider), wr.rider);
		codesize := to; datasize := 0;
		code := bytes;
		GetDecoder(processor, decoder);
		DecodeObj(decoder, adr, from, to);
		code := NIL; out.ConnectTo(NIL)
	END DecodeTo;

END DevDecoder.
