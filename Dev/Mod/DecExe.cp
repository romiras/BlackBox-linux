MODULE DevDecExe;
	IMPORT Files, Views, TextModels, TextViews, TextMappers, DevDecBase;
	
	TYPE Object = RECORD
		rva, size, pos: INTEGER;
		code, noinit: BOOLEAN;
	END;
	
	VAR
		W: TextMappers.Formatter;
		R: Files.Reader;
		F: Files.File;
		val: INTEGER;
		ObjectTablePos: INTEGER;
		NumOfObj: INTEGER;
		ImagesPos: INTEGER;
		VBase: INTEGER;
		ExportRVA: INTEGER;
		ImportRVA: INTEGER;
		ResourceRVA: INTEGER;
		FixupRVA: INTEGER;
		cpu: INTEGER;
		DecCode: BOOLEAN;
		Dir: ARRAY 1024 OF Object;
		ResTable: ARRAY 16 OF INTEGER;

	PROCEDURE WriteHex(x, n: INTEGER);
		VAR
	BEGIN
		IF n > 1 THEN WriteHex(x DIV 16, n-1) END;
		x := x MOD 16;
		IF x <= 9 THEN W.WriteChar(CHR(x + ORD("0")))
		ELSE W.WriteChar(CHR(x -10 + ORD("A")))
		END
	END WriteHex;
	
	PROCEDURE WriteChar(ch: CHAR; zero: BOOLEAN);
	BEGIN
		IF (ch >= " ") & (ch < 7FX) THEN W.WriteChar(ch)
		ELSIF (ch # 0X) OR zero THEN W.WriteChar(".")
		END
	END WriteChar;
	
	PROCEDURE WritePos;
	BEGIN
		WriteHex(R.Pos(), 6);
		W.WriteString(":  ")
	END WritePos;
	
	PROCEDURE PutChar (ch: CHAR);
	BEGIN
		IF ch = 09X THEN W.WriteTab
		ELSE W.WriteChar(ch)
		END
	END PutChar;
	
	PROCEDURE* ReadChar(OUT ch: BYTE);
	BEGIN
		R.ReadByte(ch)
	END ReadChar;
	
	PROCEDURE ReadWord(VAR x: INTEGER);
		VAR b: BYTE;
	BEGIN
		R.ReadByte(b); x := b MOD 256;
		R.ReadByte(b); x := x + 256 * b
	END ReadWord;
	
	PROCEDURE ReadLong(VAR x: INTEGER);
		VAR b: BYTE;
	BEGIN
		R.ReadByte(b); x := b MOD 256;
		R.ReadByte(b); x := x + 100H * (b MOD 256);
		R.ReadByte(b); x := x + 10000H * (b MOD 256);
		R.ReadByte(b); x := x + 1000000H * b
	END ReadLong;
	
	PROCEDURE Skip(n: INTEGER);
	BEGIN
		R.SetPos(R.Pos() + n);
	END Skip;
	
	PROCEDURE GetWord(pos: INTEGER): INTEGER;
		VAR b: BYTE; x: INTEGER; R: Files.Reader;
	BEGIN
		R := F.NewReader(NIL);
		R.SetPos(pos);
		R.ReadByte(b); x := b MOD 256;
		R.ReadByte(b); x := x + 256 * b;
		RETURN x
	END GetWord;
	
	PROCEDURE GetLong(pos: INTEGER): INTEGER;
		VAR b: BYTE; x: INTEGER; R: Files.Reader;
	BEGIN
		R := F.NewReader(NIL);
		R.SetPos(pos);
		R.ReadByte(b); x := b MOD 256;
		R.ReadByte(b); x := x + 100H * (b MOD 256);
		R.ReadByte(b); x := x + 10000H * (b MOD 256);
		R.ReadByte(b); x := x + 1000000H * b;
		RETURN x
	END GetLong;
	
	PROCEDURE CopyString(pos: INTEGER);
		VAR b: BYTE; R: Files.Reader;
	BEGIN
		R := F.NewReader(NIL);
		IF (pos < 0) OR (pos >= F.Length()) THEN
			W.WriteString("WRONG POS: "); W.WriteInt(pos); RETURN
		END;
		R.SetPos(pos);
		W.WriteChar(22X);
		R.ReadByte(b);
		WHILE b # 0 DO WriteChar(CHR(b MOD 256), FALSE); R.ReadByte(b) END;
		W.WriteChar(22X)
	END CopyString;

	PROCEDURE DumpChars(str: ARRAY OF CHAR; n: INTEGER; zero: BOOLEAN);
		VAR b: BYTE;
	BEGIN
		WritePos; W.WriteString(str);
		W.WriteChar(22X);
		WHILE n > 0 DO ReadChar(b); WriteChar(CHR(b MOD 256), zero); DEC(n) END;
		W.WriteChar(22X);
		W.WriteLn
	END DumpChars;
	
	PROCEDURE DumpByte(str: ARRAY OF CHAR);
		VAR b: BYTE;
	BEGIN
		WritePos; W.WriteString(str);
		ReadChar(b);
		WriteHex(b MOD 256, 2);
		W.WriteLn
	END DumpByte;
	
	PROCEDURE DumpWord(str: ARRAY OF CHAR);
		VAR x: INTEGER;
	BEGIN
		WritePos; W.WriteString(str);
		ReadWord(x);
		WriteHex(x, 4);
		W.WriteLn;
		val := x
	END DumpWord;
	
	PROCEDURE DumpLong(str: ARRAY OF CHAR);
		VAR x: INTEGER;
	BEGIN
		WritePos; W.WriteString(str);
		ReadLong(x);
		WriteHex(x, 8);
		W.WriteLn;
		val := x
	END DumpLong;
	
	PROCEDURE DecodeHeader;
	BEGIN
		DumpChars("Signature Bytes: ", 4, TRUE);
		DumpWord("CPU Type: "); cpu := val;
		DumpWord("# Objects: "); NumOfObj := val;
		DumpLong("Time/Data: ");
		Skip(8);
		DumpWord("NT Header Size: "); ObjectTablePos := val + R.Pos() + 2;
		DumpWord("Image Flags: ");
		Skip(2);
		DumpWord("Linker Version: ");
		Skip(12);
		DumpLong("Entrypoint RVA: ");
		Skip(8);
		DumpLong("Image Base: "); VBase := val;
		DumpLong("Object Align: ");
		DumpLong("File Align: ");
		DumpLong("OS Version: ");
		DumpLong("User Version: ");
		DumpLong("Subsys Version: ");
		Skip(4);
		DumpLong("Image Size: ");
		DumpLong("Header Size: "); ImagesPos := val;
		DumpLong("Checksum: ");
		DumpWord("Subsystem: ");
		DumpWord("DLL Flags: ");
		DumpLong("Stack Reserve Size: ");
		DumpLong("Stack Commit Size: ");
		DumpLong("Heap Reserve Size: ");
		DumpLong("Heap Commit Size: ");
		Skip(4);
		DumpLong("# RVA/Sizes: ");
		DumpLong("Export Table RVA: "); ExportRVA := val;
		DumpLong("Export Table Size: ");
		DumpLong("Import Table RVA: "); ImportRVA := val;
		DumpLong("Import Table Size: ");
		DumpLong("Resource Table RVA: "); ResourceRVA := val;
		DumpLong("Resource Table Size: ");
		DumpLong("Exception Table RVA: ");
		DumpLong("Exception Table Size: ");
		DumpLong("Security Table RVA: ");
		DumpLong("Security Table Size: ");
		DumpLong("Fixup Table RVA: "); FixupRVA := val;
		DumpLong("Fixup Table Size: ");
		DumpLong("Debug Table RVA: ");
		DumpLong("Debug Table Size: ");
		DumpLong("Image Description RVA: ");
		DumpLong("Image Description Size: ");
		DumpLong("Machine Specific RVA: ");
		DumpLong("Machine Specific Size: ");
		DumpLong("Thread Local Storage RVA: ");
		DumpLong("Thread Local Storage Size: ");
	END DecodeHeader;
	
	PROCEDURE DecodeObjTable;
		VAR n, i, x: INTEGER;
	BEGIN
		R.SetPos(ObjectTablePos); n := 1;
		WHILE n <= NumOfObj DO
			W.WriteLn;
			W.WriteString("***** Object #");
			W.WriteInt(n);
			W.WriteString(" *****"); 
			W.WriteLn;
			DumpChars("Object Name: ", 8, FALSE);
			DumpLong("Object Size: ");
			DumpLong("Object RVA: "); Dir[n].rva := val;
			DumpLong("Physical Size: "); Dir[n].size := val;
			DumpLong("Physical Offset: "); Dir[n].pos := val;
			Dir[n].code := FALSE; Dir[n].noinit := FALSE;
			Skip(12);
			WritePos; W.WriteString("Object Flags: ");
			ReadLong(x); i := 0;
			WHILE i < 32 DO
				IF ODD(x) THEN
					IF i = 5 THEN W.WriteString("code, "); Dir[n].code := TRUE
					ELSIF i = 6 THEN W.WriteString("data, ")
					ELSIF i = 7 THEN W.WriteString("noInit, "); Dir[n].noinit := TRUE
					ELSIF i = 26 THEN W.WriteString("nonCache, ")
					ELSIF i = 27 THEN W.WriteString("nonPage, ")
					ELSIF i = 28 THEN W.WriteString("shared, ")
					ELSIF i = 29 THEN W.WriteString("exec, "); Dir[n].code := TRUE
					ELSIF i = 30 THEN W.WriteString("read, ")
					ELSIF i = 31 THEN W.WriteString("write, ")
					ELSE W.WriteInt(i); W.WriteString(", ")
					END
				END;
				x := x DIV 2; INC(i)
			END;
			W.WriteLn;
			INC(n)
		END;
		W.WriteLn
	END DecodeObjTable;
	
	PROCEDURE Size(n: INTEGER): INTEGER;
		VAR a, size: INTEGER; b: BYTE;
	BEGIN
		R.SetPos(Dir[n].pos);
		a := 0; size := 0;
		WHILE a < Dir[n].size DO
			ReadChar(b); INC(a);
			IF b # 0 THEN size := a END
		END;
		RETURN size;
	END Size;
	
	PROCEDURE DecodeHex(pos, size, rva: INTEGER);
		VAR i, a: INTEGER; line: ARRAY 16 OF CHAR; b: BYTE;
	BEGIN
		R.SetPos(pos);
		a := 0;
		WHILE a < size DO
			WritePos;
			WriteHex(VBase + rva + a, 8);
			W.WriteString("  ");
			i := 0;
			WHILE i < 16 DO
				ReadChar(b); line[i] := CHR(b MOD 256);
				WriteHex(ORD(line[i]), 2);
				W.WriteChar(" ");
				INC(i);
			END;
			W.WriteChar(" "); i := 0;
			WHILE i < 16 DO
				WriteChar(line[i], TRUE);
				INC(i);
			END;
			W.WriteLn; INC(a, 16)
		END
	END DecodeHex;
	
	PROCEDURE DecodeDefaultObj(n: INTEGER);
	BEGIN
		DecodeHex(Dir[n].pos, Size(n), Dir[n].rva)
	END DecodeDefaultObj;
	
	PROCEDURE DecodeCodeObj(n: INTEGER);
		VAR adr, end, mode: INTEGER; decode: DevDecBase.DecodeProc;
	BEGIN
		IF DecCode THEN
			decode := NIL; mode := 0;
			IF cpu = 14CH THEN
				DevDecBase.GetDecode("DevDec486", decode)
			ELSIF cpu = 166H THEN
				DevDecBase.GetDecode("DevDecMIPS", decode)
			ELSIF cpu = 184H THEN
				DevDecBase.GetDecode("DevDecAlpha", decode)
			ELSIF cpu = 268H THEN
				DevDecBase.GetDecode("DevDec68k", decode)
			ELSIF cpu = 1F0H THEN
				DevDecBase.GetDecode("DevDecPPC", decode)
			ELSIF cpu = 1A2H THEN
				DevDecBase.GetDecode("DevDecSH3", decode)
			END;
			IF decode # NIL THEN
				adr := Dir[n].rva + VBase;
				end := adr + Size(n);
				R.SetPos(Dir[n].pos);
				WHILE adr < end DO
					WritePos;
					decode(adr, PutChar, ReadChar, mode);
					W.WriteLn
				END
			ELSE
				W.WriteString("unknown processor type"); W.WriteLn
			END;
			W.WriteLn
		END
	END DecodeCodeObj;
	
	PROCEDURE DecodeExportObj(n: INTEGER);
		VAR pe, pp, po, ne, np, i, offs: INTEGER;
	BEGIN
		R.SetPos(Dir[n].pos);
		offs := Dir[n].pos - ExportRVA;
		DumpLong("Export Flags: ");
		DumpLong("Time/Data: ");
		DumpLong("Version: ");
		DumpLong("Name RVA: "); pp := val;
			IF pp # 0 THEN
				W.WriteString("           ");
				CopyString(pp + offs);
				W.WriteLn
			END;
		DumpLong("Ordinal Base: ");
		DumpLong("# EAT Entries: "); ne := val;
		DumpLong("# Name Ptrs: "); np := val;
		DumpLong("Address Table RVA: "); pe := val;
		DumpLong("Name Ptr Table RVA: "); pp := val;
		DumpLong("Ordinal Table RVA: "); po := val;
		WHILE np > 0 DO
			W.WriteString("           ");
			i := GetWord(po + offs);
			W.WriteInt(i);
			W.WriteString(":  ");
			WriteHex(GetLong(pe + offs + 4 * i) + VBase, 8);
			W.WriteString("  ");
			CopyString(GetLong(pp + offs) + offs);
			W.WriteLn;
			DEC(np); INC(pp, 4); INC(po, 2)
		END;
		W.WriteLn
	END DecodeExportObj;
	
	PROCEDURE DecodeImportObj(n: INTEGER);
		VAR p, p1, x, offs: INTEGER;
	BEGIN
		R.SetPos(Dir[n].pos);
		offs := Dir[n].pos - ImportRVA;
		REPEAT
			DumpLong("Lookup Table RVA: "); p1 := val;
			DumpLong("Time/Data: ");
			DumpLong("Version: ");
			DumpLong("Name RVA: "); p := val;
			IF p # 0 THEN
				W.WriteString("           ");
				CopyString(p + offs);
				W.WriteLn
			END;
			DumpLong("Address Table RVA: "); p := val;
			IF p1 = 0 THEN p1 := p END;
			IF p # 0 THEN
				x := GetLong(p1 + offs);
				WHILE x # 0 DO
					W.WriteString("           ");
					WriteHex(p + VBase, 8);
					W.WriteString(":  ");
					IF x >= 0 THEN
						W.WriteChar("(");
						WriteHex(GetWord(x + offs), 4);
						W.WriteString(")  ");
						CopyString(x + offs + 2);
					ELSE
						WriteHex(x + 80000000H, 8)
					END;
					W.WriteLn;
					INC(p, 4); INC(p1, 4);
					x := GetLong(p1 + offs)
				END
			END;
			W.WriteLn
		UNTIL p = 0
	END DecodeImportObj;
	
	PROCEDURE WriteResString (VAR pos: INTEGER);
		VAR len, i: INTEGER;
	BEGIN
		len := GetWord(pos); INC(pos, 2); i := 0;
		WHILE i < len DO W.WriteChar(CHR(GetWord(pos))); INC(pos, 2); INC(i) END
	END WriteResString;
	
	PROCEDURE WriteResZString;
		VAR x: INTEGER;
	BEGIN
		ReadWord(x);
		WHILE x # 0 DO W.WriteChar(CHR(x)); ReadWord(x) END
	END WriteResZString;
	
	PROCEDURE DecodeResData (rva, basePos, pos, level: INTEGER);
		VAR i, a, b, x, count, adr, size, type: INTEGER; ch: BYTE;
	BEGIN
		i := 0; type := 0;
		WHILE i <= level DO
			IF ResTable[i] < 0 THEN
				a := basePos - ResTable[i];
				WriteResString(a)
			ELSIF i = 0 THEN
				type := ResTable[i];
				CASE type OF
				| 1: W.WriteString("CURSOR")
				| 2: W.WriteString("BITMAP")
				| 3: W.WriteString("ICON")
				| 4: W.WriteString("MENU")
				| 5: W.WriteString("DIALOG")
				| 6: W.WriteString("STRING")
				| 7: W.WriteString("FONTDIR")
				| 8: W.WriteString("FONT")
				| 9: W.WriteString("ACCELERATORS")
				| 10: W.WriteString("RCDATA")
				| 11: W.WriteString("MESSAGETABLE")
				| 12: W.WriteString("GROUP_CURSOR")
				| 14: W.WriteString("GROUP_ICON")
				| 16: W.WriteString("VERSION")
				| 17: W.WriteString("DLGINCLUDE")
				| 19: W.WriteString("PLUGPLAY")
				| 20: W.WriteString("VXD")
				ELSE W.WriteInt(ResTable[i])
				END;
			ELSE W.WriteInt(ResTable[i])
			END;
			W.WriteTab; INC(i)
		END;
		R.SetPos(basePos + pos);
		ReadLong(adr); WriteHex(adr, 8); W.WriteChar(" ");
		ReadLong(size); W.WriteInt(size); W.WriteLn;
		IF type = 4 THEN	(* menu *)
			R.SetPos(adr - rva + basePos);
			ReadWord(x); ReadWord(x); a := 0; b := 0;
			REPEAT
				i := 0; WHILE i < a DO W.WriteString("   "); INC(i) END;
				ReadWord(type);
				IF ODD(type DIV 16) THEN	(* popup *)
					WriteResZString; INC(a)
				ELSE
					ReadWord(x); WriteResZString; 
					IF (type = 0) & (x = 0) THEN W.WriteString("---------")
					ELSE W.WriteChar(" "); W.WriteInt(x)
					END
				END;
				IF ODD(type) THEN W.WriteString(" grayed") END;
				IF ODD(type DIV 2) THEN W.WriteString(" inactive") END;
				IF ODD(type DIV 4) THEN W.WriteString(" bitmap") END;
				IF ODD(type DIV 8) THEN W.WriteString(" checked") END;
				IF ODD(type DIV 32) THEN W.WriteString(" menubarbreak") END;
				IF ODD(type DIV 64) THEN W.WriteString(" menubreak") END;
				IF ODD(type DIV 256) THEN W.WriteString(" ownerdraw") END;
				IF ODD(type DIV 128) THEN	(* end *)
					IF ODD(type DIV 16) THEN INC(b) ELSE DEC(a) END
				END;
				W.WriteLn
			UNTIL a < b
		ELSIF type = 6 THEN	(* string *)
			a := adr - rva + basePos; b := a + size;
			WHILE a < b DO W.WriteChar('"'); WriteResString(a); W.WriteString('" ') END;
			W.WriteLn
		ELSIF type = 14 THEN	(* icon group *)
			R.SetPos(adr - rva + basePos);
			ReadWord(x); ReadWord(type); ReadWord(count); i := 0;
			WHILE i < count DO
				R.ReadByte(ch); W.WriteInt(ch MOD 256); W.WriteString(" x ");
				R.ReadByte(ch); W.WriteInt(ch MOD 256); W.WriteString(" x ");
				R.ReadByte(ch); W.WriteInt(ch MOD 256); W.WriteString(", ");
				R.ReadByte(ch);
				ReadWord(x); W.WriteInt(x); W.WriteString(" planes ");
				ReadWord(x); W.WriteInt(x); W.WriteString(" bits ");
				ReadLong(x); W.WriteInt(x); W.WriteString(" bytes ");
				ReadWord(x); W.WriteInt(x);
				INC(i); W.WriteLn
			END
		ELSIF type = 12 THEN	(* cursor group *)
			R.SetPos(adr - rva + basePos);
			ReadWord(x); ReadWord(type); ReadWord(count); i := 0;
			WHILE i < count DO
				ReadWord(x); W.WriteInt(x); W.WriteString(" x ");
				ReadWord(x); W.WriteInt(x); W.WriteString(" x ");
				ReadWord(x); W.WriteInt(x); W.WriteString(", ");
				ReadWord(x); W.WriteInt(x); W.WriteString(" bits ");
				ReadLong(x); W.WriteInt(x); W.WriteString(" bytes ");
				ReadWord(x); W.WriteInt(x);
				INC(i); W.WriteLn
			END
		ELSIF type = 9 THEN	(* accelerator *)
			R.SetPos(adr - rva + basePos);
			REPEAT
				ReadWord(type); ReadWord(a); ReadWord(b); ReadWord(x);
				IF ODD(type DIV 2) THEN W.WriteString("noinv ") END;
				IF ODD(type DIV 4) THEN W.WriteString("Shift+") END;
				IF ODD(type DIV 8) THEN W.WriteString("Ctrl+") END;
				IF ODD(type DIV 16) THEN W.WriteString("Alt+") END;
				IF ODD(type) THEN W.WriteInt(a) ELSE W.WriteChar(CHR(a)) END;
				W.WriteString("  "); W.WriteInt(b); W.WriteLn
			UNTIL ODD(type DIV 80H)
		ELSE DecodeHex(adr - rva + basePos, size, adr)
		END
	END DecodeResData;
	
	PROCEDURE DecodeResDir (rva, basePos, pos, level: INTEGER);
		VAR x, ne, ie: INTEGER;
	BEGIN
		R.SetPos(basePos + pos);
		ReadLong(x); ReadLong(x); ReadLong(x);
		ReadWord(ne); ReadWord(ie); INC(ne, ie); INC(pos, 16);
		WHILE ne > 0 DO
			R.SetPos(basePos + pos);
			ReadLong(x); x := x MOD 10000000H;
			IF ne > ie THEN ResTable[level] := -x ELSE ResTable[level] := x END;
			ReadLong(x);
			IF x >= 0 THEN
				DecodeResData(rva, basePos, x, level)
			ELSE
				x := x MOD 10000000H;
				DecodeResDir(rva, basePos, x, level + 1)
			END;
			DEC(ne); INC(pos, 8)
		END;
	END DecodeResDir;
	
	PROCEDURE DecodeResourceObj(n: INTEGER);
		VAR x, a, i, len, ne, ie, firstName, lastName, firstData, lastData, lastDir: INTEGER;
	BEGIN
(*
		R.SetPos(Dir[n].pos);
		a := 0; lastDir := 0;
		firstName := MAX(LONGINT); lastName := 0;
		firstData := MAX(LONGINT); lastData := 0;
		REPEAT
			DumpLong("Characteristics: ");
			DumpLong("Time/Date Stamp: ");
			DumpLong("Version: ");
			DumpWord("Name Entries: "); ne := val;
			DumpWord("ID Entries: "); ie := val; ne := ne + val;
			INC(a, 16);
			WHILE ne > 0 DO
				IF ne > ie THEN
					DumpLong("    Name RVA: ");
					val := val MOD 10000000H;
					IF val < firstName THEN firstName := val END;
					IF val > lastName THEN lastName := val END
				ELSE
					DumpLong("    ID: ")
				END;
				DumpLong("    Entry RVA: ");
				IF val >= 0 THEN
					IF val < firstData THEN firstData := val END;
					IF val > lastData THEN lastData := val END
				ELSE
					val := val MOD 10000000H;
					IF val > lastDir THEN lastDir := val END
				END;
				DEC(ne); INC(a, 8)
			END
		UNTIL a > lastDir;
		W.WriteLn;
		IF firstData <= lastData THEN
			R.SetPos(Dir[n].pos + firstData); a := firstData;
			WHILE a <= lastData DO
				DumpLong("Data RVA: ");
				DumpLong("Size: ");
				DumpLong("Codepage: ");
				DumpLong("Reserved: ");
				INC(a, 16)
			END;
			W.WriteLn
		END;
		IF firstName <= lastName THEN
			R.SetPos(Dir[n].pos + firstName); a := firstName;
			WHILE a <= lastName DO
				WritePos; W.WriteString('String: "');
				ReadWord(len); INC(a, 2); i := 0;
				WHILE i < len DO ReadWord(x); INC(a, 2); W.WriteChar(CHR(x)); INC(i) END;
				W.WriteChar('"'); W.WriteLn
			END;
			W.WriteLn
		END;
*)
		DecodeResDir(Dir[n].rva, Dir[n].pos, 0, 0)
	END DecodeResourceObj;
	
	PROCEDURE DecodeFixupObj(n: INTEGER);
		VAR p, l, x, t, i: INTEGER;
	BEGIN
		R.SetPos(Dir[n].pos);
		REPEAT
			DumpLong("Page RVA: "); p := val;
			DumpLong("Block Size: "); l := val;
			WHILE l > 8 DO
				i := 0; W.WriteString("       ");
				WHILE (l > 8) & (i < 4) DO
					ReadWord(x);
					IF x # 0 THEN
						W.WriteString("    ");
						t := x DIV 4096 MOD 16;
						IF t = 0 THEN W.WriteString("abs  ")
						ELSIF t = 1 THEN W.WriteString("high ")
						ELSIF t = 2 THEN W.WriteString("low  ")
						ELSIF t = 3 THEN W.WriteString("long ")
						ELSIF t = 4 THEN W.WriteString("adj  ")
						ELSIF t = 5 THEN W.WriteString("mips ")
						ELSE W.WriteString("**** ")
						END;
						WriteHex(p + VBase + x MOD 4096, 8);
					END;
					INC(i); DEC(l, 2)
				END;
				W.WriteLn
			END
		UNTIL p = 0;
	END DecodeFixupObj;
	
	PROCEDURE DecodeObjects;
		VAR n: INTEGER;
	BEGIN
		n := 1;
		WHILE n <= NumOfObj DO
			IF ~ Dir[n].noinit THEN
				W.WriteLn;
				W.WriteString("***** Object #");
				W.WriteInt(n);
				W.WriteString(" *****"); 
				W.WriteLn;
				IF Dir[n].code THEN DecodeCodeObj(n)
				ELSIF Dir[n].rva = ExportRVA THEN DecodeExportObj(n)
				ELSIF Dir[n].rva = ImportRVA THEN DecodeImportObj(n)
				ELSIF Dir[n].rva = ResourceRVA THEN DecodeResourceObj(n)
				ELSIF Dir[n].rva = FixupRVA THEN DecodeFixupObj(n)
				ELSE DecodeDefaultObj(n);
					IF (Dir[n].rva <= ExportRVA) & (ExportRVA < Dir[n].rva + Dir[n].size) THEN
						Dir[n].size := Dir[n].size - ExportRVA + Dir[n].rva;
						Dir[n].pos := Dir[n].pos + ExportRVA - Dir[n].rva;
						Dir[n].rva := ExportRVA; 
						DecodeExportObj(n)
					END
				END
			END;
			INC(n)
		END
	END DecodeObjects;
	
	PROCEDURE DecodeFile;
		VAR x: INTEGER;
	BEGIN
		R.SetPos(3CH);
		WritePos; ReadLong(x);
		W.WriteString("Header Offset: ");
		WriteHex(x, 8); W.WriteLn; W.WriteLn;
		R.SetPos(x);
		DecodeHeader;
		DecodeObjTable;
		DecodeObjects
	END DecodeFile;
	
	PROCEDURE Decode* (path, name: ARRAY OF CHAR);
		VAR T: TextModels.Model; p, n: Files.Name; loc: Files.Locator; tit: Views.Title;
	BEGIN
		p := path$; n := name$;
		loc := Files.dir.This(p);
		F := Files.dir.Old(loc, n, FALSE);
		T := TextModels.dir.New();
		W.ConnectTo(T); W.SetPos(0);
		DecCode := TRUE;
		W.WriteString("********** ");
		W.WriteString(name);
		W.WriteString(" **********");
		W.WriteLn; W.WriteLn;
		R := F.NewReader(NIL);
		DecodeFile;
		R := NIL; W.ConnectTo(NIL); F.Close; F := NIL;
		tit := name$;
		Views.OpenAux(TextViews.dir.New(T), tit)
	END Decode;

	PROCEDURE Browse* (path, name: ARRAY OF CHAR);
		VAR T: TextModels.Model; p, n: Files.Name; loc: Files.Locator; tit: Views.Title;
	BEGIN
		p := path$; n := name$;
		loc := Files.dir.This(p);
		F := Files.dir.Old(loc, n, FALSE);
		T := TextModels.dir.New();
		W.ConnectTo(T); W.SetPos(0);
		DecCode := FALSE;
		W.WriteString("********** ");
		W.WriteString(name);
		W.WriteString(" **********");
		W.WriteLn; W.WriteLn;
		R := F.NewReader(NIL);
		DecodeFile;
		R := NIL; W.ConnectTo(NIL); F.Close; F := NIL;
		tit := name$;
		Views.OpenAux(TextViews.dir.New(T), tit)
	END Browse;

END DevDecExe.
