MODULE LinLinker;

	(* bj	19.10.2001	moved the boot image from separate file to the end of the executable *)
	(* bj	20.04.2001	first implementation *)
	
	(* See implementation notes in document Lin/Docu/LinkerImplementation *)
	
	IMPORT Kernel, LinLibc, Dialog, Files, Log, TextMappers, DevCommanders;
	
	CONST 
		NewRecFP = 00000048H;
		NewArrFP = 000004A8H;
		
		fileTag = 3A4B5C6DH; version = 0; ext = "";
		bufsize = 10*1024;

	TYPE 
		Module = POINTER TO RECORD
			name: Files.Name;
			next: Module
		END;
	
	PROCEDURE Write4 (wr: Files.Writer; x: INTEGER);
	BEGIN
		wr.WriteByte(SHORT(SHORT(x MOD 256))); x := x DIV 256;
		wr.WriteByte(SHORT(SHORT(x MOD 256))); x := x DIV 256;
		wr.WriteByte(SHORT(SHORT(x MOD 256))); x := x DIV 256;
		wr.WriteByte(SHORT(SHORT(x MOD 256)))
	END Write4;
	
	PROCEDURE WriteString (wr: Files.Writer; s: ARRAY OF CHAR);
		VAR i: SHORTINT;
	BEGIN 
		i := 0;
		WHILE s[i] # 0X DO wr.WriteByte(SHORT(SHORT(ORD(s[i])))); INC(i) END;
		wr.WriteByte(0)
	END WriteString;
	
	PROCEDURE WriteExe(IN fileName: Files.Name; modlist, kernel, main: Module; nofMods: INTEGER;VAR error: BOOLEAN);
		VAR 
			m: Module; exe, img, f: Files.File;wr: Files.Writer; r: Files.Reader;
			head, tail: Files.Name; b: ARRAY bufsize OF BYTE; res, len: INTEGER; ss: ARRAY LEN(Files.Name) OF SHORTCHAR;
	BEGIN
		IF error THEN RETURN END;
		exe := Files.dir.New(Files.dir.This(""), Files.dontAsk);
		IF exe # NIL THEN
			wr := exe.NewWriter(NIL); wr.SetPos(0);
			img := Files.dir.Old(Files.dir.This("Lin").This("Rsrc"), "exe.img", Files.shared);
			IF img = NIL THEN
				Log.String("image file (Lin/Rsrc/exe.img) not found"); Log.Ln;
				res := -1
			ELSE
				(* copy the exe image *)
				r := img.NewReader(r);
				r.SetPos(0);
				len := MIN(r.Base().Length(), bufsize);
				WHILE len > 0 DO
					r.ReadBytes(b, 0, len); 
					wr.WriteBytes(b, 0, len);
					len := MIN(r.Base().Length() - r.Pos(), bufsize)
				END;
				(* write header *)
				Write4(wr, fileTag); Write4(wr, version); Write4(wr, nofMods);
				WriteString(wr, kernel.name); WriteString(wr, main.name);
				Write4(wr, NewRecFP); Write4(wr, NewArrFP);
				(* write object files *)
				m := modlist;
				WHILE (m # NIL) & ~error DO
					Kernel.SplitName(m.name, head, tail); 
					IF head = "" THEN
						f := Files.dir.Old(Files.dir.This("Code"), tail + ".ocf", Files.shared); 
						IF f = NIL THEN 
							head := "System";
							f := Files.dir.Old(Files.dir.This(head + "/Code"), tail + ".ocf", Files.shared)
						END;
					ELSE
						f := Files.dir.Old(Files.dir.This(head + "/Code"), tail + ".ocf", Files.shared);
					END;
					IF f = NIL THEN
						Log.String("code file for: " + m.name + "  not found (" + head + "/Code/" + tail + ".ocf)"); Log.Ln;
						error := TRUE
					ELSE
						r := f.NewReader(r);
						r.SetPos(0);
						len := MIN(r.Base().Length(), bufsize);
						WHILE len > 0 DO
							r.ReadBytes(b, 0, len); 
							wr.WriteBytes(b, 0, len);
							len := MIN(r.Base().Length() - r.Pos(), bufsize)
						END
					END;
					m := m.next
				END;
				IF ~error THEN 
					exe.Register(fileName, ext, Files.dontAsk, res);
					IF res = 0 THEN
						(* make it executable *)
						ss := SHORT(fileName);
						LinLibc.chmod(ss, {0, 2, 3, 4, 5, 6, 7, 8}) (* rwxrwxr-x*)
					END
				END
			END;
		END;
		IF (exe = NIL) OR (res # 0) THEN
			Log.Int(res); Log.String(" - couldn't create a new file"); Log.Ln;
			error := TRUE
		END;
		Kernel.Cleanup
	END WriteExe;
		
	PROCEDURE ValidateModList(modlist: Module; VAR error: BOOLEAN);
		VAR m: Module; f: Files.File; head, tail: Files.Name;
	BEGIN
		IF error THEN RETURN END;
		m := modlist;
		WHILE (m # NIL) & ~error DO
			Kernel.SplitName(m.name, head, tail);
			IF head = "" THEN head := "System" END;
			f := Files.dir.Old(Files.dir.This(head + "/Code"), tail + ".ocf", Files.shared);
			(* TODO: validate the order of the imports. *)
			IF f = NIL THEN
				Log.String("code file for: " + m.name + "  not found (" + head + "/Code/" + tail + ".ocf)"); Log.Ln;
				error := TRUE
			END;
			m := m.next
		END
	END ValidateModList;
		
	PROCEDURE BuildModList(s: TextMappers.Scanner; end: INTEGER; 
										OUT modlist, kernel, main: Module; OUT nofMods: INTEGER;VAR error: BOOLEAN);
		VAR m, l: Module;
	BEGIN 
		l := NIL; modlist := NIL; kernel := NIL; main := NIL; nofMods := 0;
		WHILE (s.Pos() < end) & ~error DO
			s.Scan;
			IF s.type = TextMappers.string THEN
				NEW(m); m.name := s.string$; INC(nofMods);
				IF modlist = NIL THEN modlist := m ELSE l.next := m END;
				 l := m;
			ELSIF (s.type = TextMappers.char) & (s.char = "+") THEN
				kernel := l;
			ELSIF (s.type = TextMappers.char) & (s.char = "$") THEN
				main := l;
			ELSE
				Log.String("unknown symbols in list"); Log.Ln;
				error := TRUE;
			END
		END
	END BuildModList;

	PROCEDURE Link*;
		VAR 
			s: TextMappers.Scanner; name: Files.Name; end: INTEGER; error: BOOLEAN;
			modlist, kernel, main: Module; nofMods: INTEGER;
	BEGIN
		Dialog.ShowStatus("linking");
		IF DevCommanders.par = NIL THEN Log.String("unable to parse link command"); Log.Ln; RETURN END;
		s.ConnectTo(DevCommanders.par.text);
		s.SetPos(DevCommanders.par.beg);
		end := DevCommanders.par.end;
		DevCommanders.par := NIL;
		error := TRUE;
		s.Scan;
		IF s.type = TextMappers.string THEN
			name := s.string$; s.Scan;
			IF (s.type = TextMappers.char) & (s.char = ".") THEN s.Scan;
				IF s.type = TextMappers.string THEN
					Kernel.MakeFileName(name, s.string); s.Scan
				END
			ELSE (*Kernel.MakeFileName(name, ext);*)
			END;
			IF (s.type = TextMappers.char) & (s.char = ":") THEN s.Scan;
				IF (s.type = TextMappers.char) & (s.char = "=") THEN
					error := FALSE;
					BuildModList(s, end, modlist, kernel, main, nofMods, error);
					IF modlist = NIL THEN Log.String("no modules to link"); Log.Ln; error := TRUE 
					ELSIF kernel = NIL THEN Log.String("no kernel given"); Log.Ln; error := TRUE
					ELSIF main = NIL THEN Log.String("no main module given"); Log.Ln; error := TRUE END;
					ValidateModList(modlist, error);
					WriteExe(name, modlist, kernel, main, nofMods, error);
				ELSE Log.String(" := missing"); Log.Ln
				END
			ELSE Log.String(" := missing"); Log.Ln
			END;
		ELSE Log.String("no modules to link"); Log.Ln
		END;
		IF error THEN Dialog.ShowStatus("failed") ELSE Dialog.ShowMsg("wrote: " + name); Dialog.ShowStatus("ok") END;
		modlist := NIL; kernel := NIL; main := NIL;
	END Link;
	
END LinLinker.
