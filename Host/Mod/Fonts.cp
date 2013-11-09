MODULE HostFonts;
(** cp, bh 14.01.02 *)

	(* TODO: 
		- Can the loading (MakeXlfdString) be done better? (see Lin/Spec/xlfd.ps)
			- Can the WEIGHT propery be used instead of "bold" or "medium"?
			- Which character set should be used? Always iso8859-1 or also *?
		- How can True Type fonts be used? There is something called FreeType, and a TrueType server called xfstt.
		- Use HostRegistry again
		- Fix the sysFont
	 *)
		
	(*
		Implementation docu
		On Windows typeface names only contain the family name. On Linux they also contain the foundary. They are
		separated by a "-" and they are on the form: "family-foundary". Wherever typfaces are handled, both cases
		has to be handled.
	*)


	(* bj  17.12.01	Linux version *)
	(* dg 14.10.98	bug fix (8.25 sized fonts) *)
	(* bh 08.01.96	new alien handling *)
	
	IMPORT SYSTEM, LinLibc, LinX11, LinGdk, Kernel, Fonts(*, HostRegistry*), HostGnome, Strings;

	CONST
		defSize = 8 * Fonts.point;	(* size of default font *)
		grid = 4096;	(* true type design grid *)
		figureSpace = 8FX;

	TYPE
		WTab = ARRAY 256 OF INTEGER;
		DevFont* = POINTER TO RECORD
			unit-: INTEGER;
			id-: LinGdk.GdkFont;
			next-: DevFont;
			noGap-: BOOLEAN;
			wtab-: WTab;	(* rastered width in pixels *)
			xlfd-: XlfdString; (* holds the string that was used to load the font *)
		END;
		Font* = POINTER TO RECORD (Fonts.Font)
			asc-, dsc-, w-: INTEGER;
			dev-: DevFont;	(* rastered fonts *)
			wtab-, ftab-, ttab-: WTab;	(* univeral width in units *)
			id-: LinGdk.GdkFont;	(* font used for metric*)
			alias-: Fonts.Typeface;	(* alias # typeface & typeface # "*" == alien font *)
			a, b: INTEGER	(* coefficients for metric *)
		END;

		Directory = POINTER TO RECORD (Fonts.Directory) END;
		
		Identifier = RECORD (Kernel.Identifier)
			tface: Fonts.Typeface;
			size: INTEGER;
			style: SET;
			weight: INTEGER;
		END;

		Counter = RECORD (Kernel.Identifier)
			count: INTEGER
		END;
		
		Traverser = RECORD (Kernel.Identifier)
		END;
		
		Par = RECORD [untagged]
			first, last: Fonts.TypefaceInfo
		END;
		ParPtr = POINTER TO Par;
		
		XlfdString* = ARRAY 256 OF SHORTCHAR;

	VAR
		sysFont-, defFont-, dlgFont-, dlgBoldFont-: Font;
		isUnicode-, useTTMetric-: BOOLEAN;
		dfName, dgName: Fonts.Typeface;
		dfSize, dgSize, dgWght: INTEGER;
		dgStyle: SET;
		dir: Directory;
		defUnit: INTEGER;	(* screen resolution *)
	

	PROCEDURE IntToString (x: INTEGER; OUT s: ARRAY OF SHORTCHAR);
		VAR j, k: INTEGER; ch: SHORTCHAR; a: ARRAY 32 OF SHORTCHAR;
	BEGIN
		ASSERT(x >= 0, 20);
		IF x < 0 THEN s[0] := "-"; k := 1; x := -x ELSE k := 0 END;
		j := 0; REPEAT a[j] := SHORT(CHR(x MOD 10 + ORD("0"))); x := x DIV 10; INC(j) UNTIL x = 0;
		ASSERT(k + j < LEN(s), 20);
		REPEAT DEC(j); ch := a[j]; s[k] := ch; INC(k) UNTIL j = 0;
		s[k] := 0X
	END IntToString;
	
	(* Parses an XLFD string and retreivs the attributes. *)
	PROCEDURE ParseXlfd* (IN xlfd: XlfdString; unit: INTEGER; OUT typeface: Fonts.Typeface; 
										OUT size: INTEGER; OUT style: SET; OUT weight: INTEGER);
		VAR i, cpos, res: INTEGER; s: Fonts.Typeface;
		
		PROCEDURE NextString;
		BEGIN
			s := ""; i := 0;
			IF xlfd[cpos] = "-" THEN INC(cpos) END;
			WHILE (xlfd[cpos] # 0X) & (xlfd[cpos] # "-") DO s[i] := xlfd[cpos]; INC(cpos); INC(i) END;
			s[i] := 0X
		END NextString;
		
	BEGIN
		cpos := 0;
		typeface := ""; style := {};
		NextString; 
		IF s # "*" THEN typeface := "-" + s END;
		NextString;
		typeface :=  s + typeface;
		NextString;
		IF s = "bold" THEN weight := Fonts.bold ELSE weight := Fonts.normal END;
		NextString;
		IF (s = "i") OR (s = "o") THEN INCL(style, Fonts.italic) END;
		NextString;
		NextString;
		NextString;
		res := -1;
		IF s # "*" THEN
			Strings.StringToInt(s, size, res);
		END;
		IF res = 0 THEN
			size := size * unit
		ELSE
			NextString;
			Strings.StringToInt(s, size, res);
			IF res = 0 THEN
				size := size * Fonts.point
			ELSE
				size := defSize
			END
		END
	END ParseXlfd;

	(* Crteates a valid XLFD string. Such a string can be used to load a font. *)
	PROCEDURE MakeXlfdString* (typeface: Fonts.Typeface; size, unit: INTEGER; style: SET; weight: INTEGER; 
											OUT xlfd: XlfdString);
		VAR 
			foundary, family: ARRAY LEN(Fonts.Typeface) OF SHORTCHAR; 
			ssize, sbold, sitalic: ARRAY 10 OF SHORTCHAR;
			i, j: INTEGER; ok: BOOLEAN;
			
		PROCEDURE Test;
			VAR xFontNames: LinLibc.StrArray; numFonts: INTEGER;
		BEGIN 
			(* 0:s to get scalable fonts *)
			xlfd := "-" + foundary + "-" + family + "-" + sbold + "-" + sitalic + "-*-*-" + ssize + "-0-0-0-*-0-iso8859-1";
			xFontNames := LinX11.XListFonts(LinGdk.gdk_display, xlfd, 1, numFonts);
			IF xFontNames # NIL THEN
				xlfd := xFontNames[0]$;
				LinX11.XFreeFontNames (xFontNames);
				ok := TRUE;
			ELSE
				ok := FALSE;
			END
		END Test;
		
	BEGIN
		i := 0; ok := FALSE; xlfd := "";
		IF (typeface = "") OR (typeface = ".") OR (typeface = "*") THEN
			typeface := dfName$
		END;
		WHILE (typeface[i] # 0X) & (typeface[i] # "-") DO family[i] := SHORT(typeface[i]); INC(i) END;
		family[i] := 0X;
		IF typeface[i] = "-" THEN (* foundary was included in the typeface name *)
			INC(i); j := 0;
			WHILE typeface[i] # 0X DO foundary[j] := SHORT(typeface[i]); INC(i); INC(j) END;
			foundary[j] := 0X
		ELSE (* foundary was not included in the typeface name *)
			foundary := "*"
		END;
		IF weight = Fonts.bold THEN sbold := "bold" ELSE sbold := "medium" END;
		IF Fonts.italic IN style THEN sitalic := "i" ELSE sitalic := "r" END;
		IntToString((size + unit DIV 2) DIV unit, ssize);
		Test;
		IF ~ok & (Fonts.italic IN style) THEN (* first try to change italic to oblique *)
			sitalic := "o";
			Test;
		END;
		IF ~ok THEN (* try without italic and with any foundary *)
			sitalic := "*"; foundary := "*";
			Test
		END;
		IF ~ok THEN (* remove family *)
			family := "*";
			Test;
		END;
		IF ~ok THEN (* remove bold or medium *)
			sbold := "*";
			Test
		END;
		IF ~ok THEN (* remove size *)
			ssize := "*";
			Test;
		END;
		IF ~ok THEN (* go for the default font *)
			xlfd := "-*"
		END
	END MakeXlfdString;

	(* width tab setup *)
	
	PROCEDURE NewDevFont (typeface: Fonts.Typeface; size, unit: INTEGER; style: SET; weight: INTEGER): DevFont;
		VAR df: DevFont;
	BEGIN
		IF size = 8 * Fonts.point THEN INC(size, Fonts.point DIV 4) END;
		NEW(df); df.unit := unit; df.next := NIL;
		MakeXlfdString(typeface, size, unit, style, weight, df.xlfd);
		df.id := LinGdk.gdk_fontset_load( df.xlfd);
(*		df.id := LinGdk.gdk_font_ref(df.id); TODO: Necessary? *)
		RETURN df
	END NewDevFont;
	
	PROCEDURE GetRasterWidth (fid: LinGdk.GdkFont; VAR wtab: WTab);
		VAR i: INTEGER;
	BEGIN
		i := 0; 
		WHILE i < 256 DO
			wtab[i] := LinGdk.gdk_char_width(fid, SHORT(CHR(i)));
			INC(i)
		END
	END GetRasterWidth;
	
	PROCEDURE SetupWTabs (f: Font);
		CONST isTrueType = FALSE; (* TODO: Implement true type support *)
		VAR a, b, max, x, i: INTEGER; df: DevFont;
	BEGIN
		IF useTTMetric & isTrueType THEN	(* use true type metric *)
			df := NewDevFont(f.alias, grid, 1, f.style, f.weight);
			a := f.size MOD grid; b := f.size DIV grid; f.id := df.id;
			(*
			IF res # 0 THEN
				i := 0; max := 0;
				WHILE i < 256 DO
					x := -abc[i].a;
					IF x > 0 THEN f.ftab[i] := x * a DIV grid + x * b END;
					x := -abc[i].c;
					IF x > 0 THEN f.ttab[i] := x * a DIV grid + x * b END;
					x := abc[i].a + abc[i].b + abc[i].c; x := x * a DIV grid + x * b;
					IF x > max THEN max := x END;
					f.wtab[i] := x; INC(i)
				END
			ELSE
				max := f.w
			END
				*)
		ELSE	(* use screen metric *)
			a := 0; b := defUnit; f.id := f.dev.id;
			GetRasterWidth(f.id, f.wtab);
			i := 0; max := 0;
			WHILE i < 256 DO
				x := f.wtab[i] * b;
				IF x > max THEN max := x END;
				f.wtab[i] := x; INC(i)
			END
		END;
		f.wtab[ORD(figureSpace)] := f.wtab[ORD("0")];
		f.ftab[ORD(figureSpace)] := f.ftab[ORD("0")];
		f.ttab[ORD(figureSpace)] := f.ttab[ORD("0")];
		f.asc := f.id.ascent * b;
		f.dsc := f.id.descent * b;
		f.w := max; f.a := a; f.b := b;
	END SetupWTabs;
	
	PROCEDURE Cleanup (f: Font);
		VAR df: DevFont;
	BEGIN
		df := f.dev;
		IF f.id # df.id THEN LinGdk.gdk_font_unref(f.id) END;
		WHILE df # NIL DO
			IF df.id # NIL THEN LinGdk.gdk_font_unref(df.id) END;
			df := df.next
		END;
		f.id := NIL; f.dev := NIL
	END Cleanup;
	

	(* width methods for unicode *)
	
	PROCEDURE (f: Font) wTab* (ch: CHAR): INTEGER, NEW;
		VAR i, w: INTEGER;
	BEGIN		
		i := ORD(ch);
		w := LinGdk.gdk_char_width_wc(f.id, i);
		w := w * f.a DIV grid + w * f.b;
		RETURN w
	END wTab;

	PROCEDURE (f: Font) fTab* (ch: CHAR): INTEGER, NEW;
	BEGIN
		(* For TrueType fonts this is probably more complex *)
		RETURN 0
	END fTab;

	PROCEDURE (f: Font) tTab* (ch: CHAR): INTEGER, NEW;
	BEGIN
		(* For TrueType fonts this is probably more complex *)
		RETURN 0
	END tTab;
	

	(** Font **)

	PROCEDURE (f: Font) GetBounds* (OUT asc, dsc, w: INTEGER);
	BEGIN
		asc := f.asc; dsc := f.dsc; w := f.w
	END GetBounds;
	
	PROCEDURE (f: Font) SStringWidth* (IN s: ARRAY OF SHORTCHAR): INTEGER;
		VAR i, w: INTEGER; ch: CHAR;
	BEGIN
		w := 0;
		IF s # "" THEN
			i := 0; ch := s[0];
			WHILE ch # 0X DO INC(w, f.wtab[ORD(ch)]); INC(i); ch := s[i] END;
			w := w + f.ftab[ORD(s[0])] + f.ttab[ORD(s[i-1])]
		END;
		RETURN w
	END SStringWidth;

	PROCEDURE (f: Font) StringWidth* (IN s: ARRAY OF CHAR): INTEGER;
		VAR i, w: INTEGER; lc: CHAR; is: ARRAY [untagged] 1024 OF INTEGER;
	BEGIN
		IF isUnicode THEN
			w := 0;
			IF s[0] # 0X THEN
				i := 0; lc := s[0];
				WHILE lc # 0X DO INC(w, f.wTab(lc)); INC(i); lc := s[i] END;
				w := w + f.fTab(s[0]) + f.tTab(s[i-1])
			END;
			RETURN w
		ELSE
			FOR i := 0 TO LEN(s$) DO is[i] := ORD(s[i]) END;
			RETURN f.SStringWidth(SHORT(LinGdk.gdk_wcstombs(is)$))
		END
	END StringWidth;

	PROCEDURE (f: Font) IsAlien* (): BOOLEAN;
	BEGIN
		RETURN (f.typeface # Fonts.default) & (f.alias # f.typeface)
	END IsAlien;
	
	PROCEDURE (f: Font) FINALIZE-;
	BEGIN
		Cleanup(f)
	END FINALIZE;
	


	(* Directory *)
	
	
	PROCEDURE SetupDevFont (df: DevFont);
		CONST isTrueType = FALSE;
	BEGIN
		IF isTrueType THEN
	(*
			df.noGap := (res # 0) & (abc[0].a <= 0);
			res := GDI32.GetCharWidth32A(dc, 0, 255, df.wtab);
			IF res = 0 THEN	(* win32s *)
				res := GDI32.GetCharWidthA(dc, 0, 255, df.wtab)
			END
	*)
		ELSE	(* raster *)
			df.noGap := FALSE;
			GetRasterWidth(df.id, df.wtab);
		END;
		df.wtab[ORD(figureSpace)] := df.wtab[ORD("0")]
	END SetupDevFont;
	
	PROCEDURE InsertDevFont* (font: Font; VAR df: DevFont; unit: INTEGER);
	BEGIN
		df := NewDevFont(font.alias, font.size, unit, font.style, font.weight);
		SetupDevFont(df);
		df.next := font.dev.next; font.dev.next := df	(* screen font remains at list head *)
	END InsertDevFont;
	
	PROCEDURE SetupAlias (f: Font; IN typeface: ARRAY OF CHAR; IN xlfd: XlfdString);
		VAR foundary, family: ARRAY LEN(Fonts.Typeface) OF SHORTCHAR; i, j: INTEGER;
	BEGIN
		IF (typeface = "*") OR (typeface = "") OR (typeface = ".") THEN
			f.alias := dfName
		ELSE
			i := 1; j := 0;
			WHILE (xlfd[i] # 0X) & (xlfd[i] # "-") DO foundary[j] := xlfd[i]; INC(i); INC(j) END;
			foundary[j] := 0X;
			INC(i); j := 0;
			WHILE (xlfd[i] # 0X) & (xlfd[i] # "-") DO family[j] := xlfd[i]; INC(i); INC(j) END;
			family[j] := 0X;
			IF family = typeface THEN (* only family in typeface name *)
				f.alias := SHORT(typeface)
			ELSE
				IF foundary # "" THEN 
					f.alias := family + "-" + foundary
				ELSE
					f.alias := family$
				END
			END
		END
	END SetupAlias;
	
	PROCEDURE Setup (f: Font; typeface: ARRAY OF CHAR; size: INTEGER; style: SET; weight: INTEGER);
		VAR name: Fonts.Typeface; 
	BEGIN
		SetupAlias(f, typeface, f.dev.xlfd);
		name := f.alias;
		IF typeface = Fonts.default THEN
			name := Fonts.default
		ELSIF (typeface = "") OR (typeface = ".") THEN (* System or default font. Get attributes from the loaded font. *)
			ParseXlfd(f.dev.xlfd, defUnit, name, size, style, weight);
			IF typeface = "." THEN name := Fonts.default END
		ELSIF name # typeface THEN (* the correct font could not be loaded, so use the default font *)
			LinGdk.gdk_font_unref(f.dev.id);
			f.dev := NewDevFont(dfName, size, defUnit, style, weight);
			f.alias := dfName$;
			name := typeface$
		END;
		IF size # 0 THEN
			SetupDevFont(f.dev);
			IF f.size = 0 THEN f.Init(name, size, style, weight) END;
			SetupWTabs(f)
		END;
		ASSERT(f.size > 0)
	END Setup;
	
	PROCEDURE (VAR id: Identifier) Identified (): BOOLEAN;
		VAR f: Font;
	BEGIN
		f := id.obj(Font);
		RETURN (f.typeface = id.tface) & (f.size = id.size) & (f.style = id.style) & (f.weight = id.weight)
	END Identified;

	PROCEDURE (d: Directory) This (typeface: Fonts.Typeface; size: INTEGER; style: SET; weight: INTEGER): Font;
		VAR f: Font; i: Identifier; p: ANYPTR;
	BEGIN
		ASSERT(size > 0, 20);
		style := style * {Fonts.italic, Fonts.underline, Fonts.strikeout};
		size := size - size MOD Fonts.point;
		(* IF size = 8 * Fonts.point THEN INC(size, Fonts.point DIV 4) END; *)
		IF typeface = "L Frutiger Light" THEN typeface := "Frutiger 45 Light" 
		ELSIF typeface = "R Frutiger Roman" THEN typeface := "Frutiger 55 Roman"
		ELSIF typeface = "B Frutiger Black" THEN typeface := "Frutiger 55 Roman"; weight := Fonts.bold
		END;
		i.tface := typeface$; i.size := size; i.style := style; i.weight := weight;
		i.typ := SYSTEM.TYP(Font);
		p := Kernel.ThisFinObj(i);
		IF p # NIL THEN f := p(Font)
		ELSE	(* not found in cache, search Windows fonts *)
			IF typeface = "" THEN
				f := sysFont
			ELSE
				NEW(f);
				IF typeface = Fonts.default THEN
					f.dev := NewDevFont(dfName, size, defUnit, style, weight)
				ELSE
					f.dev := NewDevFont(typeface, size, defUnit, style, weight)
				END;
				Setup(f, typeface, size, style, weight)
			END
		END;
		RETURN f
	END This;

	PROCEDURE (d: Directory) Default (): Fonts.Font;
	BEGIN
		RETURN defFont
	END Default;

	PROCEDURE (d: Directory) TypefaceList* (): Fonts.TypefaceInfo;
		CONST maxFonts = 32767; (* taken from the Gtk FontPicker *)
		VAR xFontNames: LinLibc.StrArray; numFonts, fmLen, fndLen, i, j, k: INTEGER;
			typefaceInfo, t, q: Fonts.TypefaceInfo; 
			familyName, foundryName: ARRAY 256 OF CHAR;
	BEGIN
		xFontNames := LinX11.XListFonts(LinGdk.gdk_display, "-*", maxFonts, numFonts);
		i := 0; typefaceInfo := NIL;
		WHILE i < numFonts DO
			IF xFontNames[i][0] = "+" THEN
				(* The XLFD standard calls for the possibility of a version string *)
				(* beginning with "+" prepended to the current XLFD name, and *)
				(*  reserved for possible future use. For now we omit these names. *)
				(* If they appear in the future we may need to include them. *)
			ELSIF xFontNames[i][0] = "-" THEN
				(* First char must be "-". Skip illegal XLFD name. *)
				j := 0; k := 1;
				WHILE (k < 255) & (xFontNames[i][k] # "-") DO
					foundryName[j] := Strings.Lower(xFontNames[i][k]); INC(j); INC(k)
				END;
				foundryName[j] := 0X;
				j := 0; INC(k);
				WHILE (k < 255) & (xFontNames[i][k] # "-") DO
					familyName[j] := Strings.Lower(xFontNames[i][k]); INC(j); INC(k)
				END;
				familyName[j] := 0X;
				fmLen := LEN(familyName$); fndLen := LEN(foundryName$);
				IF fmLen < LEN(Fonts.Typeface) THEN
					IF fmLen + fndLen + 1 < LEN(Fonts.Typeface) THEN
						familyName := familyName + "-" + foundryName
					END;
					q := NIL; t := typefaceInfo;
					WHILE (t # NIL) & (t.typeface < familyName) DO
						q := t; t := t.next
					END;
					IF q = NIL THEN
						IF (t = NIL) OR (t.typeface # familyName) THEN (* at front *)
							NEW(q); q.typeface := familyName$;
							q . next := t; typefaceInfo := q
						END
					ELSIF (t = NIL) OR (t.typeface # familyName) THEN (* after q *)
						NEW(t); t.typeface := familyName$;
						t.next := q.next; q.next := t
					END
				END;
			END;
			INC(i)
		END;
		LinX11.XFreeFontNames (xFontNames);
		RETURN typefaceInfo
	END TypefaceList;



	(** miscellaneous **)

	PROCEDURE (VAR id: Counter) Identified (): BOOLEAN;
	BEGIN
		INC(id.count); RETURN FALSE
	END Identified;

	PROCEDURE NofFonts* (): INTEGER;
		VAR p: ANYPTR; cnt: Counter;
	BEGIN
		cnt.typ := SYSTEM.TYP(Font); cnt.count := 0; p := Kernel.ThisFinObj(cnt);
		RETURN cnt.count
	END NofFonts;

	PROCEDURE InstallDir*;
	BEGIN
		Fonts.SetDir(dir)
	END InstallDir;
	

	PROCEDURE (VAR id: Traverser) Identified (): BOOLEAN;
		VAR f: Font;
	BEGIN
		f := id.obj(Font);
		IF (f.typeface = Fonts.default) & (f.alias # dfName) THEN
			Cleanup(f);
			f.dev := NewDevFont(dfName, f.size, defUnit, f.style, f.weight);
			Setup(f, Fonts.default, f.size, f.style, f.weight)
		ELSE
			SetupWTabs(f)
		END;
		RETURN FALSE
	END Identified;

	PROCEDURE SetTTMetric* (on: BOOLEAN);
		VAR t: Traverser; p: ANYPTR;
	BEGIN
		IF useTTMetric # on THEN
			useTTMetric := on;
			t.typ := SYSTEM.TYP(Font); p := Kernel.ThisFinObj(t);
(* TODO:			HostRegistry.WriteBool("FontTTMetric", useTTMetric)*)
		END
	END SetTTMetric;
	
	PROCEDURE SetDefaultFont* (tf: Fonts.Typeface; size: INTEGER);
		VAR s: ARRAY LEN(Fonts.Typeface) OF SHORTCHAR; t: Traverser; p: ANYPTR;
	BEGIN
		ASSERT(tf # "", 20); ASSERT(size > 0, 21);
		IF tf = Fonts.default THEN tf := dfName$ END;
		IF (dfName # tf) OR (dfSize # size) THEN
			dfName := tf$; dfSize := size;
			t.typ := SYSTEM.TYP(Font); p := Kernel.ThisFinObj(t);
			defFont := dir.This(Fonts.default, dfSize, {}, Fonts.normal);
			s := SHORT(dfName$);
(* TODO:			HostRegistry.WriteString("DefFontName", s);*)
(* TODO:			HostRegistry.WriteInt("DefFontSize", dfSize)*)
		END
	END SetDefaultFont;

	PROCEDURE SetDialogFont* (tf: Fonts.Typeface; size: INTEGER; style: SET; weight: INTEGER);
		VAR s: ARRAY LEN(Fonts.Typeface) OF SHORTCHAR; i: INTEGER;
	BEGIN
		ASSERT(tf # "", 20); ASSERT(size > 0, 21);
		IF (dgName # tf) OR (dgSize # size) OR (dgStyle # style) OR (dgWght # weight) THEN
			dgName := tf$; dgSize := size; dgStyle := style; dgWght := weight;
			dlgFont := dir.This(dgName, dgSize, dgStyle, dgWght);
			dlgBoldFont := dir.This(dgName, dgSize, dgStyle, Fonts.bold);
			s := SHORT(dgName$);
(* TODO:			HostRegistry.WriteString("DlgFontName", s);*)
(* TODO:			HostRegistry.WriteInt("DlgFontSize", dgSize);*)
			i := 0;
			IF Fonts.italic IN dgStyle THEN INC(i, 1) END;
			IF Fonts.underline IN dgStyle THEN INC(i, 2) END;
			IF Fonts.strikeout IN dgStyle THEN INC(i, 4) END;
			IF dgWght > Fonts.normal THEN INC(i, 8) END;
(* TODO:			HostRegistry.WriteInt("DlgFontStyle", i);*)
		END
	END SetDialogFont;


	PROCEDURE Init;
		VAR i: INTEGER; s: ARRAY 2 OF CHAR; df: ARRAY LEN(Fonts.Typeface) OF SHORTCHAR;
	BEGIN
		df := ""; dfSize := 0; dgSize := 0; dgStyle := {}; dgWght := Fonts.normal; i := 0;
		
		(* TODO: remove this when HostRegistry works *)
		dfName := "helvetica"; dfSize := defSize;
		dgName := "helvetica"; dgSize := defSize;
		i := 0;
(* TODO:		
		HostRegistry.ReadString("DefFontName", df, res); dfName := df$;
		HostRegistry.ReadInt("DefFontSize", dfSize, res);
		HostRegistry.ReadString("DlgFontName", df, res); dgName := df$;
		HostRegistry.ReadInt("DlgFontSize", dgSize, res);
		HostRegistry.ReadInt("DlgFontStyle", i, res);
*)		
		IF ODD(i) THEN INCL(dgStyle, Fonts.italic) END;
		IF ODD(i DIV 2) THEN INCL(dgStyle, Fonts.underline) END;
		IF ODD(i DIV 4) THEN INCL(dgStyle, Fonts.strikeout) END;
		IF ODD(i DIV 8) THEN dgWght := Fonts.bold END;
(* TODO:		HostRegistry.ReadBool("FontTTMetric", useTTMetric, res);*)
		NEW(dir); Fonts.SetDir(dir);
		defUnit := (Fonts.mm * HostGnome.ScreenHeightMM()) DIV LinGdk.gdk_screen_height();
		
(*		isUnicode := GDI32.TextOutW(dc, 0, 0, s, 0) # 0;*)
		isUnicode := TRUE;  (* TODO: is this correct? *)

(* TODO: Setup sysFont...if it is ever used...
		NEW(sysFont); NEW(sysFont.dev); sysFont.dev.unit := defUnit; sysFont.dev.next := NIL;
		sysFont.dev.id := GDI32.GetStockObject(GDI32.SystemFont);
		Setup(sysFont, "", 0, {}, 0);
*)
		NEW(defFont); NEW(defFont.dev); defFont.dev.unit := defUnit; defFont.dev.next := NIL;
		IF (dfName # "") & (dfSize > 5 * Fonts.point) & (dfSize < 100 * Fonts.point) THEN
			defFont := dir.This(Fonts.default, dfSize, {}, Fonts.normal)
		ELSE
			i := (defSize + defUnit DIV 2) DIV defUnit;
			IF i < 11 THEN i := 11 END;
(*			defFont.dev.id := GDI32.CreateFontA(-i, 0, 0, 0, Fonts.normal, 0, 0, 0, 0, 7, 2, 1, 38, "");*)
			MakeXlfdString("", i, defUnit, {}, Fonts.normal, defFont.dev.xlfd);
			defFont.dev.id := LinGdk.gdk_fontset_load( defFont.dev.xlfd);
			Setup(defFont, ".", 0, {}, 0);
			dfName := defFont.alias$
		END;
		NEW(dlgFont); NEW(dlgFont.dev); dlgFont.dev.unit := defUnit; dlgFont.dev.next := NIL;
		IF (dgName # "") & (dgSize > 5 * Fonts.point) & (dgSize < 100 * Fonts.point) THEN
			dlgFont := dir.This(dgName, dgSize, dgStyle, dgWght);
			dlgBoldFont := dir.This(dgName, dgSize, dgStyle, Fonts.bold)
		ELSE
(*			dlgFont.dev.id := GDI32.GetStockObject(GDI32.AnsiVarFont);*)
			(* TODO: Get a dialog font here...*)
			MakeXlfdString("helvetica", 11, defUnit, {}, Fonts.normal, dlgFont.dev.xlfd);
			dlgFont.dev.id := LinGdk.gdk_fontset_load( dlgFont.dev.xlfd);
			
			Setup(dlgFont, "", 0, {}, 0);
			dgName := dlgFont.alias$;
			dlgBoldFont := dir.This(dlgFont.typeface, dlgFont.size, dlgFont.style, Fonts.bold)
		END;
	END Init;

BEGIN
	Init
END HostFonts.
