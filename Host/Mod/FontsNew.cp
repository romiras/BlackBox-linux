MODULE HostFonts;
(** cp, bh 18.12.01 *)

	(* TODO: 
		- How can True Type fonts be used? There is something called FreeType.
		- Use HostRegistry again
	 *)
		
	(*
		Implementation docu
		On Windows typeface names only contain the family name. On Linux it also contains the foundary. They are
		separated by a "-" and they are on the form: "family-foundary". Wherever typfaces are handled, both cases
		has to be handled.
	*)


	(* bj  17.12.01	Linux version *)
	(* dg 14.10.98	bug fix (8.25 sized fonts) *)
	(* bh 08.01.96	new alien handling *)
	
	IMPORT SYSTEM, LinLibc, LinX11, LinGdk, Kernel, Fonts(*, HostRegistry*), HostGnome, Strings;

	CONST
		defSize = 8 * Fonts.point;	(* size of default font *)
(*
		grid = 16384;	(* true type design grid *)
*)
		grid = 4096;
		figureSpace = 8FX;
		
		maxFonts = 32767; (* used in TypefaceList *)

	TYPE
		WTab = ARRAY 256 OF INTEGER;
		DevFont* = POINTER TO RECORD
			unit-: INTEGER;
			id-: LinGdk.GdkFont;
			next-: DevFont;
			noGap-: BOOLEAN;
			wtab-: WTab;	(* rastered width in pixels *)
			xlfd: XlfdString;
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
	(*
		dc: USER32.Handle;
		fontId: GDI32.Handle;
	*)
	

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

	(* Crteates a valid XLFD string. Such a string can be used to load a font. *)
	PROCEDURE MakeXlfdString* (typeface: ARRAY OF CHAR; size, unit: INTEGER; style: SET; weight: INTEGER; 
											OUT xlfd: XlfdString);
		VAR 
			foundary, family: ARRAY LEN(Fonts.Typeface) OF SHORTCHAR; 
			ssize, sbold, sitalic: ARRAY 10 OF SHORTCHAR;
			i, j: INTEGER; ok: BOOLEAN;
			
		PROCEDURE Test;
			VAR xFontNames: LinLibc.StrArray; numFonts: INTEGER;
		BEGIN 
			xlfd := "-" + foundary + "-" + family + "-" + sbold + "-" + sitalic + "-*-*-" + ssize + "-*-*-*-*-*iso8859-1";
			xFontNames := LinX11.XListFonts(LinGdk.gdk_display, xlfd, 1, numFonts);
			IF xFontNames # NIL THEN
				LinX11.XFreeFontNames (xFontNames);
				ok := TRUE;
			ELSE
				ok := FALSE;
			END
		END Test;
		
	BEGIN
		i := 0; ok := FALSE; xlfd := "";
		IF (typeface = "") OR (typeface = ".") OR (typeface = "*") THEN
			foundary := "*"; family := "*"
		ELSE
			WHILE (typeface[i] # 0X) & (typeface[i] # "-") DO family[i] := SHORT(typeface[i]); INC(i) END;
			family[i] := 0X;
			IF typeface[i] = "-" THEN (* foundary was included in the typeface name *)
				INC(i); j := 0;
				WHILE typeface[i] # 0X DO foundary[j] := SHORT(typeface[i]); INC(i); INC(j) END;
				foundary[j] := 0X
			ELSE (* foundary was not included in the typeface name *)
				foundary := "*"
			END
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
		IF ~ok THEN (* remove bold or medium *)
			sbold := "*";
			Test
		END;
		IF ~ok THEN (* remove family *)
			family := "*";
			Test;
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
	
	PROCEDURE NewDevFont (typeface: ARRAY OF CHAR; size, unit: INTEGER; style: SET; weight: INTEGER): DevFont;
(*		VAR df: DevFont; res, it, ul, so, i: INTEGER; s: ARRAY LEN(Fonts.Typeface) OF SHORTCHAR;*)
		VAR df: DevFont; (*s: XlfdString;*)
	BEGIN
		IF size = 8 * Fonts.point THEN INC(size, Fonts.point DIV 4) END;
		(*
		it := 0; ul := 0; so := 0;
		IF Fonts.italic IN style THEN it := 1 END;
		IF Fonts.underline IN style THEN ul := 1 END;
		IF Fonts.strikeout IN style THEN so := 1 END;
		NEW(df); df.unit := unit; df.next := NIL;
		s := SHORT(typeface$);
		df.id := GDI32.CreateFontA(-((size + unit DIV 2) DIV unit), 0, 0, 0, weight, it, ul, so, 1, 0, 2, 1, 4, s);
		*)
		NEW(df); df.unit := unit; df.next := NIL;
		MakeXlfdString(typeface, size, unit, style, weight, df.xlfd);
		df.id := LinGdk.gdk_fontset_load( df.xlfd);
(*		df.id := LinGdk.gdk_font_ref(df.id); TODO: Necessary? *)
		RETURN df
	END NewDevFont;
	
	PROCEDURE GetRasterWidth (fid: LinGdk.GdkFont; VAR wtab: WTab);
(*		VAR res, i, x: INTEGER; str: ARRAY 4 OF SHORTCHAR; p: GDI32.Point;*)
		VAR i: INTEGER;
	BEGIN
	(*
		res := GDI32.GetTextExtentPoint32A(dc, "x", 1, p);
		i := 0; str := " x"; x := p.x;
		WHILE i < 256 DO
			str[0] := SHORT(CHR(i));
			res := GDI32.GetTextExtentPoint32A(dc, str, 2, p);
			wtab[i] := p.x - x; INC(i)
		END
	*)
		i := 0; 
		WHILE i < 256 DO
			wtab[i] := LinGdk.gdk_char_width(fid, SHORT(CHR(i)));
			INC(i)
		END
	END GetRasterWidth;
	
	PROCEDURE SetupWTabs (f: Font);
(*		VAR res, a, b, asc, dsc, max, x, i: INTEGER; tm: GDI32.TextMetric;
			df: DevFont; abc: ARRAY 256 OF GDI32.ABC; dc, old: USER32.Handle;*)
		CONST isTrueType = FALSE; (* TODO: Implement true type support *)
		VAR res, a, b, asc, dsc, max, x, i: INTEGER; 
			df: DevFont;
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
(*		x := tm.ascent + tm.extLeading; f.asc := x * a DIV grid + x * b;
		f.dsc := tm.descent * a DIV grid + tm.descent * b;*)
		f.asc := f.id.ascent * b;
		f.dsc := f.id.descent * b;
		f.w := max; f.a := a; f.b := b;
	END SetupWTabs;
	
	PROCEDURE Cleanup (f: Font);
		VAR (*res: INTEGER;*) df: DevFont;
	BEGIN
	(*
		df := f.dev;
		IF f.id # df.id THEN res := GDI32.DeleteObject(f.id) END;
		WHILE df # NIL DO
			res := GDI32.DeleteObject(df.id);
			df := df.next
		END;
		f.id := 0; f.dev := NIL
		*)
		df := f.dev;
		IF f.id # df.id THEN LinGdk.gdk_font_unref(f.id) END;
		WHILE df # NIL DO
			LinGdk.gdk_font_unref(f.id);
			df := df.next
		END;
		f.id := NIL; f.dev := NIL
	END Cleanup;
	

	(* width methods for unicode *)
	
	PROCEDURE (f: Font) wTab* (ch: CHAR): INTEGER, NEW;
		VAR (*res, w: INTEGER; abc: ARRAY 1 OF GDI32.ABC; wt: ARRAY 1 OF INTEGER;*)
			i, w: INTEGER;
	BEGIN
		(*
		res := GDI32.GetCharABCWidthsW(dc, ORD(ch), ORD(ch), abc);
		IF res # 0 THEN
			w := abc[0].a + abc[0].b + abc[0].c;
			w := w * f.a DIV grid + w * f.b
		ELSE
			res := GDI32.GetCharWidth32W(dc, ORD(ch), ORD(ch), wt);
			IF res # 0 THEN w := wt[0] * f.a DIV grid + wt[0] * f.b
			ELSE
				res := GDI32.GetCharWidthW(dc, ORD(ch), ORD(ch), wt);
				IF res # 0 THEN w := wt[0] * f.a DIV grid + wt[0] * f.b
				ELSE w := f.wtab[1]
				END
			END
		END;
		*)
		
		i := ORD(ch);
		w := LinGdk.gdk_char_width_wc(f.id, i);
		w := w * f.a DIV grid + w * f.b;
		RETURN w
	END wTab;

	PROCEDURE (f: Font) fTab* ((*dc: USER32.Handle;*) ch: CHAR): INTEGER, NEW;
(*		VAR res, w: INTEGER; abc: ARRAY 1 OF GDI32.ABC;*)
	BEGIN
	(*
		res := GDI32.GetCharABCWidthsW(dc, ORD(ch), ORD(ch), abc);
		IF (res # 0) & (abc[0].a < 0) THEN
			w := -abc[0].a;
			w := w * f.a DIV grid + w * f.b
		ELSE w := 0
		END;
		RETURN w
		*)
		RETURN 0
	END fTab;

	PROCEDURE (f: Font) tTab* ((*dc: USER32.Handle; *)ch: CHAR): INTEGER, NEW;
	(*
		VAR res, w: INTEGER; abc: ARRAY 1 OF GDI32.ABC;
	BEGIN
		res := GDI32.GetCharABCWidthsW(dc, ORD(ch), ORD(ch), abc);
		IF (res # 0) & (abc[0].c < 0) THEN
			w := -abc[0].c;
			w := w * f.a DIV grid + w * f.b
		ELSE w := 0
		END;
		RETURN w
	*)
	BEGIN
		RETURN 0
	END tTab;
	

	(** Font **)

	PROCEDURE (f: Font) GetBounds* (OUT asc, dsc, w: INTEGER);
	BEGIN
		asc := f.asc; dsc := f.dsc; w := f.w
	END GetBounds;
	
	PROCEDURE (f: Font) SStringWidth* (IN s: ARRAY OF SHORTCHAR): INTEGER;
		VAR i, d, w: INTEGER; ch: CHAR;
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
		VAR res, i, d, w: INTEGER; lc: CHAR; ch: SHORTCHAR; (*str: ARRAY 1024 OF SHORTCHAR;*) (*dc, old: USER32.Handle;*)
			is: ARRAY [untagged] 1024 OF INTEGER;
	BEGIN
		IF isUnicode THEN
(*			dc := USER32.GetDC(0);
			old := GDI32.SelectObject(dc, f.id);*)
			w := 0;
			IF s[0] # 0X THEN
				i := 0; lc := s[0];
				WHILE lc # 0X DO INC(w, f.wTab(lc)); INC(i); lc := s[i] END;
				w := w + f.fTab(s[0]) + f.tTab(s[i-1])
			END;
(*			res := GDI32.SelectObject(dc, old);
			res := USER32.ReleaseDC(0, dc);*)
			RETURN w
		ELSE
		(*
			ch := 1X;
			res := KERNEL32.WideCharToMultiByte(0, {}, s, -1, str, LEN(str), ch, NIL);
			str[res] := 0X;
			*)
			
			FOR i := 0 TO LEN(s$) DO is[i] := ORD(s[i]) END;
(*			str := SHORT(LinGdk.gdk_wcstombs(is)$);*)
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
(*		VAR res: INTEGER; abc: ARRAY 1 OF GDI32.ABC;*)
	BEGIN
(*		res := GDI32.GetCharABCWidthsA(dc, ORD("H"), ORD("H"), abc);*)
		IF isTrueType THEN
(*		IF res # 0 THEN	(* true type *)*)
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
	
	PROCEDURE InsertDevFont* ((*dc: USER32.Handle; *)font: Font; VAR df: DevFont; unit: INTEGER);
		VAR res: INTEGER;
	BEGIN
		df := NewDevFont(font.alias, font.size, unit, font.style, font.weight);
(*		res := GDI32.SelectObject(dc, df.id);*)
		SetupDevFont(df);
		df.next := font.dev.next; font.dev.next := df	(* screen font remains at list head *)
	END InsertDevFont;
	
	PROCEDURE SetupAlias (f: Font; IN typeface: ARRAY OF CHAR; IN xlfd: XlfdString);
		VAR foundary, family, aface: ARRAY LEN(Fonts.Typeface) OF SHORTCHAR; i, j: INTEGER;
	BEGIN
		i := 0;
		WHILE (xlfd[i] # 0X) & (xlfd[i] # "-") DO foundary[i] := xlfd[i]; INC(i) END;
		foundary[i] := 0X;
		INC(i); j := 0;
		WHILE (xlfd[i] # 0X) & (xlfd[i] # "-") DO family[j] := xlfd[i]; INC(i); INC(j) END;
		family[j] := 0X;
		IF family = typeface THEN (* only family in typeface name *)
			f.alias := SHORT(typeface)
		ELSE
			f.alias := family + "-" + foundary
		END
	END SetupAlias;
	
	PROCEDURE Setup (f: Font; typeface: ARRAY OF CHAR; size: INTEGER; style: SET; weight: INTEGER);
		VAR res: INTEGER; (*tm: GDI32.TextMetric;*) name: Fonts.Typeface; (*dc, old: USER32.Handle;*)
			s: ARRAY LEN(Fonts.Typeface) OF SHORTCHAR;
	BEGIN
		(*
		dc := USER32.GetDC(0);
		old := GDI32.SelectObject(dc, f.dev.id);
		res := GDI32.GetTextFaceA(dc, LEN(s), s); name := s$;
		res := GDI32.GetTextMetricsA(dc, tm);
		f.alias := name$;
		*)
		SetupAlias(f, typeface, f.dev.xlfd);
		name := f.alias;
		IF typeface = Fonts.default THEN
			name := Fonts.default
		ELSIF (typeface = "") OR (typeface = ".") THEN (* System or default font. Get attributes from the loaded font. *)
			(* TODO: Get the attibutes from f.dev.xlfd 
			size := ((tm.height - tm.intLeading) * defUnit + (Fonts.point DIV 2)) DIV Fonts.point * Fonts.point;
			weight := SHORT(tm.weight);
			IF typeface = "." THEN name := Fonts.default END;
			IF tm.italic # 0 THEN INCL(style, Fonts.italic) END;
			IF tm.underlined # 0 THEN INCL(style, Fonts.underline) END;
			IF tm.struckOut # 0 THEN INCL(style, Fonts.strikeout) END;
			*)
			size := 10 * Fonts.point DIV 2;
			weight := Fonts.normal;
			IF typeface = "." THEN name := Fonts.default END;
			style := {}
		ELSIF name # typeface THEN
			(* On Windows the font is switched back to the defualt font here, but here we have a better match, don't we? *)
			(*
			f.dev := NewDevFont(dfName, size, defUnit, style, weight);
			res := GDI32.DeleteObject(GDI32.SelectObject(dc, f.dev.id));
			f.alias := dfName$;
			name := typeface$
			*)
		END;
		IF size # 0 THEN
			SetupDevFont(f.dev);
			IF f.size = 0 THEN f.Init(name, size, style, weight) END;
			(*
			res := GDI32.SelectObject(dc, old);
			res := USER32.ReleaseDC(0, dc);
			*)
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
		VAR res: INTEGER; f: Font; i: Identifier; p: ANYPTR;
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
		VAR res, i: INTEGER; s: ARRAY 2 OF CHAR; df: ARRAY LEN(Fonts.Typeface) OF SHORTCHAR;
	BEGIN
		df := ""; dfSize := 0; dgSize := 0; dgStyle := {}; dgWght := Fonts.normal; i := 0;
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
(*		dc := USER32.GetDC(0);
		defUnit := 72 * Fonts.point DIV GDI32.GetDeviceCaps(dc, GDI32.LogPixelsY);*)
		defUnit := (Fonts.mm * HostGnome.ScreenHeightMM()) DIV LinGdk.gdk_screen_height();
		
(*		isUnicode := GDI32.TextOutW(dc, 0, 0, s, 0) # 0;*)
		isUnicode := TRUE;  (* TODO: is this correct? *)
		
(*		res := USER32.ReleaseDC(0, dc);*)

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
			dlgBoldFont := dir.This(dlgFont.typeface, dlgFont.size, dlgFont.style, Fonts.bold);
(*			IF KERNEL32.GetVersion() MOD 256 < 4 THEN dlgFont := dlgBoldFont END*)
		END;
	END Init;

BEGIN
	Init
END HostFonts.
