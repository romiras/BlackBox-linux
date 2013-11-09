MODULE HostDialog;

	(*
		TODO:
			- Map titles for the dialogs
			- Open and Save dialogs should start in the directory indicated by loc.
	*)

	IMPORT 
		SYSTEM, Kernel, LinLibc, LinGtk, LinGnomeUI, Dialog, Dates, Files, Stores, Views, Ports, 
		Converters, Windows, Fonts, Strings, HostFiles, HostWindows, HostFonts, Properties, StdCmds,
		Log;
	
	CONST
		(** CloseDialog res **)
		save* = 1; cancel* = 2;
		
		dirtyString = "#Host:SaveChanges";
	
	TYPE
		DatesHook = POINTER TO RECORD (Dates.Hook) END;
		DialogHook = POINTER TO RECORD (Dialog.GetHook) END;
		ShowHook = POINTER TO RECORD (Dialog.ShowHook) END;
		GetSpecHook = POINTER TO RECORD (Views.GetSpecHook) END;
		LanguageHook = POINTER TO RECORD (Dialog.LanguageHook) END;
		
	VAR 
		okClick, cancelClick, destroyed: BOOLEAN; (* to emmulate a modal dialog box for open and save *)
		dialogHook: DialogHook; 
		
	PROCEDURE OkClick (widget: LinGtk.GtkWidget; user_data: LinLibc.PtrVoid);
	BEGIN
		okClick := TRUE;
	END OkClick;
	
	PROCEDURE CancelClick (widget: LinGtk.GtkWidget; user_data: LinLibc.PtrVoid);
	BEGIN
		cancelClick := TRUE;
	END CancelClick;
	
	PROCEDURE Destroy (widget: LinGtk.GtkWidget; user_data: LinLibc.PtrVoid);
	BEGIN
		destroyed := TRUE; cancelClick := TRUE;
	END Destroy;
		
	PROCEDURE GetIntSpec* (VAR loc: Files.Locator; VAR name: Files.Name; VAR conv: Converters.Converter);
	(* asks user for a file name (for file internalization) *)
		VAR fs: LinGtk.GtkFileSelection; res, i, j, l, slashPos: INTEGER; locName: HostFiles.FullName; s: LinLibc.PtrSTR;
	BEGIN
		fs := SYSTEM.VAL(LinGtk.GtkFileSelection, LinGtk.gtk_file_selection_new("Open"));
		res := LinGtk.gtk_signal_connect(SYSTEM.VAL(LinGtk.GtkObject, fs.ok_button), "clicked", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(OkClick)), 0);
		res := LinGtk.gtk_signal_connect(SYSTEM.VAL(LinGtk.GtkObject, fs.cancel_button), "clicked", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(CancelClick)), 0);
		res := LinGtk.gtk_signal_connect(SYSTEM.VAL(LinGtk.GtkObject, fs), "destroy", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(Destroy)), 0);
		LinGtk.gtk_window_set_modal(SYSTEM.VAL(LinGtk.GtkWindow, fs), LinGtk.TRUE);
		LinGtk.gtk_widget_show_now(SYSTEM.VAL(LinGtk.GtkWidget, fs));
		okClick := FALSE; cancelClick := FALSE; destroyed := FALSE;
(*		WHILE ~okClick & ~cancelClick DO res := LinGtk.gtk_main_iteration_do(LinGtk.FALSE) END; (* simumlate a modal window *)*)
		WHILE ~okClick & ~cancelClick DO (* simumlate a modal window *)
			IF LinGtk.gtk_events_pending() # 0 THEN 
				res := LinGtk.gtk_main_iteration()
			END
		END; 
		IF okClick THEN
			s := LinGtk.gtk_file_selection_get_filename(SYSTEM.VAL(LinGtk.GtkFileSelection, fs));
			l := LEN(s$);
			i := l - 1;
			WHILE (i > 0) & (s[i] # '/') DO DEC(i) END;
			slashPos := i;
			INC(i); j := 0;
			WHILE i < l DO name[j] := s[i]; INC(i); INC(j) END;
			name[j] := 0X;
			i := slashPos;
			locName[i] := 0X;
			WHILE i > 0 DO DEC(i); locName[i] := s[i] END;
			loc := HostFiles.NewLocator(locName);
		END;
		IF ~destroyed THEN LinGtk.gtk_widget_destroy(SYSTEM.VAL(LinGtk.GtkWidget, fs)) END;
	END GetIntSpec;

	PROCEDURE GetExtSpec* (s: Stores.Store;
											VAR loc: Files.Locator; VAR name: Files.Name; VAR conv: Converters.Converter);
	(* ask user for a file name (for file externalization) *)
		VAR fs: LinGtk.GtkFileSelection; res, i, j, l, slashPos: INTEGER; locName: HostFiles.FullName; ss: LinLibc.PtrSTR;
	BEGIN
		fs := SYSTEM.VAL(LinGtk.GtkFileSelection, LinGtk.gtk_file_selection_new("Save As"));
		res := LinGtk.gtk_signal_connect(SYSTEM.VAL(LinGtk.GtkObject, fs.ok_button), "clicked", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(OkClick)), 0);
		res := LinGtk.gtk_signal_connect(SYSTEM.VAL(LinGtk.GtkObject, fs.cancel_button), "clicked", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(CancelClick)), 0);
		res := LinGtk.gtk_signal_connect(SYSTEM.VAL(LinGtk.GtkObject, fs), "destroy", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(Destroy)), 0);
		LinGtk.gtk_window_set_modal(SYSTEM.VAL(LinGtk.GtkWindow, fs), LinGtk.TRUE);
		LinGtk.gtk_widget_show_now(SYSTEM.VAL(LinGtk.GtkWidget, fs));
		okClick := FALSE; cancelClick := FALSE; destroyed := FALSE;
(*		WHILE ~okClick & ~cancelClick DO res := LinGtk.gtk_main_iteration_do(LinGtk.FALSE) END; (* simumlate a modal window *)*)
		WHILE ~okClick & ~cancelClick DO (* simumlate a modal window *)
			IF LinGtk.gtk_events_pending() # 0 THEN 
				res := LinGtk.gtk_main_iteration()
			END
		END; 
		IF okClick THEN
			ss := LinGtk.gtk_file_selection_get_filename(SYSTEM.VAL(LinGtk.GtkFileSelection, fs));
			l := LEN(ss$);
			i := l - 1;
			WHILE (i > 0) & (ss[i] # '/') DO DEC(i) END;
			slashPos := i;
			INC(i); j := 0;
			WHILE i < l DO name[j] := ss[i]; INC(i); INC(j) END;
			name[j] := 0X;
			i := slashPos;
			locName[i] := 0X;
			WHILE i > 0 DO DEC(i); locName[i] := ss[i] END;
			loc := HostFiles.NewLocator(locName);
		END;
		IF ~destroyed THEN LinGtk.gtk_widget_destroy(SYSTEM.VAL(LinGtk.GtkWidget, fs)) END;
	END GetExtSpec;
	
	
	PROCEDURE CloseDialog* (w: Windows.Window; quit: BOOLEAN; VAR res: INTEGER);
		VAR r: INTEGER; title: Views.Title; text: ARRAY 256 OF CHAR;
			stext: ARRAY 256 OF SHORTCHAR; an: ARRAY 32 OF SHORTCHAR;
	BEGIN
		w.GetTitle(title);
		Dialog.MapParamString(dirtyString, title, 0DX, 0DX, text);
		stext := SHORT(text$); an := SHORT(Dialog.appName$);
(*		r := USER32.MessageBoxA(w(HostWindows.Window).wnd, stext, an, {0, 1, 5});*)
		r := Kernel.MessageBox(Dialog.appName, text, {Kernel.mbYes, Kernel.mbNo, Kernel.mbCancel});
		IF r = Kernel.mbYes THEN res := save
		ELSIF r = Kernel.mbCancel THEN res := cancel
		ELSE res := 0
		END;
		(* TODO: Set busy cursor*)
(*r := USER32.SetCursor(HostPorts.cursors[HostPorts.busyCursor])*)
	END CloseDialog;

	PROCEDURE ColorDialog*;
	(* open color dialog and set selection to choosen color *)
		VAR set: BOOLEAN; p: Properties.StdProp; col: Ports.Color;
	BEGIN
		Properties.CollectStdProp(p);
		IF ~(Properties.color IN p.known) THEN p.color.val := Ports.black END;
		dialogHook.GetColor(p.color.val, col, set);
		IF set THEN StdCmds.Color(col) END
	END ColorDialog;
	
	PROCEDURE FontDialog0 (full: BOOLEAN; VAR typeface: Fonts.Typeface; VAR size: INTEGER;
												VAR color: Ports.Color; VAR weight: INTEGER; 
												VAR style: SET; VAR set: BOOLEAN);
		VAR 
			res: INTEGER; fsDialog: LinGtk.GtkFontSelectionDialog; fn: LinLibc.PtrSTR; 
			s: HostFonts.XlfdString;
		
	BEGIN
		set := FALSE;
		fsDialog := LinGtk.gtk_font_selection_dialog_new("Font")(LinGtk.GtkFontSelectionDialog);
		res := LinGtk.gtk_signal_connect(fsDialog.ok_button, "clicked", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(OkClick)), 0);
		res := LinGtk.gtk_signal_connect(fsDialog.cancel_button, "clicked", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(CancelClick)), 0);
		res := LinGtk.gtk_signal_connect(fsDialog, "destroy", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(Destroy)), 0);
		LinGtk.gtk_window_set_modal(fsDialog, LinGtk.TRUE);
		HostFonts.MakeXlfdString(typeface$, size, HostFonts.defFont.dev.unit, style, weight, s);
		res := LinGtk.gtk_font_selection_dialog_set_font_name(fsDialog, s);
		LinGtk.gtk_widget_show_now(fsDialog);
		okClick := FALSE; cancelClick := FALSE; destroyed := FALSE;
		WHILE ~okClick & ~cancelClick DO (* simumlate a modal window *)
			IF LinGtk.gtk_events_pending() # 0 THEN 
				res := LinGtk.gtk_main_iteration()
			END;
			IF okClick THEN
				fn := LinGtk.gtk_font_selection_dialog_get_font_name(fsDialog);
				IF fn = NIL THEN
					okClick := FALSE
				END
			END;
		END; 
		IF okClick THEN
			HostFonts.ParseXlfd(fn$, HostFonts.defFont.dev.unit, typeface, size, style, weight);
			set := TRUE
		END;
		IF ~destroyed THEN LinGtk.gtk_widget_destroy(fsDialog) END
	END FontDialog0;
	
	PROCEDURE FontDialog*;
	(** open font dialog and set selection to choosen attributes **)
		VAR set: BOOLEAN; p, p0: Properties.StdProp;
	BEGIN
		Properties.CollectStdProp(p0);
		IF Properties.typeface IN p0.known THEN
			NEW(p); p.typeface := p0.typeface$;
			p.size := p0.size; p.color.val := p0.color.val;
			p.weight := p0.weight; p.style := p0.style;
			FontDialog0(TRUE, p.typeface, p.size, p.color.val, p.weight, p.style.val, set);
			IF set THEN
				p.valid := {Properties.typeface, Properties.style, Properties.weight, Properties.size, Properties.color};
				p.style.mask := {Fonts.italic, Fonts.underline, Fonts.strikeout};
				Properties.EmitProp(NIL, p)
			END
		END
	END FontDialog;

	PROCEDURE TypefaceDialog*;
	(** open font dialog and set selection to choosen typeface **)
		VAR set: BOOLEAN; p, p0: Properties.StdProp; s: INTEGER; c: Ports.Color; w: INTEGER; st: SET;
	BEGIN
		Properties.CollectStdProp(p0);
		IF Properties.typeface IN p0.known THEN
			NEW(p); p.typeface := p0.typeface$;
			FontDialog0(FALSE, p.typeface, s, c, w, st, set);
			IF set THEN
				p.valid := {Properties.typeface};
				Properties.EmitProp(NIL, p)
			END
		END
	END TypefaceDialog;
	
	
	(* Dates Hook *)
	
	(*
	
	Some conversions are needed between the Linux and the BlackBox representations of  dates. The following
	table shows the differences:
	
		Linux	BlackBox
	year	from year 1900	from year 0000
	month	range 0-11	range 1-12
	weekday	0:sunday - 6:satruday	0:monday - 6:sunday
		*)
	
	PROCEDURE (h: DatesHook) DateToString (d: Dates.Date; format: INTEGER; OUT str: ARRAY OF CHAR);
		VAR tm: LinLibc.tmDesc; sstr: ARRAY 64 OF SHORTCHAR; res: LinLibc.size_t; 
	BEGIN
		ASSERT(format IN {Dates.short, Dates.abbreviated, Dates.long, Dates.plainAbbreviated, Dates.plainLong}, 20);
		tm.tm_year := d.year - 1900; (* Linux counts years from 1900 but BlackBox from 0000 *)
		tm.tm_mon := d.month - 1; tm.tm_mday := d.day;
		tm.tm_wday := (Dates.DayOfWeek(d) + 1) MOD 7;		
		IF format = Dates.short THEN
			res := LinLibc.strftime(sstr, LEN(sstr), "%x", SYSTEM.VAL(LinLibc.tm, SYSTEM.ADR(tm)))
		ELSIF format = Dates.abbreviated THEN
			res := LinLibc.strftime(sstr, LEN(sstr), "%a, %b %d, %Y", SYSTEM.VAL(LinLibc.tm, SYSTEM.ADR(tm)))
		ELSIF format = Dates.long THEN
			res := LinLibc.strftime(sstr, LEN(sstr), "%A, %B %d, %Y", SYSTEM.VAL(LinLibc.tm, SYSTEM.ADR(tm)))
		ELSIF format = Dates.plainAbbreviated THEN
			res := LinLibc.strftime(sstr, LEN(sstr), "%b %d, %Y", SYSTEM.VAL(LinLibc.tm, SYSTEM.ADR(tm)))
		ELSE (* format = Dates.plainLong *)
			res := LinLibc.strftime(sstr, LEN(sstr), "%B %d, %Y", SYSTEM.VAL(LinLibc.tm, SYSTEM.ADR(tm)))
		END;
		IF res > 0 THEN str := sstr$ELSE str := "invalid date"  END
	END DateToString;

	PROCEDURE (h: DatesHook) GetTime (OUT d: Dates.Date; OUT t: Dates.Time);
		VAR time: LinLibc.time_t; tm: LinLibc.tm;
	BEGIN
		time := LinLibc.time(NIL);
		tm := LinLibc.localtime(time);
		d.year := tm.tm_year + 1900; (* Linux counts years from 1900 but BlackBox from 0000 *)
		d.month := tm.tm_mon + 1;  d.day := tm.tm_mday;
		t.hour := tm.tm_hour; t.minute := tm.tm_min; t.second := tm.tm_sec
	END GetTime;

	PROCEDURE (h: DatesHook) GetUTCBias (OUT bias: INTEGER);
		VAR time: LinLibc.time_t; tm: LinLibc.tm;
	BEGIN
		time := LinLibc.time(NIL);
		tm := LinLibc.localtime(time); (* call to localtime needed to make sure that timezone is set *)
		bias := LinLibc.timezone DIV 60;
	END GetUTCBias; 

	PROCEDURE (h: DatesHook) GetUTCTime (OUT d: Dates.Date; OUT t: Dates.Time);
		VAR time: LinLibc.time_t; tm: LinLibc.tm;
	BEGIN
		time := LinLibc.time(NIL);
		tm := LinLibc.gmtime(time);
		d.year := tm.tm_year + 1900; (* Linux counts years from 1900 but BlackBox from 0000 *)
		d.month := tm.tm_mon + 1;  d.day := tm.tm_mday;
		t.hour := tm.tm_hour; t.minute := tm.tm_min; t.second := tm.tm_sec
	END GetUTCTime;

	PROCEDURE (h: DatesHook) TimeToString (t: Dates.Time; OUT str: ARRAY OF CHAR);
		VAR tm: LinLibc.tmDesc; sstr: ARRAY 64 OF SHORTCHAR; res: LinLibc.size_t;
	BEGIN
		tm.tm_hour := t.hour; tm.tm_min := t.minute; tm.tm_sec := t.second;
		res := LinLibc.strftime(sstr, LEN(sstr), "%X", SYSTEM.VAL(LinLibc.tm, SYSTEM.ADR(tm)));
		IF res > 0 THEN str := sstr$ELSE str := "invalid time"  END
	END TimeToString;
	
	(* Dialog Hook *)
	
	PROCEDURE (hook: DialogHook) GetOK (IN str, p0, p1, p2: ARRAY OF CHAR; form: SET; OUT res: INTEGER);
	BEGIN
		Dialog.ShowMsg(str); IF Dialog.cancel IN form THEN res := Dialog.cancel ELSE res := Dialog.ok END
	END GetOK;
	
	PROCEDURE (hook: DialogHook) GetIntSpec (IN defType: Files.Type; VAR loc: Files.Locator; OUT name: Files.Name);
	BEGIN
		Dialog.ShowMsg("HostDialog.DialogHook.GetIntSpec")
	END GetIntSpec;
	
	PROCEDURE (hook: DialogHook) GetExtSpec (IN default: Files.Name; IN defType: Files.Type; VAR loc: Files.Locator; OUT name: Files.Name);
	BEGIN
		Dialog.ShowMsg("HostDialog.DialogHook.GetExtSpec")
	END GetExtSpec;
	
	PROCEDURE (hook: DialogHook) GetColor (in: Ports.Color; OUT out: Ports.Color; OUT set: BOOLEAN);
		VAR 
			res: INTEGER; colorDialog: LinGtk.GtkColorSelectionDialog; color: LinGtk.GtkColor;
	BEGIN
		set := FALSE;
		colorDialog := LinGtk.gtk_color_selection_dialog_new("Color")(LinGtk.GtkColorSelectionDialog);
		res := LinGtk.gtk_signal_connect(colorDialog.ok_button, "clicked", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(OkClick)), 0);
		res := LinGtk.gtk_signal_connect(colorDialog.cancel_button, "clicked", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(CancelClick)), 0);
		res := LinGtk.gtk_signal_connect(colorDialog, "destroy", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(Destroy)), 0);
		LinGtk.gtk_widget_hide(colorDialog.help_button);
		LinGtk.gtk_window_set_modal(colorDialog, LinGtk.TRUE);
		
		color[0] := (in MOD 256) / 255.0;
		color[1] := ((in DIV 256) MOD 256) / 255.0;
		color[2] := ((in DIV 65536) MOD 256) / 255.0;
		color[3] := 0; (* opacity *)
		LinGtk.gtk_color_selection_set_color(colorDialog.colorsel, color);
		LinGtk.gtk_color_selection_set_color(colorDialog.colorsel, color);
		LinGtk.gtk_color_selection_set_opacity(colorDialog.colorsel, LinLibc.FALSE);
		LinGtk.gtk_widget_show_now(colorDialog);
		okClick := FALSE; cancelClick := FALSE; destroyed := FALSE;
		WHILE ~okClick & ~cancelClick DO (* simumlate a modal window *)
			IF LinGtk.gtk_events_pending() # 0 THEN 
				res := LinGtk.gtk_main_iteration()
			END
		END; 
		IF okClick THEN
			LinGtk.gtk_color_selection_get_color(colorDialog.colorsel, color);
			out := Ports.RGBColor(SHORT(ENTIER((color[0]*255))), SHORT(ENTIER((color[1]*255))),
												SHORT(ENTIER((color[2]*255))));
			 set := TRUE
		END;
		IF ~destroyed THEN LinGtk.gtk_widget_destroy(colorDialog) END
	END GetColor;
	
	(* Show Hook *)

	PROCEDURE ShowParamMsg* (IN str, p0, p1, p2: ARRAY OF CHAR);
		VAR res: INTEGER; st: ARRAY 512 OF CHAR; sst: ARRAY 512 OF SHORTCHAR; an: ARRAY 32 OF SHORTCHAR;
			bs: ARRAY [untagged] 4 OF LinLibc.PtrSTR; dlg: LinGtk.GtkWidget; b: LinLibc.StrArray;
	BEGIN
		ASSERT(str # "", 20);
		Dialog.MapParamString(str, p0, p1, p2, st); sst := SHORT(st$); an := SHORT(Dialog.appName$);
		
(*		res := USER32.MessageBoxA(HostWindows.ActualWnd(), sst, an, {4, 5})*)
		
		bs[0] := "OK"; bs[1] := NIL; b := SYSTEM.VAL(LinLibc.StrArray, SYSTEM.ADR(bs));
		dlg := LinGnomeUI.gnome_message_box_newv(sst, an, b);
		res := LinGnomeUI.gnome_dialog_run(dlg);
	END ShowParamMsg;

	PROCEDURE ShowParamStatus* (IN str, p0, p1, p2: ARRAY OF CHAR);
		VAR res: INTEGER; st: ARRAY 512 OF CHAR; sst: ARRAY 513 OF SHORTCHAR; w: Windows.Window;
	BEGIN
		Dialog.MapParamString(str, p0, p1, p2, st);
		sst := SHORT(" " + st$);
		HostWindows.SetStatusText(sst)
	END ShowParamStatus;

	PROCEDURE (h: ShowHook) ShowParamMsg (IN str, p0, p1, p2: ARRAY OF CHAR);
	BEGIN
		ShowParamMsg(str, p0, p1, p2)
	END ShowParamMsg;
	
	PROCEDURE (h: ShowHook) ShowParamStatus (IN str, p0, p1, p2: ARRAY OF CHAR);
	BEGIN
		ShowParamStatus(str, p0, p1, p2)
	END ShowParamStatus;
	
	(* GetSpec Hook *)
	
	PROCEDURE (h: GetSpecHook) GetIntSpec (VAR loc: Files.Locator; VAR name: Files.Name; 
															VAR conv: Converters.Converter);
	BEGIN
		GetIntSpec(loc, name, conv)
	END GetIntSpec;
	
	PROCEDURE (h: GetSpecHook) GetExtSpec (s: Stores.Store;
											VAR loc: Files.Locator; VAR name: Files.Name; VAR conv: Converters.Converter);
	BEGIN
		GetExtSpec(s, loc, name, conv)
	END GetExtSpec;
	
	(* Language Hook *)
	
	PROCEDURE (hook: LanguageHook) SetLanguage (lang: Dialog.Language; persistent: BOOLEAN; OUT ok: BOOLEAN);
	BEGIN
		ok := (lang = "") OR (LEN(lang$) = 2);
(*		IF ok & persistent THEN HostRegistry.WriteString("language", SHORT(lang)) END*) (* TODO: Save language *)
	END SetLanguage;

	PROCEDURE (hook: LanguageHook) GetPersistentLanguage (OUT lang: Dialog.Language);
		VAR res: INTEGER; s: ARRAY 32 OF SHORTCHAR;
	BEGIN
		(*HostRegistry.ReadString("language", s, res);*) res := 1; (* TODO: Do it right *)
		IF res = 0 THEN
			ASSERT((s = "") OR (LEN(s$) = 2), 100);
			lang := s$
		ELSE lang := ""
		END
	END GetPersistentLanguage;


	PROCEDURE Init;
		VAR 
			datesHook: DatesHook; showHook: ShowHook; languageHook: LanguageHook; 
			getSpecHook: GetSpecHook; 
	BEGIN
		Dialog.platform := Dialog.linux;
		Dialog.showsStatus := TRUE; (* TODO: Should be read from HostRegistry *)
		NEW(datesHook); Dates.SetHook(datesHook);
		NEW(showHook); Dialog.SetShowHook(showHook);
		NEW(dialogHook); Dialog.SetGetHook(dialogHook);
		NEW(getSpecHook); Views.SetGetSpecHook(getSpecHook);
		HostFiles.MapParamString := Dialog.MapParamString;
		NEW(languageHook); Dialog.SetLanguageHook(languageHook); Dialog.ResetLanguage
	END Init;

BEGIN
	Init
END HostDialog.