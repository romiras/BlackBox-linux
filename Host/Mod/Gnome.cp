MODULE HostGnome;

	(* This module initializes the Gnome environment, makes sure that all libraries are loaded in the
		correct order, and installes a message box hook in Kernel to allow graphical message boxes.
		Any application using the graphical user interface on Linux must have this module linked. *)

	IMPORT SYSTEM, LinLibc, LinDl, LinGdk, LinGtk, Kernel, Files;
	
	TYPE 
			Hook = POINTER TO RECORD (Kernel.GuiHook) END;
	
			(* Gnome calls are implemented with procedure variables instead of interface modules to
				be able to guarantee that libraries are loaded in the correct order. *)
			Gnome_init = PROCEDURE  (app_id, app_version: LinLibc.PtrSTR; argc: INTEGER; 
															argv: LinLibc.StrArray);	
			Gnome_message_box_newv = PROCEDURE  (message,  messagebox_type: LinLibc.PtrSTR; 
															VAR buttons: ARRAY [untagged] OF LinLibc.PtrSTR):
															LinGtk.GtkWidget;
			Gnome_dialog_run = PROCEDURE (dlg: LinGtk.GtkWidget): INTEGER;
	
	VAR 
		hook: Hook;
		gnome_init: Gnome_init;
		gnome_message_box_newv: Gnome_message_box_newv;
		gnome_dialog_run: Gnome_dialog_run;
		
	PROCEDURE (h: Hook) Beep*;
	BEGIN
		LinGdk.gdk_beep
	END Beep;
	
	PROCEDURE (h: Hook)  MessageBox* (title, msg: ARRAY OF CHAR; buttons: SET): INTEGER;
		VAR 
			 st, sm: ARRAY 1024 OF SHORTCHAR; res, i, rb: INTEGER;
			dlg: LinGtk.GtkWidget; bs: ARRAY [untagged] 7 OF LinLibc.PtrSTR;
	BEGIN
		st := SHORT(title); sm := SHORT(msg);
		i := 0;
		IF Kernel.mbOk IN buttons THEN bs[i] := "OK"; INC(i) END;
		IF Kernel.mbCancel IN buttons THEN bs[i] := "Cancel"; INC(i) END;
		IF Kernel.mbRetry IN buttons THEN bs[i] := "Retry"; INC(i) END;
		IF Kernel.mbIgnore IN buttons THEN bs[i] := "Ignore"; INC(i) END;
		IF Kernel.mbYes IN buttons THEN bs[i] := "Yes"; INC(i) END;
		IF Kernel.mbNo IN buttons THEN bs[i] := "No"; INC(i) END;		
		bs[i] := NIL; 
		dlg := gnome_message_box_newv(sm, "info", bs);
		LinGtk.gtk_window_set_title(SYSTEM.VAL(LinGtk.GtkWindow, dlg), st);
		res := gnome_dialog_run(dlg);
		IF res = -1 THEN 
			res := Kernel.mbClose
		ELSE
			(* in the right order... *)
			i := 0; rb := -1;
			WHILE (i < LEN(bs)) & (rb # res) DO IF i IN buttons THEN INC(rb) END; INC(i) END;
			res := i -1
		END;
		RETURN res
	END MessageBox;
		
	PROCEDURE ScreenHeightMM* (): INTEGER; (* screen height in mm
	
		X does not necessarily give the correct value, so a possiblity to override it is built in
		
	*)
		VAR f: Files.File; r: Files.Reader; res, i, p, l: INTEGER; b: ARRAY 10 OF BYTE;
		
		PROCEDURE IntPower (x, y: INTEGER): INTEGER;
			VAR j, r: INTEGER;
		BEGIN
			r := 1;
			FOR j := 1 TO y DO r := r * x END;
			RETURN r
		END IntPower;
		
	BEGIN
		res := 0;
		f := Files.dir.Old(Files.dir.This(""), "screenheight", Files.shared);
		IF f # NIL THEN
			r := f.NewReader(NIL);
			l := MIN(LEN(b), f.Length());
			r.ReadBytes(b, 0, l);
			i := l; res := 0; p := 0;
			WHILE i >= 0 DO
				IF (b[i] >= ORD("0")) & (b[i] <= ORD("9")) THEN
					res := IntPower(10, p) * (b[i] - ORD("0")) + res;
					INC(p)
				END;
				DEC(i)
			END
		END;
		IF res <= 0 THEN res := LinGdk.gdk_screen_height_mm() END;
		RETURN res
	END ScreenHeightMM;
	
	PROCEDURE Init;
		VAR res: INTEGER; ok: BOOLEAN; h: LinDl.HANDLE; adr: INTEGER;
	BEGIN
		LinGdk.gdk_init(SYSTEM.ADR(Kernel.bootInfo.argc), SYSTEM.ADR(Kernel.bootInfo.argv));
		LinGtk.gtk_init(SYSTEM.ADR(Kernel.bootInfo.argc), SYSTEM.ADR(Kernel.bootInfo.argv));
		(* Load libraries "from hand", to make sure that they are loaded. *)
		h := LinDl.dlopen("libgnomesupport.so", LinDl.RTLD_LAZY +  LinDl.RTLD_GLOBAL);	
		h := LinDl.dlopen("libgnome.so", LinDl.RTLD_LAZY +  LinDl.RTLD_GLOBAL);
		h := LinDl.dlopen("libgnomeui.so", LinDl.RTLD_LAZY +  LinDl.RTLD_GLOBAL);	
		IF h # LinDl.NULL THEN 
			adr := LinDl.dlsym(h, "gnome_message_box_newv");
			IF adr # 0 THEN 
				gnome_message_box_newv := SYSTEM.VAL(Gnome_message_box_newv, adr) 
			END;
			adr := LinDl.dlsym(h, "gnome_init");
			IF adr # 0 THEN  gnome_init := SYSTEM.VAL(Gnome_init, adr) END;
			adr := LinDl.dlsym(h, "gnome_dialog_run");
			IF adr # 0 THEN gnome_dialog_run := SYSTEM.VAL(Gnome_dialog_run, adr) END;
			IF (gnome_message_box_newv # NIL) & (gnome_init # NIL) & (gnome_dialog_run # NIL) THEN
				NEW(hook);
				Kernel.SetGuiHook(hook);
				gnome_init("BlackBox", "Version 1.4", Kernel.bootInfo.argc, Kernel.bootInfo.argv)
			ELSE
				res := Kernel.MessageBox("BlackBox", 
										"HostGnome: open of Gnome functions failed.", {Kernel.mbOk})
			END
		ELSE
			res := Kernel.MessageBox("BlackBox", "HostGnome: failed to load libgnomeui.so.", {Kernel.mbOk})
		END;
		(* Gnome overrides some signals, they need to be re-installed. *)
		Kernel.InstallSignals
	END Init;
	
BEGIN
	Init
END HostGnome.

DevDecoder.Decode HostGnome