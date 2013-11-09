MODULE HostClipboard;

	(* TODO: Add converters, memory files and other stuff to support multiple BlackBoxes and more formats *)

	IMPORT 
		SYSTEM, LinLibc, LinGtk, LinGdk, Dialog, Views, Controllers, HostWindows, TextModels, TextViews, TextControllers, Services;
		
	CONST
		string* = 0; view* = 1; (* types for the clipboard *)
		
	TYPE
		Clip = POINTER TO RECORD
			view: Views.View;
			w, h: INTEGER;
			isSingle: BOOLEAN;
			type: ARRAY 256 OF CHAR;
		END;
		
	VAR
		bbAtom-: INTEGER;
		curClip: Clip;
		
	PROCEDURE Register* (v: Views.View; w, h: INTEGER; isSingle: BOOLEAN);
	BEGIN
		NEW(curClip);
		curClip.view := Views.CopyOf(v, Views.deep);
		curClip.w := w; curClip.h := h; curClip.isSingle := isSingle;
		Services.GetTypeName(v, curClip.type);
		IF LinGtk.gtk_selection_owner_set(SYSTEM.VAL(LinGtk.GtkWidget, HostWindows.main), LinGdk.GDK_SELECTION_PRIMARY, 														LinGdk.GDK_CURRENT_TIME) = 0 THEN
			Dialog.ShowMsg("HostClipboard.Register failed")
		END
	END Register;
	
	PROCEDURE Clear* (w: LinGtk.GtkWidget; event, user_data: LinLibc.PtrVoid);
	BEGIN
		curClip := NIL
	END Clear;
	
	PROCEDURE ConvertCopy* (widget: LinGtk.GtkWidget; selection_data: LinGtk.GtkSelectionData; info, time: INTEGER; 
											data: LinLibc.PtrVoid);
		VAR 
			s: ARRAY 256 OF SHORTCHAR; v: Views.View; beg, end, i: INTEGER; rd: TextModels.Reader; 
			ss: POINTER TO ARRAY OF SHORTCHAR; ch: CHAR;
	BEGIN
		IF curClip # NIL THEN
			s := "BlackBox View: " + SHORT(curClip.type);
			IF info = LinGdk.GDK_SELECTION_TYPE_STRING THEN (* TODO: Current implementation always does this. *)
				v := curClip.view;
				WITH v: TextViews.View DO
					NEW(ss, v.ThisModel().Length() + 1);
					rd := v.ThisModel().NewReader(NIL);
					rd.SetPos(0);
					i := 0; rd.ReadChar(ch);
					WHILE ~rd.eot DO ss[i] := SHORT(ch); INC(i); rd.ReadChar(ch) END;
					ss[i] := 0X;
					LinGtk.gtk_selection_data_set(selection_data, LinGdk.GDK_SELECTION_TYPE_STRING, 8, 
															ss^(*SYSTEM.VAL(LinLibc.PtrSTR, ss)*), LEN(ss$))
				ELSE
					LinGtk.gtk_selection_data_set(selection_data, LinGdk.GDK_SELECTION_TYPE_STRING, 8, 
															SYSTEM.VAL(LinLibc.PtrSTR, SYSTEM.ADR(s)), LEN(s$))
				END;
			ELSIF info = bbAtom THEN
				(* TODO: This is where memory files and stuff are needed to make it work between two different BB-processes *)
				LinGtk.gtk_selection_data_set(selection_data, bbAtom, 8, SYSTEM.VAL(LinLibc.PtrSTR, SYSTEM.ADR(s)), LEN(s$))
			ELSE
				Dialog.ShowMsg("HostClipboard.ConvertCopy: Unknown type")
			END
		END
	END ConvertCopy;

	PROCEDURE AtoD (IN ss: ARRAY OF SHORTCHAR): POINTER TO ARRAY OF CHAR;
		VAR s: POINTER TO ARRAY OF CHAR; i: INTEGER;
	BEGIN
		NEW(s, LEN(ss$) + 1);
		i := 0;
		WHILE ss[i] # 0X DO 
			IF ss[i] = 0AX THEN s[i] := 0DX ELSE s[i] := ss[i] END; 
			INC(i) 
		END;
		s[i] := 0X;
		RETURN s
	END AtoD;
	
	(* Must be connected to signal "selection-received" for HostWindows.main. This is done in HostMenus. *)
	PROCEDURE DoPaste* (widget: LinGtk.GtkWidget; selection_data: LinGtk.GtkSelectionData; time: INTEGER; data: LinLibc.PtrVoid);
		VAR ops: Controllers.PollOpsMsg; msg: Controllers.EditMsg;
	BEGIN
		IF selection_data.length < 0 THEN Dialog.ShowMsg("HostClipboard.DoPaste: Selection retrieval failed.") END;
		IF selection_data.type IN {bbAtom, LinGdk.GDK_SELECTION_TYPE_STRING} THEN
			Controllers.PollOps(ops);
			IF Controllers.paste IN ops.valid THEN
				msg.clipboard := TRUE;
				IF curClip = NIL THEN (* TODO: Is this a valid way to figure out if the current copy is a BB view or not? *)
					(* TODO: Convert 0AX to 0DX to get linefeeds? *)
					msg.view := TextViews.dir.New(TextModels.dir.NewFromString(AtoD(selection_data.data$)));
					msg.isSingle := FALSE; msg.w := 0; msg.h := 0
				ELSE
					msg.view := Views.CopyOf(curClip.view, Views.deep);
					msg.isSingle := curClip.isSingle; msg.w := curClip.w; msg.h := curClip.h
				END;
				IF msg.view # NIL THEN
					msg.op := Controllers.paste; Controllers.Forward(msg)
				END
			END
		ELSE
			Dialog.ShowMsg("unknown selection type")
		END
	END DoPaste;
	
	
	PROCEDURE InitPaste*;
	BEGIN
		(* asynchronous call to DoPaste *)
(*		IF LinGtk.gtk_selection_convert(SYSTEM.VAL(LinGtk.GtkWidget, HostWindows.main), LinGdk.GDK_SELECTION_PRIMARY, 														bbAtom, LinGdk.GDK_CURRENT_TIME) = 0 THEN
			Dialog.ShowMsg("bb view paste failed, trying string");
*)
			IF LinGtk.gtk_selection_convert(SYSTEM.VAL(LinGtk.GtkWidget, HostWindows.main), LinGdk.GDK_SELECTION_PRIMARY, 														LinGdk.GDK_TARGET_STRING, LinGdk.GDK_CURRENT_TIME) = 0 THEN
				Dialog.ShowMsg("gtk_selection_convert failed")
			END
(*		END*)
	END InitPaste;

	PROCEDURE Init;
	BEGIN
		bbAtom := LinGdk.gdk_atom_intern("BBView", LinLibc.FALSE)
	END Init;

BEGIN
	Init
END HostClipboard.