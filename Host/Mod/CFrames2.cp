MODULE HostCFrames;

	(* TODO:
		- 
	*)


	(* 
		Implementation docu
	
		Most controls fit fine with widgets from Gtk. Some controls (ListBoxes, Fields) need a scrolled window around their 
		widget to allow for scrolling. Some other widgets (Captions, CheckBoxes) do not have their own window and need
		to be wrapped in an even box.
		
	 *)

	IMPORT 
		SYSTEM, Kernel, LinLibc, LinGLib, LinGdk, LinGtk, Files, Dialog, Ports, Fonts, Views, Controllers, Services, 
		StdCFrames, TextViews, TextModels, Stores, 
		HostFiles, HostFonts, HostPorts,
		Log;
		
	CONST 
		RDEL = 07X; LDEL = 08X; ESC = 1BX;
		dropDownHeight = 30 * Ports.point;
		
		(* possible values for eventMaskState *)
		eUndef = 0; eOn = 1; eOff = 2; 
		
		(* alignment in NewLabel *)
		none = 0; left = 1; right = 2; center = 3;
		
		scrollRange = 16384; lineInc = 1; pageInc = 100; defThumbSize = scrollRange DIV 20;

	TYPE
		Directory = POINTER TO RECORD (StdCFrames.Directory) END;
		
		(* Delays mouse events until the end of the command *)
		DelayedMouseEvent = POINTER TO RECORD (Services.Action) 
			eventPending: BOOLEAN;
			forwardWidget: LinGtk.GtkWidget;
			forwardEvent: LinGdk.GdkEventButtonDesc;
			delayedEvent: LinGdk.GdkEventButtonDesc;
		END;
		
		TrapCleaner = POINTER TO RECORD (Kernel.TrapCleaner) END;
		
		Info = POINTER TO RECORD 
			frame: StdCFrames.Frame; (* pointer back to the frame that owns the Info *)
			mainWidget: LinGtk.GtkWidget; (* the outer most widget *)
			eventWidget: LinGtk.GtkWidget; (* widget to recieve events, not necessarily the same as mainWidget  *)
			allowMouse, allowKey, hasFocus: BOOLEAN;
			eventMaskState: INTEGER;
		END;
		
		Caption = POINTER TO RECORD (StdCFrames.Caption) 
			labelWidget: LinGtk.GtkWidget;
			ebox: LinGtk.GtkWidget; (* a label widget does not have its own GdkWindow, so it needs to be wrapped in an event box *)
			i: Info
		END;
		CheckBox = POINTER TO RECORD (StdCFrames.CheckBox)
			ebox, checkButton, lbl: LinGtk.GtkWidget;
			hasFocus: BOOLEAN;
			i: Info
		END;
		ColorField = POINTER TO RECORD (StdCFrames.ColorField) END;
		ComboBox = POINTER TO RECORD (StdCFrames.ComboBox)
			combo: LinGtk.GtkWidget;
			isUpdate: BOOLEAN;
			i: Info
		END;
		DateField = POINTER TO RECORD (StdCFrames.DateField) END;
		Field = POINTER TO RECORD (StdCFrames.Field) 
			text, scrlw: LinGtk.GtkWidget;
			isUpdate: BOOLEAN;
			TextLen: PROCEDURE (text: LinGtk.GtkWidget): INTEGER;
			i: Info
		END;
		Group = POINTER TO RECORD (StdCFrames.Group) END;
		ListBox = POINTER TO RECORD (StdCFrames.ListBox)
			list, scrlw: LinGtk.GtkWidget;
			i: Info
		END;
		PushButton = POINTER TO RECORD (StdCFrames.PushButton) 
			button: LinGtk.GtkWidget;
			hasFocus: BOOLEAN;
			i: Info
		END;
		RadioButton = POINTER TO RECORD (StdCFrames.RadioButton)
			ebox, radioButton, lbl: LinGtk.GtkWidget;
			i: Info
		END;
		ScrollBar = POINTER TO RECORD (StdCFrames.ScrollBar)
			scrollBar: LinGtk.GtkWidget;
			isUpdate: BOOLEAN;
			i: Info
		END;
		SelectionBox = POINTER TO RECORD (StdCFrames.SelectionBox)
			list, scrlw: LinGtk.GtkWidget;
			num: INTEGER; (* the number of items in the list, updated by UpdateList *)
			i: Info
		END;
		TimeField = POINTER TO RECORD (StdCFrames.TimeField) END;
		TreeFrame = POINTER TO RECORD (StdCFrames.TreeFrame) 
			ctree, scrlw: LinGtk.GtkWidget;
			hasFocus: BOOLEAN;
			i: Info
		END;
		UpDownField = POINTER TO RECORD (StdCFrames.UpDownField)
			spin: LinGtk.GtkWidget;
			hasFocus, isUpdate: BOOLEAN;
			val: INTEGER;
			i: Info
		END;
		
	VAR 
		mouseDelayer: DelayedMouseEvent;
		
		(* icons for the tree *)
		tree_closed, tree_open, tree_leaf: LinGdk.GdkPixmap;
		tree_closed_mask, tree_open_mask, tree_leaf_mask: LinGdk.GdkBitmap; 
		
		(* used in HandleMouse *)
		released: BOOLEAN;	
		currentInfo: Info;
		
		(* holds the state of the last key event. this is used later widthin the same command. *)
		lastKeyState: SET;
		
(*
	PROCEDURE DumpEvent (e: LinGdk.GdkEventButtonDesc);
	BEGIN
		Log.String("type: "); Log.Int(e.type); Log.Ln;
		Log.String("window: "); Log.Int(e.window); Log.Ln;
		Log.String("send_event: "); Log.Int(e.send_event); Log.Ln;
		Log.String("time: "); Log.Int(e.time); Log.Ln;
		Log.String("x, y, pressure, xtilt: "); Log.Real(e.x); Log.Real(e.y); Log.Real(e.pressure); Log.Real(e.xtilt); Log.Ln;
		Log.String("state: "); Log.Set(e.state); Log.Ln;
		Log.String("button: "); Log.Int(e.button); Log.Ln;
		Log.String("source: "); Log.Int(e.source); Log.Ln;
		Log.String("deviceid: "); Log.Int(e.deviceid); Log.Ln;
		Log.String("x_root, yroot: "); Log.Real(e.x_root); Log.Real(e.y_root); Log.Ln;		
	END DumpEvent;
*)

(*
	(* connect the "event" signal to this function to spy on events to a widget *)
	PROCEDURE EventSpy (widget: LinGtk.GtkWidget; event: LinGdk.GdkEvent; user_data: LinLibc.PtrVoid): INTEGER;
	BEGIN
		CASE event.type OF
			LinGdk.GDK_NOTHING: Dialog.ShowMsg("GDK_NOTHING");
			| LinGdk.GDK_DELETE: Dialog.ShowMsg("GDK_DELETE");
			| LinGdk.GDK_DESTROY: Dialog.ShowMsg("GDK_DESTROY");
			| LinGdk.GDK_EXPOSE: Dialog.ShowMsg("GDK_EXPOSE");
			| LinGdk.GDK_MOTION_NOTIFY: Dialog.ShowMsg("GDK_MOTION_NOTIFY");
			| LinGdk.GDK_BUTTON_PRESS: Dialog.ShowMsg("GDK_BUTTON_PRESS");
			| LinGdk.GDK_2BUTTON_PRESS: Dialog.ShowMsg("GDK_2BUTTON_PRESS");
			| LinGdk.GDK_3BUTTON_PRESS: Dialog.ShowMsg("GDK_3BUTTON_PRESS");
			| LinGdk.GDK_BUTTON_RELEASE: Dialog.ShowMsg("GDK_BUTTON_RELEASE");
			| LinGdk.GDK_KEY_PRESS: Dialog.ShowMsg("GDK_KEY_PRESS");
			| LinGdk.GDK_KEY_RELEASE: Dialog.ShowMsg("GDK_KEY_RELEASE");
			| LinGdk.GDK_ENTER_NOTIFY: Dialog.ShowMsg("GDK_ENTER_NOTIFY");
			| LinGdk.GDK_LEAVE_NOTIFY: Dialog.ShowMsg("GDK_LEAVE_NOTIFY");
			| LinGdk.GDK_FOCUS_CHANGE: Dialog.ShowMsg("GDK_FOCUS_CHANGE");
			| LinGdk.GDK_CONFIGURE: Dialog.ShowMsg("GDK_CONFIGURE");
			| LinGdk.GDK_MAP: Dialog.ShowMsg("GDK_MAP");
			| LinGdk.GDK_UNMAP: Dialog.ShowMsg("GDK_UNMAP");
			| LinGdk.GDK_PROPERTY_NOTIFY: Dialog.ShowMsg("GDK_PROPERTY_NOTIFY");
			| LinGdk.GDK_SELECTION_CLEAR: Dialog.ShowMsg("GDK_SELECTION_CLEAR");
			| LinGdk.GDK_SELECTION_REQUEST: Dialog.ShowMsg("GDK_SELECTION_REQUEST");
			| LinGdk.GDK_SELECTION_NOTIFY: Dialog.ShowMsg("GDK_SELECTION_NOTIFY");
			| LinGdk.GDK_PROXIMITY_IN: Dialog.ShowMsg("GDK_PROXIMITY_IN");
			| LinGdk.GDK_PROXIMITY_OUT: Dialog.ShowMsg("GDK_PROXIMITY_OUT");
			| LinGdk.GDK_DRAG_ENTER: Dialog.ShowMsg("GDK_DRAG_ENTER");
			| LinGdk.GDK_DRAG_LEAVE: Dialog.ShowMsg("GDK_DRAG_LEAVE");
			| LinGdk.GDK_DRAG_MOTION: Dialog.ShowMsg("GDK_DRAG_MOTION");
			| LinGdk.GDK_DRAG_STATUS: Dialog.ShowMsg("GDK_DRAG_STATUS");
			| LinGdk.GDK_DROP_START: Dialog.ShowMsg("GDK_DROP_START");
			| LinGdk.GDK_DROP_FINISHED: Dialog.ShowMsg("GDK_DROP_FINISHED");
			| LinGdk.GDK_CLIENT_EVENT: Dialog.ShowMsg("GDK_CLIENT_EVENT");
			| LinGdk.GDK_VISIBILITY_NOTIFY: Dialog.ShowMsg("GDK_VISIBILITY_NOTIFY");
			| LinGdk.GDK_NO_EXPOSE: Dialog.ShowMsg("GDK_NO_EXPOSE");
		END;
		RETURN LinLibc.FALSE;
	END EventSpy;
*)

	(* Common mouse handling (filter mouse events throught the BlackBox framework) *)
	
	PROCEDURE (a: DelayedMouseEvent) Do-;
	BEGIN
		ASSERT(a.forwardWidget # NIL, 20); ASSERT(a.eventPending, 21);
		LinGtk.gtk_widget_event(a.forwardWidget, SYSTEM.VAL(LinGdk.GdkEvent, SYSTEM.ADR(a.forwardEvent)));
		a.forwardWidget := NIL; a.eventPending := FALSE;
	END Do;
	
	PROCEDURE MouseHandler (widget: LinGtk.GtkWidget; event: LinGdk.GdkEventButton; user_data: LinLibc.PtrVoid): INTEGER;
		VAR i: Info; 
	BEGIN			
		i := SYSTEM.VAL(Info, user_data);
		IF ~i.allowMouse THEN		
			LinGtk.gtk_signal_emit_stop_by_name(widget, "button-press-event");
			IF (i.frame.rider(HostPorts.Rider).port.da # NIL) & (event.type = LinGdk.GDK_BUTTON_PRESS) THEN
				ASSERT(mouseDelayer.forwardWidget = NIL, 100); ASSERT(~mouseDelayer.eventPending, 101);
				mouseDelayer.delayedEvent := event; 
				mouseDelayer.forwardEvent := event; 
				mouseDelayer.forwardEvent.window := i.frame.rider(HostPorts.Rider).port.da.window;
				mouseDelayer.forwardEvent.x := mouseDelayer.forwardEvent.x + i.mainWidget.allocation.x;
				mouseDelayer.forwardEvent.y := mouseDelayer.forwardEvent.y + i.mainWidget.allocation.y;
				mouseDelayer.forwardWidget := i.frame.rider(HostPorts.Rider).port.da;
				mouseDelayer.eventPending := TRUE;
				(* wait with propagating event to parent until after this procedure has returned *)
				Services.DoLater(mouseDelayer, Services.immediately)
			END;
			RETURN LinLibc.TRUE
		ELSE
			RETURN LinLibc.TRUE
		END 
	END MouseHandler;
	
	PROCEDURE MouseRelease (widget: LinGtk.GtkWidget; event: LinGdk.GdkEventButton; user_data: LinLibc.PtrVoid): INTEGER;
	BEGIN
		released := TRUE;
		RETURN LinLibc.FALSE
	END MouseRelease;
	
	PROCEDURE (t: TrapCleaner) Cleanup;
	BEGIN
		released := TRUE; 
		mouseDelayer.eventPending := FALSE;
		currentInfo.allowMouse := FALSE;
		currentInfo := NIL
	END Cleanup;
	
	PROCEDURE HandleMouse (i: Info; x, y: INTEGER; buttons: SET);
		VAR res: INTEGER; tc: TrapCleaner;
	BEGIN
		IF mouseDelayer.eventPending THEN (* BlackBox tries to propagate an Event that the Gtk control didn't want *)
			NEW(tc); Kernel.PushTrapCleaner(tc);
			currentInfo := i;
			LinGtk.gtk_grab_remove(i.frame.rider(HostPorts.Rider).port.da);		
(*			ASSERT((mouseDelayer.delayedEvent.x = x + 1) & (mouseDelayer.delayedEvent.y = y + 1), 20); (* TODO: why "+ 1" ? *)*)
			i.allowMouse := TRUE;
			LinGtk.gtk_widget_event(i.eventWidget, 
											SYSTEM.VAL(LinGdk.GdkEvent, SYSTEM.ADR(mouseDelayer.delayedEvent)));
			released := FALSE;		
			REPEAT
(*				Dialog.Beep;*)
				res := LinGtk.gtk_main_iteration()
			UNTIL released;
			i.allowMouse := FALSE;
			currentInfo := NIL;
			Kernel.PopTrapCleaner(tc)
		END
	END HandleMouse;	
	
	PROCEDURE UpdateEventMask (i: Info);
	(* This is a hack to get mouse events propagated to the parent when the widget is disabled *)
	(* TODO: It only seems to work with buttons... *)
		VAR mask: LinGdk.GdkEventMask;
	BEGIN
		IF  i.eventWidget.window # LinLibc.NULL THEN
			IF ~i.frame.disabled & ~i.frame.readOnly THEN
				IF i.eventMaskState # eOn THEN
					mask := LinGdk.gdk_window_get_events(i.eventWidget.window);
					mask := mask + LinGdk.GDK_BUTTON_PRESS_MASK;
					LinGdk.gdk_window_set_events(i.eventWidget.window, mask);
					i.eventMaskState := eOn
				END
			ELSIF i.eventMaskState # eOff THEN
				mask := LinGdk.gdk_window_get_events(i.eventWidget.window);
				mask := mask - LinGdk.GDK_BUTTON_PRESS_MASK;
				LinGdk.gdk_window_set_events(i.eventWidget.window, mask);	
				i.eventMaskState := eOff
			END
		ELSE
			i.eventMaskState := eUndef
		END
	END UpdateEventMask;
	
	
	(* Common key handling (filter key events throught the BlackBox framework) *)
	
	PROCEDURE KeyHandler(widget: LinGtk.GtkWidget; event: LinGdk.GdkEventKey; user_data: LinLibc.PtrVoid): INTEGER;
		VAR i: Info; 
	BEGIN			
		i := SYSTEM.VAL(Info, user_data);
		IF ~i.allowKey THEN
			lastKeyState := event.state;
			LinGtk.gtk_signal_emit_stop_by_name(widget, "key-press-event");
			RETURN LinLibc.TRUE
		ELSE
			RETURN LinLibc.FALSE
		END 
	END KeyHandler;
	
	PROCEDURE HandleKey (i: Info; char: CHAR);
		VAR ne: LinGdk.GdkEventKeyDesc; code: INTEGER;
	BEGIN
		i.allowKey := TRUE;
		CASE char OF
			| 10X: code := 0FF55H
			| 11X: code := 0FF56H
			| 12X: code := 0FF55H
			| 13X: code := 0FF56H
			| 14X: code := 0FF50H
			| 15X: code := 0FF57H
			| 16X: code := 0FF50H
			| 17X: code := 0FF57H
			| 1CX: code := 0FF51H
			| 1DX: code := 0FF53H
			| 1EX: code := 0FF52H
			| 1FX: code := 0FF54H
			| LDEL: code := 0FF08H
			| RDEL: code := 0FFFFH
			| ESC: code := 0FF1BH
		ELSE code := 0
		END;
		IF code # 0 THEN 
			ne.keyval := code;
		ELSE
			ne.keyval := ORD(char);
		END;		
		ne.length := 1;
		ne.string := SYSTEM.VAL(LinLibc.PtrSTR, SYSTEM.ADR(char));
		ne.type := LinGdk.GDK_KEY_PRESS;
		ne.window := i.eventWidget.window;
		ne.send_event := LinLibc.TRUE;
		ne.time := LinGdk.GDK_CURRENT_TIME;
		ne.state := lastKeyState;
		LinGtk.gtk_widget_event(i.eventWidget, SYSTEM.VAL(LinGdk.GdkEvent, SYSTEM.ADR(ne)));
		i.allowKey := FALSE
	END HandleKey;
	
	
	PROCEDURE NewInfo (frame: StdCFrames.Frame; mainWidget, eventWidget: LinGtk.GtkWidget): Info;
		VAR i: Info; res: INTEGER;
	BEGIN
		ASSERT(frame # NIL, 20); ASSERT(mainWidget # NIL, 21); ASSERT(eventWidget # NIL, 22);
		NEW(i);
		i.allowMouse := FALSE; i.frame := frame; i.mainWidget := mainWidget; i.eventWidget := eventWidget;
		i.eventMaskState := eUndef;
		(* connect signals for the mouse handling - the new info is passed as user_data *)
		res := LinGtk.gtk_signal_connect(eventWidget, "button_press_event", 
													SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(MouseHandler)), 
													SYSTEM.VAL(INTEGER, i));
		res := LinGtk.gtk_signal_connect_after(eventWidget, "button-release-event", 
													SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(MouseRelease)), 
													SYSTEM.VAL(INTEGER, i));
		res := LinGtk.gtk_signal_connect(eventWidget, "key-press-event", 
													SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(KeyHandler)), 
													SYSTEM.VAL(INTEGER, i));
		RETURN i
	END NewInfo;
	
		
	(* On Windows & is used to mark an Alt-shortcut. This is not available on Linux. *)
	PROCEDURE RemoveAmpersand (VAR ss: ARRAY OF CHAR);
		VAR i, j: INTEGER;
	BEGIN
		i := 0; j := 0;
		WHILE ss[i] # 0X DO
			IF ss[i] # "&" THEN 
				ss[j] := ss[i]; INC(j); 
				INC(i)
			ELSE
				INC(i);
				IF ss[i] = "&" THEN ss[j] := ss[i]; INC(j); INC(i) END
			END
		END;
		ss[j] := 0X
	END RemoveAmpersand;
	
	PROCEDURE EditableTextLen (editable: LinGtk.GtkWidget): INTEGER;
		VAR s: LinLibc.PtrSTR; res: INTEGER;
	BEGIN
		res := 0;
		s := LinGtk.gtk_editable_get_chars(editable, 0, -1);
		IF s # NIL THEN
			res := LEN(s$);
			LinGLib.g_free(SYSTEM.VAL(LinLibc.PtrVoid, s)) 
		END;
		RETURN res
	END EditableTextLen;
		
	PROCEDURE DummyRestore (f: StdCFrames.Frame; lbl: Dialog.String; l, t, r, b: INTEGER);
		VAR w, h, mx, my, dw, dh, asc, dsc: INTEGER;
	BEGIN
		RemoveAmpersand(lbl);
		f.view.context.GetSize(w, h); 
		mx := w DIV 2; my := h DIV 2;
		dw := HostFonts.dlgFont.SStringWidth(SHORT(lbl)) DIV 2;
		HostFonts.dlgFont.GetBounds(asc, dsc, dh); dh := (asc + dsc) DIV 2;	
		f.DrawRect(0, 0, w, h, Ports.fill, Ports.blue);	
		f.DrawRect(0, 0, w, h, 1, Ports.black);
		f.DrawString(mx - dw, my + dh, Ports.white, lbl, HostFonts.dlgFont)
	END DummyRestore;
	
	PROCEDURE Mark (on, focus: BOOLEAN; i: Info);
	BEGIN
		IF focus & (i # NIL) & (i.eventWidget # NIL) THEN
			IF on THEN
				IF ~i.hasFocus THEN
					LinGtk.gtk_container_set_focus_child(i.frame.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer),
						 i.eventWidget);
					LinGtk.gtk_widget_grab_focus(i.eventWidget);
					LinGtk.gtk_widget_draw_focus(i.eventWidget);
					i.hasFocus := TRUE
				END
			ELSE
				IF i.hasFocus THEN
					LinGtk.gtk_container_set_focus_child(i.frame.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer), NIL);
					LinGtk.gtk_widget_draw_default(i.eventWidget);
					i.hasFocus := FALSE
				END
			END
		END
	END Mark;
	
	PROCEDURE Paint (i: Info);
	BEGIN
		i.frame.rider(HostPorts.Rider).port.CloseBuffer;
		LinGtk.gtk_widget_show_all(i.mainWidget);
	END Paint;
	
	PROCEDURE NewStyle (font: Fonts.Font): LinGtk.GtkRcStyle;
		VAR style: LinGtk.GtkRcStyle;
	BEGIN
		style := LinGtk.gtk_rc_style_new();
		style.fontset_name := font(HostFonts.Font).dev.xlfd;
		RETURN style
	END NewStyle;
	
	PROCEDURE Align (isLeft, isRight: BOOLEAN): INTEGER;
		VAR align: INTEGER;
	BEGIN
		IF isRight & ~isLeft THEN 
			align := right
		ELSIF ~isLeft THEN
			align := center
		ELSE
			align := left
		END;
		RETURN align
	END Align;
	
	PROCEDURE NewLabel (VAR text: ARRAY OF CHAR; style: LinGtk.GtkRcStyle; align: INTEGER): LinGtk.GtkWidget;
		VAR l: LinGtk.GtkWidget; ss: ARRAY 256 OF SHORTCHAR; 
	BEGIN
		Dialog.MapString(text, text);
		RemoveAmpersand(text);
		ss := SHORT(text$); (* TODO: This is where the unicode support needs to go... *)
		l := LinGtk.gtk_label_new(ss);
		LinGtk.gtk_widget_modify_style(l, style);
		
(*		TODO: Justification does not seem to work. Is it necessary to pack the label into a GtkHBox to align it correctly? *)

		(* work around: Always align left when align # none *)
		IF align # none THEN
			LinGtk.gtk_misc_set_alignment(l, 0, 0)
		END;
		
(*		
		IF align = right THEN 
(*			LinGtk.gtk_misc_set_alignment(l, 1, 1);*)
			LinGtk.gtk_label_set_justify(l, LinGtk.GTK_JUSTIFY_RIGHT)
		ELSIF align = center THEN
(*			LinGtk.gtk_misc_set_alignment(l, 0.5, 0.5);*)
			LinGtk.gtk_label_set_justify(l, LinGtk.GTK_JUSTIFY_CENTER)
		ELSIF align = left THEN
(*			LinGtk.gtk_misc_set_alignment(l, 0, 0);*)
			LinGtk.gtk_label_set_justify(l, LinGtk.GTK_JUSTIFY_LEFT)
		END;
*)

		RETURN l
	END NewLabel;
	
	(* Caption *)

	PROCEDURE (c: Caption) Update;
	BEGIN
		IF ~c.disabled THEN
			LinGtk.gtk_widget_set_sensitive(c.labelWidget, LinLibc.TRUE)
		ELSE
			LinGtk.gtk_widget_set_sensitive(c.labelWidget, LinLibc.FALSE)
		END;
	END Update;
	
	PROCEDURE (c: Caption) Restore (l, t, r, b: INTEGER);
		VAR w, h, cx, cy, cw, ch, res: INTEGER;
	BEGIN
		IF c.labelWidget = NIL THEN
			c.noRedraw := TRUE;
			c.labelWidget := NewLabel(c.label,NewStyle(c.font), Align(c.left, c.right));
			LinGtk.gtk_widget_ref(c.labelWidget);
			c.ebox := LinGtk.gtk_event_box_new();
			LinGtk.gtk_widget_ref(c.ebox);
			c.i := NewInfo(c, c.ebox, c.ebox);
			c.rider(HostPorts.Rider).GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
			LinGtk.gtk_widget_set_usize(c.ebox, cw, ch);	
			LinGtk.gtk_container_add(c.ebox(LinGtk.GtkContainer), c.labelWidget);
			LinGtk.gtk_label_set_line_wrap(c.labelWidget, LinLibc.FALSE);
			LinGtk.gtk_fixed_put(c.rider(HostPorts.Rider).port.fixed, c.ebox, SHORT(cx), SHORT(cy));
		END;		
		c.Update;
		Paint(c.i)
	END Restore;
	
	PROCEDURE (c: Caption) Close;
	BEGIN
		IF c.labelWidget # NIL THEN
			LinGtk.gtk_widget_unref(c.labelWidget);
			LinGtk.gtk_widget_unref(c.ebox);
			LinGtk.gtk_container_remove(c.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer), c.ebox);
		END
	END Close;	
	
	(* CheckBox *)

	PROCEDURE CheckToggled (checkButton: LinGtk.GtkWidget; user_data: INTEGER);
		VAR f: CheckBox;
	BEGIN
		f := SYSTEM.VAL(CheckBox, user_data);
		IF ~f.disabled THEN
			f.Set(f, LinGtk.gtk_toggle_button_get_active(f.checkButton) # LinLibc.FALSE)
		END
	END CheckToggled;

	PROCEDURE (c: CheckBox) Update;
		VAR res: INTEGER; value, mixed: BOOLEAN; mask: LinGdk.GdkEventMask;
	BEGIN
		IF ~c.disabled THEN
			c.Get(c, value);
			IF c.undef THEN 
(*				res := USER32.SendMessageA(f.i.ctrl, USER32.BMSetCheck, 2, 0) TODO: Here what? *)
			ELSIF value THEN LinGtk.gtk_toggle_button_set_active(c.checkButton, LinLibc.TRUE)
			ELSE LinGtk.gtk_toggle_button_set_active(c.checkButton, LinLibc.FALSE)
			END;
			IF c.readOnly THEN LinGtk.gtk_widget_set_sensitive(c.checkButton, LinLibc.FALSE) 
			ELSE LinGtk.gtk_widget_set_sensitive(c.checkButton, LinLibc.TRUE)
			END
		ELSE
			LinGtk.gtk_toggle_button_set_active(c.checkButton, LinLibc.FALSE);
			LinGtk.gtk_widget_set_sensitive(c.checkButton, LinLibc.FALSE)
		END;
(*		CheckLabel(f.label, f.i.ctrl);*)
(*
		UpdateEventMask(c.i);
		IF c.lbl.window # LinLibc.NULL THEN
			mask := LinGdk.gdk_window_get_events(c.lbl.window);
			mask := mask - LinGdk.GDK_BUTTON_PRESS_MASK;
			LinGdk.gdk_window_set_events(c.lbl.window, mask)
		END	
*)
	END Update;
	
	PROCEDURE (c: CheckBox) Restore (l, t, r, b: INTEGER);
		VAR w, h, cx, cy, cw, ch, res: INTEGER;
	BEGIN
		IF c.checkButton = NIL THEN
			c.noRedraw := TRUE;
			c.lbl := NewLabel(c.label, NewStyle(c.font), left);
			LinGtk.gtk_widget_ref(c.lbl);
			c.rider(HostPorts.Rider).GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
			c.checkButton := LinGtk.gtk_check_button_new();
			LinGtk.gtk_widget_ref(c.checkButton);
			res := LinGtk.gtk_signal_connect(c.checkButton, "toggled", 
											SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(CheckToggled)),
											SYSTEM.VAL(INTEGER, c));
			LinGtk.gtk_container_add(c.checkButton(LinGtk.GtkContainer), c.lbl);
			c.ebox := LinGtk.gtk_event_box_new();
			LinGtk.gtk_widget_ref(c.ebox);
			c.i := NewInfo(c, c.ebox, c.checkButton);
			LinGtk.gtk_widget_set_usize(c.ebox, cw, ch);	
			LinGtk.gtk_container_add(c.ebox(LinGtk.GtkContainer), c.checkButton);
			LinGtk.gtk_fixed_put(c.rider(HostPorts.Rider).port.fixed, c.ebox, SHORT(cx), SHORT(cy));
		END;		
		c.Update;
		Paint(c.i)
	END Restore;
	
	PROCEDURE (c: CheckBox) Close;
	BEGIN
		IF c.checkButton # NIL THEN
			LinGtk.gtk_widget_unref(c.lbl);
			LinGtk.gtk_widget_unref(c.checkButton);
			LinGtk.gtk_widget_unref(c.ebox);
			LinGtk.gtk_container_remove(c.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer), c.ebox)
		END
	END Close;
	
	PROCEDURE (f: CheckBox) Mark (on, focus: BOOLEAN);
	BEGIN
(*		Mark(on, f.front, f.checkButton, f.hasFocus);*)
		Mark(on, f.front, f.i);
	END Mark;
	
	PROCEDURE (f: CheckBox) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;	
	
	PROCEDURE (f: CheckBox) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF ch = " "  THEN 
			LinGtk.gtk_button_clicked(f.checkButton)
		END
	END KeyDown;
	
	(* ColorField *)
		
	PROCEDURE (c: ColorField) Restore (l, t, r, b: INTEGER);
	BEGIN
		DummyRestore(c, "ColorField", l, t, r, b)
	END Restore;
		
	(* ComboBox *)	
		
	PROCEDURE ComboChanged (editable: LinGtk.GtkWidget; user_data: LinLibc.PtrVoid);
		VAR s: LinLibc.PtrSTR; c: ComboBox;
	BEGIN
		c := SYSTEM.VAL(ComboBox, user_data);
		IF ~c.isUpdate THEN 
			s := LinGtk.gtk_editable_get_chars(editable, 0, -1);
			c.Set(c, s$);
			LinGLib.g_free(SYSTEM.VAL(LinLibc.PtrVoid, s)) 
		END
	END ComboChanged;
	
	PROCEDURE (c: ComboBox) UpdateList;
		VAR 
			strs: POINTER TO ARRAY OF ARRAY [untagged] LEN(Dialog.String) OF SHORTCHAR; 
			gl: LinGLib.GList; l, i: INTEGER; s: Dialog.String;
	BEGIN
		l := 0;
		REPEAT c.GetName(c, l, s); INC(l) UNTIL s = "";
		DEC(l);
		IF l > 0 THEN
			NEW(strs, l);
			gl := NIL;
			i := 0;
			WHILE i < l DO
				c.GetName(c, i, s); Dialog.MapString(s, s);
				strs[i] := SHORT(s$);
				gl := LinGLib.g_list_append(gl, SYSTEM.VAL(LinLibc.PtrVoid, SYSTEM.ADR(strs[i])));
				INC(i);
			END;
			LinGtk.gtk_combo_set_popdown_strings(c.combo, gl)
		END;
	END UpdateList;
	
	PROCEDURE (c: ComboBox) Update;
		VAR res: INTEGER; value, mixed: BOOLEAN; mask: LinGdk.GdkEventMask;
	BEGIN
(*	
		IF ~c.disabled THEN
			c.Get(c, value);
			IF c.undef THEN 
			ELSIF value THEN LinGtk.gtk_toggle_button_set_active(c.checkButton, LinLibc.TRUE)
			ELSE LinGtk.gtk_toggle_button_set_active(c.checkButton, LinLibc.FALSE)
			END;
			IF c.readOnly THEN LinGtk.gtk_widget_set_sensitive(c.checkButton, LinLibc.FALSE) 
			ELSE LinGtk.gtk_widget_set_sensitive(c.checkButton, LinLibc.TRUE)
			END
		ELSE
			LinGtk.gtk_toggle_button_set_active(c.checkButton, LinLibc.FALSE);
			LinGtk.gtk_widget_set_sensitive(c.checkButton, LinLibc.FALSE)
		END;
		UpdateEventMask(c.i);
		IF c.lbl.window # LinLibc.NULL THEN
			mask := LinGdk.gdk_window_get_events(c.lbl.window);
			mask := mask - LinGdk.GDK_BUTTON_PRESS_MASK;
			LinGdk.gdk_window_set_events(c.lbl.window, mask)
		END	
*)
	END Update;
	
	PROCEDURE (c: ComboBox) Restore (l, t, r, b: INTEGER);
		VAR w, h, cx, cy, cw, ch, res: INTEGER; 
	BEGIN
		IF c.combo = NIL THEN
			c.noRedraw := TRUE;
			c.combo := LinGtk.gtk_combo_new();
			res := LinGtk.gtk_signal_connect(c.combo(LinGtk.GtkCombo).entry, "changed", 
											SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(ComboChanged)),
											SYSTEM.VAL(INTEGER, c));
			c.i := NewInfo(c, c.combo, c.combo);
			c.rider(HostPorts.Rider).GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
			LinGtk.gtk_widget_set_usize(c.combo, cw, ch);	
			LinGtk.gtk_fixed_put(c.rider(HostPorts.Rider).port.fixed, c.combo, SHORT(cx), SHORT(cy));
			c.UpdateList
		END;		
		c.Update;
		Paint(c.i)
	END Restore;
	
	PROCEDURE (f: ComboBox) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i);
	END Mark;
	
	PROCEDURE (f: ComboBox) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;	
	
	PROCEDURE (f: ComboBox) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		HandleKey(f.i, ch)
	END KeyDown;
	
	PROCEDURE (c: ComboBox) Close;
	BEGIN
		IF c.combo # NIL THEN
			LinGtk.gtk_container_remove(c.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer), c.combo)
		END
	END Close;
	
	PROCEDURE  (f: ComboBox) Select (from, to: INTEGER);
	BEGIN
		IF to = MAX(INTEGER) THEN to := -1 END;
		LinGtk.gtk_editable_select_region(f.combo(LinGtk.GtkCombo).entry, from, to)
	END Select;
	
	PROCEDURE  (f: ComboBox) GetSelection (OUT from, to: INTEGER);
		VAR e: LinGtk.GtkEditable;
	BEGIN
		e := f.combo(LinGtk.GtkCombo).entry(LinGtk.GtkEditable);
		IF 0 IN e.has_selection (* testing bit field *) THEN
			from := e.selection_start_pos; to := e.selection_end_pos
		ELSE	
			from := -1; to := -1;
		END
	END GetSelection;
	
	PROCEDURE  (f: ComboBox) Idle;
	BEGIN
	END Idle;
	
	PROCEDURE  (f: ComboBox) Length (): INTEGER;
	BEGIN
		RETURN EditableTextLen(f.combo)
	END Length;
	
	(* DateField *)
		
	PROCEDURE (c: DateField) Restore (l, t, r, b: INTEGER);
	BEGIN
		DummyRestore(c, "DateField", l, t, r, b)
	END Restore;


	(* Field *)
		
	PROCEDURE TextChanged (editable: LinGtk.GtkWidget; user_data: LinLibc.PtrVoid);
		VAR s: LinLibc.PtrSTR; c: Field;
	BEGIN
		c := SYSTEM.VAL(Field, user_data);
		IF ~c.isUpdate THEN 
			s := LinGtk.gtk_editable_get_chars(editable, 0, -1);
			c.Set(c, s$);
			LinGLib.g_free(SYSTEM.VAL(LinLibc.PtrVoid, s)) 
		END
	END TextChanged;
	
	PROCEDURE InsLF (VAR x: ARRAY OF CHAR);
		VAR i, j: INTEGER;
	BEGIN
		i := 0; j := 0;
		WHILE x[i] # 0X DO
			IF x[i] = 0DX THEN INC(j) END;
			INC(i); INC(j)
		END;
		x[j] := 0X;
		WHILE i # j DO
			DEC(i); DEC(j);
			IF x[i] = 0DX THEN x[j] := 0AX; DEC(j) END;
			x[j] := x[i]
		END
	END InsLF;
	
	PROCEDURE DelLF (VAR x: ARRAY OF CHAR);
		VAR i, j: INTEGER;
	BEGIN
		i := 0; j := 0;
		WHILE x[i] # 0X DO
			IF x[i] = 0AX THEN INC(i) END;
			x[j] := x[i]; INC(i); INC(j)
		END;
		x[j] := 0X;
	END DelLF;
	
	PROCEDURE Equal (f: Field; VAR x, y: ARRAY OF CHAR): BOOLEAN;
		VAR i, j: INTEGER;
	BEGIN
		DelLF(y);
		RETURN f.Equal(f, x, y)
	END Equal;
	
	PROCEDURE (f: Field) Update;
		VAR pos: INTEGER; s, s1: ARRAY 512 OF CHAR; ps, ps1: POINTER TO ARRAY OF CHAR;
			ss: ARRAY 512 OF SHORTCHAR; pss: POINTER TO ARRAY OF SHORTCHAR; style: SET;
			pstr: LinLibc.PtrSTR;
	BEGIN
		ASSERT(f.text # NIL, 20);
		IF f.maxLen > 255 THEN
			NEW(ps, 2 * f.maxLen + 1); NEW(ps1, 2 * f.maxLen + 1); NEW(pss, 2 * f.maxLen + 1);
			IF f.undef OR f.disabled THEN ps[0] := 0X ELSE f.Get(f, ps^) END;
			pstr :=  LinGtk.gtk_editable_get_chars(f.text, 0, -1);
			pss^ := pstr$;
			LinGLib.g_free(SYSTEM.VAL(LinLibc.PtrVoid, pstr)); 
			ps1^ := pss^$;
			IF (f.TextLen(f.text) >= LEN(ps^)) OR ~Equal(f, ps^, ps1^) THEN
				f.isUpdate := TRUE;
				IF f.multiLine THEN InsLF(ps^) END;
				pss^ := SHORT(ps^$); 
				LinGtk.gtk_editable_delete_text(f.text, 0, -1);
				pos := 0;
				LinGtk.gtk_editable_insert_text(f.text, pss^, LEN(pss$), pos);
				f.isUpdate := FALSE
			END
		ELSE
			IF f.undef OR f.disabled THEN s := "" ELSE f.Get(f, s) END;
			IF f.TextLen(f.text) > 0 THEN
				pstr :=  LinGtk.gtk_editable_get_chars(f.text, 0, -1);
				ss := pstr$;
				LinGLib.g_free(SYSTEM.VAL(LinLibc.PtrVoid, pstr))
			ELSE
				ss := ""
			END;
			s1 := ss$;
			IF (f.TextLen(f.text) >= LEN(s)) OR ~Equal(f, s, s1) THEN
				f.isUpdate := TRUE;
				IF f.multiLine THEN InsLF(s) END;
				ss := SHORT(s$); 
				LinGtk.gtk_editable_delete_text(f.text, 0, -1);
				pos := 0;
				LinGtk.gtk_editable_insert_text(f.text, ss, LEN(ss$), pos);
				f.isUpdate := FALSE
			END
		END;
		IF ~f.readOnly & ~f.undef THEN
			LinGtk.gtk_editable_set_editable(f.text, LinLibc.TRUE)
		ELSE
			LinGtk.gtk_editable_set_editable(f.text, LinLibc.FALSE)
		END;
		IF ~f.disabled & ~f.readOnly THEN
			LinGtk.gtk_widget_set_sensitive(f.text, LinLibc.TRUE)
		ELSE
			LinGtk.gtk_widget_set_sensitive(f.text, LinLibc.FALSE)
		END;
		LinGtk.gtk_widget_show_all(f.i.mainWidget);
		UpdateEventMask(f.i);
	END Update;
	
	PROCEDURE (c: Field) Restore (l, t, r, b: INTEGER);
		VAR w, h, cx, cy, cw, ch, res: INTEGER;
	BEGIN
		IF c.text = NIL THEN
			c.noRedraw := TRUE;
			c.view.context.GetSize(w, h);
			IF c.multiLine OR (h > dropDownHeight) THEN
				c.TextLen := LinGtk.gtk_text_get_length;
				c.text := LinGtk.gtk_text_new(NIL, NIL);
				c.scrlw := LinGtk.gtk_scrolled_window_new(NIL, NIL);
				LinGtk.gtk_widget_ref(c.scrlw);
				LinGtk.gtk_container_add(c.scrlw(LinGtk.GtkContainer), c.text);
				c.i := NewInfo(c, c.scrlw, c.text);
				LinGtk.gtk_text_set_line_wrap(c.text, LinLibc.TRUE);
				IF c.multiLine THEN
					LinGtk.gtk_scrolled_window_set_policy(c.scrlw, 
											LinGtk.GTK_POLICY_NEVER, LinGtk.GTK_POLICY_ALWAYS)
				ELSE
					LinGtk.gtk_scrolled_window_set_policy(c.scrlw, 
											LinGtk.GTK_POLICY_NEVER, LinGtk.GTK_POLICY_NEVER)
				END
			ELSE
				(* Use a GtkEntry instead of a GtkText. No GtkScrolledWindow is needed. *)
				c.TextLen := EditableTextLen;
				c.text := LinGtk.gtk_entry_new();
				c.scrlw := NIL;
				c.i := NewInfo(c, c.text, c.text);
			END;
			LinGtk.gtk_widget_ref(c.text);
			LinGtk.gtk_widget_modify_style(c.text, NewStyle(c.font));
			res := LinGtk.gtk_signal_connect(c.text, "changed", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(TextChanged)), 
														SYSTEM.VAL(INTEGER, c));
			c.rider(HostPorts.Rider).GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
			LinGtk.gtk_widget_set_usize(c.i.mainWidget, cw, ch);
			LinGtk.gtk_fixed_put(c.rider(HostPorts.Rider).port.fixed, c.i.mainWidget, SHORT(cx), SHORT(cy));
		END;
		c.Update;
		Paint(c.i)
	END Restore;
	
	PROCEDURE (c: Field) Close;
	BEGIN
		IF c.text # NIL THEN
			LinGtk.gtk_widget_unref(c.text);
			IF c.scrlw # NIL THEN LinGtk.gtk_widget_unref(c.scrlw) END;
			LinGtk.gtk_container_remove(c.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer), c.i.mainWidget)
		END
	END Close;
	
	PROCEDURE (f: Field) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i);
	END Mark;

	PROCEDURE (f: Field) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;	

	PROCEDURE (f: Field) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.multiLine OR (ch # 0DX) THEN
			HandleKey(f.i, ch)
		END
	END KeyDown;
	
	PROCEDURE  (f: Field) Select (from, to: INTEGER);
	BEGIN
		IF f.text # NIL THEN
			IF to = MAX(INTEGER) THEN to := -1 END;
			LinGtk.gtk_editable_select_region(f.text, from, to)
		END
	END Select;
	
	PROCEDURE  (f: Field) GetSelection (OUT from, to: INTEGER);
		VAR e: LinGtk.GtkEditable;
	BEGIN
		e := f.text(LinGtk.GtkEditable);
		IF 0 IN e.has_selection (* testing bit field *) THEN
			from := e.selection_start_pos; to := e.selection_end_pos
		ELSE	
			from := -1; to := -1;
		END
	END GetSelection;
	
	PROCEDURE  (f: Field) Idle;
	BEGIN
	END Idle;
	
	PROCEDURE  (f: Field) Length (): INTEGER;
	BEGIN
		RETURN f.TextLen(f.text)
	END Length;
	
	(* Group *)
		
	PROCEDURE (c: Group) Restore (l, t, r, b: INTEGER);
		VAR w, h, asc, dsc, sw: INTEGER; s: Dialog.String; col: Ports.Color;
	BEGIN
		c.view.context.GetSize(w, h); 
		Dialog.MapString(c.label, s); RemoveAmpersand(s);
		HostFonts.dlgFont.GetBounds(asc, dsc, sw); 
		sw := HostFonts.dlgFont.SStringWidth(SHORT(s));
		IF c.disabled THEN col := Ports.grey50 ELSE col := Ports.black END;
		c.DrawRect(Ports.point, asc DIV 2, w - Ports.point, h - Ports.point, Ports.point, col);
		c.DrawRect(Ports.mm, 0, Ports.mm + sw, asc + dsc, Ports.fill, Ports.dialogBackground);
		c.DrawString(Ports.mm, asc + dsc, col, s, HostFonts.dlgFont)
	END Restore;
	
	(* ListBox *)

	PROCEDURE (f: ListBox) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
(*		Adapt(f, f.i)*)
	END SetOffset;
	
	PROCEDURE (f: ListBox) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i);
	END Mark;
		
	PROCEDURE ListSelect (clist: LinGtk.GtkWidget; row, column: INTEGER; 
									event: LinGdk.GdkEventButton; user_data: LinLibc.PtrVoid);
		VAR f: ListBox; cur: INTEGER;
	BEGIN
		f := SYSTEM.VAL(ListBox, user_data);
		IF f.list # NIL THEN
			f.Get(f, cur);
			row := LinGtk.gtk_clist_get_row_data(clist, row);
			IF row # cur THEN f.Set(f, row) END
		END
	END ListSelect;
	
	PROCEDURE (f: ListBox) Update;
		VAR i, r: INTEGER;
	BEGIN
		IF f.disabled OR f.readOnly THEN
			LinGtk.gtk_widget_set_sensitive(f.list, LinLibc.FALSE);
			IF f.disabled THEN
				LinGtk.gtk_clist_unselect_all(f.list)
			END
		ELSE
			LinGtk.gtk_widget_set_sensitive(f.list, LinLibc.TRUE)
		END;
		IF ~f.disabled THEN
			IF f.undef THEN i := -1 ELSE f.Get(f, i) END; 
			IF i >= 0 THEN
				r := LinGtk.gtk_clist_find_row_from_data(f.list, i);
				IF r >= 0 THEN
					LinGtk.gtk_clist_select_row(f.list, r, 0)
				END
			END;
			LinGtk.gtk_widget_set_sensitive(f.scrlw, LinLibc.TRUE)
		ELSE
			LinGtk.gtk_widget_set_sensitive(f.scrlw, LinLibc.FALSE)
		END;
		LinGtk.gtk_widget_show_all(f.scrlw)
	END Update;
	
	PROCEDURE (c: ListBox) UpdateList;
		VAR  i, res: INTEGER; s: Dialog.String; li: LinGtk.GtkWidget;
			ss: ARRAY LEN(Dialog.String) OF SHORTCHAR; sa: ARRAY [untagged] 1 OF LinLibc.PtrSTR; 
	BEGIN
		ASSERT(c.list # NIL, 20);
		(* make sure that no selection changes are propagated when rebuilding the list *)
		LinGtk.gtk_signal_handler_block_by_func(c.list, 
			SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(ListSelect)), SYSTEM.VAL(INTEGER, c));
		LinGtk.gtk_clist_unselect_all(c.list);
		LinGtk.gtk_clist_clear(c.list);
		i := 0; c.GetName(c, i, s); 
		WHILE s # "" DO
			Dialog.MapString(s, s);
			ss := SHORT(s);			
			sa[0] := ss;
			LinGtk.gtk_clist_append(c.list, SYSTEM.VAL(LinLibc.StrArray, SYSTEM.ADR(sa)));
			LinGtk.gtk_clist_set_row_data(c.list, i, i);
			INC(i); c.GetName(c, i, s)
		END;
		IF c.sorted THEN
			LinGtk.gtk_clist_sort(c.list)
		END;
		LinGtk.gtk_signal_handler_unblock_by_func(c.list, 
			SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(ListSelect)), SYSTEM.VAL(INTEGER, c));
		LinGtk.gtk_clist_set_column_width(c.list, 0, LinGtk.gtk_clist_optimal_column_width(c.list, 0));
		c.Update;
	END UpdateList;
	
	PROCEDURE (c: ListBox) Restore (l, t, r, b: INTEGER);
		VAR cx, cy, cw, ch, res: INTEGER;
	BEGIN
		IF c.list = NIL THEN
			c.noRedraw := TRUE;
			c.list := LinGtk.gtk_clist_new(1);
			LinGtk.gtk_widget_ref(c.list);
			LinGtk.gtk_widget_modify_style(c.list, NewStyle(c.font));
(*			LinGtk.gtk_clist_set_selection_mode(c.list, LinGtk.GTK_SELECTION_BROWSE);*)
			LinGtk.gtk_clist_set_selection_mode(c.list, LinGtk.GTK_SELECTION_SINGLE);
			res := LinGtk.gtk_signal_connect(c.list, "select-row", 
											SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(ListSelect)),
											SYSTEM.VAL(INTEGER, c));
			c.scrlw := LinGtk.gtk_scrolled_window_new(NIL, NIL);
			LinGtk.gtk_widget_ref(c.scrlw);
			c.i := NewInfo(c, c.scrlw, c.list);
			c.rider(HostPorts.Rider).GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
			LinGtk.gtk_widget_set_usize(c.scrlw, cw, ch);	
			LinGtk.gtk_container_add(c.scrlw(LinGtk.GtkContainer), c.list);
			LinGtk.gtk_scrolled_window_set_policy(c.scrlw, 
															LinGtk.GTK_POLICY_AUTOMATIC, LinGtk.GTK_POLICY_AUTOMATIC);
			LinGtk.gtk_fixed_put(c.rider(HostPorts.Rider).port.fixed, c.scrlw, SHORT(cx), SHORT(cy));
			c.UpdateList
		ELSE
			c.Update;
		END;
		Paint(c.i)
	END Restore;
	
	PROCEDURE (c: ListBox) Close;
	BEGIN
		IF c.scrlw # NIL THEN 
			LinGtk.gtk_widget_unref(c.list);
			LinGtk.gtk_widget_unref(c.scrlw);
			LinGtk.gtk_container_remove(c.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer), c.scrlw);
		END;
		c.i := NIL; c.list := NIL; c.scrlw := NIL;
	END Close;
		
	PROCEDURE (f: ListBox) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;	
	
	PROCEDURE (f: ListBox) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		HandleKey(f.i, ch)
	END KeyDown;
	
	
	(* Pushbutton *)
	
	PROCEDURE Execute (f: PushButton);
	BEGIN
		IF f.Do # NIL THEN 
			Dialog.ShowStatus("");
			f.Do(f);
		END
	END Execute;
	
	PROCEDURE (f: PushButton) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		Execute(f)
	END KeyDown;
	
	PROCEDURE ButtonClick (button: LinGtk.GtkWidget; user_data: INTEGER);
		VAR f: PushButton;
	BEGIN
		f := SYSTEM.VAL(PushButton, user_data);
		ASSERT(~f.disabled, 100);
		Execute(f)
		; released := TRUE
	END ButtonClick;
	
	PROCEDURE (c: PushButton) Update;
		VAR mask: LinGdk.GdkEventMask;
	BEGIN				
		IF ~c.disabled & ~c.readOnly THEN
			LinGtk.gtk_widget_set_sensitive(c.button, LinLibc.TRUE);
			IF c.default THEN INCL(c.button.flags, 13); LinGtk.gtk_widget_grab_default(c.button) END; (* TODO: 13 is what? *)
		ELSE
			LinGtk.gtk_widget_set_sensitive(c.button, LinLibc.FALSE);
		END;
		LinGtk.gtk_widget_show_all(c.button);
		UpdateEventMask(c.i);
	END Update;
	
	PROCEDURE (c: PushButton) Restore (l, t, r, b: INTEGER);
		VAR w, h, cx, cy, cw, ch, res: INTEGER; lbl: LinGtk.GtkWidget;
	BEGIN
		IF c.button = NIL THEN
			c.noRedraw := TRUE;
			lbl := NewLabel(c.label, NewStyle(c.font), none);
			c.rider(HostPorts.Rider).GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
			c.button := LinGtk.gtk_button_new();
(*			c.button := LinGtk.gtk_button_new_with_label(SHORT(c.label$));*)
			LinGtk.gtk_widget_ref(c.button);
			res := LinGtk.gtk_signal_connect(c.button, "clicked", 
											SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(ButtonClick)),
											SYSTEM.VAL(INTEGER, c));
			c.i := NewInfo(c, c.button, c.button);
			LinGtk.gtk_widget_set_usize(c.button, cw, ch);	
			LinGtk.gtk_container_add(c.button(LinGtk.GtkContainer), lbl);
			LinGtk.gtk_fixed_put(c.rider(HostPorts.Rider).port.fixed, c.button, SHORT(cx), SHORT(cy));
		END;		
		c.Update;
		Paint(c.i)
	END Restore;
		
	PROCEDURE (c: PushButton) Close;
	BEGIN
		IF c.button # NIL THEN 
			LinGtk.gtk_widget_unref(c.button);
			LinGtk.gtk_container_remove(c.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer), c.button);
			c.button := NIL; c.i := NIL;
		END
	END Close;
	
	PROCEDURE (f: PushButton) Mark (on, focus: BOOLEAN);
	BEGIN
(*		Mark(on, f.front, f.button, f.hasFocus);*)
		Mark(on, f.front, f.i);
	END Mark;
	
	PROCEDURE (f: PushButton) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;
	
	(* RadioButton *)

	PROCEDURE RadioToggled (radioButton: LinGtk.GtkWidget; user_data: INTEGER);
		VAR f: CheckBox;
	BEGIN
		f := SYSTEM.VAL(CheckBox, user_data);
		IF ~f.disabled THEN
			f.Set(f, LinGtk.gtk_toggle_button_get_active(f.checkButton) # LinLibc.FALSE)
		END
	END RadioToggled;
	
	PROCEDURE (c: RadioButton) Update;
		VAR res: INTEGER; value, mixed: BOOLEAN; mask: LinGdk.GdkEventMask;
	BEGIN
		IF ~c.disabled THEN
			c.Get(c, value);
(*			IF USER32.IsWindowEnabled(f.i.ctrl) = 0 THEN res := USER32.EnableWindow(f.i.ctrl, 1) END;*)
			IF c.undef THEN 
(*				res := USER32.SendMessageA(f.i.ctrl, USER32.BMSetCheck, 2, 0) TODO: Here what? *)
			ELSIF value THEN LinGtk.gtk_toggle_button_set_active(c.radioButton, LinLibc.TRUE)
			ELSE LinGtk.gtk_toggle_button_set_active(c.radioButton, LinLibc.FALSE)
			END;
			IF c.readOnly THEN LinGtk.gtk_widget_set_sensitive(c.radioButton, LinLibc.FALSE) 
			ELSE LinGtk.gtk_widget_set_sensitive(c.radioButton, LinLibc.TRUE)
			END
		ELSE
			LinGtk.gtk_toggle_button_set_active(c.radioButton, LinLibc.FALSE);
			LinGtk.gtk_widget_set_sensitive(c.radioButton, LinLibc.FALSE)
		END;
(*		CheckLabel(f.label, f.i.ctrl);*)
		UpdateEventMask(c.i);
	(*	
		IF c.lbl.window # LinLibc.NULL THEN
			mask := LinGdk.gdk_window_get_events(c.lbl.window);
			mask := mask - LinGdk.GDK_BUTTON_PRESS_MASK;
			LinGdk.gdk_window_set_events(c.lbl.window, mask)
		END	
	*)
	END Update;
	
	PROCEDURE (c: RadioButton) Restore (l, t, r, b: INTEGER);
		VAR w, h, cx, cy, cw, ch, res: INTEGER;
	BEGIN
		IF c.radioButton = NIL THEN
			c.noRedraw := TRUE;
			c.lbl := NewLabel(c.label, NewStyle(c.font), left);
			LinGtk.gtk_widget_ref(c.lbl);
			c.rider(HostPorts.Rider).GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
			c.radioButton := LinGtk.gtk_check_button_new();
(*
			c.radioButton := LinGtk.gtk_radio_button_new(NIL);
*)
			LinGtk.gtk_widget_ref(c.radioButton);
			res := LinGtk.gtk_signal_connect(c.radioButton, "toggled", 
											SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(CheckToggled)),
											SYSTEM.VAL(INTEGER, c));
			LinGtk.gtk_container_add(c.radioButton(LinGtk.GtkContainer), c.lbl);
			c.ebox := LinGtk.gtk_event_box_new();
			LinGtk.gtk_widget_ref(c.ebox);
			c.i := NewInfo(c, c.ebox, c.radioButton);			
			LinGtk.gtk_widget_set_usize(c.ebox, cw, ch);
			LinGtk.gtk_container_add(c.ebox(LinGtk.GtkContainer), c.radioButton);
			LinGtk.gtk_fixed_put(c.rider(HostPorts.Rider).port.fixed, c.ebox, SHORT(cx), SHORT(cy));
		END;		
		c.Update;
		Paint(c.i)
	END Restore;
	
	PROCEDURE (c: RadioButton) Close;
	BEGIN
		IF c.radioButton # NIL THEN
			LinGtk.gtk_widget_unref(c.lbl);
			LinGtk.gtk_widget_unref(c.radioButton);
			LinGtk.gtk_widget_unref(c.ebox);
			LinGtk.gtk_container_remove(c.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer), c.ebox)
		END
	END Close;
	
	PROCEDURE (f: RadioButton) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i);
	END Mark;
	
	PROCEDURE (f: RadioButton) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;	
	
	PROCEDURE (f: RadioButton) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF ch = " "  THEN 
			LinGtk.gtk_button_clicked(f.radioButton)
		END
	END KeyDown;
	
	(* ScrollBar *)
	
(*	
	PROCEDURE Execute (f: ScrollBar);
	BEGIN
		IF f.Do # NIL THEN 
			Dialog.ShowStatus("");
			f.Do(f);
		END
	END Execute;
*)	
	PROCEDURE (f: ScrollBar) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
(*		Execute(f)*)
	END KeyDown;
(*	
	PROCEDURE ButtonClick (button: LinGtk.GtkWidget; user_data: INTEGER);
		VAR f: PushButton;
	BEGIN
		f := SYSTEM.VAL(PushButton, user_data);
		ASSERT(~f.disabled, 100);
		Execute(f)
		; released := TRUE
	END ButtonClick;
*)	
	PROCEDURE ScrollChanged (adjustment: LinGtk.GtkAdjustment; user_data: LinLibc.PtrVoid);
		VAR c: ScrollBar; size, sect, pos: INTEGER;
(*		VAR w: Window;
	BEGIN
		w := SYSTEM.VAL(Window, user_data);
		w.Scroll(adjustment, FALSE, TRUE) (* TODO: If ctrl is pressed TRUE should be passed for focus. *)*)
	BEGIN
		c := SYSTEM.VAL(ScrollBar, user_data);
		IF ~c.isUpdate THEN
			c.Get(c, size, sect, pos);
			c.Set(c, SHORT(ENTIER((size / scrollRange) * adjustment.value)))
(*			c.Set(c, adjustment.value DIV scrollRange)*)
			;Dialog.ShowMsg("scroll")
		END;
	END ScrollChanged;
	
	PROCEDURE (c: ScrollBar) Update;
		VAR  size, sect, pos, q, m: INTEGER; adj: LinGtk.GtkAdjustment;
	BEGIN				
		IF ~c.disabled THEN
			c.Get(c, size, sect, pos);
			IF size > sect THEN
				LinGtk.gtk_widget_set_sensitive(c.scrollBar, LinLibc.TRUE);
				adj := LinGtk.gtk_range_get_adjustment(SYSTEM.VAL(LinGtk.GtkRange, c.scrollBar));
				
				IF sect > 0 THEN (*q := KERNEL32.MulDiv(sect, scrollRange, size - sect); *)
					q := SHORT(ENTIER((sect / (size - sect)) * scrollRange));
					m := scrollRange + q
				ELSE 
					q := -1; m := scrollRange
				END;
				IF (adj.value # pos) OR (adj.page_size # q + 1) THEN
					adj.value := pos; 
					adj.lower := 0; adj.upper := m;
					adj.page_size := q + 1;
					adj.page_increment := size DIV sect;
					c.isUpdate := TRUE;
					LinGtk.gtk_adjustment_changed(adj);
					c.isUpdate := FALSE
				END;
			ELSE
				LinGtk.gtk_widget_set_sensitive(c.scrollBar, LinLibc.FALSE)
			END
		ELSE
			LinGtk.gtk_widget_set_sensitive(c.scrollBar, LinLibc.FALSE)
		END;
		LinGtk.gtk_widget_show_all(c.scrollBar);
(*		UpdateEventMask(c.i);*)
	END Update;
		
	PROCEDURE (c: ScrollBar) Restore (l, t, r, b: INTEGER);
		VAR w, h, cx, cy, cw, ch, res: INTEGER; lbl: LinGtk.GtkWidget; adj: LinGtk.GtkAdjustment;
	BEGIN
		IF c.scrollBar = NIL THEN
			c.noRedraw := TRUE;
(*			lbl := NewLabel(c.label, NewStyle(c.font), none);*)
			c.rider(HostPorts.Rider).GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
(*			c.button := LinGtk.gtk_button_new();*)

			adj := LinGtk.gtk_adjustment_new(0, 0, scrollRange, lineInc, pageInc, defThumbSize);				
			res := LinGtk.gtk_signal_connect(adj, "value_changed", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(ScrollChanged)),
														SYSTEM.VAL(INTEGER, c));
			c.view.context.GetSize(w, h);
			IF h > w THEN
				c.scrollBar := LinGtk.gtk_vscrollbar_new(adj)
			ELSE
				c.scrollBar := LinGtk.gtk_hscrollbar_new(adj)
			END;
			
			LinGtk.gtk_widget_ref(c.scrollBar);
(*			res := LinGtk.gtk_signal_connect(c.button, "clicked", 
											SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(ButtonClick)),
											SYSTEM.VAL(INTEGER, c));*)
			c.i := NewInfo(c, c.scrollBar, c.scrollBar);
			LinGtk.gtk_widget_set_usize(c.scrollBar, cw, ch);	
			LinGtk.gtk_fixed_put(c.rider(HostPorts.Rider).port.fixed, c.scrollBar, SHORT(cx), SHORT(cy));
		END;		
		c.Update;
		Paint(c.i)
	END Restore;
		
	PROCEDURE (c: ScrollBar) Close;
	BEGIN
		IF c.scrollBar # NIL THEN 
			LinGtk.gtk_widget_unref(c.scrollBar);
			LinGtk.gtk_container_remove(c.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer), c.scrollBar);
			c.scrollBar := NIL; c.i := NIL;
		END
	END Close;
	
	PROCEDURE (f: ScrollBar) Mark (on, focus: BOOLEAN);
	BEGIN
(*		Mark(on, f.front, f.button, f.hasFocus);*)
		Mark(on, f.front, f.i);
	END Mark;
	
	PROCEDURE (f: ScrollBar) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;
	
	
	
	(* SelectionBox *)

	PROCEDURE (f: SelectionBox) SetOffset (x, y: INTEGER);
	BEGIN
		f.SetOffset^(x, y);
(*		Adapt(f, f.i)*)
	END SetOffset;
	
	PROCEDURE (f: SelectionBox) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i);
	END Mark;
		
	PROCEDURE SelectionBoxSelect (clist: LinGtk.GtkWidget; row, column: INTEGER; 
									event: LinGdk.GdkEventButton; user_data: LinLibc.PtrVoid);
		VAR f: SelectionBox; cur: INTEGER; in: BOOLEAN;
	BEGIN
		f := SYSTEM.VAL(SelectionBox, user_data);
		IF f.list # NIL THEN
			row := LinGtk.gtk_clist_get_row_data(clist, row);
			f.Get(f, row, in);
			IF ~in THEN f.Incl(f, row, row) END
		END
	END SelectionBoxSelect;
		
	PROCEDURE SelectionBoxUnSelect (clist: LinGtk.GtkWidget; row, column: INTEGER; 
									event: LinGdk.GdkEventButton; user_data: LinLibc.PtrVoid);
		VAR f: SelectionBox; cur: INTEGER; in: BOOLEAN;
	BEGIN
		f := SYSTEM.VAL(SelectionBox, user_data);
		IF f.list # NIL THEN
			row := LinGtk.gtk_clist_get_row_data(clist, row);
			f.Get(f, row, in);
			IF in THEN f.Excl(f, row, row) 
				;Log.String("HostCFrames.SelectionBoxUnSelect: "); Log.Int(row); Log.Ln;
			END
		END
	END SelectionBoxUnSelect;
	
	PROCEDURE (c: SelectionBox) Update;
		VAR i, r: INTEGER; in: BOOLEAN;
	BEGIN
		IF c.disabled OR c.readOnly THEN
			LinGtk.gtk_widget_set_sensitive(c.list, LinLibc.FALSE);
			IF c.disabled THEN
				LinGtk.gtk_clist_unselect_all(c.list)
			END
		ELSE
			LinGtk.gtk_widget_set_sensitive(c.list, LinLibc.TRUE)
		END;
		IF ~c.disabled THEN
			IF ~c.undef THEN 
				i := 0;
				WHILE i < c.num DO
					c.Get(c, i, in);
					r := LinGtk.gtk_clist_find_row_from_data(c.list, i);
					IF in THEN
						LinGtk.gtk_clist_select_row(c.list, r, 0)
					ELSE
						LinGtk.gtk_clist_unselect_row(c.list, r, 0)
					END;
					INC(i)
				END
			END;
			LinGtk.gtk_widget_set_sensitive(c.scrlw, LinLibc.TRUE)
		ELSE
			LinGtk.gtk_widget_set_sensitive(c.scrlw, LinLibc.FALSE)
		END;
		LinGtk.gtk_widget_show_all(c.scrlw)
	END Update;
	
	PROCEDURE (c: SelectionBox) UpdateList;
		VAR  i, res: INTEGER; s: Dialog.String; li: LinGtk.GtkWidget;
			ss: ARRAY LEN(Dialog.String) OF SHORTCHAR; sa: ARRAY [untagged] 1 OF LinLibc.PtrSTR; 
	BEGIN
		ASSERT(c.list # NIL, 20);
		(* make sure that no selection changes are propagated when rebuilding the list *)
		LinGtk.gtk_signal_handler_block_by_func(c.list, 
			SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(SelectionBoxSelect)), SYSTEM.VAL(INTEGER, c));
		LinGtk.gtk_clist_unselect_all(c.list);
		LinGtk.gtk_clist_clear(c.list);
		i := 0; c.GetName(c, i, s); 
		WHILE s # "" DO
			Dialog.MapString(s, s);
			ss := SHORT(s);			
			sa[0] := ss;
			LinGtk.gtk_clist_append(c.list, SYSTEM.VAL(LinLibc.StrArray, SYSTEM.ADR(sa)));
			LinGtk.gtk_clist_set_row_data(c.list, i, i);
			INC(i); c.GetName(c, i, s)
		END;
		c.num := i;
		IF c.sorted THEN
			LinGtk.gtk_clist_sort(c.list)
		END;
		LinGtk.gtk_signal_handler_unblock_by_func(c.list, 
			SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(SelectionBoxSelect)), SYSTEM.VAL(INTEGER, c));
		LinGtk.gtk_clist_set_column_width(c.list, 0, LinGtk.gtk_clist_optimal_column_width(c.list, 0));
		c.Update;
	END UpdateList;
	
	PROCEDURE (c: SelectionBox) Restore (l, t, r, b: INTEGER);
		VAR cx, cy, cw, ch, res: INTEGER;
	BEGIN
		IF c.list = NIL THEN
			c.noRedraw := TRUE;
			c.list := LinGtk.gtk_clist_new(1);
			LinGtk.gtk_widget_ref(c.list);
			LinGtk.gtk_widget_modify_style(c.list, NewStyle(c.font));
			c.scrlw := LinGtk.gtk_scrolled_window_new(NIL, NIL);
			LinGtk.gtk_widget_ref(c.scrlw);
			LinGtk.gtk_clist_set_selection_mode(c.list, LinGtk.GTK_SELECTION_EXTENDED);
			res := LinGtk.gtk_signal_connect(c.list, "select-row", 
											SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(SelectionBoxSelect)),
											SYSTEM.VAL(INTEGER, c));
			res := LinGtk.gtk_signal_connect(c.list, "unselect-row", 
											SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(SelectionBoxUnSelect)),
											SYSTEM.VAL(INTEGER, c));
			c.i := NewInfo(c, c.scrlw, c.list);
			c.rider(HostPorts.Rider).GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
			LinGtk.gtk_widget_set_usize(c.scrlw, cw, ch);	
			LinGtk.gtk_container_add(c.scrlw(LinGtk.GtkContainer), c.list);
			LinGtk.gtk_scrolled_window_set_policy(c.scrlw, 
															LinGtk.GTK_POLICY_AUTOMATIC, LinGtk.GTK_POLICY_AUTOMATIC);
			LinGtk.gtk_fixed_put(c.rider(HostPorts.Rider).port.fixed, c.scrlw, SHORT(cx), SHORT(cy));
			c.UpdateList
		ELSE
			c.Update;
		END;
		Paint(c.i)
	END Restore;
	
	PROCEDURE (c: SelectionBox) Close;
	BEGIN
		IF c.scrlw # NIL THEN 
			LinGtk.gtk_widget_unref(c.list);
			LinGtk.gtk_widget_unref(c.scrlw);
			LinGtk.gtk_container_remove(c.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer), c.scrlw);
		END;
		c.i := NIL; c.list := NIL; c.scrlw := NIL;
	END Close;
		
	PROCEDURE (f: SelectionBox) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;	
	
	PROCEDURE (f: SelectionBox) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		HandleKey(f.i, ch)
	END KeyDown;
	
	PROCEDURE  (f: SelectionBox) GetSelection (OUT from, to: INTEGER);
	BEGIN
		from := 0; to := MAX(INTEGER)
	END GetSelection;
	
	PROCEDURE  (f: SelectionBox) Select (from, to: INTEGER);
	BEGIN
	END Select;
	
	PROCEDURE (c: TimeField) Restore (l, t, r, b: INTEGER);
	BEGIN
		DummyRestore(c, "TimeField", l, t, r, b)
	END Restore;
	
	(* TreeFrame *)
	
	PROCEDURE LoadTreeIcons (window: LinGdk.GdkWindow);
		VAR loc: HostFiles.Locator; s: ARRAY LEN(HostFiles.FullName) OF SHORTCHAR;
	BEGIN
		IF (tree_closed = LinLibc.NULL) & (window # LinLibc.NULL) THEN
			loc := Files.dir.This("")(HostFiles.Locator);
			s := SHORT(loc.path) + "/Lin/Rsrc/folder.xpm";
			tree_closed := LinGdk.gdk_pixmap_create_from_xpm (window, tree_closed_mask, NIL, s);
			s := SHORT(loc.path) + "/Lin/Rsrc/openfold.xpm";
			tree_open := LinGdk.gdk_pixmap_create_from_xpm (window, tree_open_mask, NIL, s);
			s := SHORT(loc.path) + "/Lin/Rsrc/file.xpm";
			tree_leaf := LinGdk.gdk_pixmap_create_from_xpm (window, tree_leaf_mask, NIL, s)
		END
	END LoadTreeIcons;
	
	PROCEDURE TreeSelect (ctree: LinGtk.GtkWidget; node: LinGtk.GtkCTreeNode; column: INTEGER; user_data: LinLibc.PtrVoid);
		VAR tn: Dialog.TreeNode; c: TreeFrame;
	BEGIN
		tn := SYSTEM.VAL(Dialog.TreeNode, LinGtk.gtk_ctree_node_get_row_data(ctree, node));
		c := SYSTEM.VAL(TreeFrame, user_data);
		IF tn # c.Selected(c) THEN c.Select(c, tn) END
	END TreeSelect;
	
	PROCEDURE (f: TreeFrame) RealInsert (tn: Dialog.TreeNode; parent: LinGtk.GtkCTreeNode; 
														OUT newNode: LinGtk.GtkCTreeNode), NEW;
		VAR
			name: Dialog.String; leaf, expanded: INTEGER;
			ss: ARRAY LEN(Dialog.String) OF SHORTCHAR; sa: ARRAY [untagged] 1 OF LinLibc.PtrSTR; 
	BEGIN
		ASSERT(tn # NIL, 20);
		tn.GetName(name); ss := SHORT(name);			
		sa[0] := ss;
		IF tn.IsFolder() THEN leaf := LinLibc.FALSE ELSE leaf := LinLibc.TRUE END;
		IF tn.IsExpanded() THEN expanded := LinLibc.TRUE ELSE expanded := LinLibc.FALSE END;
		IF f.foldericons THEN
			IF tn.IsFolder() THEN
				newNode:= LinGtk.gtk_ctree_insert_node(f.ctree, parent, NIL, 
								SYSTEM.VAL(LinLibc.StrArray, SYSTEM.ADR(sa)), 0,
								tree_closed, tree_closed_mask, tree_open, tree_open_mask, leaf, expanded);
			ELSE
				newNode:= LinGtk.gtk_ctree_insert_node(f.ctree, parent, NIL, 
								SYSTEM.VAL(LinLibc.StrArray, SYSTEM.ADR(sa)), 0,
								tree_leaf, tree_leaf_mask, LinLibc.NULL, LinLibc.NULL, leaf, expanded);
			END
		ELSE
			newNode:= LinGtk.gtk_ctree_insert_node(f.ctree, parent, NIL, SYSTEM.VAL(LinLibc.StrArray, SYSTEM.ADR(sa)), 0,
							LinLibc.NULL, LinLibc.NULL, LinLibc.NULL, LinLibc.NULL, leaf, expanded);
		END;
		LinGtk.gtk_ctree_node_set_row_data(f.ctree, newNode, SYSTEM.VAL(INTEGER, tn))
	END RealInsert;
	
	PROCEDURE (f: TreeFrame) InsertNode (tn: Dialog.TreeNode; parent: LinGtk.GtkCTreeNode), NEW;
		VAR  newNode: LinGtk.GtkCTreeNode;
	BEGIN
		IF tn # NIL THEN
			f.RealInsert(tn, parent, newNode);
			f.InsertNode(f.Child(f, tn), newNode);
			f.InsertNode(f.Next(f, tn), parent)
		END
	END InsertNode;
	
	PROCEDURE (f: TreeFrame) UpdateList;
		VAR 
			node: Dialog.TreeNode; name: Dialog.String; cnode: LinGtk.GtkWidget;
			ss: ARRAY LEN(Dialog.String) OF SHORTCHAR; sa: ARRAY [untagged] 1 OF LinLibc.PtrSTR; 
	BEGIN
		LoadTreeIcons(f.ctree.window);
		LinGtk.gtk_clist_clear(f.ctree);
		LinGtk.gtk_clist_freeze(f.ctree);
		f.InsertNode(f.Child(f, NIL), NIL);
		LinGtk.gtk_clist_thaw(f.ctree);
		IF f.sorted THEN
			LinGtk.gtk_clist_sort(f.ctree)
		END;
		f.Update
	END UpdateList;
	
	PROCEDURE (f: TreeFrame) Update;
		VAR tn: Dialog.TreeNode; r: INTEGER;
	BEGIN
		UpdateEventMask(f.i);
		IF f.disabled OR f.readOnly THEN
			LinGtk.gtk_widget_set_sensitive(f.ctree, LinLibc.FALSE);
			IF f.disabled THEN
				LinGtk.gtk_clist_unselect_all(f.ctree)
			END
		ELSE
			LinGtk.gtk_widget_set_sensitive(f.ctree, LinLibc.TRUE)
		END;
		IF ~f.disabled THEN
			tn := f.Selected(f);
			IF tn # NIL THEN
				r := LinGtk.gtk_clist_find_row_from_data(f.ctree, SYSTEM.VAL(INTEGER, tn));
				IF r >= 0 THEN
					LinGtk.gtk_clist_select_row(f.ctree, r, 0)
				END
			END;
			LinGtk.gtk_widget_set_sensitive(f.scrlw, LinLibc.TRUE)
		ELSE
			LinGtk.gtk_widget_set_sensitive(f.scrlw, LinLibc.FALSE)
		END;
		LinGtk.gtk_widget_show_all(f.scrlw)
	END Update;
	
	PROCEDURE (c: TreeFrame) Restore (l, t, r, b: INTEGER);
		VAR w, h, cx, cy, cw, ch, res: INTEGER;
	BEGIN
		IF c.ctree = NIL THEN
			c.noRedraw := TRUE;
			c.rider(HostPorts.Rider).GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
			c.ctree := LinGtk.gtk_ctree_new(1, 0);
			LinGtk.gtk_widget_ref(c.ctree);
			LinGtk.gtk_widget_modify_style(c.ctree, NewStyle(c.font));
			res := LinGtk.gtk_signal_connect(c.ctree, "tree-select-row", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(TreeSelect)), 
														SYSTEM.VAL(INTEGER, c));		
			c.scrlw := LinGtk.gtk_scrolled_window_new(NIL, NIL);
			LinGtk.gtk_widget_ref(c.scrlw);
			c.i := NewInfo(c, c.scrlw, c.ctree);
			LinGtk.gtk_widget_set_usize(c.scrlw, cw, ch);	
			LinGtk.gtk_container_add(c.scrlw(LinGtk.GtkContainer), c.ctree);
			LinGtk.gtk_scrolled_window_set_policy(c.scrlw, 
															LinGtk.GTK_POLICY_AUTOMATIC, LinGtk.GTK_POLICY_AUTOMATIC);
			LinGtk.gtk_fixed_put(c.rider(HostPorts.Rider).port.fixed, c.scrlw, SHORT(cx), SHORT(cy));
			c.UpdateList
		ELSE
			c.Update;
		END;		
		Paint(c.i)
	END Restore;
	
	PROCEDURE (c: TreeFrame) Close;
	BEGIN
		IF c.ctree # NIL THEN 
			LinGtk.gtk_widget_unref(c.ctree); 
			LinGtk.gtk_widget_unref(c.scrlw);
			LinGtk.gtk_container_remove(c.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer), c.scrlw);
			c.ctree := NIL; c.scrlw := NIL; c.i := NIL;
		END
	END Close;
	
	PROCEDURE (f: TreeFrame) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;	
	
	PROCEDURE (f: TreeFrame) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		HandleKey(f.i, ch)
	END KeyDown;
	
	PROCEDURE (f: TreeFrame) GetSize (OUT w, h: INTEGER);
	BEGIN
		w := 0; h := 0;
	END GetSize;
	
	(* UpDownField *)
	
	PROCEDURE SpinChanged (spin: LinGtk.GtkWidget; user_data: LinLibc.PtrVoid);
		VAR s: LinLibc.PtrSTR; c: UpDownField;
	BEGIN
		c := SYSTEM.VAL(UpDownField, user_data);
		IF ~c.isUpdate THEN 
			c.val := LinGtk.gtk_spin_button_get_value_as_int(spin);
			c.Set(c, c.val)
		END
	END SpinChanged;
	
	PROCEDURE (f: UpDownField) Update;
		VAR val: INTEGER;
	BEGIN
		f.isUpdate := TRUE;
		IF ~f.disabled THEN
			f.Get(f, val);
			LinGtk.gtk_spin_button_set_value(f.spin, val);
			LinGtk.gtk_widget_set_sensitive(f.spin, LinLibc.TRUE);
		ELSE
			LinGtk.gtk_widget_set_sensitive(f.spin, LinLibc.FALSE);
		END;
		f.isUpdate := FALSE
		;UpdateEventMask (f.i)
	END Update;
	
	PROCEDURE (c: UpDownField) Restore (l, t, r, b: INTEGER);
		VAR w, h, cx, cy, cw, ch, res: INTEGER; lbl: LinGtk.GtkWidget; adj: LinGtk.GtkAdjustment;
	BEGIN
		IF c.spin = NIL THEN
			c.noRedraw := TRUE;
			c.val := 0;
			c.rider(HostPorts.Rider).GetRect(cx, cy, cw, ch); cw := cw - cx; ch := ch - cy;
			adj := LinGtk.gtk_adjustment_new(c.val, c.min, c.max, c.inc, 
				c.inc, (c.max - c.min) DIV MAX(1, c.inc)); (* waste of pgup & pgdn *)
			c.spin := LinGtk.gtk_spin_button_new(adj, 1, 0);
			LinGtk.gtk_widget_ref(c.spin);
			LinGtk.gtk_widget_modify_style(c.spin, NewStyle(c.font));
			LinGtk.gtk_spin_button_set_numeric(c.spin, LinLibc.TRUE);
			LinGtk.gtk_spin_button_set_shadow_type(c.spin, LinGtk.GTK_SHADOW_NONE);
(*			LinGtk.gtk_spin_button_set_snap_to_ticks(c.spin, LinLibc.TRUE);*)
			LinGtk.gtk_spin_button_set_wrap(c.spin, LinLibc.TRUE);
			res := LinGtk.gtk_signal_connect(c.spin, "changed", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(SpinChanged)), 
														SYSTEM.VAL(INTEGER, c));
			c.i := NewInfo(c, c.spin, c.spin);
			LinGtk.gtk_widget_set_usize(c.spin, cw, ch);	
			LinGtk.gtk_fixed_put(c.rider(HostPorts.Rider).port.fixed, c.spin, SHORT(cx), SHORT(cy));
		END;		
		c.Update;
		Paint(c.i)
	END Restore;
	
	PROCEDURE (c: UpDownField) Close;
	BEGIN
		IF c.spin # NIL THEN 
			LinGtk.gtk_widget_unref(c.spin);
			LinGtk.gtk_container_remove(c.rider(HostPorts.Rider).port.fixed(LinGtk.GtkContainer), c.spin);
			c.spin := NIL; c.i := NIL;
		END
	END Close;
	
	PROCEDURE  (f: UpDownField) GetSelection (OUT from, to: INTEGER);
		VAR e: LinGtk.GtkEditable;
	BEGIN
		e := f.spin(LinGtk.GtkEditable);
		IF 0 IN e.has_selection (* testing bit field *) THEN
			from := e.selection_start_pos; to := e.selection_end_pos
		ELSE	
			from := -1; to := -1;
		END
	END GetSelection;

	PROCEDURE (f: UpDownField) Mark (on, focus: BOOLEAN);
	BEGIN
		Mark(on, f.front, f.i);
	END Mark;
	
	PROCEDURE (f: UpDownField) Select (from, to: INTEGER);
	BEGIN
		IF to = MAX(INTEGER) THEN to := -1 END;
		LinGtk.gtk_editable_select_region(f.spin, from , to)
	END Select;
	
	PROCEDURE  (f: UpDownField) Idle;
	BEGIN
	END Idle;
	
	PROCEDURE (f: UpDownField) MouseDown (x, y: INTEGER; buttons: SET);
	BEGIN
		ASSERT(~f.disabled, 100);
		IF f.rider # NIL THEN
			HandleMouse(f.i, x DIV f.unit, y DIV f.unit, buttons)
		END
	END MouseDown;	
	
	PROCEDURE (f: UpDownField) KeyDown (ch: CHAR);
	BEGIN
		ASSERT(~f.disabled, 100);
		HandleKey(f.i, ch)
	END KeyDown;
	
	
	(* Directory *)

	PROCEDURE (d: Directory) GetPushButtonSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 56 * Ports.point END;
		IF h = Views.undefined THEN h := 28 * Ports.point END; (* 18 on Windows *)
	END GetPushButtonSize;

	PROCEDURE (d: Directory) GetCheckBoxSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 60 * Ports.point END;
		IF h = Views.undefined THEN h := 12 * Ports.point END
	END GetCheckBoxSize;

	PROCEDURE (d: Directory) GetRadioButtonSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 60 * Ports.point END;
		IF h = Views.undefined THEN h := 12 * Ports.point END
	END GetRadioButtonSize;

	PROCEDURE (d: Directory) GetScrollBarSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 120 * Ports.point END;
		IF h = Views.undefined THEN h := 12 * Ports.point END
	END GetScrollBarSize;

	PROCEDURE (d: Directory) GetFieldSize (max: INTEGER; VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN
			IF max = 0 THEN w := 80 * Ports.point
			ELSIF max < 10 THEN w := 32 * Ports.point
			ELSIF max < 15 THEN w := 56 * Ports.point
			ELSIF max < 30 THEN w := 80 * Ports.point
			ELSIF max < 100 THEN w := 120 * Ports.point
			ELSE w := 150 * Ports.point
			END
		END;
		IF h = Views.undefined THEN h := 17 * Ports.point END
	END GetFieldSize;

	PROCEDURE (d: Directory) GetUpDownFieldSize (max: INTEGER; VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 56 * Ports.point END;
		IF h = Views.undefined THEN h := 17 * Ports.point END
	END GetUpDownFieldSize;

	PROCEDURE (d: Directory) GetDateFieldSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 72 * Ports.point END;
		IF h = Views.undefined THEN h := 17 * Ports.point END
	END GetDateFieldSize;

	PROCEDURE (d: Directory) GetTimeFieldSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 72 * Ports.point END;
		IF h = Views.undefined THEN h := 17 * Ports.point END
	END GetTimeFieldSize;

	PROCEDURE (d: Directory) GetColorFieldSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 36 * Ports.point END;
		IF h = Views.undefined THEN h := 18 * Ports.point END
	END GetColorFieldSize;

	PROCEDURE (d: Directory) GetListBoxSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 100 * Ports.point END;
		IF h = Views.undefined THEN h := 18 * Ports.point END
	END GetListBoxSize;

	PROCEDURE (d: Directory) GetSelectionBoxSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 100 * Ports.point END;
		IF h = Views.undefined THEN h := 54 * Ports.point END
	END GetSelectionBoxSize;

	PROCEDURE (d: Directory) GetComboBoxSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 100 * Ports.point END;
		IF h = Views.undefined THEN h := 18 * Ports.point END
	END GetComboBoxSize;

	PROCEDURE (d: Directory) GetCaptionSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 50 * Ports.point END;
		IF h = Views.undefined THEN h := 12 * Ports.point END
	END GetCaptionSize;

	PROCEDURE (d: Directory) GetGroupSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 100 * Ports.point END;
		IF h = Views.undefined THEN h := 100 * Ports.point END
	END GetGroupSize;
	
	PROCEDURE (d: Directory) GetTreeFrameSize (VAR w, h: INTEGER);
	BEGIN
		IF w = Views.undefined THEN w := 100 * Ports.point END;
		IF h = Views.undefined THEN h := 100 * Ports.point END
	END GetTreeFrameSize;

	PROCEDURE (d: Directory) NewPushButton (): StdCFrames.PushButton;
		VAR f: PushButton;
	BEGIN
		NEW(f); RETURN f
	END NewPushButton;

	PROCEDURE (d: Directory) NewCheckBox (): StdCFrames.CheckBox;
		VAR f: CheckBox;
	BEGIN
		NEW(f); RETURN f
	END NewCheckBox;

	PROCEDURE (d: Directory) NewRadioButton (): StdCFrames.RadioButton;
		VAR f: RadioButton;
	BEGIN
		NEW(f); RETURN f
	END NewRadioButton;

	PROCEDURE (d: Directory) NewScrollBar (): StdCFrames.ScrollBar;
		VAR f: ScrollBar;
	BEGIN
		NEW(f); RETURN f
	END NewScrollBar;

	PROCEDURE (d: Directory) NewField (): StdCFrames.Field;
		VAR f: Field;
	BEGIN
		NEW(f); RETURN f
	END NewField;

	PROCEDURE (d: Directory) NewUpDownField (): StdCFrames.UpDownField;
		VAR f: UpDownField;
	BEGIN
		NEW(f); RETURN f
	END NewUpDownField;

	PROCEDURE (d: Directory) NewDateField (): StdCFrames.DateField;
		VAR f: DateField;
	BEGIN
		NEW(f); RETURN f
	END NewDateField;

	PROCEDURE (d: Directory) NewTimeField (): StdCFrames.TimeField;
		VAR f: TimeField;
	BEGIN
		NEW(f); RETURN f
	END NewTimeField;

	PROCEDURE (d: Directory) NewColorField (): StdCFrames.ColorField;
		VAR f: ColorField;
	BEGIN
		NEW(f); RETURN f
	END NewColorField;

	PROCEDURE (d: Directory) NewListBox (): StdCFrames.ListBox;
		VAR f: ListBox;
	BEGIN
		NEW(f); RETURN f
	END NewListBox;

	PROCEDURE (d: Directory) NewSelectionBox (): StdCFrames.SelectionBox;
		VAR f: SelectionBox;
	BEGIN
		NEW(f); RETURN f
	END NewSelectionBox;

	PROCEDURE (d: Directory) NewComboBox (): StdCFrames.ComboBox;
		VAR f: ComboBox;
	BEGIN
		NEW(f); RETURN f
	END NewComboBox;

	PROCEDURE (d: Directory) NewCaption (): StdCFrames.Caption;
		VAR f: Caption;
	BEGIN
		NEW(f); RETURN f
	END NewCaption;

	PROCEDURE (d: Directory) NewGroup (): StdCFrames.Group;
		VAR f: Group;
	BEGIN
		NEW(f); RETURN f
	END NewGroup;
	
	PROCEDURE (d: Directory) NewTreeFrame (): StdCFrames.TreeFrame;
		VAR f: TreeFrame;
	BEGIN
		NEW(f); RETURN f
	END NewTreeFrame;
	
	PROCEDURE SetDefFonts*;
	BEGIN
		StdCFrames.defaultFont := HostFonts.dlgFont;
		StdCFrames.defaultLightFont := Fonts.dir.This(HostFonts.dlgFont.typeface, HostFonts.dlgFont.size,
																	HostFonts.dlgFont.style, Fonts.normal);
	END SetDefFonts;

	PROCEDURE Init;
		VAR dir: Directory;
	BEGIN
		NEW(mouseDelayer);
		StdCFrames.setFocus := TRUE;
		SetDefFonts;
		NEW(dir); StdCFrames.SetDir(dir)
	END Init;
	
BEGIN
	Init
END HostCFrames.