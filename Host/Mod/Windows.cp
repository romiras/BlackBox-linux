MODULE HostWindows;

	(*
		TODO:
			- focus when bringing to front - is it at all possible or is it completely left to the window manager?
			- sometimes it is needed to know if the control key is pressed or not. how can this be done?
	*)

	(* 
	
		Implementation docu 
	
		Gtk uses containers in a similar way as Java uses layout managers. There are containers for packing widgets together
		in vertical or horisontal boxes et.c. There is also a container for absolute positioning (GtkFixed). This is the one we use.
		So, a window contains the following widgets: a GtkWindow, a GtkFixed, a GtkDrawingArea, a GtkVScrollbar and
		a GtkHScrollbar. The window contains a fixed container, and this container holds the drawing area and the 
		(optional) scrollbars.	
		
	 *)

	IMPORT 
		SYSTEM, Kernel, LinLibc, LinGdk, LinGtk, Dialog, Windows, Controllers, Properties, Ports, Views, Containers, 
		Documents, Files, Converters, HostGnome, HostPorts, HostFiles, HostMechanisms, Models, Services;

	CONST
		inPlace* = 31;	(* flag for in place windows *)
		untitledKey = "#System:untitled";
		allocKey = "#Host:AllocatedMemory";
		totalKey = "#Host:Total";
		byteKey = "#Host:Bytes";
		useSeparators = TRUE; noSeparators = FALSE; 	(* parameters to be used with AppendInt *)
		borderW = 5 * Ports.point;
		
		scrollRange = 16384; lineInc = 1; pageInc = 100; defThumbSize = scrollRange DIV 20;
		
		guardCheck = 4;
		
		ENTER = 0DX; ESC = 1BX;
		TAB = 09X; LTAB = 0AX; RDEL = 07X; LDEL = 08X;
		PL = 10X; PR = 11X; PU = 12X; PD = 13X;
		DL = 14X; DR = 15X; DU = 16X; DD = 17X;
		AL = 1CX; AR = 1DX; AU = 1EX; AD = 1FX;
		
		(* values for the type parameter for HandleMouse *)
		press = 0; move = 1;

	TYPE
		Directory* = POINTER TO RECORD (Windows.Directory)
		END;
	
		Window* = POINTER TO RECORD(Windows.Window)
			wnd-: LinGtk.GtkWindow; (* gtk window *)
			da: LinGtk.GtkWidget; (* gtk drawing area *)
			dlg: BOOLEAN;	(* if window has a 3d dialog border *)
			fix: BOOLEAN;	(* window is opened with fix coordinates *)
			next: Window;	(* window ring, to prevent garbage collection of windows *)
			trapped: BOOLEAN;	(* if window caused trap, it won't get idle messages anymore *)
			used: BOOLEAN;	(* window received at least on message *)
			destroyed: BOOLEAN;
			title: Views.Title;
			vBar, hBar: LinGtk.GtkWidget; (* scrollbars of the window, may be NIL *)
			fixed: LinGtk.GtkWidget;
			vBarSize, hBarSize: INTEGER; (* width of the vertical scrollbar,  height of the horizontal scrollbar *)
			oldScrollVPos, oldScrollHPos: INTEGER; (* used to determine wheather page or line increments should be scrolled *)
			open: BOOLEAN;
		END;
		
	VAR
		dir: Directory;
		unit: INTEGER;
		newNumber: INTEGER;	(* number for next untitled document *)
		main-: LinGtk.GtkWindow;
		mainVBox: LinGtk.GtkContainer; (* container to hold the menu and the statusbar *)
		menuBar, statusBar, infoBar: LinGtk.GtkWidget;
		winAnchor: Window;	(* list of all windows, from top to bottom, first is dumy header *)
		
		tWindow, fWindow: Window;	(* target and front focus windows *)
		aWindow: Window;	(* activated child window *)
		statusId: INTEGER; (* used for statusbar *)
		infoId: INTEGER; (* used for info bar*)
		
		scW-, scH-: INTEGER;	(* screen width and height *)
		
		alloc, total: INTEGER;
		allocStr, totalStr, byteStr: ARRAY 256 OF CHAR;
		idleTraped: BOOLEAN;

	
	PROCEDURE AppendInt (VAR s: ARRAY OF CHAR; n: INTEGER; useSeparators: BOOLEAN);
		VAR len: INTEGER; i, j: INTEGER; d: ARRAY 12 OF CHAR;
	BEGIN
		ASSERT(n >= 0, 20);
		i := 0; REPEAT
			d[i] := CHR(30H + n MOD 10); INC(i); n := n DIV 10;
			IF useSeparators & (i MOD 4 = 3) & (n # 0) THEN d[i] := "'"; INC(i) END
		UNTIL n = 0;
		len := LEN(s) - 1;
		j := 0; WHILE s[j] # 0X DO INC(j) END;
		IF j + i < len THEN
			REPEAT DEC(i); s[j] := d[i]; INC(j) UNTIL i = 0;
			s[j] := 0X
		END
	END AppendInt;

	PROCEDURE Append (VAR s: ARRAY OF CHAR; t: ARRAY OF CHAR);
		VAR len: INTEGER; i, j: INTEGER; ch: CHAR;
	BEGIN
		len := LEN(s);
		i := 0; WHILE s[i] # 0X DO INC(i) END;
		j := 0; REPEAT ch := t[j]; s[i] := ch; INC(j); INC(i) UNTIL (ch = 0X) OR (i = len);
		s[len - 1] := 0X
	END Append;
	
	PROCEDURE StripTitle (VAR s: Views.Title);
		VAR i: INTEGER;
	BEGIN
		IF s[0] = "<" THEN
			i := 1; WHILE (s[i] # ">") & (s[i] # 0X) DO s[i - 1] := s[i]; INC(i) END;
			DEC(i); s[i] := 0X
		END
	END StripTitle;
	
	PROCEDURE GenTitle (w: Window; name: ARRAY OF CHAR; VAR title: ARRAY OF CHAR);
	(* generate window title for a document *)
		VAR newName: ARRAY 64 OF CHAR; i: INTEGER;
	BEGIN
		IF w.sub THEN title[0] := "<"; title[1] := 0X ELSE title[0] := 0X END;
		IF name # "" THEN
			i := 0;
			WHILE name[i] # 0X DO INC(i) END;
			IF (i > 4) & (name[i-4] = ".") & (CAP(name[i-3]) = "O") & (CAP(name[i-2]) = "D") & (CAP(name[i-1]) = "C") THEN
				name[i-4] := 0X
			END;
			Append(title, name)
		ELSE
			Dialog.MapString(untitledKey, newName);
			Append(title, newName); AppendInt(title, newNumber, noSeparators);
			INC(newNumber)
		END;
		IF w.sub THEN Append(title, ">") END
	END GenTitle;	

	PROCEDURE GenPathTitle (w: Window; OUT title: ARRAY OF CHAR);
		VAR loc: Files.Locator; ch: CHAR; s1, s2: Views.Title; i, j: INTEGER;
	BEGIN
		loc := w.loc; title := "";
		WITH loc: HostFiles.Locator DO
			i := 0; ch := loc.path[0]; j := 0; s2 := "";
			WHILE ch # 0X DO
				IF (ch = "\") OR (ch = "/") THEN
					s1[j] := 0X; s2 := s1$; j := 0
				ELSE
					s1[j] := ch; INC(j)
				END;
				INC(i); ch := loc.path[i]
			END;
			s1[j] := 0X;
			IF ((CAP(s1[0]) = "M") & (CAP(s1[1]) = "O") & (CAP(s1[2]) = "D") & (s1[3] = 0X) OR
				(CAP(s1[0]) = "D") & (CAP(s1[1]) = "O") & (CAP(s1[2]) = "C") & (CAP(s1[3]) = "U") & (s1[4] = 0X) OR
				(CAP(s1[0]) = "R") & (CAP(s1[1]) = "S") & (CAP(s1[2]) = "R") & (CAP(s1[3]) = "C") & (s1[4] = 0X))
				& (s2 # "") THEN
				title := "("; Append(title, s2); Append(title, ")")
			END
		ELSE
		END;
		Append(title, w.name)
	END GenPathTitle;
	
	PROCEDURE^ (w: Window) UpdateScrollbars (focus, grow: BOOLEAN), NEW;
	
	(* auxiliary portable prcedures (scrolling) *)

	PROCEDURE GetSection (w: Window; focus, vertical: BOOLEAN;
								VAR size, sect, pos: INTEGER; VAR valid: BOOLEAN);
		VAR msg: Controllers.PollSectionMsg;
	BEGIN	(* portable *)
		msg.focus := focus; msg.vertical := vertical;
		msg.wholeSize := 1; msg.partSize := 0; msg.partPos := 0;
		msg.valid := FALSE; msg.done := FALSE;
		w.ForwardCtrlMsg(msg);
		IF msg.done THEN
			size := msg.wholeSize; sect := msg.partSize; pos := msg.partPos;
			IF size < 0 THEN size := 0 END;
			IF sect < 0 THEN sect := 0 ELSIF sect > size THEN sect := size END;
			IF pos > size - sect THEN pos := size - sect END;
			IF pos < 0 THEN pos := 0 END
		ELSE size := 1; sect := 0; pos := 0
		END;
		valid := msg.valid
	END GetSection;

	PROCEDURE SetOrigin (w: Window; focus, vertical: BOOLEAN; pos: INTEGER);
	(* set origin of window's view *)
		VAR msg: Controllers.ScrollMsg;
	BEGIN	(* portable *)
		msg.focus := focus; msg.vertical := vertical;
		msg.op := Controllers.gotoPos; msg.pos := pos;
		msg.done := FALSE;
		w.ForwardCtrlMsg(msg)
	END SetOrigin;

	PROCEDURE Scroll (w: Window; focus, vertical: BOOLEAN; dir: INTEGER);
	(* scroll relative, by line or page increment or decrement *)
		VAR msg: Controllers.ScrollMsg; c: Containers.Controller; v: Views.View;
	BEGIN	(* portable *)
		c := w.doc.ThisController(); v := c.ThisFocus();
		IF (v # NIL) & (v IS Containers.View) THEN
			Containers.FadeMarks(v(Containers.View).ThisController(), FALSE)
		END;
		msg.focus := focus; msg.vertical := vertical;
		msg.op := dir;
		msg.done := FALSE;
		w.ForwardCtrlMsg(msg)
	END Scroll;
		
	(* Window *)
	
	PROCEDURE (w: Window) ForwardCtrlMsg* (VAR msg: Controllers.Message), EXTENSIBLE;
		VAR d: BOOLEAN; res: INTEGER;
	BEGIN
		IF w.frame # NIL THEN
			Views.SetRoot(w.frame, w.frame.view, w = fWindow, w.flags);
			w.ForwardCtrlMsg^(msg);
			WITH msg: Controllers.ScrollMsg DO
				w.UpdateScrollbars(FALSE, FALSE)
			ELSE
			END;
	(*		IF (w.flags * {Windows.isAux, Windows.isTool} = {}) & (w.seq # NIL) THEN
				d := ~(Windows.neverDirty IN w.flags) & w.seq.Dirty();
				IF (d # w.dirty) & (w = aWindow) THEN
					res := USER32.SendMessageA(w.wnd, USER32.WMNCActivate, 1, 0);
					IF USER32.IsZoomed(w.wnd) # 0 THEN res := USER32.DrawMenuBar(main) END
				END
			END*)
		END
	END ForwardCtrlMsg;
	
	PROCEDURE (w: Window) SetSpec* (loc: Files.Locator; name: Files.Name; conv: Converters.Converter);
		VAR title: Views.Title;
	BEGIN
		IF name # "" THEN Kernel.MakeFileName(name, "") END;
		w.SetSpec^ (loc, name, conv);
		IF (loc # NIL) & (w.wnd # NIL) THEN GenPathTitle(w, title); w.SetTitle(title) END
	END SetSpec;
	
	PROCEDURE (w: Window) UpdateCursor (x, y: INTEGER; modifiers: SET), NEW;
		VAR pw, ph: INTEGER; msg: Controllers.PollCursorMsg; cur: INTEGER;
	BEGIN
		w.port.GetSize(pw, ph);
		IF ((w = fWindow) OR (w = tWindow) (* TODO: OR ~w.child*)) & (x >= 0) & (x < pw) & (y >= 0) & (y < ph) THEN
			msg.x := x * w.frame.unit; msg.y := y * w.frame.unit; msg.cursor := Ports.arrowCursor;
			msg.modifiers := modifiers;
			w.ForwardCtrlMsg(msg); cur := msg.cursor
		ELSE cur := Ports.arrowCursor
		END;
		IF cur >= 0 THEN w.frame.SetCursor(cur) END
	END UpdateCursor;
		
	PROCEDURE (w: Window) PutOnTop, NEW;
		VAR v: Window;
	BEGIN
		v := winAnchor;
		WHILE (v # NIL) & (v.next # w) DO v := v.next END;
		IF v # NIL THEN
			v.next := w.next; w.next := winAnchor.next; winAnchor.next := w
		END
	END PutOnTop;
	
	PROCEDURE (w: Window) SetSize* (width, height: INTEGER);
		VAR res, x, y, dw, dh: INTEGER; 
	BEGIN
		IF w.port # NIL THEN
			w.SetSize^(width, height);
			dw := width - w.da.allocation.width; dh := height - w.da.allocation.height;
			IF ~(inPlace IN w.flags) & ((dw # 0) OR (dh # 0)) THEN			
				LinGtk.gtk_widget_set_usize(w.wnd, width + w.vBarSize, height + w.hBarSize);
				IF w.fixed # NIL THEN
					LinGtk.gtk_widget_set_usize(w.fixed, width + w.vBarSize, height + w.hBarSize);
					IF w.vBarSize > 0 THEN
						LinGtk.gtk_fixed_move(w.fixed, w.vBar, SHORT(width), 0);
						LinGtk.gtk_widget_set_usize(w.vBar, w.vBarSize, height);
					END;
					IF w.hBarSize > 0 THEN
						LinGtk.gtk_fixed_move(w.fixed, w.hBar, 0, SHORT(height));
						LinGtk.gtk_widget_set_usize(w.hBar, width, w.hBarSize);
					END;
				END;
				LinGtk.gtk_drawing_area_size(w.da, width, height)
			END
		END
	END SetSize;
	
	PROCEDURE (w: Window) SetTitle2 (title: Views.Title), NEW;
	(* assign name of window, generate title out of name, and update window title bar *)
		VAR res: INTEGER; h: Window; t: ARRAY 256 OF CHAR; st: ARRAY 256 OF SHORTCHAR;
	BEGIN
		ASSERT(w.wnd # NIL, 20);
		StripTitle(title);
		h := w;
		REPEAT
			GenTitle(h, title, t); st := SHORT(t$);
			LinGtk.gtk_window_set_title(w.wnd, st);
			h := h.link(Window)
		UNTIL h = w
	END SetTitle2;

	PROCEDURE (w: Window) KeyDown* (ch: CHAR; buttons: SET);
	BEGIN
		w.KeyDown^(ch, buttons);
		Properties.IncEra
	END KeyDown;
	
	PROCEDURE (w: Window) SetTitle* (title: Views.Title);
	BEGIN
		ASSERT(w.wnd # NIL, 20);
		w.title := title; Dialog.MapString(w.title, title);
		w.SetTitle2(title)
	END SetTitle;

	PROCEDURE (w: Window) RefreshTitle* ;
		VAR title: Views.Title;
	BEGIN
		ASSERT(w.wnd # NIL, 20);
		Dialog.MapString(w.title, title);
		w.SetTitle2(title)
	END RefreshTitle;
	
	PROCEDURE (w: Window) GetTitle* (OUT title: Views.Title);
	BEGIN
		title := w.title
	END GetTitle;

	PROCEDURE (w: Window) Mark (do, wk: BOOLEAN), NEW;
		VAR mark: Controllers.MarkMsg;
	BEGIN
		mark.show := do;
		mark.focus := ~wk;
		w.ForwardCtrlMsg(mark);
		Properties.IncEra
	END Mark;
	
	PROCEDURE (w: Window) ScrollDir (sdir: INTEGER; value: SHORTREAL; focus, vertical: BOOLEAN), NEW;
		VAR  size, sect, pos, type: INTEGER; valid: BOOLEAN;
	BEGIN
		GetSection(w, focus, vertical, size, sect, pos, valid);
		IF valid THEN
			IF sdir = Controllers.gotoPos THEN
				value := SHORT(ENTIER((value * (size - sect)) / scrollRange));
				ASSERT(value >= 0, 100);
				SetOrigin(w, focus, vertical, SHORT(ENTIER(value)))
			ELSE
				Scroll(w, focus, vertical, sdir)
			END;
			dir.Update(w);
		END
	END ScrollDir;

	PROCEDURE (w: Window) Scroll (adj: LinGtk.GtkAdjustment; focus, vertical: BOOLEAN), NEW;
		VAR sdir, osp: INTEGER;
	BEGIN
		IF vertical THEN osp := w.oldScrollVPos ELSE osp := w.oldScrollHPos END;
		IF adj.value = osp - lineInc THEN
			sdir := Controllers.decLine
		ELSIF adj.value = osp + lineInc THEN
			sdir := Controllers.incLine
		ELSIF adj.value = osp - pageInc THEN
			sdir := Controllers.decPage
		ELSIF adj.value = osp + pageInc THEN
			sdir := Controllers.incPage
		ELSE
			sdir := Controllers.gotoPos
		END;
		IF sdir = Controllers.gotoPos THEN
			w.ScrollDir(sdir, adj.value, focus, vertical)
		ELSE
			w.ScrollDir(sdir, 0, focus, vertical)
		END
	END Scroll;

	PROCEDURE UpdateScrollbar (w: Window; vertical, focus: BOOLEAN);
		VAR res, size, sect, pos: INTEGER; valid: BOOLEAN;
			msg: Controllers.PollSectionMsg; f: Views.Frame;
			adj: LinGtk.GtkAdjustment; trans: REAL;
	BEGIN
		IF w.frame = NIL THEN RETURN END;
		GetSection(w, focus, vertical, size, sect, pos, valid);
		IF valid THEN
			IF size = 0 THEN size := 1 END;
			IF vertical THEN
				adj := LinGtk.gtk_range_get_adjustment(SYSTEM.VAL(LinGtk.GtkRange, w.vBar));
				LinGtk.gtk_widget_show(w.vBar);
				w.vBarSize := w.vBar.requisition.width
			ELSE
				adj := LinGtk.gtk_range_get_adjustment(SYSTEM.VAL(LinGtk.GtkRange, w.hBar));
				LinGtk.gtk_widget_show(w.hBar);
				w.hBarSize := w.hBar.requisition.height
			END;
			trans := scrollRange / (size - sect);
			adj.value := SHORT(ENTIER(pos * trans));
			IF sect > 0 THEN
				adj.page_size := SHORT(ENTIER(sect * trans))
			ELSE
				adj.page_size := defThumbSize
			END;
			adj.lower := 0;
			adj.upper := scrollRange + adj.page_size;
			LinGtk.gtk_adjustment_changed(adj);
			IF vertical THEN w.oldScrollVPos := SHORT(ENTIER(adj.value)) 
			ELSE w.oldScrollHPos := SHORT(ENTIER(adj.value)) END;
		ELSIF ~focus THEN
			msg.focus := FALSE; msg.vertical := vertical; msg.done := FALSE;
			f := Views.ThisFrame(w.frame, w.doc.ThisView());
			IF f # NIL THEN
				Views.ForwardCtrlMsg(f, msg);
				IF msg.done THEN
					IF vertical THEN
						adj := LinGtk.gtk_range_get_adjustment(SYSTEM.VAL(LinGtk.GtkRange, w.vBar)); 
						LinGtk.gtk_widget_show(w.vBar);
						w.vBarSize := w.vBar.requisition.width
					ELSE
						adj := LinGtk.gtk_range_get_adjustment(SYSTEM.VAL(LinGtk.GtkRange, w.hBar));
						LinGtk.gtk_widget_show(w.hBar);
						w.hBarSize := w.hBar.requisition.height
					END;
					adj.page_size := adj.upper;
					LinGtk.gtk_adjustment_changed(adj);
				ELSE 
					IF vertical THEN
						w.vBarSize := 0;
						LinGtk.gtk_widget_hide(w.vBar)
					ELSE
						w.hBarSize := 0;
						LinGtk.gtk_widget_hide(w.hBar)
					END
				END
			ELSE
				IF vertical THEN
					w.vBarSize := 0;
					LinGtk.gtk_widget_hide(w.vBar)
				ELSE
					w.hBarSize := 0;
					LinGtk.gtk_widget_hide(w.hBar)
				END
			END
		END
	END UpdateScrollbar;
	
	PROCEDURE (w: Window) UpdateScrollbars (focus, grow: BOOLEAN), NEW;
		VAR v, h, width, height: INTEGER;
	BEGIN
	
		v := w.vBarSize; h := w.hBarSize;
	(*
		IF (USER32.GetAsyncKeyState(1) >= 0)
				& (USER32.GetAsyncKeyState(4) >= 0)
				& (USER32.GetAsyncKeyState(2) >= 0) THEN*)
			IF ~(Windows.noHScroll IN w.flags) THEN UpdateScrollbar(w, FALSE, focus) END;
			IF ~(Windows.noVScroll IN w.flags) THEN UpdateScrollbar(w, TRUE, focus) END;
(*		END*)

		IF (v # w.vBarSize) OR (h # w.hBarSize) THEN
			IF grow THEN
				w.GetSize(width, height);
				w.SetSize(width + w.vBarSize, height + w.hBarSize)
			ELSE
				w.GetSize(width, height);
				width := width - (w.vBarSize - v);
				height := height - (w.hBarSize - h);
				w.SetSize(width, height)
			END
		END
	END UpdateScrollbars;

	PROCEDURE (w: Window) Close*;
		VAR res: INTEGER; h: Window;
	BEGIN
		ASSERT(w.frame # NIL, 20);
		IF fWindow = w THEN
		(*	w.Mark(FALSE, FALSE); *)fWindow := NIL;
			IF tWindow = w THEN tWindow := NIL END
		ELSIF tWindow = w THEN
		(*	w.Mark(FALSE, FALSE); *)tWindow := NIL
		END;
		h := winAnchor;
		WHILE (h.next # NIL) & (h.next # w) DO h := h.next END;
		ASSERT(h.next = w, 21);
		h.next := w.next; w.next := NIL;		
(*		HostMechanisms.RemoveDropTarget(w.wnd);*)
		w.Close^;
		IF ~w.destroyed THEN
			w.destroyed := TRUE;
			LinGtk.gtk_container_remove(w.wnd, w.fixed);
			LinGtk.gtk_widget_destroy(w.wnd);
			(*
			IF w.child THEN
				IF USER32.IsZoomed(w.wnd) # 0 THEN
					res := USER32.SendMessageA(client, USER32.WMMDIRestore, w.wnd, 0)
				END;
				res := USER32.SendMessageA(client, USER32.WMMDIDestroy, w.wnd, 0)
			ELSE
				res := USER32.DestroyWindow(w.wnd)
			END;
			w.trapped := TRUE; w.wnd := 0
			*)
		END;
		ASSERT(w.frame = NIL, 60)
	END Close;
	
	PROCEDURE (w: Window) MouseDown* (x, y, time: INTEGER; modifiers: SET);
	(* handle a mouse down event in window *)
		VAR pw, ph: INTEGER; track: Controllers.TrackMsg;
	BEGIN
		track.modifiers := modifiers;
		w.port.GetSize(pw, ph); track.x := x * w.port.unit; track.y := y * w.port.unit;
		w.ForwardCtrlMsg(track);
		Properties.IncEra
	END MouseDown;
	
	PROCEDURE HandleChar ( w: Window; key: INTEGER; mod: SET);
		VAR ch: CHAR;
	BEGIN
		IF (key >= 20H) & (key # 7FH) THEN
(*			IF USER32.VkKeyScanA(SHORT(CHR(wParam))) >= 4 * 256 THEN EXCL(mod, HostPorts.alt) END;*)
			CASE key OF
			| 80H: ch := 20ACX	(* euro *)
			| 82H: ch := 201AX
			| 83H: ch := 0192X
			| 84H: ch := 201EX
			| 85H: ch := 2026X
			| 86H: ch := 2020X
			| 87H: ch := 2021X
			| 88H: ch := 02C6X
			| 89H: ch := 2030X
			| 8AH: ch := 0160X
			| 8BH: ch := 2039X
			| 8CH: ch := 0152X
			| 91H: ch := 2018X
			| 92H: ch := 2019X
			| 93H: ch := 201CX
			| 94H: ch := 201DX
			| 95H: ch := 2022X
			| 96H: ch := 2013X
			| 97H: ch := 2014X
			| 98H: ch := 02DCX
			| 99H: ch := 2122X
			| 9AH: ch := 0161X
			| 9BH: ch := 203AX
			| 9CH: ch := 0153X
			| 9FH: ch := 0178X
			ELSE ch := CHR(key)
			END;
			w.KeyDown(ch, mod)
		END
	END HandleChar;

	PROCEDURE HandleKey  (wndHandle, eventHandle, null: INTEGER);
		VAR 
			b: SET; w: Window; pmsg: Controllers.PollFocusMsg; scroll: BOOLEAN; c: Containers.Controller; 
			event: LinGdk.GdkEventKey; code: INTEGER;
	BEGIN
		w := SYSTEM.VAL(Window, wndHandle);
		event := SYSTEM.VAL(LinGdk.GdkEventKey, eventHandle);
		b := {}; 
		IF LinGdk.GDK_SHIFT_MASK IN event.state THEN b := b + {HostPorts.shift, Controllers.extend} END;
		IF LinGdk.GDK_CONTROL_MASK IN event.state THEN b := b + {HostPorts.ctrl, Controllers.modify} END;
		IF LinGdk.GDK_MOD1_MASK IN event.state THEN INCL(b, HostPorts.alt) END;
		w.used := TRUE;
		scroll := LinGdk.GDK_MOD5_MASK IN event.state; (* scroll lock *)
		pmsg.focus := NIL; w.ForwardCtrlMsg(pmsg);
		IF (pmsg.focus # NIL) & (pmsg.focus.view IS Containers.View) THEN
			c := pmsg.focus.view(Containers.View).ThisController();
			IF (c # NIL) & (Containers.noCaret IN c.opts) THEN scroll := TRUE END
		END;
		code := event.keyval;
		CASE code OF (* key codes defined in gdk/gdkkeysyms.h *)
		| 0FFFFH(*2EH*): w.KeyDown(RDEL, b)	(* delete -> right delete *)
		| 0FF08H(*08H*): w.KeyDown(LDEL, b)	(* backspace -> left delete *)
		| 0FF09H(*09H*):  (* tab *)
			IF Controllers.extend IN b THEN w.KeyDown(LTAB, b)	(* left tab *)
			ELSE w.KeyDown(TAB, b)	(* right tab *)
			END
		| 0FF0DH(*0DH*): w.KeyDown(ENTER, b)	(* enter *)
		| 0FF1BH(*1BH*): w.KeyDown(ESC, b)	(* escape *)
		| 0FF55H(*21H*):	(* page up *)
			IF scroll THEN
				w.ScrollDir(Controllers.decPage, 0, TRUE, ~(Controllers.modify IN b))
			ELSIF Controllers.modify IN b THEN (* move caret left one page *)
				w.KeyDown(PL, b - {Controllers.modify})
			ELSE (* move caret up one page *)
				w.KeyDown(PU, b)
			END
		| 0FF56H(*22H*): (* page down *)
			IF scroll THEN
				w.ScrollDir(Controllers.incPage, 0, TRUE, ~(Controllers.modify IN b))
			ELSIF Controllers.modify IN b THEN (* move caret right one page *)
				w.KeyDown(PR, b - {Controllers.modify})
			ELSE (* move caret down one page *)
				w.KeyDown(PD, b)
			END
		| 0FF57H(*23H*): (* end *)
			IF scroll THEN
				w.ScrollDir(Controllers.gotoPos, scrollRange, TRUE, Controllers.modify IN b)
			ELSIF Controllers.modify IN b THEN (* move caret to doc end *)
				w.KeyDown(DD, b - {Controllers.modify})
			ELSE (* move caret to line end *)
				w.KeyDown(DR, b)
			END
		| 0FF50H(*24H*): (* home *)
			IF scroll THEN
				w.ScrollDir(Controllers.gotoPos, 0, TRUE, Controllers.modify IN b)
			ELSIF Controllers.modify IN b THEN (* move caret to doc start *)
				w.KeyDown(DU, b - {Controllers.modify})
			ELSE (* move caret to line start *)
				w.KeyDown(DL, b)
			END
		| 0FF51H(*25H*): (* left *)
			IF scroll THEN
				w.ScrollDir(Controllers.decLine, 0, TRUE,  FALSE)
			ELSE
				w.KeyDown(AL, b)
			END
		| 0FF52H(*26H*): (* up *)
			IF scroll THEN
				w.ScrollDir(Controllers.decLine, 0, TRUE,  TRUE)
			ELSE
				w.KeyDown(AU, b)
			END
		| 0FF53H(*27H*): (* right *)
			IF scroll THEN
				w.ScrollDir(Controllers.incLine, 0, TRUE,  FALSE)
			ELSE
				w.KeyDown(AR, b)
			END
		| 0FF54H(*28H*): (* down *)
			IF scroll THEN
				w.ScrollDir(Controllers.incLine, 0, TRUE,  TRUE)
			ELSE
				w.KeyDown(AD, b)
			END
		ELSE
			code := ORD(event.string[0]); 
			IF code # 0 THEN HandleChar(w, code, b) END
		END;
		Properties.IncEra
	END HandleKey;

	PROCEDURE HandleMouse  (wndHandle, eventHandle, type: INTEGER);
		VAR 
			w: Window; isDown: BOOLEAN; x, y: INTEGER; b: SET; f, g: Views.Frame; 
			eventB: LinGdk.GdkEventButton; eventM: LinGdk.GdkEventMotion; e: LinGdk.GdkEvent;
			ex, ey: REAL; button: INTEGER; state: SET;
	BEGIN
		b := {};
		w := SYSTEM.VAL(Window, wndHandle);
		IF type = press THEN
			eventB := SYSTEM.VAL(LinGdk.GdkEventButton, eventHandle);
			ASSERT(~(eventB.type IN {LinGdk.GDK_2BUTTON_PRESS, LinGdk.GDK_3BUTTON_PRESS}), 100);
			button := eventB.button; state := eventB.state; ex := eventB.x; ey := eventB.y;
		ELSE
			eventM := SYSTEM.VAL(LinGdk.GdkEventMotion, eventHandle);
			button := 0; state := eventM.state; ex := eventM.x; ey := eventM.y;
		END;
		w.used := TRUE;
		IF button = 1 THEN INCL(b, HostPorts.left) END;
		IF button = 2 THEN INCL(b, HostPorts.middle) END;
		IF button = 3 THEN INCL(b, HostPorts.right) END;
		isDown := b # {};
		IF LinGdk.GDK_SHIFT_MASK IN state THEN INCL(b, HostPorts.shift); INCL(b, Controllers.extend) END;
		IF LinGdk.GDK_CONTROL_MASK IN state THEN INCL(b, HostPorts.ctrl); INCL(b, Controllers.modify) END;
		IF LinGdk.GDK_MOD1_MASK IN state THEN INCL(b, HostPorts.alt) END;
		e := LinGdk.gdk_event_peek();
		(* On Linux it would be possible to implement triple-click the same way as double click below. Would that make sense? *)
		IF (e # NIL) & (e.type = LinGdk.GDK_2BUTTON_PRESS) THEN 
			INCL(b, Controllers.doubleClick); 
			LinGdk.gdk_event_free(e);
(*			e := LinGdk.gdk_event_get()*)
		END;
(*		IF e # NIL THEN LinGdk.gdk_event_free(e) END;*)
		x := SHORT(ENTIER(ex));
		y := SHORT(ENTIER(ey));
		HostPorts.SetMouseState(x, y, b, isDown);

		(*
		IF wParam DIV 256 = 1 THEN
			IF {HostPorts.middle, HostPorts.shift, HostPorts.ctrl} - b = {} THEN
				CallHeapShow(SYSTEM.VAL(INTEGER, w))
			ELSIF {HostPorts.middle, HostPorts.shift, HostPorts.alt} - b = {} THEN
				f := w.frame; x := x * f.unit; y := y * f.unit;
				REPEAT g := f; f := Views.FrameAt(g, x - g.gx, y - g.gy) UNTIL f = NIL;
				CallHeapShow(SYSTEM.VAL(INTEGER, g))
			ELSIF ~activating THEN
				w.MouseDown(x, y, 0, b)
			END
		ELSIF wParam DIV 256 = 2 THEN
			w.UpdateCursor(x, y, b)
		END;
		*)
		IF type = press THEN
			w.MouseDown(x, y, 0, b)
		END;
		IF w.port # NIL THEN (* window could have been closed by MouseDown *)
			w.UpdateCursor(x, y, b)
		END;
				
(*		IF ~isDown THEN activating := FALSE END;*)
		Properties.IncEra
	END HandleMouse;	
	
	(* Signal handlers *)
	
	PROCEDURE MouseHandler(widget: LinGtk.GtkWidget; event: LinGdk.GdkEventButton; user_data: LinLibc.PtrVoid): INTEGER;
		VAR w: Window; 
	BEGIN
		IF event.type = LinGdk.GDK_BUTTON_PRESS THEN
			w := SYSTEM.VAL(Window, user_data);
			Controllers.SetCurrentPath(Controllers.targetPath);
			LinGtk.gtk_grab_add(w.da);
			Kernel.Try(HandleMouse, user_data, SYSTEM.VAL(INTEGER, event), press);
	(*		IF LinGtk.gtk_grab_get_current() = w.da THEN LinGtk.gtk_grab_remove(w.da) END;*)
			IF LinGtk.gtk_grab_get_current() # NIL THEN LinGtk.gtk_grab_remove(LinGtk.gtk_grab_get_current()) END;
			Controllers.ResetCurrentPath()
		END;
		RETURN 1 
	END MouseHandler;
	
	PROCEDURE WinMouseHandler(widget: LinGtk.GtkWidget; event: LinGdk.GdkEventButton; user_data: LinLibc.PtrVoid): INTEGER;
		VAR w: Window; wx, wy: INTEGER;
	BEGIN
		w := SYSTEM.VAL(Window, user_data);
		LinGdk.gdk_window_get_position(w.wnd.window, wx, wy);		
		event.x := event.x_root - wx;
		event.y := event.y_root - wy;		
		RETURN MouseHandler(w.da, event, user_data);
		RETURN 1
	END WinMouseHandler;
	
	PROCEDURE WinMouseRelease(widget: LinGtk.GtkWidget; event: LinGdk.GdkEventButton; user_data: LinLibc.PtrVoid): INTEGER;
	BEGIN
		Dialog.ShowMsg(" ################## WinMouseRelease !!!");
(*		HostTmp.released := TRUE;*)
		RETURN 1
	END WinMouseRelease;
	
	PROCEDURE MouseMove(widget: LinGtk.GtkWidget; event: LinGdk.GdkEventMotion; user_data: LinLibc.PtrVoid): INTEGER;
		VAR pw, ph, x, y: INTEGER; w: Window;
	BEGIN
		w := SYSTEM.VAL(Window, user_data);
		w.port.GetSize(pw, ph); 
		LinGdk.gdk_window_get_position(w.wnd.window, x, y);
		IF (event.x_root < x + pw) & (event.y_root < y + ph) THEN 
			Controllers.SetCurrentPath(Controllers.targetPath);
			LinGtk.gtk_grab_add(widget);
			Kernel.Try(HandleMouse, user_data, SYSTEM.VAL(INTEGER, event), move);
			IF LinGtk.gtk_grab_get_current() = widget THEN LinGtk.gtk_grab_remove(widget) END;
			Controllers.ResetCurrentPath();
		END;
		RETURN 1 
	END MouseMove;
	
	PROCEDURE KeyHandler(widget: LinGtk.GtkWidget; event: LinGdk.GdkEventKey; user_data: LinLibc.PtrVoid): INTEGER;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		Kernel.Try(HandleKey, user_data, SYSTEM.VAL(INTEGER, event), 0);
		Controllers.ResetCurrentPath();
		RETURN LinLibc.TRUE
	END KeyHandler;
	
	(* retrun 0 -> close ok. return 1 -> don't close *)
	PROCEDURE DeleteHandler(widget: LinGtk.GtkWidget; event: LinGdk.GdkEvent; user_data: LinLibc.PtrVoid): INTEGER;
		VAR res: INTEGER; w: Window;
	BEGIN
		w := SYSTEM.VAL(Window, user_data);
		IF w.destroyed THEN
			RETURN 0
		ELSE
			Dialog.Call("HostCmds.Close", "", res);
			RETURN 1
		END 
	END DeleteHandler;
	
	PROCEDURE ConfigureHandler(widget: LinGtk.GtkWidget; event: LinGdk.GdkEventConfigure; user_data: LinLibc.PtrVoid): INTEGER;
		VAR w, h, nw, nh: INTEGER; win: Window;
	BEGIN
		win := SYSTEM.VAL(Window, user_data);
		IF win.open THEN 
			win.GetSize(w, h);
			nw := event.width - win.vBarSize;
			nh := event.height - win.hBarSize;
			IF (w # nw) OR (h # nh) THEN
				win.SetSize(nw, nh)
			END;
			LinGtk.gtk_grab_add(win.da);
			LinGtk.gtk_grab_remove(win.da)
		END;
		RETURN 1 
	END ConfigureHandler;
	
	PROCEDURE ExposeEvent (widget: LinGtk.GtkWidget; event: LinGdk.GdkEventExpose; user_data: LinLibc.PtrVoid): INTEGER;
		VAR w: Window; p: HostPorts.Port;
	BEGIN
		w := SYSTEM.VAL(Window, user_data);
		w.open := TRUE;
		w.Restore(event.area.x, event.area.y, event.area.x + event.area.width, event.area.y + event.area.height);
		IF event.count = 0 THEN w.Update END;
		RETURN 1
	END ExposeEvent;
	
	PROCEDURE DestroyHandler (object: LinGtk.GtkObject; func_data: LinLibc.PtrVoid);
		VAR w: Window;
	BEGIN
		w := SYSTEM.VAL(Window, func_data);
		IF ~w.destroyed THEN
			w.destroyed := TRUE;
			w.Close
		END
	END DestroyHandler;
	
	PROCEDURE ShowHandler (object: LinGtk.GtkObject; func_data: LinLibc.PtrVoid);
		VAR w: Window;
	BEGIN
		w := SYSTEM.VAL(Window, func_data);
		w.Restore(0, 0, w.wnd.allocation.width, w.wnd.allocation.height);
		w.Update;
	END ShowHandler;
	
	PROCEDURE DummyHandler (object: LinGtk.GtkObject; func_data: LinLibc.PtrVoid);
	BEGIN
		Kernel.Beep
	END DummyHandler;

	PROCEDURE DeactivateWin (w: Window);
	BEGIN
		IF fWindow = w THEN
			w.Mark(FALSE, TRUE); fWindow := NIL;
			IF (inPlace IN w.flags) OR ~(Windows.isTool IN w.flags) THEN
				w.Mark(TRUE, TRUE);
				IF (w # aWindow) & ~(inPlace IN w.flags) THEN tWindow := NIL END
			END
		END
	END DeactivateWin;
	
	PROCEDURE ChildActivateHandler (window: LinGtk.GtkWindow; event: LinGdk.GdkEventFocus; user_data: LinLibc.PtrVoid);
		VAR w: Window;
	BEGIN
		w := SYSTEM.VAL(Window, user_data);
		IF fWindow # w THEN
			IF fWindow # NIL THEN
				DeactivateWin(fWindow)
			END;
			w.PutOnTop;
			IF (inPlace IN w.flags) OR ~(Windows.isTool IN w.flags) THEN
				w.Mark(FALSE, TRUE);
				tWindow := w; aWindow := w
			END;
			fWindow := w; w.Mark(TRUE, TRUE);
			Properties.IncEra;
			Dialog.Notify(0, 0, {guardCheck})
		END
	END ChildActivateHandler;
	
	PROCEDURE VScrollChanged (adjustment: LinGtk.GtkAdjustment; user_data: LinLibc.PtrVoid);
		VAR w: Window;
	BEGIN
		w := SYSTEM.VAL(Window, user_data);
		w.Scroll(adjustment, FALSE, TRUE) (* TODO: If ctrl is pressed TRUE should be passed for focus. *)
	END VScrollChanged;
	
	PROCEDURE HScrollChanged (adjustment: LinGtk.GtkAdjustment; user_data: LinLibc.PtrVoid);
		VAR w: Window;
	BEGIN
		w := SYSTEM.VAL(Window, user_data);
		w.Scroll(adjustment, FALSE, FALSE) (* TODO: If ctrl is pressed TRUE should be passed for focus. *)
	END HScrollChanged;
	
	(* Directory *)
	
	PROCEDURE (d: Directory) Close* (w: Windows.Window);
		VAR v, u: Windows.Window; h: Window;
	BEGIN
		h := winAnchor; WHILE (h.next # NIL) & (h.next # w) DO h := h.next END;
		IF h.next = w THEN
			IF ~w.sub THEN
				v := w.link;
				WHILE v # w DO u := v.link; v.Close; v := u END
			END;
			w.Close
		END
	END Close;
	
	PROCEDURE (d: Directory) Focus* (target: BOOLEAN): Window;
	BEGIN
		IF target THEN RETURN tWindow ELSE RETURN fWindow END
	END Focus;
	
	PROCEDURE (d: Directory) GetBounds* (OUT w, h: INTEGER);
	BEGIN
		w := scW; h := scH
	END GetBounds;
	
	PROCEDURE (d: Directory) GetThisWindow* (p: Ports.Port; px, py: INTEGER; OUT x, y: INTEGER; OUT w: Windows.Window);
	BEGIN
		Kernel.Beep;
		
		(* TODO: Implement this when implementing drag and drop. Use: LinGdk.gdk_window_at_pointer *)
		
	END GetThisWindow;
	
	PROCEDURE (d: Directory) New* (): Windows.Window;
		VAR w: Window;
	BEGIN
		NEW(w); RETURN w
	END New;

	PROCEDURE (d: Directory) First* (): Window;
	BEGIN
		RETURN winAnchor.next
	END First;

	PROCEDURE (d: Directory) Next* (w: Windows.Window): Window;
	BEGIN
		IF w # NIL THEN RETURN w(Window).next ELSE RETURN NIL END
	END Next;
	
	PROCEDURE (d: Directory) Select* (w: Windows.Window; lazy: BOOLEAN);
	BEGIN
		WITH w: Window DO
			LinGdk.gdk_window_raise(w.wnd.window);
			LinGtk.gtk_widget_grab_focus(w.wnd);
			(* On windows the raised window gets focus, but under X this is left to the Window manager to decide. *)
		END
	END Select;
	
	PROCEDURE (d: Directory) Open* (w: Windows.Window; doc: Documents.Document; flags: SET; name: Views.Title; 
													loc: Files.Locator; fname: Files.Name; conv: Converters.Converter);
		VAR p: HostPorts.Port; c: Containers.Controller; cw, ch: INTEGER;
			
		PROCEDURE ConnectSignals (w: Window);
			VAR res: INTEGER;
		BEGIN
					
			(* drawing area signals *)
			res := LinGtk.gtk_signal_connect(w.da, "expose_event", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(ExposeEvent)),
														SYSTEM.VAL(INTEGER, w));
			res := LinGtk.gtk_signal_connect(w.da, "destroy", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(DestroyHandler)), 
														SYSTEM.VAL(INTEGER, w));
			res := LinGtk.gtk_signal_connect(w.da, "show", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(ShowHandler)), 
														SYSTEM.VAL(INTEGER, w));
			res := LinGtk.gtk_signal_connect(w.da, "button-press-event", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(MouseHandler)), 
														SYSTEM.VAL(INTEGER, w));
			res := LinGtk.gtk_signal_connect(w.da, "motion-notify-event", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(MouseMove)), 
														SYSTEM.VAL(INTEGER, w));
														
			(* window signals *)

			res := LinGtk.gtk_signal_connect(w.fixed, "button-press-event", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(WinMouseHandler)), 
														SYSTEM.VAL(INTEGER, w));
			res := LinGtk.gtk_signal_connect(w.wnd, "button-press-event", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(WinMouseHandler)), 
														SYSTEM.VAL(INTEGER, w));	
			res := LinGtk.gtk_signal_connect(w.wnd, "focus-in-event", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(ChildActivateHandler)), 
														SYSTEM.VAL(INTEGER, w));
			res := LinGtk.gtk_signal_connect(w.wnd, "configure-event", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(ConfigureHandler)), 
														SYSTEM.VAL(INTEGER, w));
			res := LinGtk.gtk_signal_connect_after(w.wnd, "key-press-event", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(KeyHandler)), 
														SYSTEM.VAL(INTEGER, w));
			res := LinGtk.gtk_signal_connect(w.wnd, "delete-event", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(DeleteHandler)), 
														SYSTEM.VAL(INTEGER, w));
														
			(* set the event masks *)
			LinGtk.gtk_widget_set_events(w.da, 
				LinGdk.GDK_EXPOSURE_MASK + LinGdk.GDK_BUTTON_PRESS_MASK + 
				LinGdk.GDK_BUTTON_RELEASE_MASK + LinGdk.GDK_BUTTON_MOTION_MASK +
				LinGdk.GDK_KEY_PRESS_MASK + LinGdk.GDK_KEY_RELEASE_MASK +
				LinGdk.GDK_POINTER_MOTION_MASK);
				
			LinGtk.gtk_widget_set_events(w.wnd,  LinGdk.GDK_BUTTON_PRESS_MASK + 
				LinGdk.GDK_BUTTON_RELEASE_MASK);
				
			LinGtk.gtk_widget_set_events(w.fixed,  LinGdk.GDK_BUTTON_PRESS_MASK + 
				LinGdk.GDK_BUTTON_RELEASE_MASK);
																
		END ConnectSignals;

		PROCEDURE AddScrollbars (w: Window);
			VAR  vadj, hadj: LinGtk.GtkAdjustment; res: INTEGER; req: LinGtk.GtkRequisitionDesc;
		BEGIN
			w.fixed := LinGtk.gtk_fixed_new();
			LinGtk.gtk_fixed_put(w.fixed, w.da, 0, 0);
			IF  (Windows.noHScroll IN w.flags) & (Windows.noVScroll IN w.flags) THEN 
				w.vBarSize := 0;
				w.hBarSize := 0;
			ELSE
				vadj := LinGtk.gtk_adjustment_new(0, 0, scrollRange, lineInc, pageInc, defThumbSize);
				res := LinGtk.gtk_signal_connect(vadj, "value_changed", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(VScrollChanged)),
														SYSTEM.VAL(INTEGER, w));
				hadj := LinGtk.gtk_adjustment_new(0, 0, scrollRange, lineInc, pageInc, defThumbSize);
				res := LinGtk.gtk_signal_connect(hadj, "value_changed", 
														SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(HScrollChanged)),
														SYSTEM.VAL(INTEGER, w));
														
				w.vBar := LinGtk.gtk_vscrollbar_new(vadj);
				EXCL(w.vBar.flags, 11);
				w.hBar := LinGtk.gtk_hscrollbar_new(hadj);
				EXCL(w.hBar.flags, 11);			
(*				w.fixed := LinGtk.gtk_fixed_new();*)
(*				LinGtk.gtk_fixed_put(w.fixed, w.da, 0, 0);*)
				LinGtk.gtk_fixed_put(w.fixed, w.vBar, 100, 0);
				LinGtk.gtk_fixed_put(w.fixed, w.hBar, 0, SHORT(w.wnd.allocation.height - w.hBarSize));
(*				LinGtk.gtk_container_add(w.wnd, w.fixed);	*)
			
				LinGtk.gtk_widget_size_request(w.vBar, SYSTEM.ADR(req));
				(*w.vBarSize := req.width;*)
				w.vBarSize := 0;
				LinGtk.gtk_widget_size_request(w.hBar, SYSTEM.ADR(req));
				(*w.hBarSize := req.height;*)
				w.hBarSize := 0
			END;			
			LinGtk.gtk_container_add(w.wnd, w.fixed)
		END AddScrollbars;
	
		PROCEDURE IsDialog (w: Window; c: Containers.Controller ): BOOLEAN;
			VAR dlg: BOOLEAN; v: Views.View; f: SET; col: Ports.Color;
		BEGIN						
			IF Windows.isTool IN w.flags THEN dlg := TRUE
			ELSE
				v := w.doc.ThisView(); f := {};
				WITH v: Containers.View DO
					c := v.ThisController();
					IF c # NIL THEN f := c.opts END
				ELSE 
				END;
				col := Views.transparent; v.GetBackground(col);
				dlg := ({Containers.noCaret, Containers.noSelection} - f = {})	(* mask mode *)
						& (col = Ports.dialogBackground);	(* dialog background *)
			END;
			RETURN dlg
		END IsDialog;
		
		PROCEDURE SetSizePos (p: HostPorts.Port; w: Window);
			VAR
				res, size, sect, pos: INTEGER; 
				u, dl, dt, dr, db: INTEGER;
				req: LinGtk.GtkRequisitionDesc;
		BEGIN
			IF (d.l >= 0) & (d.t >= 0) & ~((d.l = 0) & (d.t = 0) & (d.r = 0) & (d.b = 0)) THEN
				LinGtk.gtk_widget_set_uposition(w.wnd, d.l, d.t);
				IF (d.r > d.l) & (d.b > d.t) THEN
					cw := d.r - d.l; ch := d.b - d.t; w.fix := TRUE
				END
			ELSE
				cw := w.wnd.allocation.width; ch := w.wnd.allocation.height;
			END;				
			u := w.frame.unit; 
			w.port.SetSize(0, 0);
			w.doc.PollRect(dl, dt, dr, db);
			IF w.fix THEN
				IF w.dlg THEN w.doc.SetRect(0, 0, cw * u, ch * u)
				ELSE w.doc.SetRect(borderW, borderW, cw * u - borderW, ch * u - borderW)
				END
			ELSIF w.dlg THEN
				cw := (dr - dl) DIV u;
				ch := (db - dt) DIV u;
				w.doc.SetRect(0, 0, dr - dl, db - dt)
			ELSE
				cw := (dr - dl + 2 * borderW) DIV u + 1;
				ch := (db - dt + 2 * borderW) DIV u + 1;
				IF ~(Windows.noHScroll IN w.flags) & (cw > scW - 40) THEN cw := scW - 80 END;
				IF ~(Windows.noVScroll IN w.flags) & (ch > scH - 40) THEN ch := scH - 160 END;
				w.doc.SetRect(borderW, borderW, borderW + dr - dl, borderW + db - dt)
			END;
			IF cw < 0 THEN cw := 0 END;
			IF ch < 0 THEN ch := 0 END;
			LinGtk.gtk_drawing_area_size(w.da, cw, ch);	
			w.SetSize(cw, ch);
			IF (Windows.isTool IN w.flags) THEN
				LinGtk.gtk_window_set_position(w.wnd, LinGtk.GTK_WIN_POS_CENTER)
			ELSE
			END
		END SetSizePos;
		
		PROCEDURE ShowWindow (w: Window);
		BEGIN
			LinGtk.gtk_widget_show(w.wnd);
			IF w.fixed # NIL THEN LinGtk.gtk_widget_show(w.fixed) END;
			IF w.vBar # NIL THEN LinGtk.gtk_widget_show(w.vBar) END;
			IF w.hBar # NIL THEN LinGtk.gtk_widget_show(w.hBar) END;
			LinGtk.gtk_widget_show(w.da);
			
		(*			
			w.Restore(0, 0, cw, ch);
			w.Update;
		*)
			w.UpdateScrollbars(FALSE, TRUE);
		END ShowWindow;
		
	BEGIN
		WITH w: Window DO
			w.open := FALSE;
			NEW(p); p.Init(unit, FALSE);
			w.Init(p);
			w.wnd := SYSTEM.VAL(LinGtk.GtkWindow, LinGtk.gtk_window_new(LinGtk.GTK_WINDOW_TOPLEVEL));
			IF w.wnd = NIL THEN
(*				Log.String("Could not create window"); Log.Ln*)
			ELSE
				d.Open^(w, doc, flags, name, loc, fname, conv);
				w.next := winAnchor.next; winAnchor.next := w;	(* new top window *)
				w.da := LinGtk.gtk_drawing_area_new();
				AddScrollbars(w);
				ConnectSignals(w);
				c := w.doc.ThisController();
				c.SetFocus(w.doc.ThisView());
				w.dlg := IsDialog(w, c);
				SetSizePos(p, w);		
				p.SetDA(w.da); 		
				p.SetFW(w.fixed); 
				IF (loc # NIL) & (name = fname) THEN GenPathTitle(w, name) END;
				w.SetTitle(name);	
				ShowWindow(w);
				d.l := -1; d.t := -1; d.r := -1; d.b := -1;
				IF w.dlg THEN
					LinGtk.gtk_window_set_policy(w.wnd, 0, 0, 1) 
				ELSE
					LinGtk.gtk_window_set_policy(w.wnd, 1, 1, 1)
				END;
				w.SetSize(cw, ch)
			END
		END
	END Open;
	
	PROCEDURE UpdateInfo;
		VAR res: INTEGER; str: ARRAY 256 OF CHAR; sstr: ARRAY 256 OF SHORTCHAR; msgId: INTEGER;
	BEGIN
		IF (alloc # Kernel.Allocated()) OR (total # Kernel.Used()) THEN
			alloc := Kernel.Allocated(); total := Kernel.Used();
			str := allocStr$; AppendInt(str, alloc, useSeparators); Append(str, byteStr);
			sstr := SHORT(str$);
			LinGtk.gtk_statusbar_pop(infoBar, infoId);
			msgId := LinGtk.gtk_statusbar_push(infoBar, infoId, sstr);
		END
	END UpdateInfo;

	PROCEDURE Idle*;
		VAR w: Window; tick: Controllers.TickMsg; focus: BOOLEAN;
	BEGIN
		w := dir.Focus(FALSE);
		IF (w # NIL) & ~w.trapped THEN
			w.trapped := TRUE;
			IF w.frame # NIL THEN
(*				tick.tick := KERNEL32.GetTickCount();*)
				tick.tick := SHORT(Kernel.Time());
				w.ForwardCtrlMsg(tick)
			END;
			w.trapped := FALSE
		END;
		(*
		focus := ScrollModPressed(); TODO: Should check if control key is pressed
		*) focus := FALSE;
		w := dir.First(); 
		WHILE w # NIL DO
			IF ~w.trapped THEN
				w.trapped := TRUE;
				w.UpdateScrollbars(focus & (w = fWindow), FALSE);
				w.trapped := FALSE
			END;
			w := dir.Next(w)
		END;
		IF ~idleTraped THEN
			idleTraped := TRUE;
			(*
			IF USER32.GetAsyncKeyState(1) >= 0 THEN activating := FALSE END; Left mouse button down
			IF USER32.GetAsyncKeyState(2) >= 0 THEN activating := FALSE END; Right mouse button down
			*)
			UpdateInfo;
			idleTraped := FALSE
		END;
		Services.actionHook.Step
	END Idle;

	PROCEDURE ShowMain*;
	BEGIN
		LinGtk.gtk_widget_show_all(main);
		HostPorts.ResetColors(main)
(*
		Ports.dialogBackground := Ports.RGBColor(
			(menuBar.style.bg[LinGtk.GTK_STATE_NORMAL].red DIV 256) MOD 256,
			(menuBar.style.bg[LinGtk.GTK_STATE_NORMAL].green DIV 256) MOD 256, 
			(menuBar.style.bg[LinGtk.GTK_STATE_NORMAL].blue DIV 256) MOD 256);
*)
	END ShowMain;
	
	PROCEDURE RaiseMain*;
	BEGIN
		LinGdk.gdk_window_raise(main.window)
	END RaiseMain;
	
	PROCEDURE CreateMainWindows*;
		VAR msgId: INTEGER; hBox: LinGtk.GtkContainer;
	BEGIN
		main :=  SYSTEM.VAL(LinGtk.GtkWindow, LinGtk.gtk_window_new(LinGtk.GTK_WINDOW_TOPLEVEL));
		LinGtk.gtk_window_set_title(main, "BlackBox");
		LinGtk.gtk_widget_set_uposition(main, 0, 0);
		mainVBox := SYSTEM.VAL(LinGtk.GtkContainer, LinGtk.gtk_vbox_new(LinLibc.FALSE, 0));
		LinGtk.gtk_container_add(main, mainVBox);
		statusBar := LinGtk.gtk_statusbar_new();
		statusId := LinGtk.gtk_statusbar_get_context_id(statusBar, "BlackBoxStatus");
		infoBar := LinGtk.gtk_statusbar_new();
		infoId := LinGtk.gtk_statusbar_get_context_id(infoBar, "BlackBoxInfo");		

		hBox := SYSTEM.VAL(LinGtk.GtkContainer, LinGtk.gtk_hbox_new(LinLibc.TRUE, 0));
		LinGtk.gtk_container_add(hBox, statusBar);
		LinGtk.gtk_container_add(hBox, infoBar);
		LinGtk.gtk_container_add(mainVBox, hBox);
		
		Dialog.MapString(allocKey, allocStr);
		Dialog.MapString(totalKey, totalStr);
		Dialog.MapString(byteKey, byteStr);

	END CreateMainWindows;
	
	PROCEDURE SetMenu* (newMenuBar: LinGtk.GtkWidget);
	BEGIN
		IF menuBar # NIL THEN
			LinGtk.gtk_container_remove(mainVBox, menuBar);
		END;
		menuBar := newMenuBar;
		LinGtk.gtk_container_add(mainVBox, menuBar);
		LinGtk.gtk_box_reorder_child(mainVBox, menuBar, 0)
	END SetMenu;
	
	PROCEDURE SetStatusText* (IN str: ARRAY OF SHORTCHAR);
		VAR msgId: INTEGER;
	BEGIN
		LinGtk.gtk_statusbar_pop(statusBar, statusId);
		msgId := LinGtk.gtk_statusbar_push(statusBar, statusId, str);
	END SetStatusText;

	PROCEDURE Init;
	BEGIN
		scW := LinGdk.gdk_screen_width();
		scH := LinGdk.gdk_screen_height();
		unit := (Ports.mm * HostGnome.ScreenHeightMM()) DIV scH;
		NEW(winAnchor); winAnchor.next := NIL;	(* dummy header *)
		tWindow := NIL; fWindow := NIL; aWindow := NIL;
		NEW(dir); Windows.SetDir(dir)
	END Init;
	
BEGIN
	Init
END HostWindows.