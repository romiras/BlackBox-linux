MODULE HostPorts;

	(* TODO: 
		- OpenBuffer can be optimized to only open a buffer large enough for the clipingrectangle 
		- MarkRect needs to be looked at for some parameter values
		- ResetColors should find system values
		- Underline is not available on Linux and is thus implemented in Draw(S)Sting. This can be improved.
	*)
	
	IMPORT SYSTEM, LinLibc, LinGdk, LinGtk, Dialog, Ports, Fonts, HostFonts, Log;
	
	CONST
	
		(** buttons **)
		left* = 16; middle* = 17; right* = 18;
		shift* = 24; ctrl* = 25; opt* = 26; cmd* = 27; alt* = 28;
		focusPat* = 5;
		
		resizeHCursor* = 16; resizeVCursor* = 17; resizeLCursor* = 18; resizeRCursor* = 19; resizeCursor* = 20;
		busyCursor* = 21; stopCursor* = 22;
		moveCursor* = 23; copyCursor* = 24; linkCursor* = 25; pickCursor* = 26;
	
		extend = 1; modify = 2; (* same as Controllers.extend and Controllers.modify  !!! *)
	
	TYPE
	
		Port* = POINTER TO RECORD (Ports.Port)
			bl, bt, br, bb: INTEGER;	(* buffer rectangle *)
			w, h: INTEGER; (* size of port *)
			da-: LinGtk.GtkDrawingArea; (* drawing widget *)
			fixed-: LinGtk.GtkWidget; (* fixed container for absolute positioning *)
			map: LinGdk.GdkPixmap; (* off screen buffer *)
			gc: LinGdk.GdkGC; (* graphic context *)
		END;
		
		Rider* = POINTER TO RECORD (Ports.Rider)
			l-, t-, r-, b-: INTEGER;
			dx, dy: INTEGER;	(* scroll offset *)
			port-: Port; (* port for the rider *)
			map: LinGdk.GdkPixmap;	(* save bitmap *)
			gc: LinGdk.GdkGC;	(* save gc *)
			sl, st, sr, sb: INTEGER	(* save rect *)
		END;	
	
	VAR
		(* system colors *)
		textCol-, selBackground-, selTextCol-,
		dialogTextCol-, dialogShadowCol-, dialogLightCol-: Ports.Color;
		
		mx, my: INTEGER;	(* actual mouse coordinates *)
		mb: SET;	(* actual mouse buttons & modifiers *)	
		dim25Col, dim50Col, dim75Col: Ports.Color;
		
		cursors-: ARRAY 32 OF LinGdk.GdkCursor;
	
	
	(* Auxiliary procedures *)
		
	PROCEDURE UnsignedShortInt(i: INTEGER): SHORTINT;
	BEGIN
		ASSERT(i < 2 * MAX(SHORTINT) + 2, 20);
		IF i <= MAX(SHORTINT) THEN RETURN SHORT(i) END;
		RETURN SHORT(i -  (2 * MAX(SHORTINT) + 2))
	END UnsignedShortInt;
	
	PROCEDURE AllocateColor (bbColor: Ports.Color; OUT gdkColor: LinGdk.GdkColorDesc);
		VAR cm: LinGdk.GdkColormap; i: INTEGER;
	BEGIN
		cm := LinGdk.gdk_colormap_get_system(); (* TODO: keep in global variable? *)
		i := bbColor MOD 256; gdkColor.red := UnsignedShortInt(i * 257);
		i := (bbColor DIV 256) MOD 256; gdkColor.green := UnsignedShortInt(i * 257);
		i := (bbColor DIV (256 * 256)) MOD 256; gdkColor.blue := UnsignedShortInt(i * 257);
		IF LinGdk.gdk_colormap_alloc_color(cm, SYSTEM.ADR(gdkColor), LinGdk.TRUE, LinGdk.TRUE) # LinGdk.TRUE THEN
			Dialog.ShowMsg("gdk_colormap_alloc_color failed")
		END
	END AllocateColor;
	
	(* Port *)
	
	PROCEDURE (p: Port) OpenBuffer* (l, t, r, b: INTEGER);
		(*VAR rect: LinGdk.GdkRectangleDesc;*)
	BEGIN
		ASSERT(p.da # NIL, 20);
		IF l < 0 THEN l := 0 END;
		IF t < 0 THEN t := 0 END;
		IF r > p.w THEN r := p.w END;
		IF b > p.h THEN b := p.h END;
		IF (l < r) & (t < b) THEN
			p.bl := l; p.bt := t; p.br := r; p.bb := b;
			p.map := LinGdk.gdk_pixmap_new(p.da.window, p.w, p.h , -1);
			p.gc := LinGdk.gdk_gc_new(p.map)
		END
	END OpenBuffer;
		
	PROCEDURE (p: Port) CloseBuffer*;
	BEGIN
		IF p.map # LinLibc.NULL THEN 
			LinGdk.gdk_draw_pixmap(p.da.window, p.da.style.white_gc, p.map, p.bl, p.bt, p.bl, p.bt, p.br - p.bl, p.bb - p.bt );
			LinGdk.gdk_pixmap_unref(p.map);
			p.map := LinLibc.NULL;
			LinGdk.gdk_gc_unref(p.gc); 
			p.gc := NIL
		END
	END CloseBuffer;

	PROCEDURE (p: Port) SetSize* (w, h: INTEGER);
	BEGIN
		ASSERT(w >= 0, 20); ASSERT(h >= 0, 21);
		p.w := w; p.h := h
	END SetSize;
	
	PROCEDURE (p: Port) GetSize* (OUT w, h: INTEGER);
	BEGIN
		w := p.w; h := p.h
	END GetSize;
	
	PROCEDURE (p: Port) NewRider* (): Rider;
		VAR h: Rider;
	BEGIN
		NEW(h); h.port := p; RETURN h
	END NewRider;
	
	PROCEDURE (p: Port) SetDA* (da: LinGtk.GtkDrawingArea), NEW;
	BEGIN
		p.da := da
	END SetDA;
	
	PROCEDURE (p: Port) SetFW* (fixed: LinGtk.GtkWidget), NEW;
	BEGIN
		p.fixed := fixed
	END SetFW;
	
	
	(* Rider *)
	
	PROCEDURE (rd: Rider) DrawingBuf (VAR map: LinGdk.GdkDrawable; VAR gc: LinGdk.GdkGC), NEW;
		VAR r: LinGdk.GdkRectangleDesc;
	BEGIN
		ASSERT(rd.port # NIL, 20);
		IF (rd.port.map # LinLibc.NULL) & (rd.port.gc # NIL) THEN (* buffered drawing *)
			map := rd.port.map; gc := rd.port.gc; LinGdk.gdk_gc_ref(gc)
		ELSE (* unbuffered drawing *)
			map := rd.port.da.window; gc := LinGdk.gdk_gc_new(map)
		END;		
		r.x := SHORT(rd.l); r.y := SHORT(rd.t); r.width := SHORT(rd.r - rd.l + 1); r.height := SHORT(rd.b - rd.t + 1) ;
		LinGdk.gdk_gc_set_clip_rectangle(gc, SYSTEM.ADR(r));
		LinGdk.gdk_gc_set_clip_origin(gc, 0, 0)
	END DrawingBuf;
	
	PROCEDURE (rd: Rider) Base* (): Ports.Port;
	BEGIN
		RETURN rd.port
	END Base;

	PROCEDURE (rd: Rider) SetRect* (l, t, r, b: INTEGER);
	BEGIN
		ASSERT((l <= r) & (t <= b), 20);
		ASSERT(rd.port # NIL, 21);
		rd.l := l; rd.t := t; rd.r := r; rd.b := b
	END SetRect;

	PROCEDURE (rd: Rider) GetRect* (OUT l, t, r, b: INTEGER);
	BEGIN
		l := rd.l; t := rd.t; r := rd.r; b := rd.b
	END GetRect;
	
	PROCEDURE (rd: Rider) DrawLine* (x0, y0, x1, y1, s: INTEGER; col: Ports.Color);
		VAR gdkColor: LinGdk.GdkColorDesc; map: LinGdk.GdkDrawable; gc: LinGdk.GdkGC;
	BEGIN  
		ASSERT(s >= 0, 21); ASSERT(rd.port # NIL, 100);
		rd.DrawingBuf(map, gc);
		AllocateColor(col, gdkColor);
		LinGdk.gdk_gc_set_foreground(gc, SYSTEM.ADR(gdkColor)); 
		LinGdk.gdk_gc_set_line_attributes(gc, s, LinGdk.GDK_LINE_SOLID, LinGdk.GDK_CAP_ROUND, LinGdk.GDK_JOIN_ROUND);
		LinGdk.gdk_draw_line(map, gc, x0, y0, x1, y1);
		LinGdk.gdk_gc_unref(gc)
	END DrawLine;
	
	PROCEDURE (rd: Rider) DrawOval* (l, t, r, b, s: INTEGER; col: Ports.Color);
		VAR gdkColor: LinGdk.GdkColorDesc;map: LinGdk.GdkDrawable; gc: LinGdk.GdkGC; h: INTEGER;
	BEGIN
		ASSERT(rd.port # NIL, 20); 
		rd.DrawingBuf(map, gc);
		AllocateColor(col, gdkColor);
		LinGdk.gdk_gc_set_foreground(gc, SYSTEM.ADR(gdkColor)); 
		LinGdk.gdk_gc_set_background(gc, SYSTEM.ADR(gdkColor)); 
		IF s = Ports.fill THEN
			LinGdk.gdk_gc_set_line_attributes(gc, 1, LinGdk.GDK_LINE_SOLID, LinGdk.GDK_CAP_ROUND, LinGdk.GDK_JOIN_ROUND);
			LinGdk.gdk_draw_arc(map, gc, LinGdk.TRUE,  l, t, r - l, b - t, 0, 360 * 64)
		ELSE
			IF s = 0 THEN s := 1 END;
			h := s DIV 2; INC(l, h); INC(t, h); h := (s-1) DIV 2; DEC(r, h); DEC(b, h);
			LinGdk.gdk_gc_set_line_attributes(gc, s, LinGdk.GDK_LINE_SOLID, LinGdk.GDK_CAP_ROUND, LinGdk.GDK_JOIN_ROUND);
			LinGdk.gdk_draw_arc(map, gc, LinGdk.FALSE,  l, t, r - l, b - t, 0, 360  * 64)
		END;
		LinGdk.gdk_gc_unref(gc)
	END DrawOval;
	
	PROCEDURE (rd: Rider) DrawPath* (IN pts: ARRAY OF Ports.Point; n, s: INTEGER; col: Ports.Color; path: INTEGER);
		TYPE
			PAP = POINTER TO ARRAY [1] OF LinGdk.GdkPointDesc;

		VAR
			gdkColor: LinGdk.GdkColorDesc;
			i, j, k: INTEGER; 
			pap: PAP; poly: ARRAY 256 OF LinGdk.GdkPointDesc;
			polyPtr: POINTER TO ARRAY OF Ports.Point; polyLen: INTEGER;
			sp: ARRAY [untagged] 256 OF LinGdk.GdkPointDesc;
			map: LinGdk.GdkDrawable; gc: LinGdk.GdkGC;

		PROCEDURE Bezier(x0, y0, xd0, yd0, x1, y1, xd1, yd1: INTEGER);
			VAR x, y, xd, yd, i: INTEGER;
		BEGIN
			IF ABS(xd0 - xd1) + ABS(yd0 - yd1) < 8 THEN
				IF k > polyLen - 2 THEN
					NEW(polyPtr, polyLen * 2);
					i := 0; WHILE i < polyLen DO polyPtr[i] := SYSTEM.VAL(Ports.Point, pap[i]); INC(i) END;
					polyLen := polyLen * 2; pap := SYSTEM.VAL(PAP, SYSTEM.ADR(polyPtr^))
				END;
				pap[k].x := SHORT(x0); pap[k].y := SHORT(y0); INC(k)
			ELSE
				x := ((xd0 - xd1) DIV 4 + x0 + x1 + 1) DIV 2;
				y := ((yd0 - yd1) DIV 4 + y0 + y1 + 1) DIV 2;
				xd := ((x1 - x0) * 3 - (xd0 + xd1) DIV 2 + 2) DIV 4;
				yd := ((y1 - y0) * 3 - (yd0 + yd1) DIV 2 + 2) DIV 4;
				Bezier(x0, y0, xd0 DIV 2, yd0 DIV 2, x, y, xd, yd);
				Bezier(x, y, xd, yd, x1, y1, xd1 DIV 2, yd1 DIV 2)
			END
		END Bezier;
	
	BEGIN
		ASSERT(rd.port # NIL, 20); 
		rd.DrawingBuf(map, gc);
		AllocateColor(col, gdkColor);
		LinGdk.gdk_gc_set_foreground(gc, SYSTEM.ADR(gdkColor)); 
(*		pap := SYSTEM.VAL(PAP, SYSTEM.ADR(pts));*)
		FOR i := 0 TO n - 1 DO
			sp[i].x := SHORT(pts[i].x); sp[i].y := SHORT(pts[i].y)   (* The GdkPoint structure is only a 16 bit integer....*)
		END;
		pap := SYSTEM.VAL(PAP, SYSTEM.ADR(sp));
		
		ASSERT(n >= 0, 20); ASSERT(n <= LEN(pts), 21);
		ASSERT(s >= Ports.fill, 23);
		IF s < 0 THEN
			LinGdk.gdk_gc_set_line_attributes(gc, 1, LinGdk.GDK_LINE_SOLID, LinGdk.GDK_CAP_ROUND, LinGdk.GDK_JOIN_ROUND);
			LinGdk.gdk_gc_set_background(gc, SYSTEM.ADR(gdkColor)); 
			IF path = Ports.closedPoly THEN
				ASSERT(n >= 2, 20);
				LinGdk.gdk_draw_polygon(map, gc, LinGdk.TRUE, SYSTEM.VAL(LinGdk.GdkPoint, pap), n)
			ELSE
				ASSERT(n >= 3, 20);
				ASSERT(path = Ports.closedBezier, 22);
				ASSERT(n MOD 3 = 0, 24);
				pap := SYSTEM.VAL(PAP, SYSTEM.ADR(poly)); polyLen := LEN(poly);
				i := 0; k := 0;
				WHILE i < n DO
					j := i+3;
					IF j = n THEN j := 0 END;
					Bezier(pts[i].x, pts[i].y, (pts[i+1].x - pts[i].x) * 3, (pts[i+1].y - pts[i].y) * 3,
							pts[j].x, pts[j].y, (pts[j].x - pts[i+2].x) * 3, (pts[j].y - pts[i+2].y) * 3);
					INC(i, 3)
				END;
				LinGdk.gdk_draw_polygon(map, gc, LinGdk.TRUE, SYSTEM.VAL(LinGdk.GdkPoint, pap), k);
			END;
		ELSE
			LinGdk.gdk_gc_set_line_attributes(gc, s, LinGdk.GDK_LINE_SOLID, LinGdk.GDK_CAP_ROUND, LinGdk.GDK_JOIN_ROUND);
			IF s = 0 THEN s := 1 END;
			IF path = Ports.closedPoly THEN
				ASSERT(n >= 2, 20);
				LinGdk.gdk_draw_polygon(map, gc, LinGdk.FALSE, SYSTEM.VAL(LinGdk.GdkPoint, pap), n);
			ELSIF path = Ports.openPoly THEN
				ASSERT(n >= 2, 20);
				LinGdk.gdk_draw_lines(map, gc, SYSTEM.VAL(LinGdk.GdkPoint, pap), n);
			ELSE
				IF path = Ports.closedBezier THEN
					ASSERT(n >= 3, 20);
					ASSERT(n MOD 3 = 0, 24)
				ELSE
					ASSERT(n >= 4, 20);
					ASSERT(path = Ports.openBezier, 25);
					ASSERT(n MOD 3 = 1, 24)
				END;
				pap := SYSTEM.VAL(PAP, SYSTEM.ADR(poly)); polyLen := LEN(poly);
				i := 0;
				WHILE i < n-2 DO
					k := 0; j := i+3;
					IF j = n THEN j := 0 END;
					Bezier(pts[i].x, pts[i].y, (pts[i+1].x - pts[i].x) * 3, (pts[i+1].y - pts[i].y) * 3,
							pts[j].x, pts[j].y, (pts[j].x - pts[i+2].x) * 3, (pts[j].y - pts[i+2].y) * 3);
					pap[k].x := SHORT(pts[j].x); pap[k].y := SHORT(pts[j].y); INC(k);
					LinGdk.gdk_draw_lines(map, gc, SYSTEM.VAL(LinGdk.GdkPoint, pap), k);
					INC(i, 3)
				END
			END;
		END;
		LinGdk.gdk_gc_unref(gc)
	END DrawPath;
	
	PROCEDURE (rd: Rider) DrawRect* (l, t, r, b, s: INTEGER; col: Ports.Color);
		VAR gdkColor: LinGdk.GdkColorDesc; map: LinGdk.GdkDrawable; gc: LinGdk.GdkGC; h: INTEGER;
	BEGIN
		ASSERT(rd.port # NIL, 20); 
		rd.DrawingBuf(map, gc);
		AllocateColor(col, gdkColor);
		LinGdk.gdk_gc_set_foreground(gc, SYSTEM.ADR(gdkColor)); 
		LinGdk.gdk_gc_set_background(gc, SYSTEM.ADR(gdkColor)); 
		IF s = Ports.fill THEN
			LinGdk.gdk_gc_set_line_attributes(gc, 1, LinGdk.GDK_LINE_SOLID, LinGdk.GDK_CAP_ROUND, 
														LinGdk.GDK_JOIN_MITER);
			LinGdk.gdk_draw_rectangle(map, gc, LinGdk.TRUE,  l, t, r - l, b - t)
		ELSE
			h := s DIV 2; INC(l, h); INC(t, h); h := (s-1) DIV 2; DEC(r, h); DEC(b, h);
			LinGdk.gdk_gc_set_line_attributes(gc, s, LinGdk.GDK_LINE_SOLID, LinGdk.GDK_CAP_ROUND, 
														LinGdk.GDK_JOIN_MITER);
			LinGdk.gdk_draw_rectangle(map, gc, LinGdk.FALSE,  l, t, r - l, b - t)
		END;
		LinGdk.gdk_gc_unref(gc)
	END DrawRect;
	
	PROCEDURE (rd: Rider) DrawSString* (x, y: INTEGER; col: Ports.Color; IN s: ARRAY OF SHORTCHAR; font: Fonts.Font);
		VAR gdkColor: LinGdk.GdkColorDesc; map: LinGdk.GdkDrawable; gc: LinGdk.GdkGC; width: INTEGER;
	BEGIN
		ASSERT(rd.port # NIL, 20); 
		rd.DrawingBuf(map, gc);
		AllocateColor(col, gdkColor);
		LinGdk.gdk_gc_set_foreground(gc, SYSTEM.ADR(gdkColor)); 
		WITH font: HostFonts.Font DO
			LinGdk.gdk_draw_string(map, font.id, gc, x, y, s)
		END;
		IF Fonts.underline IN font.style THEN
			LinGdk.gdk_gc_set_foreground(gc, SYSTEM.ADR(gdkColor));
			IF font.weight > Fonts.normal THEN width := 2 ELSE width := 1 END;
			LinGdk.gdk_gc_set_line_attributes(gc, 1, 
				LinGdk.GDK_LINE_SOLID, LinGdk.GDK_CAP_ROUND, LinGdk.GDK_JOIN_ROUND);
			LinGdk.gdk_draw_line(map, gc, x, y, x + font.SStringWidth(s) DIV rd.port.unit , y);
		END;
		LinGdk.gdk_gc_unref(gc)
	END DrawSString;
	
	PROCEDURE (rd: Rider) DrawString* (x, y: INTEGER; col: Ports.Color; IN s: ARRAY OF CHAR; font: Fonts.Font);
		VAR gdkColor: LinGdk.GdkColorDesc; map: LinGdk.GdkDrawable; gc: LinGdk.GdkGC; width: INTEGER;
			is: ARRAY [untagged] 1024 OF INTEGER; i: INTEGER;
	BEGIN
		ASSERT(rd.port # NIL, 20); 
		rd.DrawingBuf(map, gc);
		AllocateColor(col, gdkColor);
		LinGdk.gdk_gc_set_foreground(gc, SYSTEM.ADR(gdkColor));
		FOR i := 0 TO LEN(s$) DO is[i] := ORD(s[i]) END;
		WITH font: HostFonts.Font DO
			LinGdk.gdk_draw_text_wc(map, font.id, gc, x, y, is, LEN(s$))
		END;
		IF Fonts.underline IN font.style THEN
			LinGdk.gdk_gc_set_foreground(gc, SYSTEM.ADR(gdkColor));
			IF font.weight > Fonts.normal THEN width := 2 ELSE width := 1 END;
			LinGdk.gdk_gc_set_line_attributes(gc, 1, 
				LinGdk.GDK_LINE_SOLID, LinGdk.GDK_CAP_ROUND, LinGdk.GDK_JOIN_ROUND);
			LinGdk.gdk_draw_line(map, gc, x, y, x + font.StringWidth(s) DIV rd.port.unit , y);
		END;
		LinGdk.gdk_gc_unref(gc)
	END DrawString;
	
	PROCEDURE SetMouseState* (x, y: INTEGER; but: SET; isDown: BOOLEAN);
	BEGIN
		mx := x; my := y; mb := but
	END SetMouseState;
	
	PROCEDURE (rd: Rider) Input* (OUT x, y: INTEGER; OUT modifiers: SET; OUT isDown: BOOLEAN);
		VAR 
			event: LinGdk.GdkEvent; motion: LinGdk.GdkEventMotion; button: LinGdk.GdkEventButton; key: LinGdk.GdkEventKey;
			state: SET; gotState: BOOLEAN;
	BEGIN
		IF LinGdk.gdk_events_pending() # LinLibc.FALSE THEN
			gotState := FALSE;
			event := LinGdk.gdk_event_get();
			IF event # NIL THEN 
				IF event.type = LinGdk.GDK_MOTION_NOTIFY THEN
					motion := SYSTEM.VAL(LinGdk.GdkEventMotion, event);					
					LinGdk.gdk_window_get_position(rd.port.fixed.parent.window, mx, my);
					mx := SHORT(ENTIER(motion.x_root)) - mx;
					my := SHORT(ENTIER(motion.y_root)) - my;
					state := motion.state; gotState := TRUE;
				ELSIF event.type IN {LinGdk.GDK_BUTTON_PRESS, LinGdk.GDK_BUTTON_RELEASE} THEN
					button := SYSTEM.VAL(LinGdk.GdkEventButton, event);
					LinGdk.gdk_window_get_position(rd.port.fixed.parent.window, mx, my);
					mx := SHORT(ENTIER(button.x_root)) - mx;
					my := SHORT(ENTIER(button.y_root)) - my;
					IF event.type = LinGdk.GDK_BUTTON_PRESS THEN 
						IF button.button = 1 THEN INCL(mb, left) END;
						IF button.button = 2 THEN INCL(mb, middle) END;
						IF button.button = 3 THEN INCL(mb, right) END
					ELSE
						IF button.button = 1 THEN EXCL(mb, left) END;
						IF button.button = 2 THEN EXCL(mb, middle) END;
						IF button.button = 3 THEN EXCL(mb, right) END
					END;
					state := button.state; gotState := TRUE;
				ELSIF event.type IN {LinGdk.GDK_KEY_PRESS, LinGdk.GDK_KEY_RELEASE} THEN
					key := SYSTEM.VAL(LinGdk.GdkEventKey, event);
					IF (key.keyval = LinGdk.GDK_Shift_L) OR  (key.keyval = LinGdk.GDK_Shift_R) THEN
						IF key.type = LinGdk.GDK_KEY_PRESS THEN mb := mb + {shift, extend} ELSE  mb := mb - {shift, extend} END
					END;
					IF (key.keyval = LinGdk.GDK_Control_L) OR (key.keyval = LinGdk.GDK_Control_R) THEN
						IF key.type = LinGdk.GDK_KEY_PRESS THEN mb := mb + {ctrl, modify} ELSE  mb := mb - {ctrl, modify} END
					END;
					IF (key.keyval = LinGdk.GDK_Alt_L) OR (key.keyval = LinGdk.GDK_Alt_R) THEN
						IF key.type = LinGdk.GDK_KEY_PRESS THEN INCL(mb, alt); ELSE  EXCL(mb, alt) END
					END;
				ELSE
					LinGtk.gtk_main_do_event(event);
				END;
				IF gotState THEN
					IF LinGdk.GDK_SHIFT_MASK IN state THEN mb := mb + {shift, extend} ELSE  mb := mb - {shift, extend} END;
					IF LinGdk.GDK_CONTROL_MASK IN state THEN mb := mb + {ctrl, modify} ELSE  mb := mb - {ctrl, modify} END;
					IF LinGdk.GDK_MOD1_MASK IN state THEN INCL(mb, alt) ELSE EXCL(mb, alt) END;
				END;
				LinGdk.gdk_event_free(event)
			END
		END; 
		x := mx; y := my; modifiers := mb; isDown := mb * {left, middle, right} # {}
	END Input;
	
	PROCEDURE (rd: Rider) MarkRect* (l, t, r, b, s, mode: INTEGER; show: BOOLEAN);
		VAR 
			gdkColor: LinGdk.GdkColorDesc; gc: LinGdk.GdkGC;
			vals: LinGdk.GdkGCValuesDesc; map: LinGdk.GdkDrawable;
	BEGIN
		IF rd.port.map # LinLibc.NULL THEN
			map := rd.port.map; (* buffered drawing *)
		ELSE
			map := rd.port.da.window; (* unbuffered drawing *)
		END;		
		IF (mode = Ports.invert) OR (mode = Ports.hilite) THEN
			vals.foreground := rd.port.da.style.white;
		ELSIF mode = Ports.dim25 THEN AllocateColor(dim25Col, gdkColor); vals.foreground := gdkColor
		ELSIF mode = Ports.dim50 THEN AllocateColor(dim50Col, gdkColor); vals.foreground := gdkColor
		ELSIF mode = Ports.dim75 THEN AllocateColor(dim75Col, gdkColor); vals.foreground := gdkColor
		ELSE (* mode = focusPat *)
			(* TODO: which color should be used here? *)
			AllocateColor(Ports.red, gdkColor);
			vals.foreground := gdkColor;
		END;
		vals.function := LinGdk.GDK_XOR;
		vals.line_style := LinGdk.GDK_LINE_SOLID;
		vals.line_width := 1;
		vals.subwindow_mode := LinGdk.GDK_INCLUDE_INFERIORS;
		gc := LinGdk.gdk_gc_new_with_values(map, SYSTEM.ADR(vals), 
			LinGdk.GDK_GC_FOREGROUND + LinGdk.GDK_GC_LINE_WIDTH + 
			LinGdk.GDK_GC_LINE_STYLE + LinGdk.GDK_GC_SUBWINDOW + LinGdk.GDK_GC_FUNCTION);
		IF s = 0 THEN s := 1 END;
		IF (s < 0) OR (r-l < 2*s) OR (b-t < 2*s) THEN
			LinGdk.gdk_gc_set_line_attributes(gc, 1, LinGdk.GDK_LINE_SOLID, LinGdk.GDK_CAP_ROUND, 
														LinGdk.GDK_JOIN_MITER);
			LinGdk.gdk_draw_rectangle(map, gc, LinGdk.TRUE,  l, t, r - l, b - t)
		ELSE
			LinGdk.gdk_gc_set_line_attributes(gc, 1, LinGdk.GDK_LINE_SOLID, LinGdk.GDK_CAP_ROUND, 
														LinGdk.GDK_JOIN_MITER);
			LinGdk.gdk_draw_rectangle(map, gc, LinGdk.FALSE,  l, t, s, b-t); DEC(r, s);
			LinGdk.gdk_draw_rectangle(map, gc, LinGdk.FALSE,  r, t, s, b-t); INC(l, s);
			LinGdk.gdk_draw_rectangle(map, gc, LinGdk.FALSE,  l, t, r-l, s); DEC(b, s);
			LinGdk.gdk_draw_rectangle(map, gc, LinGdk.FALSE,  l, b, r-l, s)
			(*
			res := GDI32.PatBlt(dc, l, t, s, b-t, xor); DEC(r, s);
			res := GDI32.PatBlt(dc, r, t, s, b-t, xor); INC(l, s);
			res := GDI32.PatBlt(dc, l, t, r-l, s, xor); DEC(b, s);
			res := GDI32.PatBlt(dc, l, b, r-l, s, xor);*)
		END;
		LinGdk.gdk_gc_unref(gc);
	END MarkRect;

	PROCEDURE (rd: Rider) InitPort* (port: Port), NEW;
	BEGIN
		ASSERT(rd.port = NIL, 20); ASSERT(port # NIL, 21); ASSERT(port.da # NIL, 22);
		rd.port := port; rd.dx := 0; rd.dy := 0
	END InitPort;
	
	PROCEDURE (rd: Rider) Move* (dx, dy: INTEGER);
	BEGIN
		INC(rd.dx, dx); INC(rd.dy, dy)
	END Move;

	PROCEDURE (rd: Rider) SCharIndex* (x, pos: INTEGER; IN s: ARRAY OF SHORTCHAR; font: Fonts.Font): INTEGER;
		VAR d, u, i, n, a, b, c, w: INTEGER; df: HostFonts.DevFont; ch: SHORTCHAR;
	BEGIN
		ASSERT(rd.port # NIL, 100);
		WITH font: HostFonts.Font DO
			u := rd.port.unit; df := font.dev;
			WHILE (df # NIL) & (df.unit # u) DO df := df.next END;
			IF df = NIL THEN HostFonts.InsertDevFont(font, df, u) END;
			a := 0; b := 0; n := LEN(s) - 1; i := 0; ch := s[0];
			WHILE (i < n) & (ch # 0X) DO
				INC(a, df.wtab[ORD(ch)]);
				INC(b, font.wtab[ORD(ch)]);
				INC(i); ch := s[i]
			END;
			n := i; c := b DIV u - a; i := 0; w := 0; a := 0;
			INC(x, font.ftab[ORD(s[0])] DIV u);
			d := df.wtab[ORD(s[0])];
			WHILE (i < n) & (pos > x + d DIV 2) DO
				INC(w, c); b := w DIV n; INC(d, b - a); a := b;
				INC(i); INC(x, d); d := df.wtab[ORD(s[i])]
			END
		END;
		RETURN i
	END SCharIndex;

	PROCEDURE (rd: Rider) SCharPos* (x, index: INTEGER; IN s: ARRAY OF SHORTCHAR; font: Fonts.Font): INTEGER;
		VAR i, u, n, a, b, c, w: INTEGER; df: HostFonts.DevFont; ch: SHORTCHAR;
	BEGIN
		ASSERT(rd.port # NIL, 100); ASSERT(index <= LEN(s), 101);
		WITH font: HostFonts.Font DO
			u := rd.port.unit; df := font.dev;
			WHILE (df # NIL) & (df.unit # u) DO df := df.next END;
			IF df = NIL THEN HostFonts.InsertDevFont(font, df, u) END;
			a := 0; b := 0; n := LEN(s) - 1; i := 0; ch := s[0];
			WHILE (i < n) & (ch # 0X) DO
				INC(a, df.wtab[ORD(ch)]);
				INC(b, font.wtab[ORD(ch)]);
				INC(i); ch := s[i]
			END;
			n := i; c := b DIV u - a; i := 0; w := 0; a := 0;
			INC(x, font.ftab[ORD(s[0])] DIV u);
			WHILE (i < n) & (i < index) DO
				INC(w, c); b := w DIV n; INC(x, b - a); a := b;
				INC(x, df.wtab[ORD(s[i])]); INC(i)
			END
		END;
		RETURN x
	END SCharPos;

	PROCEDURE (rd: Rider) CharIndex* (x, pos: INTEGER; IN s: ARRAY OF CHAR; font: Fonts.Font): INTEGER;
		VAR d, u, i, a, b, c, w, n: INTEGER;  is: ARRAY 1024 OF INTEGER; df: HostFonts.DevFont;
	BEGIN
		IF ~HostFonts.isUnicode THEN
			FOR i := 0 TO LEN(s$) DO is[i] := ORD(s[i]) END;
			RETURN rd.SCharIndex(x, pos, SHORT(LinGdk.gdk_wcstombs(is)$), font)
		END;
		ASSERT(rd.port # NIL, 100);
		WITH font: HostFonts.Font DO
			u := rd.port.unit; df := font.dev;
			WHILE (df # NIL) & (df.unit # u) DO df := df.next END;
			a := 0; b := 0; n := LEN(s) - 1; i := 0;
			WHILE (i < n) & (s[i] # 0X) DO INC(b, font.wTab(s[i])); INC(i) END;
			INC(x, font.fTab(s[0]) DIV u);
			IF df = NIL THEN HostFonts.InsertDevFont(font, df, u)
			END;
			n := i; i := 0;
			WHILE i < n DO
				c := LinGdk.gdk_char_width_wc(df.id, ORD(s[i]));
				INC(a, c); INC(i)
			END;
			d := LinGdk.gdk_char_width_wc(df.id, ORD(s[0]));
			WHILE (i < n) & (pos > x + d DIV 2) DO
				INC(w, c); b := w DIV n; INC(d, b - a); a := b;
				INC(i); INC(x, d);
				d := LinGdk.gdk_char_width_wc(df.id, ORD(s[i]));
			END
		END;
		RETURN i
	END CharIndex;
	
	PROCEDURE (rd: Rider) CharPos* (x, index: INTEGER; IN s: ARRAY OF CHAR; font: Fonts.Font): INTEGER;
		VAR i, u, a, b, c, w, d, n: INTEGER; is: ARRAY 1024 OF INTEGER; df: HostFonts.DevFont;
	BEGIN
		IF ~HostFonts.isUnicode THEN
			FOR i := 0 TO LEN(s$) DO is[i] := ORD(s[i]) END;
			RETURN rd.SCharPos(x, index,SHORT(LinGdk.gdk_wcstombs(is)$), font)
		END;
		ASSERT(rd.port # NIL, 100); ASSERT(index <= LEN(s), 101);
		WITH font: HostFonts.Font DO
			u := rd.port.unit; df := font.dev;
			WHILE (df # NIL) & (df.unit # u) DO df := df.next END;
			a := 0; b := 0; n := LEN(s) - 1; i := 0;
			WHILE (i < n) & (s[i] # 0X) DO INC(b, font.wTab(s[i])); INC(i) END;
			INC(x, font.fTab(s[0]) DIV u);
			IF df = NIL THEN HostFonts.InsertDevFont(font, df, u)
			END;
			n := i; i := 0;
			WHILE i < n DO
				c := LinGdk.gdk_char_width_wc(df.id, ORD(s[i]));
				INC(a, c); INC(i)
			END;
			c := b DIV u - a; i := 0; w := 0; a := 0;
			WHILE (i < n) & (i < index) DO
				INC(w, c); b := w DIV n; INC(x, b - a); a := b;
				d := LinGdk.gdk_char_width_wc(df.id, ORD(s[i]));
				INC(x, d); INC(i)
			END
		END;
		RETURN x
	END CharPos;
	
	PROCEDURE (rd: Rider) SaveRect* (l, t, r, b: INTEGER; VAR res: INTEGER);
		VAR p: Port;
	BEGIN
		res := 1; p := rd.port;
		IF l < 0 THEN l := 0 END;
		IF t < 0 THEN t := 0 END;
		IF r > p.w THEN r := p.w END;
		IF b > p.h THEN b := p.h END;
		IF (l < r) & (t < b) THEN
			rd.sl := l; rd.st := t; rd.sr := r; rd.sb := b;
			rd.gc := LinGdk.gdk_gc_new(p.da.window);
			IF rd.gc # NIL THEN
				rd.map := LinGdk.gdk_pixmap_new(p.da.window, r - l, b - t, -1);
				IF rd.map # 0 THEN
					LinGdk.gdk_gc_set_exposures(rd.gc, LinGdk.TRUE);
					LinGdk.gdk_draw_pixmap(rd.map, rd.gc, p.da.window, l, t, 0, 0, r - l, b- t);
					res := 0
				ELSE
					LinGdk.gdk_gc_unref(rd.gc); 
					rd.gc := NIL
				END
			END
		END
	END SaveRect;
	
	PROCEDURE (rd: Rider) RestoreRect* (l, t, r, b: INTEGER; dispose: BOOLEAN);
	BEGIN
		IF rd.gc # NIL THEN
			IF l < rd.sl THEN l := rd.sl END;
			IF t < rd.st THEN t := rd.st END;
			IF r > rd.sr THEN r := rd.sr END;
			IF b > rd.sb THEN b := rd.sb END;
			LinGdk.gdk_draw_pixmap(rd.port.da.window, rd.port.da.style.white_gc, rd.map, l - rd.sl, t - rd.st, l, t, r - l, b - t);
			IF dispose THEN
				LinGdk.gdk_pixmap_unref(rd.map);
				rd.map := LinLibc.NULL;
				LinGdk.gdk_gc_unref(rd.gc); 
				rd.gc := NIL
			END
		END
	END RestoreRect;
		
	PROCEDURE (rd: Rider) Scroll* (dx, dy: INTEGER);
		VAR width, height, x, y, sx, sy: INTEGER; gc: LinGdk.GdkGC; 
	BEGIN
		ASSERT(rd.port # NIL, 100);
		IF dx < 0 THEN
			sx := rd.l - dx; x := rd.l; width := rd.r - rd.l + dx
		ELSE
			sx := rd.l; x := rd.l + dx; width := rd.r - rd.l - dx
		END;
		IF dy < 0 THEN
			sy := rd.t - dy; y := rd.t; height := rd.b - rd.t + dy
		ELSE
			sy := rd.t; y := rd.t + dy; height := rd.b - rd.t - dy
		END;
		gc := LinGdk.gdk_gc_new(rd.port.da.window);
		LinGdk.gdk_gc_set_exposures(gc, LinGdk.TRUE);
		INC(width); INC(height);
		LinGdk.gdk_window_copy_area(rd.port.da.window, gc, x, y,
												rd.port.da.window, sx, sy, width, height );
		LinGdk.gdk_gc_unref(gc);
		
		(* Invalidate the new area *)

		IF dy < 0 THEN
			LinGtk.gtk_widget_queue_draw_area(rd.port.da, rd.l, rd.b + dy, rd.r - rd.l + 1,  -dy + 1)
		ELSIF dy > 0 THEN
			LinGtk.gtk_widget_queue_draw_area(rd.port.da, rd.l, rd.t, rd.r - rd.l + 1,  dy + 1)
		END; 
		IF dx < 0 THEN
			LinGtk.gtk_widget_queue_draw_area(rd.port.da, rd.r + dx, rd.t, -dx + 1, rd.b - rd.t + 1)
		ELSIF dx > 0 THEN
			LinGtk.gtk_widget_queue_draw_area(rd.port.da, rd.l, rd.t, dx + 1,  rd.b - rd.t + 1)
		END; 

		(* pattern origin correction *)
		INC(rd.dx, dx); INC(rd.dy, dy)
	END Scroll;
	
	PROCEDURE (rd: Rider) SetCursor* (cursor: INTEGER);
	BEGIN
		LinGdk.gdk_window_set_cursor(rd.port.da.window, cursors[cursor]);
	END SetCursor;

	PROCEDURE ResetColors* (basewidget: LinGtk.GtkWidget);
	BEGIN
		ASSERT(basewidget # NIL, 20);
	(*
		TODO: Use the actual system colors 
		Ports.background := USER32.GetSysColor(5);
		textCol := USER32.GetSysColor(8);
		selBackground := USER32.GetSysColor(13);
		selTextCol := USER32.GetSysColor(14);
		Ports.dialogBackground := USER32.GetSysColor(15);
		dialogTextCol := USER32.GetSysColor(18);
		dialogShadowCol := USER32.GetSysColor(16);
		dialogLightCol := USER32.GetSysColor(20);
		*)
		Ports.background := Ports.white;
		textCol := Ports.black;
		selBackground := Ports.black;
		selTextCol := Ports.white;
		
(*		Ports.dialogBackground := Ports.grey25;*)

		Ports.dialogBackground := Ports.RGBColor(
			(basewidget.style.bg[LinGtk.GTK_STATE_NORMAL].red DIV 256) MOD 256,
			(basewidget.style.bg[LinGtk.GTK_STATE_NORMAL].green DIV 256) MOD 256, 
			(basewidget.style.bg[LinGtk.GTK_STATE_NORMAL].blue DIV 256) MOD 256);
			
		dialogTextCol := Ports.black;
		dialogShadowCol := Ports.grey75;
		dialogLightCol := Ports.grey50;
		dim25Col := Ports.white - Ports.grey75;
		dim50Col := Ports.white - Ports.grey75; (* TODO: On Windows this is a dashed line... *)
		dim75Col := Ports.white - Ports.grey25;
	END ResetColors;
	
	PROCEDURE Init;
		VAR i: INTEGER; wnd: LinGtk.GtkWidget;
	BEGIN
		wnd := LinGtk.gtk_window_new(LinGtk.GTK_WINDOW_TOPLEVEL);
		LinGtk.gtk_widget_ref(wnd);
		ResetColors(wnd);
		LinGtk.gtk_widget_unref(wnd);
		cursors[Ports.arrowCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_LEFT_PTR);
		cursors[Ports.textCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_XTERM);
		cursors[Ports.graphicsCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_CROSSHAIR);
		cursors[Ports.bitmapCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_CROSSHAIR);
		cursors[Ports.tableCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_CROSS);
		cursors[Ports.refCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_HAND2);
		cursors[resizeHCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_SB_H_DOUBLE_ARROW);
		cursors[resizeVCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_SB_V_DOUBLE_ARROW);
		cursors[resizeLCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_TOP_LEFT_CORNER);
		cursors[resizeRCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_TOP_RIGHT_CORNER);
		cursors[resizeCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_FLEUR);
		cursors[busyCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_WATCH);
		cursors[stopCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_CIRCLE);
		(* TODO: On windows these are bitmaps stored as resources... *)
		cursors[moveCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_SAILBOAT);
		cursors[copyCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_SAILBOAT);
		cursors[linkCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_SAILBOAT);
		cursors[pickCursor] := LinGdk.gdk_cursor_new(LinGdk.GDK_SAILBOAT);
		i := 0;
		WHILE i < LEN(cursors) DO
			IF cursors[i] = NIL THEN cursors[i] := cursors[Ports.arrowCursor] END;
			INC(i)
		END
	END Init;
	
BEGIN
	Init
END HostPorts.
