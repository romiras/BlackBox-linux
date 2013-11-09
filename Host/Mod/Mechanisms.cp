MODULE HostMechanisms;
	
	(*
		TODO:
			Add drag'n drop support
	*)
	
	IMPORT Services, Ports, Properties, Controllers, Views, Containers, Documents, Mechanisms, HostPorts;
	
	CONST
		handleSize = Ports.point * 11 DIV 2;
		clipMiddleHandle = handleSize DIV 2 + 2 * Ports.point;
		infiniteMiddleHandle = 48 * Ports.point;
		targetBorderSize = 2;

		outside = 0; left = 1; top = 2; right = 3; bottom = 4;

		pick = -1; escape = -2;
		both = 31; iclick = 30;
		
		fixed = 31;	(* controller option *)
	
	TYPE
		Hook = POINTER TO RECORD (Mechanisms.Hook) END;
	
	
	(** focus borders **)

	PROCEDURE Fixed (host: Views.Frame; v: Views.View): BOOLEAN;
		VAR sp: Properties.ResizePref; c: Containers.Controller;
	BEGIN
		c := host.view(Containers.View).ThisController();
		IF c.opts * {Containers.noCaret, Documents.pageWidth..Documents.winHeight, fixed} # {} THEN
			RETURN TRUE
		END;
		sp.fixed := FALSE; Views.HandlePropMsg(v, sp); RETURN sp.fixed
	END Fixed;

	PROCEDURE PaintFocusBorder (f: Views.Frame; focus: Views.View; l, t, r, b: INTEGER);
		VAR u, s,  w, h,  mx, my,  l0, t0, r0, b0: INTEGER;
		
		PROCEDURE PaintHandle (x, y: INTEGER; l, t, r, b: BOOLEAN);
		BEGIN
			IF l THEN f.DrawRect(x - u, y, x, y + s, Ports.fill, Ports.background) END;
			IF t THEN f.DrawRect(x, y - u, x + s, y, Ports.fill, Ports.background) END;
			IF r THEN f.DrawRect(x + s, y, x + s + u, y + s, Ports.fill, Ports.background) END;
			IF b THEN f.DrawRect(x, y + s, x + s, y + s + u, Ports.fill, Ports.background) END;
			f.DrawRect(x, y, x + s, y + s, Ports.fill, Ports.defaultColor)
		END PaintHandle;
		
	BEGIN
		f.rider.GetRect(l0, t0, r0, b0);
		s := (handleSize - f.dot) DIV f.unit;
		f.rider.SetRect(l0 - s, t0 - s, r0 + s, b0 + s);
		u := f.dot; s := s * f.unit;
		w := r - l; h := b - t;
		f.DrawRect(l, t - s, r, t, Ports.fill, Ports.background);
		f.DrawRect(l, b, r, b + s, Ports.fill, Ports.background);
		f.DrawRect(l - s, t - s, l, b + s, Ports.fill, Ports.background);
		f.DrawRect(r, t - s, r + s, b + s, Ports.fill, Ports.background);
		DEC(s, u);
		f.MarkRect(l, t - s, r, t, Ports.fill, HostPorts.focusPat, Ports.show);
		f.MarkRect(l, b, r, b + s, Ports.fill, HostPorts.focusPat, Ports.show);
		f.MarkRect(l - s, t - s, l, b + s, Ports.fill, HostPorts.focusPat, Ports.show);
		f.MarkRect(r, t - s, r + s, b + s, Ports.fill, HostPorts.focusPat, Ports.show);
		IF ~Fixed(f, focus) THEN
			PaintHandle(l - s, t - s, FALSE, FALSE, TRUE, TRUE);
			PaintHandle(r, t - s, TRUE, FALSE, FALSE, TRUE);
			PaintHandle(l - s, b, FALSE, TRUE, TRUE, FALSE);
			PaintHandle(r, b, TRUE, TRUE, FALSE, FALSE);
			IF w > 2 * clipMiddleHandle THEN
				mx := (l + r - s) DIV 2;
				PaintHandle(mx, t - s, TRUE, FALSE, TRUE, FALSE);
				PaintHandle(mx, b, TRUE, FALSE, TRUE, FALSE)
			END;
			IF h > 2 * clipMiddleHandle THEN
				my := (t + b - s) DIV 2;
				PaintHandle(l - s, my, FALSE, TRUE, FALSE, TRUE);
				PaintHandle(r, my, FALSE, TRUE, FALSE, TRUE)
			END
		END;
		f.DrawRect(l - u, t - u, r + u, b + u, u, Ports.defaultColor);
		f.rider.SetRect(l0, t0, r0, b0)
	END PaintFocusBorder;


	PROCEDURE RestoreBorderArea (f: Views.Frame; l, t, r, b: INTEGER);
	(* restore area under destructive border mark *)
		VAR g: Views.RootFrame; res, s, dx, dy: INTEGER;
	BEGIN
		g := Views.RootOf(f);
		dx := f.gx - g.gx; dy := f.gy - g.gy;
		s := (handleSize - f.dot) DIV f.unit * f.unit;
		INC(l, dx); INC(t, dy); INC(r, dx); INC(b, dy);
		Views.ValidateRoot(g);
		Views.RestoreRoot(g, l - s, t - s, r + s, b + s);
	END RestoreBorderArea;
	(** selection borders **)

	PROCEDURE PaintSelBorder (f: Views.Frame; view: Views.View; l, t, r, b: INTEGER);
		VAR res, u, d, w, h,  mx, my: INTEGER; sizeable: BOOLEAN; l0, t0, r0, b0: INTEGER;
		
		PROCEDURE PaintHandle (x, y: INTEGER);
			VAR s: INTEGER; ci, co: Ports.Color;
		BEGIN
			DEC(x, d); DEC(y, d); s := d * 2 + u;
			IF sizeable THEN ci := HostPorts.selBackground; co := HostPorts.selTextCol
			ELSE ci := HostPorts.selTextCol; co := HostPorts.selBackground
			END;
			f.DrawRect(x, y, x + s, y + s, Ports.fill, co);
			INC(x, u); INC(y, u); DEC(s, 2 * u);
			f.DrawRect(x, y, x + s, y + s, Ports.fill, ci);
(*			
			f.DrawRect(x, y, x + s, y + s, Ports.fill, ci);
			f.DrawRect(x, y, x + s, y + s, 0, co)
*)
		END PaintHandle;
		
	BEGIN
		d := (handleSize - f.dot) DIV f.unit DIV 2;
		f.rider.GetRect(l0, t0, r0, b0);
		f.rider.SetRect(l0 - d - 1, t0 - d - 1, r0 + d + 1, b0 + d + 1);
		d := d * f.unit; u := f.dot;
		w := r - l; h := b - t; sizeable := ~Fixed(f, view);
		DEC(l, u); DEC(t, u);
(*
f.SaveRect(l - d, t - d, r + u + d, b + u + d, res);
*)
		f.DrawRect(l, t, r + u, b + u, u, HostPorts.selBackground);
		IF f.front THEN
			IF (w > clipMiddleHandle) & (h > clipMiddleHandle) THEN
				PaintHandle(l, t);
				PaintHandle(r, t);
				PaintHandle(l, b);
				PaintHandle(r, b);
				IF w > 2 * clipMiddleHandle THEN
					mx := (l + r) DIV 2;
					PaintHandle(mx, t);
					PaintHandle(mx, b)
				END;
				IF h > 2 * clipMiddleHandle THEN
					my := (t + b) DIV 2;
					PaintHandle(l, my);
					PaintHandle(r, my)
				END
			ELSIF sizeable THEN
				PaintHandle(r, b)
			END
		END;
		f.rider.SetRect(l0, t0, r0, b0)
	END PaintSelBorder;

	PROCEDURE RestoreViewArea (f: Views.Frame; l, t, r, b: INTEGER);
	(* restore area under destructive selection mark *)
		VAR g: Views.RootFrame; res, d, u, dx, dy: INTEGER;
	BEGIN
		g := Views.RootOf(f);
		dx := f.gx - g.gx; dy := f.gy - g.gy;
		d := (handleSize - f.dot) DIV f.unit DIV 2 * f.unit + f.dot;
		INC(l, dx); INC(t, dy); INC(r, dx); INC(b, dy);
		Views.ValidateRoot(g);
		Views.RestoreRoot(g, l - d, t - d, r + d, b + d)
	END RestoreViewArea;

	
	(* Hook *)
	
	PROCEDURE (hook: Hook) MarkFocusBorder* (host: Views.Frame; focus: Views.View; l, t, r, b: INTEGER; show: BOOLEAN);
	BEGIN
		IF focus # NIL THEN
			IF show THEN
				PaintFocusBorder(host, focus, l, t, r, b)
			ELSE
				RestoreBorderArea(host, l, t, r, b)
			END
		END
	END MarkFocusBorder;

	PROCEDURE (hook: Hook) MarkSingletonBorder* (host: Views.Frame; view: Views.View; l, t, r, b: INTEGER; show: BOOLEAN);
	BEGIN
		IF view # NIL THEN
			IF show THEN
				PaintSelBorder(host, view, l, t, r, b)
			ELSE
				RestoreViewArea(host, l, t, r, b)
			END
		END
	END MarkSingletonBorder;

	PROCEDURE (hook: Hook) FocusBorderCursor* (host: Views.Frame; view: Views.View; l, t, r, b: INTEGER;
										x, y: INTEGER): INTEGER;
	BEGIN
		RETURN 0
	END FocusBorderCursor;

	PROCEDURE (hook: Hook) SelBorderCursor* (f: Views.Frame; view: Views.View; l, t, r, b: INTEGER;
									x, y: INTEGER): INTEGER;
		VAR d, u, w, h, mx, my: INTEGER; cursor: INTEGER;
		
		PROCEDURE CheckHandle (x0, y0: INTEGER; c: INTEGER);
		BEGIN
			IF (x >= x0 - d) & (x <= x0 + d) & (y >= y0 - d) & (y <= y0 + d) THEN
				cursor := c
			END
		END CheckHandle;
		
	BEGIN
		IF (x < l) OR (x > r) OR (y < t) OR (y > b) THEN cursor := Mechanisms.outside
		ELSE cursor := Mechanisms.inside
		END;
		IF (view # NIL) & ~Fixed(f, view) THEN
			d := (handleSize - f.dot) DIV f.unit DIV 2 * f.unit;
			w := r - l; h := b - t; u := f.dot;
			DEC(l, u); DEC(t, u);
			IF (w > clipMiddleHandle) & (h > clipMiddleHandle) THEN
				CheckHandle(l, t, HostPorts.resizeLCursor);
				CheckHandle(r, t, HostPorts.resizeRCursor);
				CheckHandle(l, b, HostPorts.resizeRCursor);
				CheckHandle(r, b, HostPorts.resizeLCursor);
				IF w > 2 * clipMiddleHandle THEN
					mx := (l + r) DIV 2;
					CheckHandle(mx, t, HostPorts.resizeVCursor);
					CheckHandle(mx, b, HostPorts.resizeVCursor)
				END;
				IF h > 2 * clipMiddleHandle THEN
					my := (t + b) DIV 2;
					CheckHandle(l, my, HostPorts.resizeHCursor);
					CheckHandle(r, my, HostPorts.resizeHCursor)
				END
			ELSE
				CheckHandle(r, b, HostPorts.resizeLCursor)
			END
		END;
		RETURN cursor
	END SelBorderCursor;
	
	PROCEDURE (hook: Hook) TrackToResize* (host: Views.Frame; view: Views.View;
										minW, maxW, minH, maxH: INTEGER;
										VAR l, t, r, b: INTEGER; VAR op: INTEGER;
										VAR x, y: INTEGER; VAR buttons: SET);
		VAR area: SET; isDown: BOOLEAN; c: INTEGER; m: SET; p: Properties.SizePref;
			x1, y1,  dx, dy,  dl, dt, dr, db,  l0, t0, r0, b0,  l1, t1, r1, b1,  w, h,  dw, dh: INTEGER;
	BEGIN
		l0 := l; t0 := t; r0 := r; b0 := b;  dl := 0; dt := 0; dr := 0; db := 0;
		x1 := (l + r) DIV 2; y1 := (t + b) DIV 2;
		IF (r - l <= 2 * clipMiddleHandle) OR (ABS(x - x1) > handleSize DIV 2) THEN
			IF x < x1 THEN dl := 1 ELSE dr := 1 END
		END;
		IF (b - t <= 2 * clipMiddleHandle) OR (ABS(y - y1) > handleSize DIV 2) THEN
			IF y < y1 THEN dt := 1 ELSE db := 1 END
		END;
		IF (Controllers.extend IN buttons) & (dl # dr) THEN dl := 1; dr := 1 END;
		IF (Controllers.extend IN buttons) & (dt # db) THEN dt := 1; db := 1 END;
		host.MarkRect(l, t, r, b, 0, Ports.dim50, Ports.show);
		REPEAT
			host.Input(x1, y1, m, isDown);
			IF x1 < host.l THEN x1 := host.l ELSIF x1 > host.r THEN x1 := host.r END;
			IF y1 < host.t THEN y1 := host.t ELSIF y1 > host.b THEN y1 := host.b END;
			dx := x1 - x; dy := y1 - y;
			l1 := l0 + dl * dx; t1 := t0 + dt * dy; r1 := r0 + dr * dx; b1 := b0 + db * dy;
			w := r1 - l1; h := b1 - t1;
			IF (w > 0) & (h > 0) THEN
				p.fixedH := (dl = 0) & (dr = 0); p.fixedW := (dt = 0) & (db = 0);
				p.w := w; p.h := h; Views.HandlePropMsg(view, p); w := p.w; h := p.h;
				IF w < minW THEN w := minW ELSIF w > maxW THEN w := maxW END;
				IF h < minH THEN h := minH ELSIF h > maxH THEN h := maxH END;
				dw := w - (r1 - l1); dh := h - (b1 - t1);
				DEC(l1, dl * dw); DEC(t1, dt * dh);
				IF (dl + dr = 0) & (dw # 0) THEN INC(r1, dw) ELSE INC(r1, dr * dw) END;
				IF (dt + db = 0) & (dh # 0) THEN INC(b1, dh) ELSE INC(b1, db * dh) END;
				IF (l1 # l) OR (t1 # t) OR (r1 # r) OR (b1 # b) THEN
					host.MarkRect(l, t, r, b, 0, Ports.dim50, Ports.hide);
					l := l1; t := t1; r := r1; b := b1;
					host.MarkRect(l, t, r, b, 0, Ports.dim50, Ports.show)
				END
			END
		UNTIL ~isDown;
		host.MarkRect(l, t, r, b, 0, Ports.dim50, Ports.hide);
		x := x1; y := y1; buttons := {};
		IF (l # l0) OR (t # t0) OR (r # r0) OR (b # b0) THEN op := Mechanisms.resize
		ELSE op := Mechanisms.cancelResize
		END
	END TrackToResize;
	
	PROCEDURE (hook: Hook) TrackToDrop* (f: Views.Frame; view: Views.View;
									isSingle: BOOLEAN; w, h, rx, ry: INTEGER;
									VAR dest: Views.Frame; VAR destX, destY: INTEGER; VAR op: INTEGER;
									VAR x, y: INTEGER; VAR buttons: SET);
	BEGIN
		(* TODO: Implement *)
	END TrackToDrop;
	

	PROCEDURE PickMode (f, dest: Views.Frame; x, y: INTEGER): INTEGER;
		VAR mode, cursor: INTEGER;
	BEGIN
	(* TODO *)
	(*	IF USER32.GetAsyncKeyState(1BH) < 0 THEN mode := escape; cursor := Ports.arrowCursor
		ELS*)IF dest = NIL THEN mode := Mechanisms.cancelPick; cursor := HostPorts.stopCursor
		ELSE
			cursor := HostPorts.pickCursor;
			IF Services.SameType(dest.view, f.view) THEN
				mode := Mechanisms.pick
			ELSE mode := Mechanisms.pickForeign
			END
		END;
		f.SetCursor(cursor);
		RETURN mode
	END PickMode;
	
	PROCEDURE (hook: Hook) TrackToPick* (f: Views.Frame;
									VAR dest: Views.Frame; VAR destX, destY: INTEGER; VAR op: INTEGER;
									VAR x, y: INTEGER; VAR buttons: SET);
		VAR d, d0: Views.Frame;
			dx, dy,  x0, y0,  x1, y1: INTEGER; isDown: BOOLEAN; m: SET;
	BEGIN
		x0 := x; y0 := y;
		Properties.PollPick(x, y, f, x0, y0, Properties.mark, Properties.show, dest, destX, destY);
(* MarkTarget(dest, dest # f); *)
		op := PickMode(f, dest, x, y);
		REPEAT
(* CheckWindow(TRUE); *)
			f.Input(x1, y1, m, isDown);
			IF (x1 # x) OR (y1 # y) THEN
				Properties.PollPick(x1, y1, f, x0, y0, Properties.noMark, Properties.show, d, dx, dy);
				IF (d # dest) OR (dx # destX) OR (dy # destY) THEN
					d0 := dest;
(* MarkTarget(dest, (dest # f) & (d # d0)); *)
					Properties.PollPick(x, y, f, x0, y0, Properties.mark, Properties.hide, dest, destX, destY);
					x := x1; y := y1;
					Properties.PollPick(x, y, f, x0, y0, Properties.mark, Properties.show, d, dx, dy);
					dest := d; destX := dx; destY := dy;
(* MarkTarget(dest, (dest # f) (* ~home *) & (d # d0)); *)
				ELSE
					Properties.PollPick(x, y, f, x0, y0, Properties.mark, Properties.hide, d, dx, dy);
					x := x1; y := y1;
					Properties.PollPick(x, y, f, x0, y0, Properties.mark, Properties.show, d, dx, dy)
				END
			END;
			op := PickMode(f, dest, x, y)
		UNTIL ~isDown OR (op = escape);
		Properties.PollPick(x, y, f, x0, y0, Properties.mark, Properties.hide, d, dx, dy);
		IF op = escape THEN
			REPEAT f.Input(x, y, m, isDown) UNTIL ~isDown;
			op := Mechanisms.cancelPick
		END;
(* MarkTarget(dest, dest # f); *)
(* CheckWindow(FALSE) *)
		buttons := {}
	END TrackToPick;
	
	PROCEDURE (hook: Hook) PopUpAndSelect* (f: Views.Frame;
										n, this: INTEGER;
										string: ARRAY OF ARRAY OF CHAR;
										enabled, checked: ARRAY OF BOOLEAN;
										VAR i: INTEGER;
										VAR x, y: INTEGER; VAR buttons: SET);
	BEGIN
		(* TODO *)
	END PopUpAndSelect;
										
										
	PROCEDURE Init*;
		VAR h: Hook;
	BEGIN
		NEW(h); Mechanisms.SetHook(h);
	END Init;

BEGIN
	Init
END HostMechanisms.
