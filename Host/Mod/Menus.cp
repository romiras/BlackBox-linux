MODULE HostMenus;

	(*
		TODO:
			- minusCode
			- Right click menu stays open if no command is chosen
			- In TranslateAccelerators, charachter codes for NBHYPHEN and more, needs to be set and sent
			- Checking of items need to be looked at. Can GtkCheckMenuItem or GtkRadioMenuItem be used?
			- Interpret more switches in ReadCommandLine
	*)

	(* 
	
		Implementation Docu
		
		The BlackBox main menu is a GtkMenu with a set of GtkMenuItem that correspond to the
		name of the menus. Each such menu item has a GtkMenu as submenu, and the submenu
		contains the actual menu items in form of GtkMenuItem-objects.
		
	*)

	IMPORT 
		SYSTEM, Kernel, LinLibc, LinGdk, LinGtk, 
		Files, Dialog, Strings, Services, Properties, Controllers, Views, Stores, Containers, Windows, Documents, Converters, 
		StdDialog, 
		HostFiles, HostPorts, HostWindows, 
		Log, StdCmds, 
	
		(* Not needed imports but modules that need to be loaded: *)
		Fonts, Printers, Models, Controls, StdInterpreter, HostFonts, HostDialog, HostCFrames, HostClipboard,
		
		(* Needed, but should be loaded after unneeded modules *)
		HostCmds;
	
	CONST
		idlePeriod = 50; (* ms *)
		gcCycle = 100;
		
		(* hints *)
		impAll = 0;	(* can import all file types *)
		
		HYPHEN = 90X; NBHYPHEN = 91X; SOFTHYPHEN = 0ADX;
		NBSPACE = 0A0X; NUMSPACE = 8FX;
		
		iClose = 100; (* known in HostWindows *)
		iUndo = 112; iCut = 114; iCopy = 115; iPaste = 116;
		iObject = 120; iPopup = 160; iProperties = 163;
		iObjEdit = 161; iObjOpen = 162; iVerb0 = 200;
		iOpen = 102; iUpdateMenus = 111; iExit = 110;
		
		(* custom menus *)
		firstId = 300;
		
		(* File for specifying command line options *)
		cmdLinePath = "System/Rsrc"; cmdLineFile = "CommandLine.txt";
		
	TYPE
		Item* = POINTER TO RECORD (StdDialog.Item)
			(*id-,*) code-, hotKey-: INTEGER;
			shift-, ctrl-, del: BOOLEAN;
			mi: LinGtk.GtkWidget
		END;

		Menu* = POINTER TO RECORD
			next-: Menu;
			menu-, type-: Dialog.String;
			firstItem-, lastItem: Item;
			menuH-: LinGtk.GtkMenu;
			isWinMenu-: BOOLEAN;
			isPopup-: BOOLEAN;
			class-: INTEGER;
			hot, maxId: INTEGER;
			mi: LinGtk.GtkWidget
		END;
	
	VAR
		(* active menu bar state *)
		menus-: Menu;
		menuBar-: LinGtk.GtkWidget;
		lastId-: INTEGER;	(* last custom menu id *)
		
		type: Stores.TypeName;
		
		(* new menu bar state *)
		newMenuBar(*, newWinMenu*): LinGtk.GtkWidget;
		popMenu, newPopMenu: LinGtk.GtkMenu;
		nextId: INTEGER;	(* id of next menu item *)
		firstMenu, lastMenu, curMenu: Menu;
		
		quit: BOOLEAN;
		gc: INTEGER;	(* how many events must be executed before next GC *)
		shiftStr, ctrlStr, spaceStr: Dialog.String;
		num: INTEGER;
		minusCode: INTEGER;	(* key code for "-" *)
		
	
	(* menu dispatching *)

	PROCEDURE PrepareMenu (menu, notUsed1, notUsed2: INTEGER);
	(* this procedure is called after the user has clicked into the menu bar, but before
		showing the menu; to prepare item enabling/disabling, check marks, etc. *)
		VAR res, n: INTEGER; failed, ok: BOOLEAN; par: Dialog.Par; m: Menu; i: StdDialog.Item; str: Dialog.String;
			ss: ARRAY 256 OF SHORTCHAR; pstr: LinLibc.PtrSTR; bin: LinGtk.GtkBin; lch: SHORTCHAR;
	BEGIN
		m := menus;
		WHILE (m # NIL) & (m.menuH # menu) DO m := m.next END;
		IF m # NIL THEN
			i := m.firstItem; n := 0;
			WHILE i # NIL DO
				WITH i: Item DO
					IF i.filter^ # "" THEN	(* custom menu item with custom guard *)
						StdDialog.CheckFilter(i, failed, ok, par);
						IF ~failed THEN
							IF par.label = "-" THEN
								IF ~i.del THEN
(*									res := USER32.RemoveMenu(m.menuH, n, {US1ER32.MFByPosition});*)
									i.del := TRUE
								END;
								DEC(n)
							ELSE
								IF i.del THEN
(*									res := USER32.InsertMenuA(m.menuH, n, {USER312.MFByPosition}, i.id, "+");*)
									i.del := FALSE
								END;
								IF par.label # i.item$ THEN
									Dialog.MapString(par.label, str);
									(*
									ChangeItem(m, i, str); ss := SHORT(str$);
									IF i.id = iObject THEN
										res := USER32.ModifyMenuA(m.menuH, n, {USER32.MFByPosition, USER32.MFPopup},
																				objMenu, ss)
									ELSE
										res := USER32.ModifyMenuA(m.menuH, n, {USER32.MFByPosition}, i.id, ss)
									END
									*)
								END;
								IF par.disabled THEN
										LinGtk.gtk_widget_set_sensitive(i.mi, LinLibc.FALSE);
								ELSE
										LinGtk.gtk_widget_set_sensitive(i.mi, LinLibc.TRUE)
								END;
								
								(* TODO: Handle checking better *)
								ss := "";
								bin := SYSTEM.VAL(LinGtk.GtkBin, i.mi);
								LinGtk.gtk_label_get(bin.child, pstr);
								lch := pstr[LEN(pstr$) - 1];
								IF par.checked THEN					
									IF lch # "*" THEN ss := pstr + "*" END;
								ELSE
									IF lch = "*" THEN ss := pstr$; ss[LEN(ss$) - 1] := 0X END;
								END;
								IF ss # "" THEN LinGtk.gtk_label_set_text(bin.child, ss) END;
								
								IF ~ok THEN
									(* mark with "?" !!! *)
									LinGtk.gtk_widget_set_sensitive(i.mi, LinLibc.FALSE);
								END
							END
						END
					END
				END;
				i := i.next; INC(n)
			END
		END
	END PrepareMenu;
		
(*
	PROCEDURE HandleCustomMenu (id: INTEGER);
		VAR m: Menu; i: StdDialog.Item; 
	BEGIN
		m := menus;
		WHILE (m # NIL) & (m.maxId < id) DO m := m.next END;
		IF m # NIL THEN i := m.firstItem;
			WHILE (i # NIL) & (i(Item).id # id) DO i := i.next END;
			IF i # NIL THEN StdDialog.HandleItem(i) END
		END
	END HandleCustomMenu;
*)	
	
	(* Menu command handler *)
	
	PROCEDURE MenuSelect (item: LinGtk.GtkItem; user_data: LinLibc.PtrVoid);
	(*
		VAR id, res: INTEGER;
	BEGIN
		DEC(gc);
		id := SYSTEM.VAL(INTEGER, user_data);
		IF id < firstId THEN HandleVerb(id - iVerb0)
		ELSE
			res := 0;	(* prevent from parasitic anchors on stack *)
			HandleCustomMenu(id)
		END;
		Properties.IncEra;
	*)
		VAR i: Item; res: INTEGER;
	BEGIN
		DEC(gc);
		i := SYSTEM.VAL(Item, user_data);
		res := 0;	(* prevent from parasitic anchors on stack *)
		IF i # NIL THEN StdDialog.HandleItem(i) END;
		Properties.IncEra;
	END MenuSelect;
	
	PROCEDURE MenuActivate (item: LinGtk.GtkItem; user_data: LinLibc.PtrVoid);
		VAR id, res: INTEGER;
	BEGIN
		Controllers.SetCurrentPath(Controllers.frontPath);
		Kernel.Try(PrepareMenu, user_data, 0, 0);
		Controllers.ResetCurrentPath()
	END MenuActivate;
	

	(* shortcut support *)

	PROCEDURE Append (VAR a: ARRAY OF CHAR; b: ARRAY OF CHAR);	(* a := a + b *)
		VAR i, j: INTEGER;
	BEGIN
		i := 0; j := 0;
		WHILE a[i] # 0X DO INC(i) END;
		WHILE b[j] # 0X DO a[i] := b[j]; INC(i); INC(j) END;
		a[i] := 0X
	END Append;
	
	PROCEDURE SetShortcut (VAR i: Item; VAR name: Dialog.String);
		VAR j, n: INTEGER; ch, nch: CHAR; a: ARRAY 4 OF CHAR;
	BEGIN
		i.shift := FALSE; i.ctrl := FALSE; i.code := 0; j := 0; ch := i.shortcut[0];
		WHILE (ch # 0X) & (i.code = 0) DO INC(j);
			IF (ch >= "a") & (ch <= "z") THEN ch := CAP(ch) END;
			nch := i.shortcut[j];
			IF ch = "*" THEN i.shift := TRUE
			ELSIF ch = "^" THEN i.ctrl := TRUE
			ELSIF (ch >= "A") & (ch <= "Z") OR (ch >= "0") & (ch <= "9") OR (ch = " ") THEN
				IF (nch >= "a") & (nch <= "z") THEN nch := CAP(nch) END;
				IF nch = 0X THEN i.code := ORD(ch); i.ctrl := TRUE
				ELSIF ch = "F" THEN
					n := 0;
					WHILE (nch >= "0") & (nch <= "9") DO
						n := 10 * n + ORD(nch) - ORD("0"); INC(j); nch := i.shortcut[j]
					END;
					IF (n >= 1) & (n <= 16) THEN i.code := 0FFBEH - 1 + n END
				END
			END;
			ch := nch
		END;
		(* only needed under Windows?
		IF i.code # 0 THEN
			Append(name, "	");	(* tab *)
			IF i.shift THEN Append(name, shiftStr) END;
			IF i.ctrl THEN Append(name, ctrlStr) END;
			IF i.code >= 70H THEN
				a[0] := "F"; n := i.code - 70H + 1; j := 1;
				IF n > 9 THEN a[1] := "1"; DEC(n, 10); INC(j) END;
				a[j] := CHR(n + ORD("0")); a[j+1] := 0X;
				Append(name, a)
			ELSIF i.code = ORD(" ") THEN Append(name, spaceStr)
			ELSE a[0] := CHR(i.code); a[1] := 0X; Append(name, a)
			END
		END
		*)
	END SetShortcut;
	
	
	(* hotkey support *)

	PROCEDURE NextWord (VAR name: Dialog.String; i: INTEGER): INTEGER;
	BEGIN
		WHILE (name[i] # 0X) & (name[i] # " ") DO INC(i) END;
		WHILE name[i] = " " DO INC(i) END;
		IF (CAP(name[i]) < "A") OR (CAP(name[i]) > "Z") THEN i := -1 END;
		RETURN i
	END NextWord;
	
	PROCEDURE SetHotkey (VAR name: Dialog.String; i: INTEGER);
		VAR j: INTEGER;
	BEGIN
		IF name[i] # "&" THEN
			j := i;
			WHILE name[j] # 0X DO INC(j) END;
			WHILE j >= i DO name[j+1] := name[j]; DEC(j) END;
			name[i] := "&"
		END
	END SetHotkey;
	
	PROCEDURE GetHotkey (VAR name: Dialog.String; VAR pos: INTEGER; VAR used: SET);
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; ch := name[0];
		WHILE ch # 0X DO
			IF ch = "&" THEN
				ch := name[i + 1];
				IF ch = "&" THEN INC(i)
				ELSE
					pos := i; ch := CAP(ch);
					IF (ch >= "A") & (ch <= "Z") THEN INCL(used, ORD(ch) - ORD("A")) END;
					RETURN 
				END
			END;
			INC(i); ch := name[i]
		END;
		pos := -1
	END GetHotkey;
	
	PROCEDURE FirstFree (VAR name: Dialog.String; VAR used: SET): INTEGER;
		VAR i: INTEGER; ch: CHAR;
	BEGIN
		i := 0; ch := CAP(name[0]);
		WHILE (name[i] # 0X) & ((ch < "A") OR (ch > "Z") OR (ORD(ch) - ORD("A") IN used)) DO
			INC(i); ch := CAP(name[i])
		END;
		IF ch # 0X THEN INCL(used, ORD(ch) - ORD("A"))
		ELSE i := -1
		END;
		RETURN i
	END FirstFree;

	PROCEDURE SetHotkeys (VAR tab: ARRAY OF Dialog.String; n: INTEGER);
		VAR i, j: INTEGER; ch: CHAR; pos: POINTER TO ARRAY OF INTEGER; used: SET;
	BEGIN
		NEW(pos, LEN(tab));
		used := {}; i := 0;
		WHILE i < n DO GetHotkey(tab[i], pos[i], used); INC(i) END;
		i := 0;
		WHILE i < n DO
			IF pos[i] = -1 THEN
				ch := CAP(tab[i, 0]);
				IF (ch >= "A") & (ch <= "Z") & ~(ORD(ch) - ORD("A") IN used) THEN
					INCL(used, ORD(ch) - ORD("A"));
					pos[i] := 0
				END
			END;
			INC(i)
		END;
		i := 0;
		WHILE i < n DO
			IF pos[i] = -1 THEN
				j := NextWord(tab[i], 0);
				WHILE j >= 0 DO
					ch := CAP(tab[i, j]);
					IF ~(ORD(ch) - ORD("A") IN used) THEN
						INCL(used, ORD(ch) - ORD("A"));
						pos[i] := j; j := -1
					ELSE
						j := NextWord(tab[i], j)
					END
				END
			END;
			INC(i)
		END;
		i := 0;
		WHILE i < n DO
			IF pos[i] = -1 THEN pos[i] := FirstFree(tab[i], used) END;
			IF pos[i] >= 0 THEN SetHotkey(tab[i], pos[i]) END;
			INC(i)
		END
	END SetHotkeys;
	
	PROCEDURE UpdateHotkey (VAR old, new: Dialog.String);
		VAR i, j: INTEGER; used: SET; ch: CHAR;
	BEGIN
		GetHotkey(new, i, used);
		IF i = -1 THEN
			used := {}; GetHotkey(old, i, used);
			IF used # {} THEN
				used := -used; i := -1; j := 0;
				WHILE j >= 0 DO
					ch := CAP(new[j]);
					IF ~(ORD(ch) - ORD("A") IN used) THEN
						i := j; j := -1
					ELSE
						j := NextWord(new, j)
					END
				END;
				IF i = -1 THEN i := FirstFree(new, used) END;
				IF i >= 0 THEN SetHotkey(new, i) END
			END
		END
	END UpdateHotkey;
	

		
		
	(* Menus *)

	PROCEDURE FirstMenu* (): Menu;
	BEGIN
		RETURN menus
	END FirstMenu;

	PROCEDURE DeleteAll*;
	BEGIN
(*		WHILE USER32.RemoveMenu(menuBar, 0, {USER32.MFByPosition}) # 0 DO END;*)
		firstMenu := NIL; lastMenu := NIL; curMenu := NIL;
(*		newWinMenu := 0; *)
		newPopMenu := 0;
		nextId := firstId
	END DeleteAll;

	PROCEDURE Open* (menu, type: ARRAY OF CHAR);
	BEGIN
		ASSERT(curMenu = NIL, 20); ASSERT(menu # "", 21);
		NEW(curMenu); curMenu.next := NIL;
		curMenu.menuH := LinGtk.gtk_menu_new();
		Dialog.MapString(menu, curMenu.menu);
		curMenu.type := type$;
		curMenu.firstItem := NIL
	END Open;
	
	PROCEDURE AddItem* (item, string, shortcut, filter: Dialog.String);
		VAR i: Item; id: INTEGER;
	BEGIN
		ASSERT(curMenu # NIL, 20); ASSERT(item # "", 21); ASSERT(string # "", 22);
		IF string = "HostMenus.WindowList" THEN
			curMenu.isWinMenu := TRUE
		ELSE
			NEW(i); i.next := NIL;
			IF curMenu.lastItem = NIL THEN curMenu.firstItem := i ELSE curMenu.lastItem.next := i END;
			curMenu.lastItem := i;
			StdDialog.AddItem(i, item, string, filter, shortcut);
			IF string = "HostMenus.ObjectMenu" THEN id := iObject
			ELSE id := nextId; INC(nextId)
			END;
(*			i.id := id;*)
			IF id > curMenu.maxId THEN curMenu.maxId := id END
		END
	END AddItem;
(*
	PROCEDURE ChangeItem (m: Menu; i: Item; VAR name: Dialog.String);
		VAR res: INTEGER; old: Dialog.String; so: ARRAY 256 OF SHORTCHAR;
	BEGIN
		res := USER32.GetMenuStringA(m.menuH, i.id, so, LEN(old), {}); old := so$;
		UpdateHotkey(old, name);
		SetShortcut(i, name)
	END ChangeItem;
	*)
	
	PROCEDURE AddSeparator*;
		VAR i: Item;
	BEGIN
		ASSERT(curMenu # NIL, 20);
		NEW(i); i.next := NIL;
		IF curMenu.lastItem = NIL THEN curMenu.firstItem := i ELSE curMenu.lastItem.next := i END;
		curMenu.lastItem := i;
		StdDialog.AddItem(i, "", "", "", "");
(*		i.id := 0*)
	END AddSeparator;
		
	(* On Windows & is used to mark a Alt-shortcut. On Linux underscore is used. (&& should be interpreted as &) *)
	PROCEDURE AmpersandToUline(VAR ss: ARRAY OF SHORTCHAR);
		VAR i, j: INTEGER;
	BEGIN
		i := 0; j := 0;
		WHILE ss[i] # 0X DO
			IF (ss[i] = "&") & (ss[i + 1] = "&") THEN
				ss[j] := "&";
				INC(i)
			ELSIF ss[i] = "&" THEN ss[j] := "_"
			ELSE ss[j] := ss[i]
			END;
			INC(i); INC(j);
		END;
		ss[j] := 0X
	END AmpersandToUline;
	
	PROCEDURE NewMenuItem (ss: ARRAY OF SHORTCHAR; OUT mi: LinGtk.GtkWidget; OUT hotKey: INTEGER);
		VAR bin: LinGtk.GtkBin; ch: CHAR;
	BEGIN
		AmpersandToUline(ss);
		mi := LinGtk.gtk_menu_item_new_with_label(ss);
(*
		mi := LinGtk.gtk_check_menu_item_new_with_label(ss);
		LinGtk.gtk_check_menu_item_set_active(mi, LinLibc.FALSE);
		LinGtk.gtk_check_menu_item_set_show_toggle(mi, LinLibc.FALSE);

*)		
		bin := SYSTEM.VAL(LinGtk.GtkBin, mi);
		hotKey := LinGtk.gtk_label_parse_uline(bin.child, ss);
		ch := CHR(hotKey); IF (ch >= "a") & (ch <= "z") THEN ch := CAP(ch); hotKey := ORD(ch) END
	END NewMenuItem;
		
	PROCEDURE Close*;
		VAR 
			res, j, n: INTEGER; i: StdDialog.Item; tab: POINTER TO ARRAY OF Dialog.String; 
			ss: ARRAY 256 OF SHORTCHAR;
			(*mi: LinGtk.GtkWidget; *)mask: SET; accGroup: LinGtk.GtkAccelGroup;
	BEGIN
		ASSERT(curMenu # NIL, 20);
		i := curMenu.firstItem; n := 0;
		WHILE i # NIL DO i := i.next; INC(n) END;
		NEW(tab, n);
		i := curMenu.firstItem; j := 0;
		WHILE i # NIL DO Dialog.MapString(i.item, tab[j]); (* tab[j] := i.item^$; *)i := i.next; INC(j) END;
		SetHotkeys(tab, j);
		i := curMenu.firstItem; j := 0;
		accGroup := LinGtk.gtk_accel_group_get_default();
		LinGtk.gtk_accel_group_unlock(accGroup);
		WHILE i # NIL DO
			WITH i: Item DO
				IF i.item^ # "" THEN
					SetShortcut(i, tab[j]); ss := SHORT(tab[j]$);
(*					IF i.id = iObject THEN*)
(*						res := USER32.AppendMenuA(curMenu.menuH, {USER32.MFPopup}, objMenu, ss)*)
(*					ELSE*)
						NewMenuItem(ss, i.mi, i.hotKey);
						LinGtk.gtk_menu_append(curMenu.menuH, i.mi);
						res := LinGtk.gtk_signal_connect(i.mi, "activate", 
													SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(MenuSelect)),
													SYSTEM.VAL(INTEGER, i));
						mask := {};
						IF i.ctrl THEN INCL(mask, LinGdk.GDK_CONTROL_MASK) END;
						IF i.shift THEN INCL(mask, LinGdk.GDK_SHIFT_MASK) END;
						LinGtk.gtk_widget_add_accelerator(i.mi, "activate", accGroup, i.code,
																		mask, {LinGtk.GTK_ACCEL_VISIBLE});
(*					END*)
				ELSIF i.next # NIL THEN
(*					res := USER32.AppendMenuA(curMenu.menuH, {USER32.MFSeparator}, 0, NIL)*)
					LinGtk.gtk_menu_append(curMenu.menuH, LinGtk.gtk_menu_item_new())
				END
			END;
			i := i.next; INC(j)
		END;
		IF curMenu.menu = "*" THEN curMenu.isPopup := TRUE END;
		IF curMenu.type = "WindowMenu" THEN curMenu.isWinMenu := TRUE; curMenu.type := "" END;
	(*	IF curMenu.isWinMenu THEN newWinMenu := curMenu.menuH*)
		(*ELS*)IF curMenu.type = "PopupMenu" THEN newPopMenu := curMenu.menuH
		END;
		IF lastMenu = NIL THEN firstMenu := curMenu ELSE lastMenu.next := curMenu END;
		lastMenu := curMenu; curMenu := NIL;
		LinGtk.gtk_accel_group_lock(accGroup)
	END Close;

	PROCEDURE InitMenus*;
		VAR m, n, old: Menu; res, i: INTEGER; used, u: SET; tp: Stores.TypeName; oldBar: LinGtk.GtkWidget;
			ss: ARRAY 256 OF SHORTCHAR;
	BEGIN
		ASSERT(curMenu = NIL, 20);
		IF firstMenu # NIL THEN
			used := {}; m := firstMenu;
			WHILE m # NIL DO GetHotkey(m.menu, m.hot, used); m := m.next END;
			m := firstMenu; i := 0;
			WHILE m # NIL DO
				IF (m.hot = -1) & (m.type = "") THEN m.hot := FirstFree(m.menu, used) END;
				IF m.isWinMenu THEN m.class := 4; i := 100
				ELSIF m.isPopup THEN m.class := 10
				ELSIF i = 0 THEN m.class := 0
				ELSIF i < 3 THEN m.class := 1
				ELSIF i < 100 THEN m.class := 3
				ELSE m.class := 5
				END;
				m := m.next; INC(i)
			END;
			m := firstMenu;
			WHILE m # NIL DO
				IF m.hot = -1 THEN
					tp := m.type$; u := used; n := m;
					WHILE n # NIL DO
						IF (n.hot = -1) & (n.type = tp) THEN n.hot := FirstFree(n.menu, u) END;
						n := n.next
					END
				END;
				IF m.hot >= 0 THEN SetHotkey(m.menu, m.hot) END;
				m := m.next
			END;
			newMenuBar := LinGtk.gtk_menu_bar_new();
			m := firstMenu;
			WHILE m # NIL DO
				IF ((m.type = "") OR (m.type = type)) & ~m.isPopup THEN
					ss := SHORT(m.menu$);
					NewMenuItem(ss, m.mi, m.hot);
					res := LinGtk.gtk_signal_connect(m.mi, "activate", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(MenuActivate)),
												m.menuH);
					LinGtk.gtk_menu_item_set_submenu(SYSTEM.VAL(LinGtk.GtkMenuItem, m.mi), 
																	SYSTEM.VAL(LinGtk.GtkWidget, m.menuH));
					LinGtk.gtk_menu_bar_append(SYSTEM.VAL(LinGtk.GtkMenuBar, newMenuBar), m.mi)
				END;
				m := m.next
			END;
			oldBar := menuBar; menuBar := newMenuBar;
(*			winMenu := newWinMenu; *) 
			popMenu := newPopMenu;
			old := menus; menus := firstMenu; lastId := nextId;
(*			IF UpdateOleMenus # NIL THEN UpdateOleMenus() END;*)


			(*IF oldBar # NIL THEN LinGtk.gtk_container_remove(HostWindows.main, oldBar) END;*)
			(*LinGtk.gtk_container_add(HostWindows.main, menuBar);*)
			HostWindows.SetMenu(menuBar);
			LinGtk.gtk_widget_show_all(menuBar);
			(*
			res := USER32.SendMessageA(HostWindows.client, USER32.WMMDISetMenu, menuBar, winMenu);
			IF res # 0 THEN
				res := USER32.DrawMenuBar(HostWindows.main);
				m := old;
				WHILE m # NIL DO	(* prevent submenus from being deleted *)
					WHILE USER32.RemoveMenu(m.menuH, 0, {USER32.MFByPosition}) # 0 DO END;
					res := USER32.DestroyMenu(m.menuH);
					m := m.next
				END;
				res := USER32.DestroyMenu(oldBar)
			END
			*)
		END
	END InitMenus;
	
	PROCEDURE UpdateMenus;
		VAR res: INTEGER; m: Menu; old, w: LinGtk.GtkWidget; ss: ARRAY 256 OF SHORTCHAR;
	BEGIN
		old := menuBar; menuBar := LinGtk.gtk_menu_bar_new();
		m := menus;
		WHILE m # NIL DO
			IF ((m.type = "") OR (m.type = type)) & ~m.isPopup THEN
(*				res := USER32.AppendMenuA(menuBar, {USER32.MFPopup},  m.menuH, ss)*)
				m.mi := LinGtk.gtk_menu_get_attach_widget(m.menuH);
				IF m.mi = NIL THEN
					ss := SHORT(m.menu$);
					NewMenuItem(ss, m.mi, m.hot);
					res := LinGtk.gtk_signal_connect(m.mi, "activate", 
												SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(MenuActivate)),
												m.menuH);
					LinGtk.gtk_menu_item_set_submenu(SYSTEM.VAL(LinGtk.GtkMenuItem, m.mi), 
																SYSTEM.VAL(LinGtk.GtkWidget, m.menuH));
				ELSE
					IF m.mi.parent # NIL THEN
						(*LinGtk.gtk_container_remove(SYSTEM.VAL(LinGtk.GtkContainer, old), mi)*)
						LinGtk.gtk_container_remove(SYSTEM.VAL(LinGtk.GtkContainer, m.mi.parent), m.mi)
					END;
				END;
				LinGtk.gtk_menu_bar_append(SYSTEM.VAL(LinGtk.GtkMenuBar, menuBar), m.mi)
			ELSE
				LinGtk.gtk_object_ref(SYSTEM.VAL(LinGtk.GtkObject, m.menuH)); (* TODO: Where should the unref be done? *)
				m.mi := LinGtk.gtk_menu_get_attach_widget(m.menuH);
				IF m.mi # NIL THEN LinGtk.gtk_menu_detach(m.menuH) END
			END;
			m := m.next
		END;
		(*
		IF old # NIL THEN LinGtk.gtk_container_remove(HostWindows.main, old) END;
		LinGtk.gtk_container_add(HostWindows.main, menuBar);
		*)
		HostWindows.SetMenu(menuBar);
		LinGtk.gtk_widget_show_all(menuBar);
(*
		res := USER32.SendMessageA(HostWindows.client, USER32.WMMDISetMenu, menuBar, winMenu);
		res := USER32.DrawMenuBar(HostWindows.main);
		WHILE USER32.RemoveMenu(old, 0, {USER32.MFByPosition}) # 0 DO END;
		res := USER32.DestroyMenu(old)*)
	END UpdateMenus;
	
	PROCEDURE TimerTick (notUsed0, notUsed1, notUsed2: INTEGER);
		VAR ops: Controllers.PollOpsMsg;
	BEGIN
		IF ~Log.synch THEN Log.FlushBuf END;
		HostWindows.Idle;
		Controllers.SetCurrentPath(Controllers.targetPath);
		Controllers.PollOps(ops);
		IF (ops.type # type) & (menus # NIL) (*& (USER32.GetMenu(HostWindows.main) = menuBar)*) THEN
			type := ops.type$;
			UpdateMenus
		END;
		Controllers.ResetCurrentPath()
	END TimerTick;		
	
	PROCEDURE DoTimerTick (data: LinLibc.PtrVoid): INTEGER;
		VAR ops: Controllers.PollOpsMsg;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		Kernel.Try(TimerTick, 0, 0, 0);
		Controllers.ResetCurrentPath();
		RETURN LinLibc.TRUE
	END DoTimerTick;		

	PROCEDURE Exit*;
	BEGIN
		LinGtk.gtk_signal_emit_by_name(HostWindows.main, "delete-event");
		IF HostCmds.quit THEN
			quit := TRUE
		END
(*		LinGtk.gtk_widget_destroy(HostWindows.main)*)
	END Exit;
	
	PROCEDURE SetFocus;
		VAR c: Containers.Controller; f: Views.Frame; v, s: Views.View;
	BEGIN
		f := Controllers.FocusFrame(); v := f.view;
		WITH v: Containers.View DO
			c := v.ThisController();
			s := c.Singleton();
			IF s # NIL THEN c.SetFocus(s) END
		ELSE
		END
	END SetFocus;
	
	PROCEDURE OpenWindow;
		VAR c: Containers.Controller; f: Views.Frame; v, s: Views.View; doc: Documents.Document;
			win: Windows.Window; title: Views.Title;
	BEGIN
		f := Controllers.FocusFrame(); v := f.view;
		WITH v: Containers.View DO
			c := v.ThisController();
			s := c.Singleton();
			IF (s # NIL) & (s.ThisModel() # NIL) THEN
				win := Windows.dir.Focus(Controllers.frontPath); ASSERT(win # NIL, 100);
				doc := win.doc.DocCopyOf(s);
				c := doc.ThisController();
				c.SetOpts(c.opts - {Documents.pageWidth, Documents.pageHeight}
										+ {Documents.winWidth, Documents.winHeight});
				(* Stores.InitDomain(doc, v.domain); done by DocCopyOf *)
				win.GetTitle(title);
				Windows.dir.OpenSubWindow(Windows.dir.New(), doc, {Windows.isAux}, title)
			END
		ELSE
		END
	END OpenWindow;		
	
	PROCEDURE HandleVerb (n: INTEGER);
		VAR v: Views.View; dvm: Properties.DoVerbMsg;
	BEGIN
		v := Containers.FocusSingleton();
		IF v # NIL THEN
			dvm.frame := Views.ThisFrame(Controllers.FocusFrame(), v);
			dvm.verb := SHORT(n);
			Views.HandlePropMsg(v, dvm)
		END
	END HandleVerb;
	
	PROCEDURE CheckVerb (v: Views.View; n: INTEGER; VAR pvm: Properties.PollVerbMsg);
	BEGIN
		pvm.verb := SHORT(n);
		pvm.label := "";
		pvm.disabled := FALSE; pvm.checked := FALSE;
		Views.HandlePropMsg(v, pvm)
	END CheckVerb;
	
	PROCEDURE PrimaryVerb*;
		VAR v: Views.View; pvm: Properties.PollVerbMsg;
	BEGIN
		v := Containers.FocusSingleton();
		IF v # NIL THEN
			CheckVerb(v, 0, pvm);
			IF pvm.label # "" THEN HandleVerb(0)
			ELSE  SetFocus
			END
		END
	END PrimaryVerb;
	
	PROCEDURE PopupMenu*;
		VAR f: Views.Frame; res, x, y: INTEGER; d: BOOLEAN; m: SET;
			menu: Menu; menuH:LinGtk.GtkMenu; w: LinGtk.GtkWidget;
	BEGIN
		f := Controllers.FocusFrame();
		IF (f # NIL) & f.front THEN
			menu := menus;
			WHILE (menu # NIL) & (~menu.isPopup OR (menu.type # "") & (menu.type # type)) DO menu := menu.next END;
			IF menu # NIL THEN 
				 menuH := menu.menuH 
			ELSE menuH := popMenu; Dialog.Beep END;
			IF menuH # 0 THEN 
			(*
				w := LinGtk.gtk_grab_get_current();
				IF w # NIL THEN LinGtk.gtk_grab_remove(w) END;
			*)
				Kernel.Try(PrepareMenu, menuH, 0, 0);
				LinGtk.gtk_menu_popup(menuH, NIL, NIL, 0, 0, 0, LinGdk.GDK_CURRENT_TIME); 
				LinGtk.gtk_widget_show_all(SYSTEM.VAL(LinGtk.GtkContainer, menuH))
			END; 
		(*
			IF USER32.GetCapture() # 0 THEN f.Input(x, y, m, d)
			ELSE x := (f.l + f.r) DIV 2; y := (f.t + f.b) DIV 2
			END;
			pt.x := (x + f.gx) DIV f.unit; pt.y := (y + f.gy) DIV f.unit;
			res := USER32.ClientToScreen(f.rider(HostPorts.Rider).port.wnd, pt);
			res := USER32.TrackPopupMenu(menuH, {1}, pt.x, pt.y + 2, 0, HostWindows.main, NIL)
		*)
		END
	END PopupMenu;
	
	PROCEDURE DispatchSpecialShortCuts (id: INTEGER);
		VAR res: INTEGER; 
	BEGIN

(*old := USER32.SetCursor(HostPorts.cursors[HostPorts.busyCursor]);*)
Dialog.ShowStatus("");

		DEC(gc);
		CASE id OF
		| iClose: HostCmds.Close
		| iUndo: StdCmds.Undo
		| iCut: HostCmds.Cut
		| iCopy: HostCmds.Copy
		| iPaste: HostCmds.Paste
		| iOpen: HostCmds.Open
		| iExit: Exit
		| iUpdateMenus: Dialog.Call("StdMenuTool.UpdateAllMenus", "", res)
		| iPopup: PopupMenu
		| iObjEdit: SetFocus
		| iObjOpen: OpenWindow
		| iProperties: StdCmds.ShowProp
(*	
		ELSE
			(* TODO: Can this ELSE be removed? *)
			IF id < firstId THEN HandleVerb(id - iVerb0)
			ELSE
				res := 0;	(* prevent from parasitic anchors on stack *)
				HandleCustomMenu(id)
			END		
*)		
		END;
		Properties.IncEra;

(*old := USER32.SetCursor(old)*)

	END DispatchSpecialShortCuts;
	
	(* RETURN LinLibc.TRUE -> remove event, RETURN LinLibc.FALSE -> let Gtk handle the event *)
	PROCEDURE TranslateAccelerators  (widget: LinGtk.GtkWidget; event: LinGdk.GdkEventKey; 
																	user_data: LinLibc.PtrVoid): INTEGER;
		VAR m: Menu; i: StdDialog.Item; id, code: INTEGER; ctrl, shift, alt, done: BOOLEAN; ch: CHAR;
			failed, ok: BOOLEAN; par: Dialog.Par; filter: SET;
	BEGIN
		done := FALSE;
		filter := {0..5};
		IF event.type = LinGdk.GDK_KEY_PRESS THEN
			code := event.keyval; (*msg.wParam;*) 
			id := 0; ch := 0X;
			shift := LinGdk.GDK_SHIFT_MASK IN event.state; (*USER32.GetKeyState(10H) < 0;*)
			ctrl := LinGdk.GDK_CONTROL_MASK IN event.state; (*USER32.GetKeyState(11H) < 0;*)
			alt := LinGdk.GDK_MOD1_MASK IN event.state;   (*ODD(msg.lParam DIV 20000000H);*)
			IF 1 IN filter THEN
				(* TODO: Check the charachter codes here (gdkkeysyms.h) *)
				IF shift & (code = 0FFC7H (*79H*)) THEN id := iPopup	(* shift F10 *)
				ELSIF alt THEN
					IF code = 0FF08H (*08H*) THEN id := iUndo	(* alt bs *)
					ELSIF code = 0FF0DH (*0DH*) THEN id := iProperties	(* alt enter *)
(*					ELSIF code = 5DH THEN id := iProperties	(* alt application *) ??? TODO: application?*)
					ELSIF (code = minusCode) & shift THEN ch := NBHYPHEN
					ELSIF (code = ORD(" ")) & shift THEN ch := NBSPACE
					END
				ELSIF ctrl THEN
					IF code = ORD(" ") THEN
						IF shift THEN ch := NUMSPACE END
					ELSIF code = 0FF63H (*2DH*) THEN
						id := iCopy	(* ctrl insert *)
					ELSIF code = minusCode THEN
						IF shift THEN ch := SOFTHYPHEN ELSE ch := HYPHEN END
					END
				ELSIF shift THEN
					IF code = 0FFFFH (*2EH*) THEN id := iCut	(* shift delete *)
					ELSIF code = 0FF63H (*2DH*) THEN id := iPaste	(* shift insert *)
					END
(*				ELSIF code = 5DH THEN id := iPopup	(* application *) ??? TODO: application?*)
(*				ELSIF code = 0FF1BH (*1BH*) THEN done := TRUE	(* esc *) TODO: why? *)
				END;
				(*TODO: If ch was change a new event needs to be sent, or can you change the current event? *)
				IF id # 0 THEN
					DispatchSpecialShortCuts(id);
					done := TRUE
				END
			END;
			IF ~done THEN
				ch := CHR(code); IF (ch >= "a") & (ch <= "z") THEN ch := CAP(ch) END;
				code := ORD(ch);
				IF ~alt & (ctrl OR (code >= 70H) & (code <= 7FH)) (* function key *) THEN
					m := menus;
					WHILE (m # NIL) & ~done DO
						IF ((m.type = "") OR (m.type = type)) & ~m.isPopup & (m.class IN filter) THEN
							i := m.firstItem;
							WHILE (i # NIL) &
								((i(Item).code # code) OR (i(Item).ctrl # ctrl) OR (i(Item).shift # shift)) DO i := i.next END;
							IF i # NIL THEN
								IF i.filter^ # "" THEN StdDialog.CheckFilter(i, failed, ok , par) END;
								IF (i.filter^ = "") OR ~failed & ~par.disabled THEN 
									LinGtk.gtk_menu_item_activate(i(Item).mi)
								END;
								done := TRUE
							END
						END;
						m := m.next
					END
				ELSIF alt & ~ctrl & ~shift THEN (* search hot keys *)
					m := menus;
					WHILE (m # NIL) & ~done DO
						IF m.hot = code THEN
						(*
							m.mi := LinGtk.gtk_menu_get_attach_widget(m.menuH);
							LinGtk.gtk_menu_item_activate(m.mi);
							*)
							(*
							LinGtk.gtk_menu_popup(m.menuH, menuBar, m.mi, 0, 0, 1,
																			LinGdk.GDK_CURRENT_TIME);
							*)
							(*
							m.mi := LinGtk.gtk_menu_get_attach_widget(m.menuH);
							LinGtk.gtk_menu_shell_activate_item(SYSTEM.VAL(LinGtk.GtkMenuShell, menuBar),
																						m.mi, LinLibc.FALSE);
							LinGtk.gtk_grab_add(menuBar);
							*)
(*							LinGtk.gtk_menu_item_select(SYSTEM.VAL(LinGtk.GtkMenuItem, m.mi));*)
							LinGtk.gtk_menu_shell_select_item(SYSTEM.VAL(LinGtk.GtkMenuShell, menuBar), m.mi);
							done := TRUE
						END;
						m := m.next
					END
				END
			END
		END;
		IF done THEN
			RETURN LinLibc.TRUE
		ELSE
			RETURN LinLibc.FALSE
		END
	END TranslateAccelerators;

	PROCEDURE PathToSpec (VAR path: ARRAY OF CHAR; VAR loc: Files.Locator; VAR name: Files.Name);
		VAR i, j: INTEGER; ch: CHAR;
	BEGIN
		i := 0; j := 0; loc := Files.dir.This("");
		WHILE (loc.res = 0) & (i < LEN(path) - 1) & (j < LEN(name) - 1) & (path[i] # 0X) DO
			ch := path[i]; INC(i);
			IF (j > 0) & ((ch = "/") OR (ch = "\")) THEN
				name[j] := 0X; j := 0;
				IF name = "*" THEN
					IF Dialog.language # "" THEN loc := loc.This(Dialog.language) END
				ELSE loc := loc.This(name)
				END
			ELSE
				name[j] := ch; INC(j)
			END
		END;
		IF path[i] = 0X THEN name[j] := 0X
		ELSE loc.res := 1; name := ""
		END
	END PathToSpec;
	
	PROCEDURE OpenFile (VAR name: ARRAY OF CHAR; l, t, r, b: INTEGER; VAR ok: BOOLEAN);
		VAR res: INTEGER; loc: Files.Locator; (*np: KERNEL32.StringPtr; *)(*path: HostFiles.FullName;*)
			file: Files.Name; v: Views.View; conv: Converters.Converter; f: Files.File;
			sp: ARRAY 260 OF SHORTCHAR;
	BEGIN
		ok := FALSE;
		PathToSpec(name, loc, file);
		IF file # "" THEN
			f := Files.dir.Old(loc, file, Files.shared);
			IF f # NIL THEN
				conv := Converters.list;
				WHILE (conv # NIL) & (conv.fileType # f.type) DO conv := conv.next END;
				IF conv = NIL THEN
					conv := Converters.list;
					WHILE (conv # NIL) & ~(impAll IN conv.opts) DO conv := conv.next END
				END;
				IF f.type = "" THEN file := file + "." END;
				v := Views.Old(Views.dontAsk, loc, file, conv);
				IF v # NIL THEN
					Windows.dir.l := l; Windows.dir.t := t; Windows.dir.r := r; Windows.dir.b := b;
					Views.Open(v, loc, file, conv); ok := TRUE;
					Windows.dir.l := 0; Windows.dir.t := 0; Windows.dir.r := 0; Windows.dir.b := 0
				END
			END
		END
	END OpenFile;
	
	PROCEDURE IncludingFileCommandLine(IN line: ARRAY OF CHAR): POINTER TO ARRAY OF CHAR;
		VAR f: Files.File; r: Files.Reader; i, len: INTEGER; 
			header: ARRAY 12 OF BYTE; keyword: ARRAY 12 OF CHAR;
			b: POINTER TO ARRAY OF BYTE;
			l2: POINTER TO ARRAY OF CHAR;
	BEGIN
		len := LEN(line$);
		f := Files.dir.Old(Files.dir.This(cmdLinePath), cmdLineFile, Files.shared);
		IF (f # NIL) & (f.Length() > LEN(header)) THEN
			r := f.NewReader(NIL); r.ReadBytes(header, 0, LEN(header));
			FOR i := 0 TO LEN(header) - 1 DO keyword[i] := CHR(header[i]) END;
			keyword[LEN(keyword) - 1] := 0X;
			IF keyword = 'COMMANDLINE' THEN
				NEW(b, f.Length() - LEN(header)); NEW(l2, LEN(b) + len + 1);
				r.ReadBytes(b, 0, LEN(b));
				FOR i := 0 TO len - 1 DO l2[i] := line[i] END; l2[i] := " ";
				FOR i := 0 TO LEN(b) - 1 DO l2[i + len + 1] := SHORT(CHR(b[i])) END;
				RETURN l2
			END
		END;
		NEW(l2, len); 
		FOR i := 0 TO len - 1 DO l2[i] := line[i] END;
		RETURN l2
	END IncludingFileCommandLine;
	
	PROCEDURE ReadCommandLine (IN line: ARRAY OF CHAR; open: BOOLEAN);
		VAR name, opt: ARRAY 260 OF CHAR; i, l, t, r, b, res: INTEGER;
			ok: BOOLEAN; ln: ARRAY 260 OF CHAR;
		
		PROCEDURE CopyName;
			VAR ch, tch: CHAR; j: INTEGER;
		BEGIN
			j := 0; ch := line[i]; tch := " ";
			WHILE ch = " " DO INC(i); ch := line[i] END;
			IF (ch = "'") OR (ch = '"') THEN tch := ch; INC(i); ch := line[i] END;
			WHILE (ch >= " ") & (ch # tch) DO
				name[j] := ch;
				IF (ch >= "a") & (ch <= "z") OR (ch >= "à") & (ch <= "ö") OR (ch >= "ø") & (ch <= "þ") THEN ch := CAP(ch)
				ELSIF ch = "-" THEN ch := "/"
				END;
				opt[j] := ch; INC(j); INC(i); ch := line[i]
			END;
			IF ch > " " THEN INC(i); ch := line[i] END;
			WHILE (ch # 0X) & (ch <= " ") DO INC(i); ch := line[i] END;
			name[j] := 0X; opt[j] := 0X
		END CopyName;
		
	BEGIN
		l := 0; t := 0; r := 0; b := 0; i := 0;
		CopyName;	(* skip program name *)
		WHILE line[i] > " " DO
			CopyName;
			IF opt = "/LOAD" THEN	(* load module *)
				CopyName; ln := name$;
				IF open THEN Kernel.LoadMod(ln) END
			ELSIF opt = "/USE" THEN	(* use directory *)
				CopyName	(* working directory: handled in HostFiles *)
			ELSIF opt = "/P" THEN	(* print file *)	(* to be completed !!! *)
				CopyName;
				IF open THEN
					OpenFile(name, 0, 0, 0, 0, ok);
					IF ok THEN HostCmds.Print END
				END
			ELSIF opt = "/PT" THEN	(* print file to printer *)
				CopyName; CopyName; CopyName; CopyName	(* to be completed !!! *)
	(*
			ELSIF opt = "/EMBEDDING" THEN	(* start as server *)
				IF ~open THEN state := embedded END
			ELSIF opt = "/NOAPPWIN" THEN	(* start without application window *)
				IF ~open THEN state := noAppWin; HostWindows.noAppWin := TRUE END
			ELSIF opt = "/NOSCROLL" THEN	(* no scroll bars in  application window *)
				HostWindows.noClientScroll := TRUE
			ELSIF opt = "/FULLSIZE" THEN
				HostWindows.fullSize := TRUE
	*)	
			ELSIF opt = "/LTRB" THEN	(* window position *)
				CopyName; ln := name$; Strings.StringToInt(ln, l, res);
				CopyName; ln := name$; Strings.StringToInt(ln, t, res);
				CopyName; ln := name$; Strings.StringToInt(ln, r, res);
				CopyName; ln := name$; Strings.StringToInt(ln, b, res)
			ELSIF opt = "/LANG" THEN
				CopyName; ln := name$;
				IF LEN(ln$) = 2 THEN Strings.ToLower(ln, ln); Dialog.SetLanguage(ln$, Dialog.nonPersistent) END
			ELSIF opt = "/O" THEN	(* open file *)
				CopyName; (*openUsed := TRUE;*)
				IF open THEN OpenFile(name, l, t, r, b, ok) END;
				l := 0; t := 0; r := 0; b := 0
			ELSIF opt = "/PAR" THEN
				CopyName;
				Dialog.commandLinePars := name$
			ELSE	(* open file *)
				IF open THEN OpenFile(name, l, t, r, b, ok) END;
				l := 0; t := 0; r := 0; b := 0
			END
		END
	END ReadCommandLine;
	
	PROCEDURE Quit (object: LinGtk.GtkObject; func_data: LinLibc.PtrVoid);
	BEGIN
		quit := TRUE
	END Quit;
	
	PROCEDURE TryQuit (notUsed0, notUsed1, notUsed2: INTEGER);
	BEGIN
		HostCmds.Quit
	END TryQuit;
	
	(* retrun 0 -> close ok. return 1 -> don't close *)
	PROCEDURE DeleteHandler(widget: LinGtk.GtkWidget; event: LinGdk.GdkEvent; user_data: LinLibc.PtrVoid): INTEGER;
	BEGIN
		Controllers.SetCurrentPath(Controllers.targetPath);
		Kernel.Try(TryQuit, 0, 0, 0);
		IF HostCmds.quit THEN
(*			HostWindows.SaveWindowState; TODO: implement*)
			Controllers.ResetCurrentPath();
			RETURN 0
		ELSE 
			gc := 0;
			Controllers.ResetCurrentPath();
			RETURN 1
		END
	END DeleteHandler;
	
	PROCEDURE StyleSet (widget: LinGtk.GtkWidget; previous_style: LinGtk.GtkStyle; user_data: INTEGER);
	BEGIN
		HostPorts.ResetColors(widget)
	END StyleSet;

	PROCEDURE OpenApp*;
		VAR res: INTEGER; mb, mi, si: LinGtk.GtkWidget; menu: LinGtk.GtkMenu;
	BEGIN
		HostWindows.CreateMainWindows;
		res := LinGtk.gtk_signal_connect(HostWindows.main, "destroy", 
													SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(Quit)), 0);
		res := LinGtk.gtk_signal_connect(HostWindows.main, "delete-event", 
													SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(DeleteHandler)), 0);
		res := LinGtk.gtk_signal_connect(HostWindows.main, "style-set", 
													SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(StyleSet)), 0);
													
		(* copy/paste functionallity (selections) *)
		res := LinGtk.gtk_signal_connect(HostWindows.main, "selection-received", 
													SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(HostClipboard.DoPaste)), 0);
		LinGtk.gtk_selection_add_target(HostWindows.main, LinGdk.GDK_SELECTION_PRIMARY, 																								LinGdk.GDK_TARGET_STRING, LinGdk.GDK_SELECTION_TYPE_STRING);
		LinGtk.gtk_selection_add_target(HostWindows.main, LinGdk.GDK_SELECTION_PRIMARY, 																								HostClipboard.bbAtom, HostClipboard.bbAtom);
		res := LinGtk.gtk_signal_connect(HostWindows.main, "selection-get", 
													SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(HostClipboard.ConvertCopy)), 0);
		res := LinGtk.gtk_signal_connect(HostWindows.main, "selection-clear-event", 
													SYSTEM.VAL(LinGtk.GtkSignalFunc, SYSTEM.ADR(HostClipboard.Clear)), 0);
		
		(* timer ticks *)
		res := LinGtk.gtk_timeout_add(idlePeriod, DoTimerTick, 0);
		
		(* install a keysnooper to handle shortcuts *)
		res := LinGtk.gtk_key_snooper_install(TranslateAccelerators, 0)
	END OpenApp;
	
	PROCEDURE Loop;
		VAR res, n: INTEGER;
	BEGIN
		HostWindows.ShowMain;
(*					ReadCommandLine(s, TRUE);*)
		quit := FALSE;
		gc := gcCycle;
		n := 0;
		WHILE ~quit DO
			Services.actionHook.Loop;
			INC(n);
			IF (n > num) OR (LinGtk.gtk_events_pending() = LinLibc.FALSE) THEN 
				Windows.dir.Update(NIL); n := 0
			ELSE
				res := LinGtk.gtk_main_iteration()
			END;
			IF gc <= 0 THEN
				Kernel.Collect;
				gc := gcCycle
			END;
		END;
		Kernel.Quit(0)
	END Loop;

(*
	PROCEDURE Loop;
		VAR res, n: INTEGER;
	BEGIN
		HostWindows.ShowMain;
		quit := FALSE;
		gc := gcCycle;
		n := 0;
		WHILE ~quit DO
			Services.actionHook.Loop;
			INC(n);
			IF (n > num) OR (LinGtk.gtk_events_pending() = LinLibc.FALSE) THEN 
				Windows.dir.Update(NIL); n := 0
			ELSE
				res := LinGtk.gtk_main_iteration()
			END;
			IF gc <= 0 THEN
				Kernel.Collect;
				gc := gcCycle
			END;
		END;
		Kernel.Quit(0)
	END Loop;
*)	
(*	
	PROCEDURE Loop;
		VAR res, n: INTEGER; e: LinGdk.GdkEvent;
	BEGIN
		HostWindows.ShowMain;
		quit := FALSE;
		gc := gcCycle;
		n := 0;
		WHILE ~quit DO
			Services.actionHook.Loop;
			INC(n);
			IF (n > num) OR (LinGtk.gtk_events_pending() = LinLibc.FALSE) THEN 
				Windows.dir.Update(NIL); n := 0;
(*				Services.actionHook.Step*)
			ELSE
			
	(*			e := LinGdk.gdk_event_peek();*)
				e := LinGdk.gdk_event_get();
				IF (e # NIL) & (e.type = LinGdk.GDK_BUTTON_PRESS) THEN
					HostCFrames.FilterMouseEvent(e);
				END;
				IF e # NIL THEN 
					LinGdk.gdk_event_put(e);
(*					LinGtk.gtk_main_do_event(e);*)
					LinGdk.gdk_event_free(e) 
				END;
				res := LinGtk.gtk_main_iteration()
			END;
			IF gc <= 0 THEN
				Kernel.Collect;
				gc := gcCycle
			END;
		END;
		Kernel.Quit(0)
	END Loop;
*)	

	PROCEDURE Run*;
	BEGIN
(*		ReadCommandLine(IncludingFileCommandLine(Kernel.cmdLine), TRUE);*)
		ReadCommandLine(Kernel.cmdLine, TRUE);
		Kernel.Start(Loop)
	END Run;
	
	PROCEDURE SetNum* (n: INTEGER);
	BEGIN
		num := n
	END SetNum;
	
	PROCEDURE Init;
	BEGIN
		num := 10;
		popMenu := 0;
		
(*		minusCode := USER32.VkKeyScanA("-");
		IF minusCode # -1 THEN minusCode := minusCode MOD 256 END;*)
		minusCode := -1; (* TODO: *)
	END Init;
	
BEGIN
	Init
(* TODO: do we need a close section to clean up the menus? *)
END HostMenus.