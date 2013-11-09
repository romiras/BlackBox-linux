MODULE LinGdk ["libgdk.so"];

	IMPORT LinLibc, LinX11;
	
	CONST
	
		FALSE* = 0;
		TRUE* = 1;
		
		GDK_CURRENT_TIME* = 0H;
				
		(* possible values of GdkFontType *)
		GDK_FONT_FONT* = 0;
		GDK_FONT_FONTSET* = 1;
		
		(* possible values for GdkEventType *)
		GDK_NOTHING* = -1;
		GDK_DELETE* = 0;
		GDK_DESTROY* = 1;
		GDK_EXPOSE* = 2;
		GDK_MOTION_NOTIFY* = 3;
		GDK_BUTTON_PRESS* = 4;
		GDK_2BUTTON_PRESS* = 5;
		GDK_3BUTTON_PRESS* = 6;
		GDK_BUTTON_RELEASE* = 7;
		GDK_KEY_PRESS* = 8;
		GDK_KEY_RELEASE* = 9;
		GDK_ENTER_NOTIFY* = 10;
		GDK_LEAVE_NOTIFY* = 11;
		GDK_FOCUS_CHANGE* = 12;
		GDK_CONFIGURE* = 13;
		GDK_MAP* = 14;
		GDK_UNMAP* = 15;
		GDK_PROPERTY_NOTIFY* = 16;
		GDK_SELECTION_CLEAR* = 17;
		GDK_SELECTION_REQUEST* = 18;
		GDK_SELECTION_NOTIFY* = 19;
		GDK_PROXIMITY_IN* = 20;
		GDK_PROXIMITY_OUT* = 21;
		GDK_DRAG_ENTER* = 22;
		GDK_DRAG_LEAVE* = 23;
		GDK_DRAG_MOTION* = 24;
		GDK_DRAG_STATUS* = 25;
		GDK_DROP_START* = 26;
		GDK_DROP_FINISHED* = 27;
		GDK_CLIENT_EVENT* = 28;
		GDK_VISIBILITY_NOTIFY* = 29;
		GDK_NO_EXPOSE* = 30;

		(* possible values for GdkInputSource *)
		GDK_SOURCE_MOUSE* = 0;
		GDK_SOURCE_PEN* = 1;
		GDK_SOURCE_ERASER* = 2;
		GDK_SOURCE_CURSOR* = 3;
		
		(* possible values for GdkEventMask *)
         GDK_EXPOSURE_MASK* = {1};
         GDK_POINTER_MOTION_MASK* = {2};
         GDK_POINTER_MOTION_HINT_MASK* = {3};
         GDK_BUTTON_MOTION_MASK* = {4};
         GDK_BUTTON1_MOTION_MASK* = {5};
         GDK_BUTTON2_MOTION_MASK* = {6};
         GDK_BUTTON3_MOTION_MASK* = {7};
         GDK_BUTTON_PRESS_MASK* = {8};
         GDK_BUTTON_RELEASE_MASK* = {9};
         GDK_KEY_PRESS_MASK* = {10};
         GDK_KEY_RELEASE_MASK* = {11};
         GDK_ENTER_NOTIFY_MASK* = {12};
         GDK_LEAVE_NOTIFY_MASK* = {13};
         GDK_FOCUS_CHANGE_MASK* = {14};
         GDK_STRUCTURE_MASK* = {15};
         GDK_PROPERTY_CHANGE_MASK* = {16};
         GDK_VISIBILITY_NOTIFY_MASK* = {17};
         GDK_PROXIMITY_IN_MASK* = {18};
         GDK_PROXIMITY_OUT_MASK* = {19};
         GDK_SUBSTRUCTURE_MASK* = {20};
         GDK_ALL_EVENTS_MASK* = {0 .. 21};  (* 0x0FFFFF *)

		(* possible values for modifier states (GdkModifierType) *)
         GDK_SHIFT_MASK* = 0;
         GDK_LOCK_MASK* = 1;
         GDK_CONTROL_MASK* = 2;
         GDK_MOD1_MASK* = 3;
         GDK_MOD2_MASK* = 4;
         GDK_MOD3_MASK*  = 5;
         GDK_MOD4_MASK* = 6;
         GDK_MOD5_MASK* = 7;
         GDK_BUTTON1_MASK* = 8;
         GDK_BUTTON2_MASK* = 9;
         GDK_BUTTON3_MASK* = 10;
         GDK_BUTTON4_MASK* = 11;
         GDK_BUTTON5_MASK* = 12;
         GDK_RELEASE_MASK* =  13;
         GDK_MODIFIER_MASK* = 03FFFH;

		(* key constants from gdk/gdkkeysym.h *)
		GDK_Shift_L* = 0FFE1H;
		GDK_Shift_R* = 0FFE2H;
		GDK_Control_L* = 0FFE3H;
		GDK_Control_R* = 0FFE4H;
		GDK_Alt_L* = 0FFE9H;
		GDK_Alt_R* = 0FFEAH;
		GDK_Scroll_Lock* = 0FF14H;
		
		(* Possible values for GdkFunction *)
		GDK_COPY* = 0;
		GDK_INVERT* = 1;
		GDK_XOR* = 2;
		GDK_CLEAR* = 3;
		GDK_AND* = 4;
		GDK_AND_REVERSE* = 5;
		GDK_AND_INVERT* = 6;
		GDK_NOOP* = 7;
		GDK_OR* = 8;
		GDK_EQUIV* = 9;
		GDK_OR_REVERSE* = 10;
		GDK_COPY_INVERT* = 11;
		GDK_OR_INVERT* = 12;
		GDK_NAND* = 13;
		GDK_SET* = 14;
	
		(* Possible values for GdkFill *)  
		GDK_SOLID*= 0;
		GDK_TILED* = 1;
		GDK_STIPPLED* = 2;
		GDK_OPAQUE_STIPPLED* = 3;
		
		(* Possible values for GdkSubwindowMode *)  
		GDK_CLIP_BY_CHILDREN* = 0;
		GDK_INCLUDE_INFERIORS* = 1;
		
		(* Possible values for GdkLineStyle *)
		GDK_LINE_SOLID* = 0;
		GDK_LINE_ON_OFF_DASH* = 1;
		GDK_LINE_DOUBLE_DASH* = 2;
		
		(* Possible values for GdkCapStyle *)
		GDK_CAP_NOT_LAST* = 0;
		GDK_CAP_BUTT* = 1;
		GDK_CAP_ROUND* = 2;
		GDK_CAP_PROJECTING* = 3;
		
		(* Possible values for GdkJoinStyle *)
		GDK_JOIN_MITER* = 0;
		GDK_JOIN_ROUND* = 1;
		GDK_JOIN_BEVEL* = 2;
		
		(* Possible values for GdkGCValuesMask *)
		GDK_GC_FOREGROUND* = {0};  
		GDK_GC_BACKGROUND* = {1};
		GDK_GC_FONT* = {2};
		GDK_GC_FUNCTION* = {3};
		GDK_GC_FILL* = {4};
		GDK_GC_TILE* = {5};
		GDK_GC_STIPPLE* = {6};
		GDK_GC_CLIP_MASK* = {7};
		GDK_GC_SUBWINDOW* = {8};
		GDK_GC_TS_X_ORIGIN* = {9};
		GDK_GC_TS_Y_ORIGIN* = {10};
		GDK_GC_CLIP_X_ORIGIN* = {11};
		GDK_GC_CLIP_Y_ORIGIN* = {12};
		GDK_GC_EXPOSURES* = {13};
		GDK_GC_LINE_WIDTH* = {14};
		GDK_GC_LINE_STYLE* = {15};
		GDK_GC_CAP_STYLE* = {16};
		GDK_GC_JOIN_STYLE* = {17};
		
		(* selections *)
		GDK_SELECTION_PRIMARY* = 1;
		GDK_SELECTION_SECONDARY* = 2;
		
		(* targets (GdkTarget) *)
		GDK_TARGET_BITMAP* = 5;
		GDK_TARGET_COLORMAP* = 7;
		GDK_TARGET_DRAWABLE* = 17;
		GDK_TARGET_PIXMAP* = 20;
		GDK_TARGET_STRING* = 31;
		
		(* selection types (GdkSelectionType) *)
		GDK_SELECTION_TYPE_ATOM* = 4;
		GDK_SELECTION_TYPE_BITMAP* = 5;
		GDK_SELECTION_TYPE_COLORMAP* = 7;
		GDK_SELECTION_TYPE_DRAWABLE* = 17;
		GDK_SELECTION_TYPE_INTEGER* = 19;
		GDK_SELECTION_TYPE_PIXMAP* = 20;
		GDK_SELECTION_TYPE_WINDOW* = 33;
		GDK_SELECTION_TYPE_STRING* = 31;
		
		(* cursor types *)
		GDK_LAST_CURSOR* = 155;
		GDK_CURSOR_IS_PIXMAP* = -1;
		GDK_NUM_GLYPHS* = 154;
		GDK_X_CURSOR* = 0;
		GDK_ARROW* = 2;
		GDK_BASED_ARROW_DOWN* = 4;
		GDK_BASED_ARROW_UP* = 6;
		GDK_BOAT* = 8;
		GDK_BOGOSITY* = 10;
		GDK_BOTTOM_LEFT_CORNER* = 12;
		GDK_BOTTOM_RIGHT_CORNER* = 14;
		GDK_BOTTOM_SIDE* = 16;
		GDK_BOTTOM_TEE* = 18;
		GDK_BOX_SPIRAL* = 20;
		GDK_CENTER_PTR* = 22;
		GDK_CIRCLE* = 24;
		GDK_CLOCK* = 26;
		GDK_COFFEE_MUG* = 28;
		GDK_CROSS* = 30;
		GDK_CROSS_REVERSE* = 32;
		GDK_CROSSHAIR* = 34;
		GDK_DIAMOND_CROSS* = 36;
		GDK_DOT* = 38;
		GDK_DOTBOX* = 40;
		GDK_DOUBLE_ARROW* = 42;
		GDK_DRAFT_LARGE* = 44;
		GDK_DRAFT_SMALL* = 46;
		GDK_DRAPED_BOX* = 48;
		GDK_EXCHANGE* = 50;
		GDK_FLEUR* = 52;
		GDK_GOBBLER* = 54;
		GDK_GUMBY* = 56;
		GDK_HAND1* = 58;
		GDK_HAND2* = 60;
		GDK_HEART* = 62;
		GDK_ICON* = 64;
		GDK_IRON_CROSS* = 66;
		GDK_LEFT_PTR* = 68;
		GDK_LEFT_SIDE* = 70;
		GDK_LEFT_TEE* = 72;
		GDK_LEFTBUTTON* = 74;
		GDK_LL_ANGLE* = 76;
		GDK_LR_ANGLE* = 78;
		GDK_MAN* = 80;
		GDK_MIDDLEBUTTON* = 82;
		GDK_MOUSE* = 84;
		GDK_PENCIL* = 86;
		GDK_PIRATE* = 88;
		GDK_PLUS* = 90;
		GDK_QUESTION_ARROW* = 92;
		GDK_RIGHT_PTR* = 94;
		GDK_RIGHT_SIDE* = 96;
		GDK_RIGHT_TEE* = 98;
		GDK_RIGHTBUTTON* = 100;
		GDK_RTL_LOGO* = 102;
		GDK_SAILBOAT* = 104;
		GDK_SB_DOWN_ARROW* = 106;
		GDK_SB_H_DOUBLE_ARROW* = 108;
		GDK_SB_LEFT_ARROW* = 110;
		GDK_SB_RIGHT_ARROW* = 112;
		GDK_SB_UP_ARROW* = 114;
		GDK_SB_V_DOUBLE_ARROW* = 116;
		GDK_SHUTTLE* = 118;
		GDK_SIZING* = 120;
		GDK_SPIDER* = 122;
		GDK_SPRAYCAN* = 124;
		GDK_STAR* = 126;
		GDK_TARGET* = 128;
		GDK_TCROSS* = 130;
		GDK_TOP_LEFT_ARROW* = 132;
		GDK_TOP_LEFT_CORNER* = 134;
		GDK_TOP_RIGHT_CORNER* = 136;
		GDK_TOP_SIDE* = 138;
		GDK_TOP_TEE* = 140;
		GDK_TREK* = 142;
		GDK_UL_ANGLE* = 144;
		GDK_UMBRELLA* = 146;
		GDK_UR_ANGLE* = 148;
		GDK_WATCH* = 150;
		GDK_XTERM* = 152;
		
		(* possible values for GdkFilterReturn *)
		GDK_FILTER_CONTINUE* = 0;	  (* Event not handled, continue processesing *)
		GDK_FILTER_TRANSLATE* = 1;	  (* Translated event stored *)
		GDK_FILTER_REMOVE* = 2;	  (* Terminate processing, removing event *)
		
	
	TYPE
		GdkAtom* = INTEGER;
		GdkBitmap* = LinLibc.PtrVoid;
		GdkCapStyle* = INTEGER;
		GdkColor* = POINTER TO GdkColorDesc;
		GdkColorDesc* = RECORD [untagged] pixel*: INTEGER; red*, green*, blue*: SHORTINT END;
		GdkColormap* = POINTER TO GdkColormapDesc;
		GdkColormapDesc* = RECORD [untagged] size*: INTEGER; colors*: GdkColor END;
		GdkCursor* = POINTER TO GdkCursorDesc;
		GdkCursorDesc* = RECORD [untagged] type: GdkCursorType END;
		GdkCursorType* = INTEGER;
		GdkDrawable* = LinLibc.PtrVoid;
		GdkEventMask* = SET;
		GdkEventType* = INTEGER; (* enum, for possible values see above. *)
		GdkFill* = INTEGER;
		GdkFilterFunc* = PROCEDURE (xevent: GdkXEvent; event: GdkEvent; user_data: LinLibc.PtrVoid): GdkFilterReturn;
		GdkFilterReturn* = INTEGER;
		GdkFontType* = INTEGER;
		GdkFont* = POINTER TO RECORD [untagged]
			type*: GdkFontType;
			ascent*, descent*: INTEGER;
		END;
		GdkFunction* = INTEGER;
		GdkGC* = POINTER TO GdkGCDesc;
		GdkGCDesc* = RECORD [untagged] dummy_var*: INTEGER END;
		GdkInputSource* = INTEGER;
		GdkJoinStyle* = INTEGER;
		GdkLineStyle* = INTEGER;
		GdkModifierType* = SET;
		GdkPixmap* = LinLibc.PtrVoid;
		GdkPointDesc* = RECORD [untagged] x*, y*: SHORTINT END;
		GdkPoint* = POINTER [untagged] TO GdkPointDesc;
		GdkRectangle* = POINTER TO GdkRectangleDesc;
		GdkRectangleDesc* = RECORD [untagged] x*, y*, width*, height*: SHORTINT END;
		GdkSubwindowMode* = INTEGER;
		GdkGCValuesMask* = SET;
		GdkVisual* = LinLibc.PtrVoid;
		GdkWChar* = POINTER TO ARRAY [untagged] OF GdkWCharDesc;
		GdkWCharDesc* = INTEGER;
		GdkWindow* = LinLibc.PtrVoid;
		GdkXEvent* = LinLibc.PtrVoid;
		
		
		(* Event types*)
		
		GdkEvent* = POINTER TO RECORD [untagged] (* Base type, union of all events *)
			type*: GdkEventType;
		END;
		
		GdkEventButton* = POINTER TO GdkEventButtonDesc;
		GdkEventButtonDesc* = RECORD [untagged]
			type*: GdkEventType;
			window*: GdkWindow;
			send_event*: BYTE;
			time*: INTEGER;
			x*, y*, pressure*, xtilt*, ytilt: REAL;
			state*: GdkEventMask;
			button*: INTEGER;
			source*: GdkInputSource;
			deviceid*: INTEGER;
			x_root*, y_root*: REAL
		END;
		GdkEventConfigure* = POINTER TO RECORD [untagged]
			type*: GdkEventType;
			window*: GdkWindow;
			send_event*: BYTE;
			x*, y*, width*, height*: SHORTINT
		END;
		GdkEventExpose* = POINTER TO RECORD [untagged] 
			type*: GdkEventType;
			window*: GdkWindow;
			send_event*: BYTE;
			area*: GdkRectangleDesc;
			count*: INTEGER
		END;
		GdkEventFocus* = POINTER TO RECORD [untagged]
			type*: GdkEventType;
			window*: GdkWindow;
			send_event*: BYTE;
			in*: SHORTINT
		END;
		GdkEventKey* = POINTER TO GdkEventKeyDesc;
		GdkEventKeyDesc* = RECORD [untagged]
			type*: GdkEventType;
			window*: GdkWindow;
			send_event*: BYTE;
			time*: INTEGER;
			state*: SET;
			keyval*: INTEGER;
			length*: INTEGER;
			string*: LinLibc.PtrSTR;
		END;
		GdkEventMotion* = POINTER TO RECORD [untagged]
			type*: GdkEventType;
			window*: GdkWindow;
			send_event*: BYTE;
			time*: INTEGER;
			x*, y*, pressure*, xtilt*, ytilt: REAL;
			state*: SET;
			is_hint*: SHORTINT;
			source*: GdkInputSource;
			deviceid*: INTEGER;
			x_root*, y_root*: REAL
		END;

		GdkGCValues* = POINTER TO GdkGCValuesDesc;
		GdkGCValuesDesc* = RECORD [untagged]
			foreground*, background*: GdkColorDesc;
			font*: GdkFont;
			function*: GdkFunction;
			fill*: GdkFill;
			tile*, stipple*, clip_mask*: GdkPixmap;
			subwindow_mode*: GdkSubwindowMode;
			ts_x_origin*, ts_y_origin*, clip_x_origin*, clip_y_origin*, graphics_exposures*, line_width*: INTEGER;
			line_style*: GdkLineStyle;
			cap_style*: GdkCapStyle;
			join_style*: GdkJoinStyle;
		END;
 
	VAR
		gdk_display*: LinX11.Display;


	PROCEDURE gdk_add_client_message_filter* (message_type: GdkAtom; func: GdkFilterFunc; user_data: LinLibc.PtrVoid);
	PROCEDURE gdk_atom_intern* (IN atom_name: LinLibc.PtrSTR; only_if_exists: INTEGER): GdkAtom;
	PROCEDURE gdk_beep*;
	PROCEDURE gdk_char_width* (font: GdkFont; char: SHORTCHAR): INTEGER;
	PROCEDURE gdk_char_width_wc* (font: GdkFont; char: GdkWCharDesc): INTEGER;
	PROCEDURE gdk_colormap_get_system* (): GdkColormap;
	PROCEDURE gdk_colormap_alloc_color* (colormap: GdkColormap; color: GdkColor; writable, best_match: INTEGER): INTEGER;
	PROCEDURE gdk_color_change* (colormap: GdkColormap; color: GdkColor): INTEGER;
	PROCEDURE gdk_color_parse* (spec: LinLibc.PtrSTR; color: GdkColor):INTEGER;
	PROCEDURE gdk_cursor_new* (type: GdkCursorType): GdkCursor;
	PROCEDURE gdk_draw_arc* (drawable: GdkDrawable; gc: GdkGC; filled, x, y, width, height, angle1, angle2: INTEGER);
	PROCEDURE gdk_draw_line* (drawable: GdkDrawable; gc: GdkGC; x1, y1, x2, y2: INTEGER);
	PROCEDURE gdk_draw_lines* (drawable: GdkDrawable; gc: GdkGC; points: GdkPoint; npoints: INTEGER);
	PROCEDURE gdk_draw_pixmap* (drawable: GdkDrawable; gc: GdkGC; src: GdkDrawable; 
												xsrc, ysrc, xdest, ydest, width, height: INTEGER);
	PROCEDURE gdk_draw_polygon* (drawable: GdkDrawable; gc: GdkGC; filled: INTEGER; points: GdkPoint; npoints: INTEGER);
	PROCEDURE gdk_draw_rectangle* (drawable: GdkDrawable; gc: GdkGC;filled, x, y, width, height: INTEGER);
	PROCEDURE gdk_draw_string* (drawable: GdkDrawable; font: GdkFont; gc: GdkGC; x, y: INTEGER; string: LinLibc.PtrSTR);
	PROCEDURE gdk_draw_text_wc* (drawable: GdkDrawable; font: GdkFont; gc: GdkGC; x, y: INTEGER; text: GdkWChar; text_length: INTEGER);
	PROCEDURE gdk_event_free* (event: GdkEvent); 
	PROCEDURE gdk_event_get* (): GdkEvent;
	PROCEDURE gdk_event_peek* (): GdkEvent;
	PROCEDURE gdk_event_put* (event: GdkEvent);
	PROCEDURE gdk_events_pending* (): INTEGER;
	PROCEDURE gdk_font_load* (font_name: LinLibc.PtrSTR): GdkFont;
	PROCEDURE gdk_font_ref* (font: GdkFont): GdkFont;
	PROCEDURE gdk_font_unref* (font: GdkFont);
	PROCEDURE gdk_fontset_load* (font_name: LinLibc.PtrSTR): GdkFont;
	PROCEDURE gdk_gc_new* (window: GdkWindow): GdkGC;
	PROCEDURE gdk_gc_new_with_values*(window: GdkWindow; values: GdkGCValues; value_mask: GdkGCValuesMask): GdkGC;
	PROCEDURE gdk_gc_ref* (gc: GdkGC);
	PROCEDURE gdk_gc_set_clip_origin* (gc: GdkGC; x, y: INTEGER);
	PROCEDURE gdk_gc_set_clip_rectangle* (gc: GdkGC;  rectangle: GdkRectangle);
	PROCEDURE gdk_gc_set_background* (gc: GdkGC; color: GdkColor);
	PROCEDURE gdk_gc_set_dashes* (gc: GdkGC; dash_offset: INTEGER; dash_list: POINTER TO ARRAY [untagged] OF BYTE; n: INTEGER);
	PROCEDURE gdk_gc_set_foreground* (gc: GdkGC; color: GdkColor);
	PROCEDURE gdk_gc_set_exposures* (gc: GdkGC; exposures: INTEGER);
	PROCEDURE gdk_gc_set_line_attributes*  (gc: GdkGC; line_width: INTEGER; line_style: GdkLineStyle;
															cap_style: GdkCapStyle; join_style: GdkJoinStyle);
	PROCEDURE gdk_gc_unref* (gc: GdkGC);
	PROCEDURE gdk_init* (pargc, pargv: LinLibc.PtrVoid); (* (argc: INTEGER; argv: LinLibc.StrArray);*)
	PROCEDURE gdk_mbstowcs* (dest, src: LinLibc.PtrSTR; dest_max: INTEGER): INTEGER;
	PROCEDURE gdk_pixmap_create_from_xpm* (window: GdkWindow; mask: GdkBitmap; transparent_color: GdkColor; 
		filename: LinLibc.PtrSTR): GdkPixmap;
	PROCEDURE gdk_pixmap_new* (window: GdkWindow; width, height, depth: INTEGER): GdkPixmap;
	PROCEDURE gdk_pixmap_ref* (pixmap: GdkPixmap);
	PROCEDURE gdk_pixmap_unref* (pixmap: GdkPixmap);
	PROCEDURE gdk_rectangleIntersect* (src1, src2, dst: GdkRectangle): INTEGER;
	PROCEDURE gdk_screen_height* (): INTEGER;
	PROCEDURE gdk_screen_height_mm* (): INTEGER;
	PROCEDURE gdk_screen_width* (): INTEGER;
	PROCEDURE gdk_string_extents* (font: GdkFont; string: LinLibc.PtrSTR;
												VAR lbearing: INTEGER; VAR rbearing: INTEGER;
												VAR width: INTEGER; VAR ascent: INTEGER; VAR descent: INTEGER);
	PROCEDURE gdk_string_width* (font: GdkFont; text: LinLibc.PtrSTR): INTEGER;
	PROCEDURE gdk_text_width* (font: GdkFont; text: LinLibc.PtrSTR; text_length: INTEGER): INTEGER;
	PROCEDURE gdk_text_width_wc* (font: GdkFont; text: GdkWChar; text_length: INTEGER): INTEGER; 
	PROCEDURE gdk_wcstombs* (src: GdkWChar): LinLibc.PtrWSTR;
	PROCEDURE gdk_window_add_filter* (window: GdkWindow; func: GdkFilterFunc; user_data: LinLibc.PtrVoid);
	PROCEDURE gdk_window_at_pointer* (x, y: INTEGER): GdkWindow;
	PROCEDURE gdk_window_clear_area* (window: GdkWindow; x, y, width, height: INTEGER);
	PROCEDURE gdk_window_copy_area* (window: GdkWindow; gc: GdkGC; x, y: INTEGER; 
														source_window: GdkWindow; source_x, source_y, width, height: INTEGER);
	PROCEDURE gdk_window_get_position* (window: GdkWindow; VAR x, y: INTEGER);
	PROCEDURE gdk_window_get_events* (window: GdkWindow): GdkEventMask;
	PROCEDURE gdk_window_raise* (window: GdkWindow);
	PROCEDURE gdk_window_set_background* (window: GdkWindow; color: GdkColor);
	PROCEDURE gdk_window_set_cursor* (window: GdkWindow; cursor: GdkCursor);
	PROCEDURE gdk_window_set_events* (window: GdkWindow; mask: GdkEventMask);
	PROCEDURE gdk_window_show* (window: GdkWindow);

END LinGdk.