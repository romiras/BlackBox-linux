MODULE LinGtk ["libgtk.so"];

	IMPORT LinLibc, LinGLib, LinGdk;

	CONST
		NULL* = 0; FALSE* = 0; TRUE* = 1;
		
		(* types for calls to gtk_window_new, these are the possible values of GtkWindowType *)
		GTK_WINDOW_TOPLEVEL* =0;
		GTK_WINDOW_DIALOG* = 1;
		GTK_WINDOW_POPUP* = 2;
		
		(* GTK_STATE_TYPE enum *)
		GTK_STATE_NORMAL* = 0;
		GTK_STATE_ACTIVE* = 1;
		GTK_STATE_PRELIGHT* = 2;
		GTK_STATE_SELECTED* = 3;
		GTK_STATE_INSENSITIVE* = 4;

		(* GtkAccelFlags enum *)
		GTK_ACCEL_VISIBLE* = 0; (* should the accelerator appear in the widget's display? *)
		GTK_ACCEL_SIGNAL_VISIBLE* = 1; (* should the signal associated with this accelerator be also visible? *)
		GTK_ACCEL_LOCKED* = 2; (* may the accelerator be removed again? *)
		GTK_ACCEL_MASK* = {0, 1, 2};
		
		(* Values for GtkWindowPosition *)
		GTK_WIN_POS_NONE* = 0;
		GTK_WIN_POS_CENTER* = 1;
		GTK_WIN_POS_MOUSE* = 2;
		
		(* Values for GtkSelectionMode *) 
		GTK_SELECTION_SINGLE* = 0;
		GTK_SELECTION_BROWSE* = 1;
		GTK_SELECTION_MULTIPLE* = 2;
		GTK_SELECTION_EXTENDED* = 3;

		(* Values for GtkPolicyType *)
		GTK_POLICY_ALWAYS* = 0;
		GTK_POLICY_AUTOMATIC* = 1;
		GTK_POLICY_NEVER* = 2;

		(* Values for GtkJustification *)
		GTK_JUSTIFY_LEFT* = 0;
		GTK_JUSTIFY_RIGHT* = 1;
		GTK_JUSTIFY_CENTER* = 2;
		GTK_JUSTIFY_FILL* = 3;
		
		(* Values for GtkRcFlags *)
		GTK_RC_FG* = 0;
		GTK_RC_BG* = 1;
		GTK_RC_TEXT* = 2;
		GTK_RC_BASE* = 3;
		
		(* Values for GtkShadowType *) 
		GTK_SHADOW_NONE* = 0;
		GTK_SHADOW_IN* = 1;
		GTK_SHADOW_OUT* = 2;
		GTK_SHADOW_ETCHED_IN* = 3;
		GTK_SHADOW_ETCHED_OUT* = 4;

	TYPE 
	
		GtkAccelFlags* = SET;
		GtkAllocation* = POINTER TO GtkAllocationDesc;
		GtkAllocationDesc* = RECORD [noalign] x*, y*, width*, height*: SHORTINT END;
		GtkCallBack* = PROCEDURE (widget: GtkWidget; data: LinLibc.PtrVoid);
		GtkColor* = ARRAY [untagged] 4 OF REAL;
		GtkCTreeNode* = POINTER TO RECORD [untagged] list: LinGLib.GList END;
		GData* = LinLibc.PtrVoid;
		GSList* = LinLibc.PtrVoid;
		GtkDrawingArea* = GtkWidget;
		GtkFunction* = PROCEDURE (data: LinLibc.PtrVoid): INTEGER;
		GtkKeySnoopFunc* = PROCEDURE (widget: GtkWidget; event: LinGdk.GdkEventKey; 
																	user_data: LinLibc.PtrVoid): INTEGER;
		GtkItem* = LinLibc.PtrVoid;
		GtkJustification* = INTEGER;
		GtkMenu* = LinLibc.PtrVoid;
		GtkMenuBar* = LinLibc.PtrVoid;
		GtkMenuDetachFunc* = LinLibc.PtrVoid;
		GtkMenuItem* = LinLibc.PtrVoid;
		GtkMenuShell* = LinLibc.PtrVoid;
		GtkPixmap* = LinLibc.PtrVoid;
		GtkRange* = LinLibc.PtrVoid;
		GtkRcFlags* = SET;
		GtkRequisition* = POINTER TO GtkRequisitionDesc;
		GtkRequisitionDesc* = RECORD [noalign] width*, height*: SHORTINT END;
		GtkPolicyType* = INTEGER;
		GtkSelectionMode* = INTEGER;
		GtkSignalFunc* = LinLibc.PtrVoid; (* PROCEDURE (object: GtkObject; ... ; func_data: LinLibc.PtrVoid); *)
		GtkShadowType* = INTEGER;
		GtkStateType* = INTEGER;
		GtkStatusbar* = GtkWidget;
		GtkThemeEngine* = LinLibc.PtrVoid;
		GtkWindowPosition* = INTEGER;
		GtkWindowType* = INTEGER;
		
		GtkAccelGroup* = POINTER TO GtkAccelGroupDesc;
		GtkAccelGroupDesc* = RECORD [untagged]
			ref_count*, lock_count*: INTEGER;
			modifier_mask*: LinGdk.GdkModifierType;
			attach_objects*: GSList
		END;
		
		GtkStyle* = POINTER TO RECORD [noalign]
			klass*: LinLibc.PtrVoid; (* GtkStyleClass *)
			fg*, bg*, light*, dark*, mid*, text*, base*: ARRAY 5 OF LinGdk.GdkColorDesc;
			black*, white*: LinGdk.GdkColorDesc;
			font*: LinGdk.GdkFont;
			fg_gc*, bg_gc*, light_gc*, dark_gc*, mid_gc*, text_gc*, base_gc*: ARRAY [untagged] 5 OF LinGdk.GdkGC;
			black_gc*, white_gc*: LinGdk.GdkGC;
			bg_pixmap*: POINTER TO ARRAY [untagged] 5 OF LinGdk.GdkPixmap;
		END;
		
		GtkRcStyle* = POINTER TO RECORD [noalign]
			name*, font_name*, fontset_name*: LinLibc.PtrSTR;
			bg_pixmap_name*: ARRAY [untagged] 5 OF LinLibc.PtrSTR;
			color_flags*: ARRAY [untagged] 5 OF GtkRcFlags;
			fg*, bg*, text*, base*: ARRAY [untagged] 5 OF LinGdk.GdkColorDesc;
			engine*: GtkThemeEngine;
			engine_data*: LinLibc.PtrVoid;
			ref_count*: INTEGER;
		END;

		
		
		(* Object hierarchy *)
		
		GtkObject* = POINTER TO GtkObjectDesc;
		GtkObjectDesc* = EXTENSIBLE RECORD [noalign]
			klass*: GtkObjectClass;
			flags*: SET;
			ref_count*: INTEGER;
			object_data*: GData;
		END;
		
		GtkData* = POINTER TO GtkDataDesc;
		GtkDataDesc* = EXTENSIBLE RECORD (GtkObjectDesc)
		END;
		
		GtkAdjustment* = POINTER TO GtkAdjustmenDesc;
		GtkAdjustmenDesc* = EXTENSIBLE RECORD (GtkDataDesc)
			lower*, upper*, value*, step_increment*, page_increment*, page_size*: SHORTREAL 
		END;
		
		GtkWidget* = POINTER TO GtkWidgetDesc;
		GtkWidgetDesc* = EXTENSIBLE RECORD (GtkObjectDesc)
			private_flags*: SHORTINT;    
			state*: BYTE;     (* see GTK_STATE_TYPE enum above *)
			saved_state*: BYTE;   
			name*: LinLibc.PtrSTR;    
			style*: GtkStyle;    
			requisition*: GtkRequisitionDesc;    
			allocation*: GtkAllocationDesc;    
			window*: LinGdk.GdkWindow;  
			parent*: GtkWidget
		END;
		
		GtkEditable* = POINTER TO GtkEditableDesc;
		GtkEditableDesc* = EXTENSIBLE RECORD (GtkWidgetDesc) 
			current_pos-, selection_start_pos-, selection_end_pos-: INTEGER;
			has_selection-: SET (* !!! this is a bit field of size 1 *)
		END;
		
		GtkContainer* = POINTER TO GtkContainerDesc;
		GtkContainerDesc* = EXTENSIBLE RECORD (GtkWidgetDesc)
			focus_child*: GtkWidget;
			bitFieldValues: INTEGER; (* border_width: 16; need_resize: 1; resize_mode: 2; reallocate_redraws: 1; *)
			resize_widgets*: LinLibc.PtrVoid (* The list of children that requested a resize *)
		END;
			
		GtkBin* = POINTER TO GtkBinDesc;
		GtkBinDesc* = EXTENSIBLE RECORD (GtkContainerDesc)
			child*: GtkWidget;
		END;
		
		GtkBox* = POINTER TO GtkBoxDesc;
		GtkBoxDesc* = EXTENSIBLE RECORD (GtkContainerDesc)
			chlidren*: LinGLib.GList;
			spacing*: SHORTINT;
			homogeneous*: SHORTINT
		END;
		
		GtkHBox* = POINTER TO GtkHBoxDesc;
		GtkHBoxDesc* = EXTENSIBLE RECORD (GtkBox) END;
		
		GtkCombo* = POINTER TO GtkComboDesc;
		GtkComboDesc* = EXTENSIBLE RECORD (GtkHBox)
			entry*, button*, popup*, popwin*, list*: GtkWidget;
			entry_change_id*, list_change_id*: INTEGER;
			someBits: SHORTINT;				
			current_button*: SHORTINT;
			activate_id*: INTEGER;
		END;
		
		GtkWindow* = POINTER TO GtkWindowDesc;
		GtkWindowDesc* = EXTENSIBLE RECORD (GtkBinDesc)
			title*: LinLibc.PtrSTR;
			wmclass_name*: LinLibc.PtrSTR;
			wmclass_class*: LinLibc.PtrSTR;
			type*: GtkWindowType;
			focus_widget*: GtkWidget;
			default_widget*: GtkWidget;
			transient_parent*: GtkWindow;
			resize_count*: SHORTINT;
			bitFieldValues2: SHORTINT; 
			(* allow_shrink: 1; allow_grow: 1;auto_shrink: 1; handling_resize:1; position: 2; use_uposition: 1; modal: 1 *)
		END;
		
		GtkFileSelection* = POINTER TO GtkFileSelectionDesc;
		GtkFileSelectionDesc* = EXTENSIBLE RECORD (GtkWindowDesc)
			dir_list*, file_list*, selection_entry*, selection_text*, main_vbox*: GtkWidget;
			ok_button*, cancel_button*, help_button*, history_pulldown*, history_menu*: GtkWidget;
			history_list*: LinLibc.PtrVoid;
			fileop_dialog*, fileop_entry*: GtkWidget;
			fileop_file*: LinLibc.PtrSTR;
			cmpl_state*: LinLibc.PtrVoid;
			fileop_c_dir*, fileop_del_file*, fileop_ren_file*, button_area*, action_area*: GtkWidget
		END;
		
		GtkSelectionData* = POINTER TO GtkSelectionDataDesc;
		GtkSelectionDataDesc* = RECORD [untagged]
			selection*, target*, type*: LinGdk.GdkAtom;
			format*: INTEGER;
			data*: LinLibc.PtrSTR;
			length*: INTEGER;
		END; 
		
		GtkColorSelectionDialog* = POINTER TO GtkColorSelectionDialogDesc;
		GtkColorSelectionDialogDesc* = EXTENSIBLE RECORD (GtkWindowDesc)
			colorsel*: GtkWidget;
			main_vbox*: GtkWidget;
			ok_button*: GtkWidget;
			reset_button*: GtkWidget;
			cancel_button*: GtkWidget;
			help_button*: GtkWidget;
		END;
		
		GtkFontSelectionDialog* = POINTER TO GtkFontSelectionDialogDesc;
		GtkFontSelectionDialogDesc* = EXTENSIBLE RECORD (GtkWindowDesc)
			fontsel*: GtkWidget;
			main_vbox*: GtkWidget;
			action_area*: GtkWidget;
			ok_button*: GtkWidget;
			apply_button*: GtkWidget; (* The 'Apply' button is not shown by default but you can show/hide it. *)
			cancel_button*: GtkWidget;

			(* If the user changes the width of the dialog, we turn auto-shrink off. *)
			dialog_width: INTEGER;
			auto_resize: INTEGER;
		END;
		
		GtkObjectClass* = POINTER TO EXTENSIBLE RECORD[noalign]
			type*: INTEGER;
			content0: ARRAY [untagged] 9 OF INTEGER;
		END;
		
		GtkWidgetClass* = POINTER TO EXTENSIBLE RECORD(GtkObjectClass)
			activate_signal*, set_scroll_adjustments_signal*: INTEGER;
			content1: ARRAY [untagged] 59 OF INTEGER;
		END;

		GtkContainerClass* = POINTER TO EXTENSIBLE RECORD(GtkWidgetClass)
			n_child_args: INTEGER;
			
			content2: ARRAY [untagged] 12 OF INTEGER;
			(**)
		END;
	
		GtkBinClass* = POINTER TO EXTENSIBLE RECORD (GtkContainerClass)
		END;
		
		GtkScrolledWindowClass* = POINTER TO EXTENSIBLE RECORD (GtkBinClass)
			scrollbar_spacing: INTEGER;
		END;

		
		
	PROCEDURE gtk_accel_group_add* (accel_group: GtkAccelGroup; accel_key: INTEGER; accel_mods: LinGdk.GdkModifierType;
                                              accel_flags: GtkAccelFlags; object: GtkObject; accel_signal: LinLibc.PtrSTR);
	PROCEDURE gtk_accel_group_attach* (accel_group: GtkAccelGroup; object: GtkObject);
	PROCEDURE gtk_accel_group_get_default* (): GtkAccelGroup;
	PROCEDURE gtk_accel_group_new* (): GtkAccelGroup;
	PROCEDURE gtk_accel_group_lock* (accel_group: GtkAccelGroup);
	PROCEDURE gtk_accel_group_unlock* (accel_group: GtkAccelGroup);
	PROCEDURE gtk_accelerator_valid* (keyval: INTEGER; modifier: LinGdk.GdkModifierType): INTEGER; 
	PROCEDURE gtk_adjustment_new* (value, lower, upper, step_increment, page_increment, page_size: SHORTREAL): GtkAdjustment; 
	PROCEDURE gtk_adjustment_changed* (adjustment: GtkAdjustment);
	PROCEDURE gtk_box_reorder_child* (box: GtkWidget; widget: GtkWidget; position: INTEGER);
	PROCEDURE gtk_button_new* (): GtkWidget;
	PROCEDURE gtk_button_new_with_label* (label: LinLibc.PtrSTR): GtkWidget;
	PROCEDURE gtk_button_pressed* (button: GtkWidget);
	PROCEDURE gtk_button_clicked* (button: GtkWidget); 
	PROCEDURE gtk_calendar_new* (): GtkWidget;
	PROCEDURE gtk_check_button_new* (): GtkWidget;
	PROCEDURE gtk_check_button_new_with_label* (label: LinLibc.PtrSTR): GtkWidget;
	PROCEDURE gtk_check_menu_item_new_with_label* (label: LinLibc.PtrSTR): GtkWidget;
	PROCEDURE gtk_check_menu_item_set_active* (check_menu_item: GtkWidget; is_active: INTEGER);
	PROCEDURE gtk_check_menu_item_set_show_toggle* (check_menu_item: GtkWidget; always: INTEGER);
	PROCEDURE gtk_color_selection_new* (): GtkWidget;
	PROCEDURE gtk_color_selection_dialog_new* (title: LinLibc.PtrSTR): GtkWidget;
	PROCEDURE gtk_color_selection_get_color* (colorsel: GtkWidget; VAR color: GtkColor);
	PROCEDURE gtk_color_selection_set_color* (colorsel: GtkWidget; VAR color: GtkColor);
	PROCEDURE gtk_color_selection_set_opacity* (colorsel: GtkWidget; use_opacity: INTEGER);
	PROCEDURE gtk_combo_new* (): GtkWidget;
	PROCEDURE gtk_combo_set_popdown_strings* (combo: GtkWidget; strings: LinGLib.GList);
	PROCEDURE gtk_container_add* (container: GtkContainer; widget: GtkWidget);
	PROCEDURE gtk_container_children* (container: GtkContainer): LinGLib.GList;
	PROCEDURE gtk_container_foreach* (container: GtkContainer;callback: GtkCallBack; callback_data: LinLibc.PtrVoid);
	PROCEDURE gtk_container_remove* (container: GtkContainer; widget: GtkWidget);
	PROCEDURE gtk_container_set_focus_child* (container: GtkContainer; child: GtkWidget);
	PROCEDURE gtk_clist_append* (clist: GtkWidget; text: LinLibc.StrArray);
	PROCEDURE gtk_clist_clear* (clist: GtkWidget);
	PROCEDURE gtk_clist_find_row_from_data* (clist: GtkWidget; data: LinLibc.PtrVoid): INTEGER;
	PROCEDURE gtk_clist_freeze* (clist: GtkWidget);
	PROCEDURE gtk_clist_get_row_data* (clist: GtkWidget; row: INTEGER): LinLibc.PtrVoid;
	PROCEDURE gtk_clist_get_text* (clist: GtkWidget; row, column: INTEGER; VAR text: LinLibc.PtrSTR);
	PROCEDURE gtk_clist_insert* (clist: GtkWidget; row: INTEGER; text: LinLibc.StrArray);
	PROCEDURE gtk_clist_moveto* (clist: GtkWidget; row, column: INTEGER; row_align, col_align: SHORTREAL);
	PROCEDURE gtk_clist_new* (columns: INTEGER): GtkWidget;
	PROCEDURE gtk_clist_optimal_column_width* (clist: GtkWidget; column: INTEGER): INTEGER;
	PROCEDURE gtk_clist_prepend* (clist: GtkWidget; text: LinLibc.StrArray);
	PROCEDURE gtk_clist_select_row* (clist: GtkWidget; row, column: INTEGER);
	PROCEDURE gtk_clist_set_background* (clist: GtkWidget; row: INTEGER; color: LinGdk.GdkColor);
	PROCEDURE gtk_clist_set_column_justification* (clist: GtkWidget; column: INTEGER; justification: GtkJustification);
	PROCEDURE gtk_clist_set_column_width* (clist: GtkWidget; column, width: INTEGER);
	PROCEDURE gtk_clist_set_row_data* (clist: GtkWidget; row: INTEGER; data: LinLibc.PtrVoid);
	PROCEDURE gtk_clist_set_selection_mode* (clist: GtkWidget; mode: GtkSelectionMode);
	PROCEDURE gtk_clist_set_text* (clist: GtkWidget; row, column: INTEGER; text: LinLibc.PtrSTR);
	PROCEDURE gtk_clist_sort* (clist: GtkWidget);
	PROCEDURE gtk_clist_thaw* (clist: GtkWidget);
	PROCEDURE gtk_clist_unselect_all* (clist: GtkWidget);
	PROCEDURE gtk_clist_unselect_row* (clist: GtkWidget; row, column: INTEGER);
	PROCEDURE gtk_ctree_insert_node* (ctree: GtkWidget; parent, sibling: GtkCTreeNode; text: LinLibc.StrArray; 
		spacing: INTEGER;
		pixmap_closed: LinGdk.GdkPixmap;  mask_closed: LinGdk.GdkBitmap; 
		pixmap_open: LinGdk.GdkPixmap; mask_open: LinGdk.GdkBitmap;
		is_leaf, expanded: INTEGER
		): GtkCTreeNode; (* returns the inserted GtkCTreeNode *)
	PROCEDURE gtk_ctree_new* (columns, tree_column: INTEGER): GtkWidget;
	PROCEDURE gtk_ctree_node_get_row_data* (ctree: GtkWidget; node: GtkCTreeNode): LinLibc.PtrVoid;
	PROCEDURE gtk_ctree_node_set_row_data* (ctree: GtkWidget; node: GtkCTreeNode; data: LinLibc.PtrVoid);
	PROCEDURE gtk_ctree_node_nth* (ctree: GtkWidget; row: INTEGER): GtkCTreeNode;
	PROCEDURE gtk_ctree_sort_recursive* (ctree: GtkWidget; tree_node: GtkCTreeNode);
	PROCEDURE gtk_draw_oval* (style: GtkStyle; window: GtkWindow; state_type: GtkStateType; shadow_type: GtkShadowType;
											x, y, width, height: INTEGER);
	PROCEDURE gtk_draw_box* (style: GtkStyle; window: GtkWindow; state_type: GtkStateType; shadow_type: GtkShadowType;
											x, y, width, height: INTEGER);
	PROCEDURE gtk_draw_string* (style: GtkStyle; window: GtkWindow; state_type: GtkStateType; 
											x, y: INTEGER; string: LinLibc.PtrSTR);
	PROCEDURE gtk_drawing_area_new* (): GtkWidget;
	PROCEDURE gtk_drawing_area_size* (darea: GtkDrawingArea; width, height: INTEGER);
	PROCEDURE gtk_editable_get_chars* (editable: GtkWidget; start, end: INTEGER): LinLibc.PtrSTR;
	PROCEDURE gtk_editable_get_position* (editable: GtkWidget): INTEGER;
	PROCEDURE gtk_editable_delete_text* (editable: GtkWidget; start, end: INTEGER);
	PROCEDURE gtk_editable_insert_text* (editable: GtkWidget; new_text: LinLibc.PtrSTR; length: INTEGER; VAR pos: INTEGER);
	PROCEDURE gtk_editable_select_region* (editable: GtkWidget; start, end: INTEGER);
	PROCEDURE gtk_editable_set_editable* (text: GtkWidget; editable: INTEGER);
	PROCEDURE gtk_entry_new* (): GtkWidget;
	PROCEDURE gtk_entry_set_text* (entry: GtkWidget; text: LinLibc.PtrSTR);
	PROCEDURE gtk_get_event_widget* (event: LinGdk.GdkEvent): GtkWidget;
	PROCEDURE gtk_event_box_new* (): GtkWidget;
	PROCEDURE gtk_events_pending* (): INTEGER;
	PROCEDURE gtk_file_selection_get_filename* (filesel: GtkFileSelection): LinLibc.PtrSTR;
	PROCEDURE gtk_file_selection_new* (title: LinLibc.PtrSTR): GtkWidget;
	PROCEDURE gtk_fixed_new* (): GtkWidget;
	PROCEDURE gtk_fixed_put* (fixed, widget: GtkWidget; x, y: SHORTINT);
	PROCEDURE gtk_fixed_move* (fixed, widget: GtkWidget; x, y: SHORTINT);
	PROCEDURE gtk_font_selection_dialog_get_font_name* (fsd: GtkFontSelectionDialog): LinLibc.PtrSTR;
	PROCEDURE gtk_font_selection_dialog_new* (title: LinLibc.PtrSTR): GtkWidget;
	PROCEDURE gtk_font_selection_dialog_set_font_name* (fsd: GtkFontSelectionDialog; 
																								fontname: LinLibc.PtrSTR): INTEGER;
	PROCEDURE gtk_get_current_event* (): LinGdk.GdkEvent;
	PROCEDURE gtk_grab_add* (widget: GtkWidget);
	PROCEDURE gtk_grab_get_current* (): GtkWidget;
	PROCEDURE gtk_grab_remove* (widget: GtkWidget);
	PROCEDURE gtk_handler_block_by_func* (object: GtkObject; func: GtkSignalFunc; data: LinLibc.PtrVoid);
	PROCEDURE gtk_hbox_new* (homogeneous, spacing: INTEGER): GtkWidget;
	PROCEDURE gtk_hscrollbar_new* (adjustment: GtkAdjustment): GtkWidget;
	PROCEDURE gtk_hseparator_new* (): GtkWidget;
	PROCEDURE gtk_init* (pargc, pargv: LinLibc.PtrVoid);
	PROCEDURE gtk_key_snooper_install* (snooper: GtkKeySnoopFunc; user_data: LinLibc.PtrVoid): INTEGER;
	PROCEDURE gtk_label_new* (str: LinLibc.PtrSTR): GtkWidget;
	PROCEDURE gtk_label_get* (label: GtkWidget; VAR str: LinLibc.PtrSTR);
	PROCEDURE gtk_label_set_justify* (label: GtkWidget; jtype: GtkJustification);
	PROCEDURE gtk_label_set_line_wrap* (label: GtkWidget; wrap: INTEGER);
	PROCEDURE gtk_label_set_text* (label: GtkWidget; str: LinLibc.PtrSTR);
	PROCEDURE gtk_label_parse_uline* (label: GtkWidget; string: LinLibc.PtrSTR): INTEGER;
	PROCEDURE gtk_list_child_position* (list, child: GtkWidget): INTEGER;
	PROCEDURE gtk_list_clear_items* (list: GtkWidget; start, end: INTEGER);
	PROCEDURE gtk_list_new* (): GtkWidget;
	PROCEDURE gtk_list_item_new* (): GtkWidget;
	PROCEDURE gtk_list_item_new_with_label* (label: LinLibc.PtrSTR): GtkWidget;
	PROCEDURE gtk_list_select_item* (list: GtkWidget; item: INTEGER);
	PROCEDURE gtk_list_set_selection_mode* (list: GtkWidget; mode: GtkSelectionMode);
	PROCEDURE gtk_list_unselect_all* (list: GtkWidget);
	PROCEDURE gtk_main* ();
	PROCEDURE gtk_main_do_event* (event: LinGdk.GdkEvent);
	PROCEDURE gtk_main_iteration* (): INTEGER;
	PROCEDURE gtk_main_iteration_do* (blocking: INTEGER): INTEGER;
	PROCEDURE gtk_main_quit* ();
	PROCEDURE gtk_menu_append* (menu: GtkMenu; child: GtkWidget);
	PROCEDURE gtk_menu_attach_to_widget* (menu: GtkMenu; attatch_widget: GtkWidget; detacher: GtkMenuDetachFunc);
	PROCEDURE gtk_menu_bar_append* (menu_bar: GtkMenuBar; child: GtkWidget);
	PROCEDURE gtk_menu_bar_new* (): GtkWidget;
	PROCEDURE gtk_menu_detach* (menu: GtkMenu);
	PROCEDURE gtk_menu_get_attach_widget* (menu: GtkMenu): GtkWidget;
	PROCEDURE gtk_menu_item_activate* (menu_item: GtkWidget);
	PROCEDURE gtk_menu_item_configure* (menu_item: GtkWidget; 
															show_toggle_indicator, show_submenu_indicator: INTEGER);
	PROCEDURE gtk_menu_item_deselect* (menu_item: GtkMenuItem);
	PROCEDURE gtk_menu_item_new* (): GtkWidget;
	PROCEDURE gtk_menu_item_new_with_label* (label: LinLibc.PtrSTR): GtkWidget;
	PROCEDURE gtk_menu_item_remove_submenu* (menu_item: GtkMenuItem);
	PROCEDURE gtk_menu_item_select* (menu_item: GtkMenuItem);
	PROCEDURE gtk_menu_item_set_submenu* (menu_item: GtkMenuItem; submenu: GtkWidget);
	PROCEDURE gtk_menu_new* (): GtkMenu;
	PROCEDURE gtk_menu_popup* (menu: GtkMenu; parent_menu_shell, parent_menu_item: GtkWidget;
															func, data: LinLibc.PtrVoid; 
															button: INTEGER; activate_time: INTEGER);
	PROCEDURE gtk_menu_prepend* (menu: GtkMenu; child: GtkWidget);
	PROCEDURE gtk_menu_set_title* (menu: GtkMenu; title: LinLibc.PtrSTR);
	PROCEDURE gtk_menu_shell_activate_item* (menu_shell: GtkMenuShell; menu_item: GtkWidget; 
																					force_deactivate: INTEGER);
	PROCEDURE gtk_menu_shell_select_item* (menu_shell: GtkMenuShell; menu_item: GtkWidget);
	PROCEDURE gtk_misc_set_alignment* (misc: GtkWidget; xalign, yalign: REAL);
	PROCEDURE gtk_misc_set_padding* (misc: GtkWidget; xpad, ypad: INTEGER);
	PROCEDURE gtk_notebook_append_page* (notebook, child, tab_label: GtkWidget);
	PROCEDURE gtk_notebook_new* (): GtkWidget;
	PROCEDURE gtk_object_get_user_data* (object: GtkObject): LinLibc.PtrVoid;
	PROCEDURE gtk_object_ref* (object: GtkObject);
	PROCEDURE gtk_object_set_user_data* (object: GtkObject; data: LinLibc.PtrVoid);
	PROCEDURE gtk_object_unref* (object: GtkObject);
	PROCEDURE gtk_pixmap_new* (pixmap: LinGdk.GdkPixmap; mask: LinGdk.GdkBitmap): GtkWidget;
	PROCEDURE gtk_propagate_event* (widget: GtkWidget; event: LinGdk.GdkEvent);
	PROCEDURE gtk_radio_button_new* (group: LinGLib.GList): GtkWidget; 
	PROCEDURE gtk_radio_button_new_from_widget* (radioButton: GtkWidget): GtkWidget;
	PROCEDURE gtk_rc_get_style* (widget: GtkWidget): GtkStyle;
	PROCEDURE gtk_rc_style_new* (): GtkRcStyle; 
	PROCEDURE gtk_rc_style_ref* (style: GtkRcStyle);
	PROCEDURE gtk_rc_style_unref* (style: GtkRcStyle);
	PROCEDURE gtk_range_get_adjustment* (range: GtkRange): GtkAdjustment;
	PROCEDURE gtk_range_set_adjustment* (range: GtkRange; adjustment: GtkAdjustment);
	PROCEDURE gtk_range_slider_update* (range: GtkRange);
	PROCEDURE gtk_scrolled_window_add_with_viewport* (scrolled_window, child: GtkWidget);
	PROCEDURE gtk_scrolled_window_get_vadjustment* (scrolled_window: GtkWidget): GtkAdjustment;
	PROCEDURE gtk_scrolled_window_new* (hadjustment, vadjustment: GtkAdjustment): GtkWidget;
	PROCEDURE gtk_scrolled_window_set_hadjustment* (scrolled_window: GtkWidget; hadjustment: GtkAdjustment);
	PROCEDURE gtk_scrolled_window_set_policy* (scrolled_window: GtkWidget; 
																hscrollbar_policy, vscrollbar_policy: GtkPolicyType);
	PROCEDURE gtk_scrolled_window_set_vadjustment* (scrolled_window: GtkWidget; vadjustment: GtkAdjustment);
	PROCEDURE gtk_selection_add_target* (widget: GtkWidget; selection, target: LinGdk.GdkAtom; info: INTEGER);
	PROCEDURE gtk_selection_convert* (widget: GtkWidget; selection, target: LinGdk.GdkAtom; time: INTEGER): INTEGER;
	PROCEDURE gtk_selection_data_set* (selection_data: GtkSelectionData; type: LinGdk.GdkAtom; format: INTEGER; 
														data: LinLibc.PtrSTR; length: INTEGER);
	PROCEDURE gtk_selection_owner_set* (widget: GtkWidget; selection: LinGdk.GdkAtom; time: INTEGER): INTEGER;
	PROCEDURE gtk_signal_handler_block_by_func* (object: GtkObject; func: GtkSignalFunc; func_data: LinLibc.PtrVoid);
	PROCEDURE gtk_signal_connect* (object: GtkObject; name: LinLibc.PtrSTR; 
													func: GtkSignalFunc; func_data: LinLibc.PtrVoid): INTEGER;
	PROCEDURE gtk_signal_connect_after* (object: GtkObject; name: LinLibc.PtrSTR; 
													func: GtkSignalFunc; func_data: LinLibc.PtrVoid): INTEGER;
	PROCEDURE gtk_signal_disconnect* (object: GtkObject; handler_id: INTEGER);
	PROCEDURE gtk_signal_emit_by_name* (object: GtkObject; name: LinLibc.PtrSTR);
	PROCEDURE gtk_signal_emit_stop_by_name* (object: GtkObject; name: LinLibc.PtrSTR);
	PROCEDURE gtk_signal_handler_unblock_by_func* (object: GtkObject; func: GtkSignalFunc; func_data: LinLibc.PtrVoid);
	PROCEDURE gtk_spin_button_get_value_as_int* (spin_button: GtkWidget): INTEGER;
	PROCEDURE gtk_spin_button_new* (adjustment: GtkAdjustment; climb_rate: SHORTREAL; digits: INTEGER): GtkWidget;
	PROCEDURE gtk_spin_button_set_numeric* (spin_button: GtkWidget; numeric: INTEGER);
	PROCEDURE gtk_spin_button_set_shadow_type* (spin_button: GtkWidget; shadow_type: GtkShadowType);
	PROCEDURE gtk_spin_button_set_snap_to_ticks* (spin_button: GtkWidget; snap_to_ticks: INTEGER);
	PROCEDURE gtk_spin_button_set_value* (spin_button: GtkWidget; value: SHORTREAL);
	PROCEDURE gtk_spin_button_set_wrap* (spin_button: GtkWidget; wrap: INTEGER);
	PROCEDURE gtk_spin_button_update* (spin_button: GtkWidget);
	PROCEDURE gtk_statusbar_get_context_id* (statusbar: GtkStatusbar; context_description: LinLibc.PtrSTR): INTEGER;
	PROCEDURE gtk_statusbar_new* (): GtkWidget;
	PROCEDURE gtk_statusbar_pop* (statusbar: GtkStatusbar; context_id: INTEGER);
	PROCEDURE gtk_statusbar_push* (statusbar: GtkStatusbar; context_id: INTEGER; text: LinLibc.PtrSTR): INTEGER;
	PROCEDURE gtk_style_copy* (style: GtkStyle): GtkStyle;
	PROCEDURE gtk_style_ref* (style: GtkStyle);
	PROCEDURE gtk_style_unref* (style: GtkStyle);
	PROCEDURE gtk_text_get_length* (text: GtkWidget): INTEGER;
	PROCEDURE gtk_text_insert* (text: GtkWidget; font: LinGdk.GdkFont; fore, back: LinGdk.GdkColor; 
											chars: LinLibc.PtrSTR; length: INTEGER);
	PROCEDURE gtk_text_new* (hadjustment, vadjustment: GtkAdjustment): GtkWidget;
	PROCEDURE gtk_text_set_editable* (text: GtkWidget; editable: INTEGER);
	PROCEDURE gtk_text_set_line_wrap* (text: GtkWidget; line_wrap: INTEGER);
	PROCEDURE gtk_text_set_word_wrap* (text: GtkWidget; word_wrap: INTEGER);
	PROCEDURE gtk_timeout_add* (interval: INTEGER; function: GtkFunction; data: LinLibc.PtrVoid): INTEGER;
	PROCEDURE gtk_toggle_button_get_active* (toggle_button: GtkWidget): INTEGER;
	PROCEDURE gtk_toggle_button_set_active* (toggle_button: GtkWidget; is_active: INTEGER);
	PROCEDURE gtk_toggle_button_toggled* (toggle_button: GtkWidget);
	PROCEDURE gtk_tree_new* (): GtkWidget;
	PROCEDURE gtk_tree_append* (tree, tree_item: GtkWidget);
	PROCEDURE gtk_tree_insert* (tree, tree_item: GtkWidget; position: INTEGER);
	PROCEDURE gtk_tree_item_new* (): GtkWidget;
	PROCEDURE gtk_tree_item_new_with_label* (label: LinLibc.PtrSTR): GtkWidget;
	PROCEDURE gtk_tree_item_set_subtree* (tree_item, subtree: GtkWidget);
	PROCEDURE gtk_tree_prepend* (tree, tree_item: GtkWidget);
	PROCEDURE gtk_tree_select_child* (tree, item: GtkWidget);
	PROCEDURE gtk_tree_select_item* (tree: GtkWidget; item: INTEGER);
	PROCEDURE gtk_tree_unselect_child* (tree, item: GtkWidget);
	PROCEDURE gtk_tree_unselect_item* (tree: GtkWidget; item: INTEGER);
	PROCEDURE gtk_vbox_new* (homogeneous, spacing: INTEGER): GtkWidget;
	PROCEDURE gtk_vscrollbar_new* (adjustment: GtkAdjustment): GtkWidget;
	PROCEDURE gtk_vseparator_new* (): GtkWidget;
	PROCEDURE gtk_widget_add_accelerator* (widget: GtkWidget; accel_signal: LinLibc.PtrSTR; 
															accel_group: GtkAccelGroup; accel_key: INTEGER; 
															accel_mods: LinGdk.GdkModifierType; accel_flags: GtkAccelFlags);
	PROCEDURE gtk_widget_destroy* (object: GtkWidget);
	PROCEDURE gtk_widget_draw* (widget: GtkWidget; area: LinGdk.GdkRectangle); 
	PROCEDURE gtk_widget_draw_focus* (widget: GtkWidget); 
	PROCEDURE gtk_widget_draw_default* (widget: GtkWidget); 
	PROCEDURE gtk_widget_event* (widget: GtkWidget; event: LinGdk.GdkEvent);
	PROCEDURE gtk_widget_get_events* (widget: GtkWidget): LinGdk.GdkEventMask;
	PROCEDURE gtk_widget_get_parent_window* (widget: GtkWidget): LinGdk.GdkWindow;
	PROCEDURE gtk_widget_get_style* (widget: GtkWidget): GtkStyle;
	PROCEDURE gtk_widget_get_visual* (widget: GtkWidget): LinGdk.GdkVisual;
	PROCEDURE gtk_widget_grab_default* (widget: GtkWidget);
	PROCEDURE gtk_widget_grab_focus* (widget: GtkWidget);
	PROCEDURE gtk_widget_hide* (widget: GtkWidget);
	PROCEDURE gtk_widget_hide_all* (widget: GtkWidget);
	PROCEDURE gtk_widget_modify_style* (widget: GtkWidget; style: GtkRcStyle);
	PROCEDURE gtk_widget_ref* (widget: GtkWidget);
	PROCEDURE gtk_widget_set_events* (widget: GtkWidget; events: LinGdk.GdkEventMask);
	PROCEDURE gtk_widget_set_sensitive* (widget: GtkWidget; sensitive: INTEGER);
	PROCEDURE gtk_widget_set_state* (widget: GtkWidget; state: INTEGER);
	PROCEDURE gtk_widget_set_uposition* (widget: GtkWidget; x, y: INTEGER);
	PROCEDURE gtk_widget_set_usize* (widget: GtkWidget; width, height: INTEGER);
	PROCEDURE gtk_widget_show* (widget: GtkWidget);
	PROCEDURE gtk_widget_show_all* (widget: GtkWidget);
	PROCEDURE gtk_widget_show_now* (widget: GtkWidget);
	PROCEDURE gtk_widget_size_allocate* (widget: GtkWidget; allocation: GtkAllocation);
	PROCEDURE gtk_widget_size_request* (widget: GtkWidget; requisition: GtkRequisition);
	PROCEDURE gtk_widget_unparent* (widget: GtkWidget);
	PROCEDURE gtk_widget_unref* (widget: GtkWidget);
	PROCEDURE gtk_widget_queue_clear* (widget: GtkWidget);
	PROCEDURE gtk_widget_queue_draw* (widget: GtkWidget);
	PROCEDURE gtk_widget_queue_draw_area* (widget: GtkWidget; x, y, width, height: INTEGER); 
	PROCEDURE gtk_viewport_new* (hadjustment, vadjustmen: GtkAdjustment): GtkWidget;
	PROCEDURE gtk_window_activate_focus* (window: GtkWindow): INTEGER;
	PROCEDURE gtk_window_add_accel_group* (window: GtkWindow; accel_group: GtkAccelGroup);
	PROCEDURE gtk_window_new* (type: INTEGER): GtkWidget;
	PROCEDURE gtk_window_set_default_size* (window: GtkWindow; width, height: INTEGER); 
	PROCEDURE gtk_window_set_focus* (window: GtkWindow; focus: GtkWidget);
	PROCEDURE gtk_window_set_modal* (window: GtkWindow; modal: INTEGER);
	PROCEDURE gtk_window_set_policy* (window: GtkWindow; allow_shrink, allow_grow, auto_shrink: INTEGER);
	PROCEDURE gtk_window_set_position* (window: GtkWindow; position: GtkWindowPosition);
	PROCEDURE gtk_window_set_title* (window: GtkWindow; title: LinLibc.PtrSTR);
	
	
END LinGtk.