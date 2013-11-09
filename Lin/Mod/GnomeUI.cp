MODULE LinGnomeUI ["libgnomeui.so"];
	
	IMPORT LinLibc, LinGtk;
	
	CONST
	
		NULL* = 0;
	
		(* possible values for the mode parameter to gnome_mdi_set_mode *)
		GNOME_MDI_NOTEBOOK* = 0;
		GNOME_MDI_TOPLEVEL* = 1;
		GNOME_MDI_MODAL* = 2;
		GNOME_MDI_DEFAULT_MODE* = 42;


		(* These values identify the item type that a particular GnomeUIInfo structure specifies *)
		GNOME_APP_UI_ENDOFINFO* = 0;
		GNOME_APP_UI_ITEM* = 1;
		GNOME_APP_UI_TOGGLEITEM* = 2;
		GNOME_APP_UI_RADIOITEMS* = 3;
		GNOME_APP_UI_SUBTREE* = 4;
		GNOME_APP_UI_SEPARATOR* = 5;
		GNOME_APP_UI_HELP* = 6;
		GNOME_APP_UI_BUILDER_DATA* = 7;
		GNOME_APP_UI_ITEM_CONFIGURABLE* = 8;
		GNOME_APP_UI_SUBTREE_STOCK* = 9;
		
		GNOME_APP_CONFIGURABLE_ITEM_NEW* = 0;        
		GNOME_APP_CONFIGURABLE_ITEM_OPEN* = 1;  
		GNOME_APP_CONFIGURABLE_ITEM_SAVE* = 2;   
		GNOME_APP_CONFIGURABLE_ITEM_SAVE_AS* = 3;        
		GNOME_APP_CONFIGURABLE_ITEM_REVERT* = 4;
		GNOME_APP_CONFIGURABLE_ITEM_PRINT* = 5;
		GNOME_APP_CONFIGURABLE_ITEM_PRINT_SETUP* = 6;        
		GNOME_APP_CONFIGURABLE_ITEM_CLOSE* = 7;
		GNOME_APP_CONFIGURABLE_ITEM_EXIT* = 8;
		GNOME_APP_CONFIGURABLE_ITEM_CUT* = 9;
		GNOME_APP_CONFIGURABLE_ITEM_COPY* = 10;     
		GNOME_APP_CONFIGURABLE_ITEM_PASTE* = 11; 
		GNOME_APP_CONFIGURABLE_ITEM_CLEAR* = 12; 
		GNOME_APP_CONFIGURABLE_ITEM_UNDO* = 13;
		GNOME_APP_CONFIGURABLE_ITEM_REDO* = 14;
		GNOME_APP_CONFIGURABLE_ITEM_FIND* = 15;
		GNOME_APP_CONFIGURABLE_ITEM_FIND_AGAIN* = 16;        
		GNOME_APP_CONFIGURABLE_ITEM_REPLACE* = 17;
		GNOME_APP_CONFIGURABLE_ITEM_PROPERTIES* = 18;     
		GNOME_APP_CONFIGURABLE_ITEM_PREFERENCES* = 19;
		GNOME_APP_CONFIGURABLE_ITEM_ABOUT* = 20;
		GNOME_APP_CONFIGURABLE_ITEM_SELECT_ALL* = 21;	
		GNOME_APP_CONFIGURABLE_ITEM_NEW_WINDOW* = 22;	
		GNOME_APP_CONFIGURABLE_ITEM_CLOSE_WINDOW* = 23;
		GNOME_APP_CONFIGURABLE_ITEM_NEW_GAME* = 24;
		GNOME_APP_CONFIGURABLE_ITEM_PAUSE_GAME* = 25;
		GNOME_APP_CONFIGURABLE_ITEM_RESTART_GAME* = 26;
		GNOME_APP_CONFIGURABLE_ITEM_UNDO_MOVE* = 27;
		GNOME_APP_CONFIGURABLE_ITEM_REDO_MOVE* = 28;
		GNOME_APP_CONFIGURABLE_ITEM_HINT* = 29;
		GNOME_APP_CONFIGURABLE_ITEM_SCORES* = 30;	
		GNOME_APP_CONFIGURABLE_ITEM_END_GAME* = 31;
	
	TYPE
		GnomeMDIChild* = LinLibc.PtrVoid;
		GnomeMDIGenericChild* = LinLibc.PtrVoid;
		GnomeMDIChildCreator* = PROCEDURE (config: LinLibc.PtrSTR): GnomeMDIChild;
		GnomeMDIChildViewCreator* = PROCEDURE (child: GnomeMDIChild; data: LinLibc.PtrVoid): LinGtk.GtkWidget;
		GnomeUIInfoType* = INTEGER;
		GnomeUIPixmapType* = INTEGER;
		GdkModifierType* = INTEGER;
		
		(* This is the structure that defines an item in a menu bar or toolbar.  
		    The idea is to create an array of such structures with the information needed to create menus or toolbars.  *)
		GnomeUIInfo* = RECORD [untagged]
			type*: GnomeUIInfoType;
			label*: LinLibc.PtrSTR;
			hint*: LinLibc.PtrSTR;
			more_info*: LinLibc.PtrVoid;
			user_data*: LinLibc.PtrVoid;
			unused_data*: LinLibc.PtrVoid;
			pixmap_type*: GnomeUIPixmapType;
			pixmap_info*: LinLibc.PtrVoid;
			accelerator_key*: INTEGER;
			ac_mods*: GdkModifierType;
			widget*: LinGtk.GtkWidget;
		END;
		
	
	PROCEDURE gnome_dialog_run* (dlg: LinGtk.GtkWidget): INTEGER;
	PROCEDURE gnome_init* (app_id, app_version: LinLibc.PtrSTR; argc: INTEGER; argv: LinLibc.StrArray);
	PROCEDURE gnome_message_box_newv*  (message,  messagebox_type: LinLibc.PtrSTR; 
																VAR buttons: ARRAY [untagged] OF LinLibc.PtrSTR): LinGtk.GtkWidget;
	PROCEDURE gnome_mdi_add_child* (mdi: LinGtk.GtkObject; child: GnomeMDIChild);
	PROCEDURE gnome_mdi_add_view* (mdi: LinGtk.GtkObject; child: GnomeMDIChild);
	PROCEDURE gnome_mdi_generic_child_new* (name: LinLibc.PtrSTR): GnomeMDIGenericChild;
	PROCEDURE gnome_mdi_generic_child_set_view_creator* (child: GnomeMDIGenericChild; 
																					func: GnomeMDIChildViewCreator; data: LinLibc.PtrVoid);			
	PROCEDURE gnome_mdi_new* (appname, title: LinLibc.PtrSTR): LinGtk.GtkObject;
	PROCEDURE gnome_mdi_open_toplevel* (mdi: LinGtk.GtkObject);
	PROCEDURE gnome_mdi_restore_state* (mdi: LinGtk.GtkObject; section: LinLibc.PtrSTR; 
															child_create_func: GnomeMDIChildCreator): INTEGER;
	PROCEDURE gnome_mdi_save_state* (mdi: LinGtk.GtkObject; section: LinLibc.PtrSTR);		
	PROCEDURE gnome_mdi_set_menubar_template* (mdi: LinGtk.GtkObject; VAR tbar_tmpl: ARRAY [untagged] OF GnomeUIInfo);
	PROCEDURE gnome_mdi_set_child_list_path* (mdi: LinGtk.GtkObject; path: LinLibc.PtrSTR);
	PROCEDURE gnome_mdi_set_mode* (mdi: LinGtk.GtkObject; mode: INTEGER);
								
	
END LinGnomeUI.