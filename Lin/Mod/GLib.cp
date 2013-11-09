MODULE LinGLib ["libglib.so"];

	IMPORT LinLibc;

	TYPE
		GList* = POINTER TO GListDesc;
		GListDesc* = RECORD [untagged] 
			data*: LinLibc.PtrVoid;
			next*, prev*: GList
		END;
		
	PROCEDURE g_list_append* (list: GList; data: LinLibc.PtrVoid): GList;
	PROCEDURE g_list_first* (list: GList): GList;
	PROCEDURE g_list_free* (list: GList);
	PROCEDURE g_free* (mem: LinLibc.PtrVoid);
	
END LinGLib.