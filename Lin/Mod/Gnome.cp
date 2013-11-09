MODULE LinGnome ["libgnome.so"];
	
	IMPORT LinLibc, LinGnomeSupport;
	
	PROCEDURE gnomelib_init* (app_id, version: LinLibc.PtrSTR);
	
END LinGnome.