MODULE LinGnomeSupport ["libgnomesupport.so"];

	IMPORT LinLibc;

	PROCEDURE poptStrerror* (error: INTEGER): LinLibc.PtrSTR;
	
END LinGnomeSupport.