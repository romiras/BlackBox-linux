MODULE LinX11 ["libX11.so"];

	IMPORT LinLibc;
	
	TYPE
		Display* = INTEGER;
	
	
	PROCEDURE XFreeFontNames* (list: LinLibc.StrArray);	
	PROCEDURE XListFonts* (display: Display; pattern: LinLibc.PtrSTR; maxnames: INTEGER; 
									VAR actual_count_return: INTEGER): LinLibc.StrArray;
	PROCEDURE XOpenDisplay* (VAR [nil] display_name: LinLibc.PtrSTR): Display;

END LinX11.