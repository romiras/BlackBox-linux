MODULE HostRegistry;

	(* Dummy implementation *)
	
	(* 
		TODO:
		
		This should probably be implemented using files instead of GnomeConfig, but maybe an abstraction on top should be added, 
		something like StdRegistry.
	*)

	VAR
		localeId*: INTEGER;

	PROCEDURE ReadBool* (key: ARRAY OF SHORTCHAR; VAR x: BOOLEAN; VAR res: INTEGER);
	BEGIN res := -1
	END ReadBool;
	
	PROCEDURE ReadInt* (key: ARRAY OF SHORTCHAR; VAR x, res: INTEGER);
	BEGIN res := -1
	END ReadInt;
	
	PROCEDURE ReadIntList* (key: ARRAY OF SHORTCHAR; VAR x: ARRAY OF INTEGER; VAR res: INTEGER);
	BEGIN res := -1
	END ReadIntList;
	
	PROCEDURE ReadString* (key: ARRAY OF SHORTCHAR; VAR str: ARRAY OF SHORTCHAR; VAR res: INTEGER);
	BEGIN res := -1
	END ReadString;
	
	PROCEDURE WriteBool* (key: ARRAY OF SHORTCHAR; x: BOOLEAN);
	BEGIN
	END WriteBool;
	
	PROCEDURE WriteInt* (key: ARRAY OF SHORTCHAR; x: INTEGER);
	BEGIN
	END WriteInt;
	
	PROCEDURE WriteIntList* (key: ARRAY OF SHORTCHAR; VAR x: ARRAY OF INTEGER);
	BEGIN
	END WriteIntList;
	
	PROCEDURE WriteString* (key: ARRAY OF SHORTCHAR; str: ARRAY OF SHORTCHAR);
	BEGIN
	END WriteString;
	

END HostRegistry.