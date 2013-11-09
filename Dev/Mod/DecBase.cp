MODULE DevDecBase;

	IMPORT Meta;
	
	TYPE
		GetProc* = PROCEDURE(OUT ch: BYTE);
		PutProc* = PROCEDURE(ch: CHAR);
		DecodeProc* = PROCEDURE(VAR ad: INTEGER; Put: PutProc; Get: GetProc; mode: INTEGER);
		
		PValue = RECORD (Meta.Value)
			p: DecodeProc
		END;
	
	PROCEDURE GetDecode* (mod: ARRAY OF CHAR; OUT decode: DecodeProc);
		VAR module, proc: Meta.Item; val: PValue; ok: BOOLEAN;
	BEGIN
		Meta.Lookup(mod$, module);
		IF module.obj = Meta.modObj THEN
			module.Lookup("DecodeLine", proc);
			IF proc.obj = Meta.procObj THEN
				proc.GetVal(val, ok);
				IF ok THEN decode := val.p END
			END
		END
	END GetDecode;
	
END DevDecBase.

	