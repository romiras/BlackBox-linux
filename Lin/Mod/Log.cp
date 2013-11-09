MODULE LinLog;
	
	(* Implements Log.Hook and print to stnandard out *)
	
	IMPORT Dialog, LinLibc, Strings, Log;
	
	CONST 
		strLen = 1024;
		
	TYPE
		Hook = POINTER TO RECORD (Log.Hook) END;
		ShowHook = POINTER TO RECORD (Dialog.ShowHook) END;
	
	VAR
		hook: Hook; showHook: ShowHook;
		s: ARRAY strLen OF CHAR; 
		ss: ARRAY strLen OF SHORTCHAR; 
	
	PROCEDURE Printf;
		VAR res: INTEGER;
	BEGIN
		res := LinLibc.printf(ss); res := LinLibc.fflush(LinLibc.NULL)
	END Printf;
	
	(* Hook *)
	
	PROCEDURE (h: Hook) String* (IN str: ARRAY OF CHAR);
	BEGIN
		ss := SHORT(str);
		Printf()
	END String;
	
	PROCEDURE (h: Hook) Ln*;
	BEGIN
		ss[0] := 0AX; ss[1] := 0X;
		Printf()
	END Ln;
	
	PROCEDURE (h: Hook) Int* (n: INTEGER);
	BEGIN
	      Strings.IntToString(n, s);
		ss := SHORT(" " + s);
		Printf()
	END Int;
	
	PROCEDURE (h: Hook) Bool* (x: BOOLEAN);
	BEGIN
		IF x THEN ss := "$TRUE" ELSE ss := "$FALSE" END;
		Printf()
	END Bool;
	
	PROCEDURE (log: Hook) Beep*;
	BEGIN
		Dialog.Beep
	END Beep;
	
	PROCEDURE (log: Hook) Char* (ch: CHAR);
	BEGIN
		ss[0] := SHORT(ch); ss[1] := 0X;
		Printf()
	END Char;
	
	PROCEDURE (log: Hook) ClearBuf*;
	BEGIN
		(* has no buffer *)
	END ClearBuf;
	
	PROCEDURE (log: Hook) FlushBuf*;
	BEGIN
		(* has no buffer *)
	END FlushBuf;
	
	PROCEDURE (log: Hook) Guard* (o: ANYPTR): BOOLEAN;
	BEGIN
		RETURN TRUE
	END Guard;
	
	PROCEDURE (log: Hook) IntForm* (x, base, minWidth: INTEGER; fillCh: CHAR; showBase: BOOLEAN);
	BEGIN
	      Strings.IntToStringForm(x, base, minWidth, fillCh, showBase, s);
		ss := SHORT(" " + s);
		Printf()
	END IntForm;
	
	PROCEDURE (log: Hook) Para*;
	BEGIN
		log.Ln
	END Para;
	
	PROCEDURE (log: Hook) ParamMsg* (IN s, p0, p1, p2: ARRAY OF CHAR);
	BEGIN
		log.String(s + " - ParamMsg not implemented."); log.Ln
	END ParamMsg;
	
	PROCEDURE (log: Hook) Real* (x: REAL);
	BEGIN
	      Strings.RealToString(x, s);
		ss := SHORT(" " + s);
		Printf()
	END Real;
	
	PROCEDURE (log: Hook) RealForm* (x: REAL; precision, minW, expW: INTEGER; fillCh: CHAR);
	BEGIN
	      Strings.RealToStringForm(x, precision, minW, expW, fillCh, s);
		ss := SHORT(" " + s);
		Printf()
	END RealForm;
	
	PROCEDURE (log: Hook) Set* (x: SET);
		VAR i, j: INTEGER;
	BEGIN
		log.Char("{");
		i := MIN(SET);
		WHILE x # {} DO
			IF i IN x THEN log.Int(i); EXCL(x, i);
				IF (i + 2 <= MAX(SET)) & (i+1 IN x) & (i+2 IN x) THEN log.String("..");
					x := x - {i+1, i+2}; INC(i, 3);
					WHILE (i <= MAX(SET)) & (i IN x) DO EXCL(x, i); INC(i) END;
					log.Int(i-1)
				END;
				IF x # {} THEN log.String(", ") END
			END;
			INC(i)
		END;
		log.Char("}")
	END Set;
	
	PROCEDURE (log: Hook) Tab*;
	BEGIN
		ss[0] := 0BX; ss[1] := 0X;
		Printf()
	END Tab;
	
	PROCEDURE (log: Hook) View* (v: ANYPTR);
	BEGIN
		log.String("View not implemented."); log.Ln
	END View;
	
	PROCEDURE (log: Hook) ViewForm* (v: ANYPTR; w, h: INTEGER);
	BEGIN
		log.String("ViewForm not implemented."); log.Ln
	END ViewForm;
	
	
	(* ShowHook *)

	PROCEDURE (shook: ShowHook) ShowParamMsg (IN s, p0, p1, p2: ARRAY OF CHAR);
	BEGIN
		hook.String(s); hook.String(" ^0: " + p0); hook.String(" ^1: " + p1); hook.String(" ^2: " + p2); hook.Ln
	END ShowParamMsg;
	
	PROCEDURE (shook: ShowHook) ShowParamStatus (IN s, p0, p1, p2: ARRAY OF CHAR);
	BEGIN
		hook.String(s); hook.String(" ^0: " + p0); hook.String(" ^1: " + p1); hook.String(" ^2: " + p2); hook.Ln
	END ShowParamStatus;
	
	
	PROCEDURE Open*;
	BEGIN
		Log.SetHook(hook);
		Dialog.SetShowHook(showHook)
	END Open;
	
BEGIN
	NEW(showHook); 
	NEW(hook); 
END LinLog.

 DevDecoder.Decode LinLog