MODULE DevInterpreter;  (* 28 Jan 1998 *)

IMPORT 
	TextModels, Stores, Sequencers, Models, Kernel, Dialog, Views, StdLog, TextViews, DevMarkers,
	SDR:= CommSDR, 
		OPS:=DevCPS,   (* scanner *) 
		OPT:=DevCPT,   (* import *) 
		OPB:=DevCPB,   (* functions *)
		OPM:=DevCPM,  (* errors *)
		OPV:=DevM68CPV,  (* typSize *)
		OPP:=DevCPPI;  (* Expression *)	

	CONST
		(*OP2 Options*)	
		oinxchk=0; otypchk=1; onilchk=2; onewsf=3; optrinit=4; orefinfo=5; odbginfo=6; oassert=7;
		ofindpc=9; ogc=10; oportbl=11; ooberon2=12;
		defopt={oinxchk, otypchk, onilchk, optrinit, ogc, orefinfo, oassert, ooberon2};


		(*symbol values*)
		eql = 9;rparen = 23; ident = 48; semicolon = 49;lparen = 40; eof = 76;period = 19;comma = 20; 
		
		(* Structure forms *)
		Undef = 0; Byte = 1; Bool = 2; Char8 = 3; Int8 = 4; Int16 = 5; Int32 = 6;
		Real32 = 7; Real64 = 8; Set = 9; String8 = 10; NilTyp = 11; NoTyp = 12;
		Pointer = 13; ProcTyp = 14; Comp = 15;
		Char16 = 16; String16 = 17; Int64 = 18;
		intSet = {Int8..Int32, Int64}; charSet = {Char8, Char16};
		
		(* composite structure forms *)
		Basic = 1; Array = 2; DynArr = 3; Record = 4;


		(* object modes *)
		Var = 1; Con = 3; Mod = 11;
		
		(* nodes classes *)
		Nconst = 7; 
		
	VAR
		level: BYTE;
		sourceR: TextModels.Reader;
		endText: INTEGER;
		modname, procname: OPT.Name; key: INTEGER;
		
		(* par: ARRAY 8*1024 OF BYTE; pos: SHORTINT; *)
		sym: BYTE;
		

	PROCEDURE err(n: INTEGER);
	BEGIN OPM.err(n) END err;

	PROCEDURE CheckSym(s: INTEGER);
	BEGIN
		IF sym = s THEN OPS.Get(sym) ELSE err(s) END
	END CheckSym;
	
(*
	modname string ¦ varname string ¦ key long ¦ [ type char ¦ data char ..char] ¦ 0X
*)

	PROCEDURE InsertCmd(VAR sdrbuf: SDR.Buffer);
	VAR i: INTEGER;
	BEGIN  
		i:=1; WHILE	(i < OPT.nofGmod) & (modname#OPT.GlbMod[i].name^)	DO	INC(i)	END;
		IF	i < OPT.nofGmod	THEN	key:=OPT.GlbMod[i].adr ELSE err(152) END;
		sdrbuf.WriteSString(modname); sdrbuf.WriteSString(procname); 
		SDR.SwapL(key); sdrbuf.WriteInt(key);
	END InsertCmd;

	PROCEDURE InsertPar(n: OPT.Node; fp: OPT.Object; VAR sdrbuf: SDR.Buffer);
	VAR cval: OPT.Const; 
	BEGIN
		CASE	n.class	OF
		| Nconst: cval:=n.conval;
			sdrbuf.WriteByte(n.typ.form);
			CASE	n.typ.form	OF
			| Bool:  sdrbuf.WriteBool(cval.intval#0);
			| Byte, Char8, Int8: sdrbuf.WriteByte(SHORT(SHORT(cval.intval)));
			| Int16, Char16: sdrbuf.WriteSInt(SHORT(cval.intval));
			| Int32, NilTyp, Pointer:  sdrbuf.WriteInt(cval.intval);
			| Real32:	sdrbuf.WriteSReal(SHORT(cval.realval)); 
			| Real64:	sdrbuf.WriteReal(cval.realval); 
			| Set:	sdrbuf.WriteSet(cval.setval); 
			| String8, String16: 
				IF (fp.typ.form=Comp) & (fp.typ.comp=DynArr) THEN sdrbuf.WriteByte(DynArr)
				ELSIF (fp.typ.form=Comp) & (fp.typ.comp=Array) THEN sdrbuf.WriteByte(Array)
				ELSE HALT(100) 
				END;
				sdrbuf.WriteSString(cval.ext^); 
			ELSE HALT(100); err(14); END;
		ELSE 
		END;
	END InsertPar;

	PROCEDURE Block (VAR sdrbuf: SDR.Buffer);
		VAR fpar, obj: OPT.Object; x, apar: OPT.Node; selfName: OPT.Name; done: BOOLEAN;
	BEGIN 
		LOOP
			IF sym = ident THEN OPS.Get(sym) ELSIF sym=eof THEN EXIT ELSE err(14); EXIT END;
			IF sym = eql THEN
				OPT.Insert(OPS.name, obj); (*  CheckMark(obj^.vis);  export  *)
				obj^.typ := OPT.int8typ; obj^.mode := Var;	(* Var to avoid recursive definition *)
				OPS.Get(sym); 
				OPP.sym:=sym; OPP.Expression(x);  sym:=OPP.sym; IF x^.class # Nconst THEN err(50); EXIT END;
				obj^.mode := Con; obj^.typ := x^.typ; obj^.conval := x^.conval; (* ConstDesc ist not copied *)
				CheckSym(semicolon)
			END;
			IF sym = period THEN
				x := NIL;
				OPT.Find(OPS.name, obj); modname := OPS.name$;
				IF obj=NIL THEN 
					OPT.Import(modname, modname, done); 
					OPT.Find(OPS.name, obj) 
				END;
				IF (obj # NIL) & (obj^.mode = Mod) THEN
					OPS.Get(sym);
					IF sym = ident THEN procname := OPS.name$;
						OPT.FindImport(OPS.name, obj, obj); (* obj.adr, obj.linkadr ? *)
						OPS.Get(sym)
					ELSE err(ident); obj := NIL
					END
				ELSE err(0) END;
				IF obj#NIL THEN x:=OPB.NewLeaf(obj); (* selector(x); *) OPB.PrepCall(x, fpar) END;
				(* Check of Proc *)
				InsertCmd(sdrbuf); 
				IF sym = lparen THEN  OPS.Get(sym); 
					IF sym # rparen THEN
						LOOP 
							OPP.sym:=sym; OPP.Expression(apar); sym:=OPP.sym;
							IF fpar # NIL THEN OPB.Param(apar, fpar);  InsertPar(apar, fpar, sdrbuf); fpar := fpar^.link;
							ELSE err(64)
							END ;
							IF sym = comma THEN OPS.Get(sym)
							ELSIF (lparen <= sym) & (sym <= ident) THEN err(comma)
							ELSE EXIT
							END
						END
					END ;
					IF fpar # NIL THEN err(65) END;
					CheckSym(rparen)
				ELSE apar := NIL;
					IF fpar # NIL THEN err(65) END
				END;
				sdrbuf.WriteByte(0); (* InsertCall; *)
				CheckSym(semicolon);
				IF sourceR.Pos()>endText THEN sym:=eof; EXIT END;
			END;
		END;
	END Block;

	PROCEDURE Module (source: TextModels.Reader; opt: SET; log: TextModels.Model; VAR error: BOOLEAN; VAR sdrbuf: SDR.Buffer);
		VAR ext, new, clean: BOOLEAN; mod: OPT.Node;
			script: Stores.Operation; text: TextModels.Model; modName: OPT.Name; newSF: BOOLEAN; 				
			str: Dialog.String; procdec, statseq: OPT.Node; c: INTEGER;
	BEGIN
(*
		text := source.Base(); 
		
		clean := (text.ThisDomain() # NIL) & ~text.ThisDomain()(Sequencers.Sequencer).Dirty();
		Models.BeginScript(text, "#Dev:InsertMarkers", script); 
*)
		OPM.Init(source, log); 
		IF 31 IN opt THEN INCL(OPM.options, 31) END;
		OPT.Init(opt);
		OPB.typSize := OPV.TypeSize;
		OPS.Init; 
		newSF:=TRUE; 
		OPT.OpenScope(0, NIL); level := 0;
		OPT.Open("Interpreter");
		OPS.Get(sym); c := OPM.errpos;
		Block(sdrbuf);
		OPT.CloseScope; OPM.LogWLn; error:=~OPM.noerr;
		mod:=NIL; Kernel.FastCollect;
		IF error  THEN
			OPM.InsertMarks(source.Base());
			Dialog.MapString("#Dev:ErrorsDetected", str);
			StdLog.String(" "); StdLog.String(str);StdLog.Ln;
		END;		
(*		
		Models.EndScript(source.Base(), script);
		IF clean THEN text.ThisDomain()(Sequencers.Sequencer).SetDirty(FALSE) END
*)
	END Module;

	PROCEDURE Open;
	BEGIN
		Dialog.ShowStatus("#Dev:Interpreting");
	END Open;
	
	PROCEDURE Close;
	BEGIN
		IF OPM.noerr THEN Dialog.ShowStatus("#Dev:Ok") END;
		sourceR := NIL;
		Kernel.Cleanup;
	END Close;

	PROCEDURE CompileSelection* (VAR text: TextModels.Model; VAR sdrbuf: SDR.Buffer): BOOLEAN;
		VAR  error: BOOLEAN; v: TextViews.View; 
	BEGIN
		Open;
		endText:=text.Length();
		sourceR := text.NewReader(NIL); sourceR.SetPos(0); 
		Module(sourceR, defopt, StdLog.text, error, sdrbuf);
		IF error THEN 
			v:= TextViews.dir.New(text); Views.OpenAux(v, "Interpreter");
			DevMarkers.ShowFirstError(text, TextViews.focusOnly) 
		END;
		Close;
		RETURN ~error
	END CompileSelection;

END DevInterpreter.
