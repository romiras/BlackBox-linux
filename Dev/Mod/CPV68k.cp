MODULE DevCPV68k;
(* control module for MC68020 backend: based on OPV Samuel Urech 23.2.93 *)
(* ip  *)

(* 
	bh	4.4.00	eliminated Trap upon Cancel in "create directory Code" dialog
	bh	31.8.99	Overflow check in TypeSize
	ip	26.11.97	PushParams: adapted to passing Strings to ARRAYs OF CHAR
	bh	27.10.97	CPE version
	cp	13-Oct-97	TypeSize changed according to bh
	ip	2.9.97	use of MinMax instead of Min, Max
	ip	10.8.97	Assign defined, StringCopy removed.
	ip	10.8.97	CompStat & CompRelease: defined, use of NewFunc for temporary Objects
	ip	10.8.97	Ncomp: defined and used in Expr and StatSeq
	ip	10.8.97	NewFunc: moved outside of StatSeq
	ip	10.8.97	call DevCPM.Mark for error 129
	ip	10.8.97	Imports renamed to Dev[Host]CPx
	ip	2.6.97	Module: generate MacsBug-Strings for open and close section
	bh	13.5.97	Module: changed for close section
	ip	27.2.97	StringCopy:
	bh	28.1.97	Unions in TypeSize
	bh	27.1.97	Various changes for Compas (marked with "!!!")
	bh	11.1.97	TypeSize changed for portable symbol files
	bh	7.1.97	Checkpc calls changed 
	bh	26.11.96	Fix in PushParam:  passing an <X> to <pointer to X> works now
	ip	22.7.96	DevHostCPL.Free anstelle von FreeReg
	ip	11.6.96	PushParams: behandelt ARRAY [nil] OF und VAR [nil] xy: ARRAY
	ip	6.6.96	NewFunc: testet auf negative Parameter
	
	RELEASE 1.2
	ip	28.3.96	If-Statement benutzt FreeAllRegs vor Expression
	ip	1.2.96	PushParams: behandelt VAR[nil]
	ip	1.2.96	MakeProc nachgeführt
	ip	1.2.96	Designator übernimmt recv - Parameter zur Weitergabe an MakeProc
	ip	1.2.96	DeRef nachgeführt.
	ip	26.1.96	NewFunc: uebergibt speziellen Tag anstelle des ArrayTyps
	ip	24.1.96	NewFunc: uebergibt ArrayTyp als Parameter an NewArr
	ip	22.5.95	Init: Macsbugstring als Default
	ip	7.4.95	Prozedurnamen in der Ref ohne "%" und "("
*)

	IMPORT DevCPM, DevCPT, DevCPC68k, DevCPL68k, DevCPE, DevCPH;
	
	CONST
		processor* = 20;	(* 68000 *)	(* bh *)

		(* object modes *)
		Var = 1; VarPar = 2; Con = 3; Fld = 4; Typ = 5; LProc = 6; XProc = 7;
		SProc = 8; CProc = 9; IProc = 10; Mod = 11; Head = 12; TProc = 13;

		(* opcodes *)
		ASh = 0; LSh = 1; ROt = 3;

		(* Condition codes *)
		false = 1; true = 0;
		CC = 4; CS = 5; EQ = 7; GE = 12; GT = 14; HI = 2; LE = 15;
		LS = 3; LT = 13; MI = 11; NE = 6; PL = 10; VC = 8; VS = 9;

		(* operation node subclasses *)
		times = 1; slash = 2; div = 3; mod = 4;
		and = 5; plus = 6; minus = 7; or = 8; eql = 9;
		neq = 10; lss = 11; leq = 12; gtr = 13; geq = 14;
		in = 15; is = 16; ash = 17; msk = 18; len = 19;
		conv = 20; abs = 21; cap = 22; odd = 23; not = 33;
		(* SYSTEM *)
		adr = 24; cc = 25; bit = 26; lsh = 27; rot = 28; val = 29;
		min = 34; max = 35; typfn = 36;
		thisrecfn = 45; thisarrfn = 46;

		(* structure forms *)
		Undef = 0; Byte = 1; Bool = 2; Char8 = 3; Int8 = 4; Int16 = 5; Int32 = 6;
		Real = 7; LReal = 8; Set = 9; String8 = 10; NilTyp = 11; NoTyp = 12;
		Pointer = 13; ProcTyp = 14; Comp = 15;
		AnyPtr = 14; AnyRec = 15;	(* sym file only *)
		Char16 = 16; String16 = 17; Int64 = 18;
		
		(* composite structure forms *)
		Basic = 1; Array = 2; DynArr = 3; Record = 4;

		intSet = { Int8, Int16, Int32 }; realSet = { Real, LReal, Int64 };

		(* node classes *)
		Nvar = 0; Nvarpar = 1; Nfield = 2; Nderef = 3; Nindex = 4; Nguard = 5; Neguard = 6;
		Nconst = 7; Ntype = 8; Nproc = 9; Nupto = 10; Nmop = 11; Ndop = 12; Ncall = 13;
		Ninittd = 14; Nif = 15; Ncaselse = 16; Ncasedo = 17; Nenter = 18; Nassign = 19;
		Nifelse =20; Ncase = 21; Nwhile = 22; Nrepeat = 23; Nloop = 24; Nexit = 25;
		Nreturn = 26; Nwith = 27; Ntrap = 28;
		Ncomp = 30;

		(* function numbers *)
		assign = 0; newfn = 1; incfn = 13; decfn = 14;
		inclfn = 15; exclfn = 16; copyfn = 18; assertfn = 32;
		lchrfn = 33; lentierfcn = 34;

		(* SYSTEM function numbers *)
		getfn = 24; putfn = 25; getrfn = 26; putrfn = 27; sysnewfn = 30; movefn = 31;

		VarParSize = DevCPM.PointerSize;
		RecVarParSize = 2 * DevCPM.PointerSize;
		ParOff = 8;

		(* procedure flags *)
		hasBody = 1; isRedef = 2;
		
		(* attribute flags (attr.adr, struct.attribute, proc.conval.setval) *)
		newAttr = 16; absAttr = 17; limAttr = 18; empAttr = 19; extAttr = 20;
		
		(* case statement flags (conval^.setval) *)
		useTable = 1; useTree = 2;
		
		(* sysflag *)
		untagged = 1; handle = 2; union = 7;

		(* accessibility of objects *)
		internal = 0; external = 1; externalR = 2; inPar = 3; outPar = 4;

		(* trap numbers *)
		withTrap = 129; caseTrap = 130; funcTrap = 131; typTrap = 132;
		recTrap = 133; ranTrap = 134; inxTrap = 135; copyTrap = 136;
		stackTrap = 137;
		NILTrap = 201;

	VAR assert, macsbug : BOOLEAN;
			loopEnd : DevCPL68k.Label;
			dummy: DevCPL68k.Item;
			snsz: SHORTINT;

(* Typeallocation based on IOPV.Mod bh: 16.6.94 *)

	PROCEDURE Init*(opt: SET);
		CONST ass = 2;
	BEGIN
		DevCPL68k.Init(opt);  DevCPC68k.Init(opt);
		assert := ass IN opt; macsbug := TRUE;
	END Init;

	PROCEDURE Close*;
	BEGIN
		DevCPL68k.Close
	END Close;

	PROCEDURE FPIntfProc(obj: DevCPT.Object);
		VAR fprint: INTEGER;
	
		PROCEDURE Par(par: DevCPT.Object; no: INTEGER);
		VAR
			typ: DevCPT.Struct; fpinc: INTEGER;
		BEGIN
			IF par # NIL THEN
				Par(par.link, no+1);
				typ := par.typ;
				IF (par.mode = VarPar) OR ((typ.form = Comp) & (typ.size > 4)) THEN
					fpinc := 3;
				ELSE
					CASE typ.size OF
					1: fpinc := 1;
					|2: fpinc := 2;
					ELSE fpinc := 3;
					END;
				END;
				fpinc := fpinc * 64;
				WHILE no # 0 DO
					fpinc := fpinc * 4;
					DEC(no);
				END;
				INC(fprint, fpinc);
			END;
		END Par;
		
	BEGIN
		fprint := 0;
		Par(obj.link, 0);
		IF (obj.typ # NIL) & (obj.typ.form # NoTyp) THEN
			CASE obj.typ.size OF
			1: INC(fprint, 16);
			|2: INC(fprint, 32);
			|4: INC(fprint, 48);
			ELSE
			END;
		END;
		obj.fprint := fprint; obj.fpdone := TRUE
	END FPIntfProc;

	PROCEDURE Align(VAR offset: INTEGER; size: INTEGER; align: BYTE); (* ip *)
	BEGIN
		IF size > 1 THEN INC(offset, offset MOD align) END;
	END Align;
	
	PROCEDURE NegAlign(VAR offset: INTEGER; size: INTEGER; align: BYTE); (* ip *)
	BEGIN
		IF size > 1 THEN DEC(offset, offset MOD align) END;
	END NegAlign;
	
(* ----------------------------------------------------- !!!
	reference implementation of TypeSize for portable symbol files
	mandatory for all non-system structures

	PROCEDURE TypeSize (typ: OPT.Struct);
		VAR f, c: SHORTINT; offset: LONGINT; fld: OPT.Object; btyp: OPT.Struct;
	BEGIN
		IF typ.size = -1 THEN
			f := typ.form; c := typ.comp; btyp := typ.BaseTyp;
			IF c = Record THEN
				IF btyp = NIL THEN offset := 0 ELSE TypeSize(btyp); offset := btyp.size END;
				fld := typ.link;
				WHILE (fld # NIL) & (fld.mode = Fld) DO
					btyp := fld.typ; TypeSize(btyp);
					IF btyp.size >= 4 THEN INC(offset, (-offset) MOD 4)
					ELSIF btyp.size >= 2 THEN INC(offset, offset MOD 2)
					END;
					fld.adr := offset; INC(offset, btyp.size);
					fld := fld.link
				END;
				IF offset > 2 THEN INC(offset, (-offset) MOD 4) END;
				typ.size := offset; typ.align := 4;
				typ.n := -1  (* methods not counted yet *)
			ELSIF c = Array THEN
				TypeSize(btyp);
				typ.size := typ.n * btyp.size
			ELSIF f = Pointer THEN
				typ.size := OPM.PointerSize
			ELSIF f = ProcTyp THEN
				typ.size := OPM.ProcSize
			ELSE (* c = DynArr *)
				TypeSize(btyp);
				IF btyp.comp = DynArr THEN typ.size := btyp.size + 4
				ELSE typ.size := 8
				END
			END
		END
	END TypeSize;

----------------------------------------------------- *)

	PROCEDURE TypeSize*(typ: DevCPT.Struct);	(* also called from DevCPT.InStruct for arrays *)
		VAR f, c: BYTE; offset, align, falign: INTEGER;
			fld: DevCPT.Object; btyp: DevCPT.Struct;
	BEGIN
		IF typ = DevCPT.undftyp THEN DevCPM.err(58)
		ELSIF typ^.size = -1 THEN
			f := typ^.form; c := typ^.comp; btyp := typ^.BaseTyp;
			IF c = Record THEN
				IF btyp = NIL THEN offset := 0 ELSE TypeSize(btyp); offset := btyp^.size END ;
				fld := typ^.link;
				WHILE (fld # NIL) & (fld^.mode = Fld) DO
					btyp := fld^.typ; TypeSize(btyp);
					IF typ.sysflag = union THEN
						fld.adr := 0;
						IF btyp.size > offset THEN offset := btyp.size END;
					ELSE
						IF typ.sysflag = 0 THEN	(* !!! *)
							IF btyp.size >= 4 THEN INC(offset, (-offset) MOD 4)
							ELSIF btyp.size >= 2 THEN INC(offset, offset MOD 2)
							END
						ELSE Align(offset, btyp^.size, 2)
						END;
						fld^.adr := offset;
						IF offset <= MAX(INTEGER) - 4 - btyp.size THEN INC(offset, btyp.size)
						ELSE offset := 4; DevCPM.Mark(214, typ.txtpos)
						END						
					END;
					fld := fld^.link
				END ;
				IF typ.sysflag = 0 THEN	(* !!! *)
					IF offset > 2 THEN INC(offset, (-offset) MOD 4) END
				ELSE Align(offset, 2, 2)
				END;
				typ^.size := offset; typ.align := 4;
				typ^.n := -1  (* methods not counted yet *)
			ELSIF c = Array THEN
				TypeSize(btyp);
				IF (btyp.size = 0) OR (typ.n <= MAX(INTEGER) DIV btyp.size) THEN typ.size := typ.n * btyp.size
				ELSE typ.size := 4; DevCPM.Mark(214, typ.txtpos)
				END
			ELSIF f = Pointer THEN
				typ^.size := DevCPM.PointerSize
			ELSIF f = ProcTyp THEN
				typ^.size := DevCPM.ProcSize
			ELSE (* c = DynArr *)
				TypeSize(btyp);
				IF typ.untagged THEN
					typ^.size := 4
				ELSE
					IF btyp^.comp = DynArr THEN typ^.size := btyp^.size + 4 ELSE typ^.size := 8 END;
				END
			END
		END
	END TypeSize;

	PROCEDURE CountTProcs(rec: DevCPT.Struct);
		VAR btyp: DevCPT.Struct;

		PROCEDURE TProcs(obj: DevCPT.Object);	(* obj^.mnolev = 0, TProcs of base type already counted *)
			VAR redef: DevCPT.Object;
		BEGIN
			IF obj # NIL THEN
				TProcs(obj^.left);
				IF obj^.mode = TProc THEN
					DevCPT.FindBaseField(obj^.name^, rec, redef);
					(* obj^.adr := 0 *)
					IF redef # NIL THEN
						obj^.num := redef^.num (*mthno*);
						IF ~(isRedef IN obj^.conval^.setval) THEN DevCPM.err(119) END
					ELSE
						obj^.num := rec^.n; INC(rec^.n)
					END ;
					IF obj.conval.setval * {hasBody, absAttr, empAttr} = {} THEN DevCPM.Mark(129, obj.adr) END
				END ;
				TProcs(obj^.right)
			END
		END TProcs;

	BEGIN
		IF rec^.n = -1 THEN
			IF rec.untagged THEN rec.n := 0 ELSE rec.n := DevCPT.anytyp.n END;
			btyp := rec^.BaseTyp;
			IF btyp # NIL THEN
				CountTProcs(btyp); rec^.n := btyp^.n;
			END;
			TProcs(rec^.link)
		END
	END CountTProcs;

	PROCEDURE ^Parameters(firstPar, proc: DevCPT.Object);

	PROCEDURE ^TProcedures(obj: DevCPT.Object);

	PROCEDURE TypeAlloc(typ: DevCPT.Struct);
		VAR f, c: SHORTINT; fld: DevCPT.Object; btyp: DevCPT.Struct;
	BEGIN
		IF ~typ^.allocated THEN	(* not imported, not predefined, not allocated yet *)
			typ^.allocated := TRUE;
			TypeSize(typ);
			f := typ^.form; c := typ^.comp; btyp := typ^.BaseTyp;
			IF c = Record THEN
				CountTProcs(typ);
				IF typ^.extlev > DevCPM.MaxExts THEN DevCPM.Mark(233, typ^.txtpos) END;
				IF btyp # NIL THEN
					TypeAlloc(btyp);
				END;
				IF ~typ^.untagged THEN DevCPE.AllocTypDesc(typ) END;
				fld := typ^.link;
				WHILE (fld # NIL) & (fld^.mode = Fld) DO
					TypeAlloc(fld^.typ); fld := fld^.link
				END;
				TProcedures(typ^.link)
			ELSIF f = Pointer THEN
				IF btyp = DevCPT.undftyp THEN
					DevCPM.Mark(128, typ^.txtpos)
				ELSE
					TypeAlloc(btyp);
				END
			ELSIF f = ProcTyp THEN
				TypeAlloc(btyp);
				Parameters(typ^.link, NIL)
			ELSE (* c IN {Array, DynArr} *) 
				TypeAlloc(btyp);
			END
		END
	END TypeAlloc;

	PROCEDURE Parameters(firstPar, proc: DevCPT.Object);
		(* must correspond to DevCPC68k.ReceiverOffset *)
	(* firstPar^.mnolev = 0 *)
		VAR padr: INTEGER;

		PROCEDURE Par(par: DevCPT.Object);
			VAR typ: DevCPT.Struct;
		BEGIN
			IF par # NIL THEN
				Par(par.link);
				typ := par.typ; TypeAlloc(typ);
				IF (par.mode = VarPar) & (typ.comp # DynArr) THEN
					par.adr := padr;
					IF (typ^.comp = Record) & ~typ^.untagged THEN INC(padr, 8) ELSE INC(padr, 4) END;
				ELSIF (typ.form = Comp) & (typ.comp # DynArr) & (typ.size > 4) THEN
					par.num := padr;
					INC(padr, 4);
					IF proc # NIL THEN
						DEC(proc.conval.intval2, typ.size); NegAlign(proc.conval.intval2, typ.size, 2);
						par.adr := proc.conval.intval2;
					END;
				ELSE
					par.adr := padr;
					INC(padr, typ.size); Align(padr, 2, 2);
				END;
			END;
		END Par;
			
	BEGIN
		padr := ParOff;
		IF (proc # NIL) & (proc.mnolev > 0) THEN INC(padr,4); END;
		Par(firstPar);
		IF proc # NIL THEN
			proc^.conval^.intval := padr;
		END
	END Parameters;
	
	PROCEDURE Variables(var: DevCPT.Object; VAR varSize: INTEGER); (* allocates only offsets, regs allocated in DevCPC68k.Enter *)
		VAR adr: INTEGER; typ: DevCPT.Struct;
	BEGIN
		adr := varSize;
		WHILE var # NIL DO
			typ := var^.typ; TypeAlloc(typ);
			DEC(adr, typ.size); NegAlign(adr, typ.size, 4);
			var^.adr := adr;
			var := var^.link
		END;
		NegAlign(adr, 4, 4); varSize := adr
	END Variables;
	
	PROCEDURE ^Objects(obj: DevCPT.Object);

	PROCEDURE Procedure(obj: DevCPT.Object);
	(* obj^.mnolev = 0 *)
		VAR oldPos: INTEGER;
	BEGIN
		oldPos := DevCPM.errpos; DevCPM.errpos := obj^.scope^.adr;
		TypeAlloc(obj^.typ);
		obj^.conval^.intval2 := 0;
		Variables(obj^.scope^.scope, obj^.conval^.intval2);	(* local variables *)
		Parameters(obj^.link, obj);
		IF ~(hasBody IN obj^.conval^.setval) THEN DevCPM.Mark(129, obj.adr) END ;
		Objects(obj^.scope^.right);
		DevCPM.errpos := oldPos;
(*
		IF (obj.sysflag = -20)
			OR (obj.mnolev < 0)
				& (DevCPT.GlbMod[-obj.mnolev].strings # NIL) & (DevCPT.GlbMod[-obj.mnolev].strings.ext # NIL) THEN
			FPIntfProc(obj) (* Interface Procedure, generate Fingerprint for MixedModeMgr *)
		END
*)
	END Procedure;

	PROCEDURE TProcedures(obj: DevCPT.Object);
	(* obj^.mnolev = 0 *)
		VAR par: DevCPT.Object; psize: INTEGER;
	BEGIN
		IF obj # NIL THEN
			TProcedures(obj^.left);
			IF obj^.mode = TProc THEN
				TypeAlloc(obj^.typ);
				Variables(obj^.scope^.scope, obj^.conval^.intval2);	(* local variables *)
				Parameters(obj^.link, obj);
				Objects(obj^.scope^.right)
			END;
			TProcedures(obj^.right)
		END
	END TProcedures;

	PROCEDURE Objects(obj: DevCPT.Object);
	BEGIN
		IF obj # NIL THEN
			Objects(obj^.left);
			IF obj^.mode IN {Con, Typ, LProc, XProc, CProc, IProc} THEN
				IF (obj^.mode IN {Con, Typ}) THEN TypeAlloc(obj^.typ) ELSE Procedure(obj) END;
			END ;
			Objects(obj^.right);
		END
	END Objects;

	PROCEDURE Allocate*;
		VAR gvarSize: INTEGER;
	BEGIN
		DevCPM.errpos := DevCPT.topScope^.adr;	(* text position of scope used if error *)
		gvarSize := 0;
		Variables(DevCPT.topScope^.scope, gvarSize); DevCPE.dsize := -gvarSize;
		Objects(DevCPT.topScope^.right)
	END Allocate;

(* ------------- *)
	
	PROCEDURE BaseTyp( typ : DevCPT.Struct ) : DevCPT.Struct;
	(* Returns the record type belonging to typ. *)
	BEGIN (* BaseTyp *)
		IF typ.form = Pointer THEN RETURN typ.BaseTyp
		ELSE RETURN typ
		END
	END BaseTyp;

	PROCEDURE ^Expr( node : DevCPT.Node; VAR res : DevCPL68k.Item );

	PROCEDURE Designator( node : DevCPT.Node; recv: DevCPT.Object; VAR res : DevCPL68k.Item );
	(* Returns an item for a designator. res.mode is in { regx, pcx }. *)
	VAR
		index, tag : DevCPL68k.Item;
	BEGIN (* Designator *)
		CASE node.class OF
		Nvar, Nvarpar :
			DevCPC68k.MakeVar( node.obj, res );
		| Nfield :
			Designator( node.left, recv, res );
			DevCPC68k.MakeField( res, node.obj.adr, node.typ );
		| Nderef :
			Designator( node.left, recv, res );
			IF node.subcl # 0 THEN (* string deref: no change on item, result is nonconstant string !!! *)
			ELSE DevCPC68k.DeRef( node.typ, res, FALSE )
			END
		| Nindex :
			Expr( node.right, index );
			Designator( node.left, recv, res );
			DevCPC68k.MakeIndex( index, res );
		| Nguard, Neguard :
			Designator( node.left, recv, res );
			IF ~res.typ.untagged THEN
				DevCPC68k.MakeTag( node.left.obj, node.left.typ, res, tag );
				DevCPC68k.TypeTest( tag, BaseTyp( node.typ ), TRUE, node.class = Neguard );
			END;
		| Nproc :
			DevCPC68k.MakeProc( node.obj, recv, node.subcl, res );
		| Ntype:
			DevCPC68k.StaticTag(node.typ, res);
		| Ncall:	(* function call allowed in designator now !!! ok *)
			Expr( node, res )
		END; (* CASE *)
		res.typ := node.typ;
	END Designator;

	PROCEDURE AllocParams( formalPar : DevCPT.Object; VAR psize : INTEGER );
	(* Allocates space on the stack for the parameters and increments psize by their size. *)
	VAR typ: DevCPT.Struct;
	BEGIN (* AllocParams *)
		WHILE formalPar # NIL DO
			typ := formalPar.typ;
			IF formalPar.mode = VarPar THEN
				IF (typ.comp = Record) & ~typ.untagged THEN INC( psize, RecVarParSize )
				ELSIF typ.comp = DynArr THEN INC( psize, typ.size )
				ELSE INC( psize, VarParSize )
				END;
			ELSIF (typ.form = Comp) & (typ.comp # DynArr) & (typ.size > 4) THEN
				INC(psize, 4);
			ELSE
				INC( psize, typ.size );
			END; (* IF *)
			Align( psize, 2, 2);
			formalPar := formalPar.link;
		END; (* WHILE *)
	END AllocParams;

	PROCEDURE PushParams( formalPar : DevCPT.Object; actualPar : DevCPT.Node );
	(* Moves the actual parameters to the stack. *)
		VAR par, tag : DevCPL68k.Item;
	BEGIN (* AssignParams *)
		WHILE formalPar # NIL DO
			IF (actualPar.class = Ndop) & ((actualPar.subcl = thisarrfn) OR (actualPar.subcl = thisrecfn)) THEN
				Expr(actualPar.right, par);
				DevCPC68k.PushStack(par);	(* push type/length !!! ok *)
				Expr(actualPar.left, par);
				DevCPC68k.PushStack(par);	(* push adr !!! ok *)
			ELSIF formalPar.typ.comp = DynArr THEN
				IF actualPar.typ = DevCPT.niltyp THEN
					DevCPC68k.MakeIntConst(0, DevCPT.sysptrtyp, par);
					DevCPC68k.PushStack(par);
				ELSE
					Expr( actualPar, par );
					IF formalPar.typ.untagged THEN
						DevCPC68k.Adr(par);
						DevCPC68k.PushStack(par);
					ELSE
						DevCPC68k.PushDynArrStack( formalPar.typ, (* formalPar.adr - ParOff, *) par );
					END;
				END;
			ELSIF formalPar.mode = VarPar THEN
				IF actualPar.typ = DevCPT.niltyp THEN
					DevCPC68k.MakeIntConst(0, DevCPT.sysptrtyp, par);
					DevCPC68k.PushStack(par);
				ELSIF actualPar.class = Nconst THEN
					Expr(actualPar, par);
					DevCPC68k.PushAdrStack(par);
				ELSE
					Designator( actualPar, NIL, par );
					IF ~formalPar.typ.untagged THEN
						IF formalPar.typ.comp = Record THEN
							DevCPC68k.MakeTag( actualPar.obj, actualPar.typ, par, tag );
							DevCPC68k.PushStack(tag);
						(* ELSIFs deleted, pushing tags in case of sysptr *)
						END; (* IF *)
					END;
					DevCPC68k.PushAdrStack(par);
				END;
			ELSIF (formalPar.typ.form = Comp) & (formalPar.typ.size > 4)
				OR (actualPar.typ.form IN {String8, String16, Comp}) & (formalPar.typ.form = Pointer) THEN
				Expr(actualPar, par);
				IF (par.acttyp.form = Comp) & (par.acttyp.comp = DynArr) THEN
					(* Passing the string contents of a dynamic array to a fixed size array. Do not push the address of the address. *)
					ASSERT(par.typ.form IN {String8, String16});
					par.typ := DevCPT.sysptrtyp;
					DevCPC68k.PushStack(par);
				ELSE
					DevCPC68k.PushAdrStack(par);
				END;
			ELSE
				par.tJump := DevCPL68k.NewLabel;
				par.fJump := DevCPL68k.NewLabel;
				Expr( actualPar, par );
				DevCPC68k.Convert( par, formalPar.typ );
				DevCPC68k.PushStack(par);
				DevCPL68k.Free(par);
			END; (* IF *)
			DevCPL68k.usedRegs := DevCPL68k.reservedRegs;
			actualPar := actualPar.link;
			formalPar := formalPar.link;
		END; (* WHILE *)
	END PushParams;
	
	PROCEDURE NewFunc(node: DevCPT.Node);
	VAR
		typ, eltyp: DevCPT.Struct;
		len: DevCPT.Node;
		nilret: DevCPL68k.Label;
		nofdim, fact, n: INTEGER;
		constnofel: BOOLEAN;
		tag, designator, proc, res, nofel, adr, resadr, sp: DevCPL68k.Item;
	BEGIN
		typ := node.left.typ.BaseTyp;
		IF typ.untagged THEN DevCPM.err(138) END;
		IF typ.comp = Record THEN
			DevCPC68k.StaticTag(typ, tag);
			DevCPC68k.AddToSP(-4);
			DevCPC68k.PushStack(tag);
			DevCPC68k.MakeProc(DevCPE.KNewRec, NIL, 0, proc);
			DevCPC68k.Call(proc, DevCPE.KNewRec);
			Designator(node.left, NIL, designator); 
			res.mode := 3; res.reg := 15;
			res.typ := designator.typ; res.acttyp := designator.typ;
			DevCPC68k.Assign(res, designator);
		ELSE (* ptr to arrray *)
			eltyp := typ.BaseTyp;
			IF typ.comp = Array THEN
				 nofdim := 0; DevCPC68k.MakeIntConst(typ.n, DevCPT.int32typ, nofel); fact := typ.n;
				 constnofel := TRUE;
			ELSE (* DynArr *)
				nofdim := 1; len := node.right.link;
				Expr(node.right, nofel); DevCPC68k.Convert(nofel, DevCPT.int32typ);
				DevCPL68k.Load(nofel); DevCPC68k.PushStack(nofel);
				DevCPL68k.Trapcc(LE, 134);
				WHILE len # NIL DO
					Expr(len, res); DevCPC68k.Convert(res, DevCPT.int32typ);
					DevCPC68k.PushStack(res);
					DevCPL68k.Trapcc(LE, 134);
					DevCPC68k.Mul(DevCPT.int32typ, res, nofel); DevCPL68k.Free(res);
					DevCPL68k.Trapcc(VS, 134);
					len := len.link; INC(nofdim);
					eltyp := eltyp.BaseTyp;
				END;
				constnofel := FALSE; fact := 1;
			END;
			WHILE eltyp.comp = Array DO fact := fact * eltyp.n; eltyp := eltyp.BaseTyp END;
			IF eltyp.comp = Record THEN
				DevCPC68k.StaticTag(eltyp, tag);
			ELSIF (eltyp.form = Pointer) & ~eltyp.untagged THEN
				DevCPC68k.MakeIntConst(0, DevCPT.int32typ, tag);
			ELSE (* eltyp is pointerless basic type or untagged pointer *)
				CASE eltyp.form OF
				| Undef, Byte, Char8: n := 1;
				| Int16: n := 2;
				| Int8: n := 3;
				| Int32: n := 4;
				| Bool: n := 5;
				| Set: n := 6;
				| Real: n := 7;
				| LReal: n := 8;
				| Char16: n := 9;
				| Int64: n := 10;
				| ProcTyp: n := 11;
				| Pointer: n := 12;
				END;
				DevCPC68k.MakeIntConst(n, DevCPT.int32typ, tag);
			END;
			IF constnofel THEN
				DevCPC68k.MakeIntConst(fact, DevCPT.int32typ, nofel); nofel.obj := NIL;
			ELSIF fact > 1 THEN
				DevCPC68k.MakeIntConst(fact, DevCPT.int32typ, res);
				DevCPC68k.Mul(DevCPT.int32typ, res, nofel); DevCPL68k.Free(res);
			END;
			DevCPC68k.AddToSP(-4);
			DevCPC68k.PushStack(tag); DevCPC68k.PushStack(nofel);
			DevCPC68k.MakeIntConst(nofdim, DevCPT.int32typ, res); DevCPC68k.PushStack(res);
			DevCPC68k.MakeProc(DevCPE.KNewArr, NIL, 0, proc);
			DevCPC68k.Call(proc, DevCPE.KNewArr);
			Designator(node.left, NIL, designator);
			IF nofdim >0 THEN
				res.mode := 5; res.reg := 15;
				res.bd := 0; res.inxReg := -1; res.typ := DevCPT.int32typ; res.acttyp := DevCPT.int32typ;
				adr.mode := 1; adr.reg := DevCPL68k.GetAdrReg();
				adr.bd := 0; adr.inxReg := -1; adr.typ := DevCPT.int32typ; adr.acttyp := DevCPT.int32typ;
				DevCPL68k.Move(res, adr);
				res.mode := 3; res.reg := 15; res.typ := designator.typ; res.acttyp := designator.typ;
				DevCPC68k.Assign(res, designator);
				nilret := DevCPL68k.NewLabel;
				DevCPL68k.Jump(EQ, nilret);
				adr.mode := 5; adr.bd := 12;
				DevCPL68k.Lea(adr, adr.reg);
				adr.mode := 3; adr.bd := 0;
				WHILE nofdim > 0 DO
					DevCPL68k.Move(res, adr);
					DEC(nofdim);
				END;
				DevCPL68k.SetLabel(nilret);
			ELSE
				res.mode := 3; res.reg := 15; res.typ := designator.typ; res.acttyp := designator.typ;
				DevCPC68k.Assign(res, designator);
			END;
		END;
	END NewFunc;
	
	PROCEDURE Assign(left, right: DevCPT.Node);
	VAR dest, source, destlen: DevCPL68k.Item;
	BEGIN
		IF right.typ.form IN {String8, String16} THEN
			Designator(left, NIL, dest);
			destlen.mode := -1;
			WHILE right.class = Ndop DO
				ASSERT(right.subcl = plus);
				Expr(right.left, source);
				DevCPC68k.Copy(source, dest, destlen, TRUE, FALSE);
				DevCPL68k.Free(source);
				right := right.right
			END;
			Expr(right, source);
			DevCPC68k.Copy(source, dest, destlen, TRUE, TRUE);
			DevCPL68k.Free(source); DevCPL68k.Free(dest); DevCPL68k.Free(destlen);
		ELSE
			source.tJump := DevCPL68k.NewLabel;
			source.fJump := DevCPL68k.NewLabel;
			Expr(right, source);
			DevCPC68k.LoadCC(source);
			Designator(left, NIL, dest);
			DevCPC68k.Assign(source, dest);
			DevCPL68k.Free(dest); DevCPL68k.Free(source);
			DevCPL68k.AssignTo(left.obj);
		END;
	END Assign;
	
	PROCEDURE CompStat(node: DevCPT.Node);
	BEGIN
		WHILE (node # NIL) & DevCPM.noerr DO
			ASSERT(node.class = Nassign);
			IF node.subcl = assign THEN
				Assign(node.left, node.right);
			ELSE
				ASSERT(node.subcl = newfn);
				NewFunc(node);
				DevCPL68k.AssignTo(node.left.obj);
			END;
			node := node.link;
		END;
	END CompStat;
	
	PROCEDURE CompRelease(n: DevCPT.Node);
	BEGIN
		(* Nothing to release. Temporary are allocated in the heap. *)
	END CompRelease;

	PROCEDURE Expr( node : DevCPT.Node; VAR res : DevCPL68k.Item );
	(* Returns an item for the result of an exression. *)
		VAR expr1, expr2, expression, set, element, procItem, arr, tag, resadr, sp : DevCPL68k.Item;
			restyp: DevCPT.Struct;
				swap : DevCPL68k.Label;
				savedRegs : SET;
				psize: INTEGER;
	BEGIN (* Expr *)
		CASE node.class OF
			Nconst :
				DevCPC68k.MakeConst( node.obj, node.conval, node.typ, res );
			| Nupto :
				Expr( node.left, expr1 );
				Expr( node.right, expr2 );
				DevCPC68k.UpTo( expr1, expr2, res );
			| Nmop :
				CASE node.subcl OF
					not :
						swap := res.tJump;
						res.tJump := res.fJump;
						res.fJump := swap;
						Expr( node.left, res );
						swap := res.tJump;
						res.tJump := res.fJump;
						res.fJump := swap;
						DevCPC68k.Not( res );
					| minus :
						Expr( node.left, res );
						DevCPC68k.Neg( res );
					| is :
						Designator( node.left, NIL, res );
						tag.tJump := res.tJump;
						tag.fJump := res.fJump;
						DevCPC68k.MakeTag( node.left.obj, node.left.typ, res, tag );
						DevCPC68k.TypeTest( tag, BaseTyp( node.obj.typ ), FALSE, FALSE );
						res := tag;
					| bit :	(* int -> {int} conversion !!! ok *)
						Expr( node.left, res );
						DevCPC68k.SetElem( res );
					| conv :
						Expr( node.left, res );
						IF node.typ.form IN {String8, String16} THEN
							IF (node.typ.form = String8) & ((res.acttyp.form = String16) OR ((res.acttyp.form = Comp) & (res.acttyp.comp = Array) & (res.acttyp.BaseTyp.form = Char16))) THEN
								res.acttyp := DevCPC68k.shortString16typ;
							END;
							(* do nothing otherwise *)
							(* magic is done according to acttyp in DevCPC68k.Copy. ABRAKADABRA! *)
						ELSE	(* must include int -> BITS(int) & set -> ORD(set) conversion !!! ok *)
							DevCPC68k.Convert( res, node.typ );
						END
					| abs :
						Expr( node.left, res );
						DevCPC68k.Abs( res );
					| cap :
						Expr( node.left, res );
						DevCPC68k.Cap( res );
					| odd :
						Expr( node.left, res );
						DevCPC68k.Odd( res );
					| adr :
						Expr( node.left, res );
						DevCPC68k.Adr( res );
					| typfn :
						(* get type tag of res !!! ok *)
						Expr(node.left, expr1);
						DevCPC68k.MakeTag(node.left.obj, node.left.typ, expr1, res);
					| cc :
						DevCPC68k.MakeCocItem( SHORT( node.left.conval.intval ), res );
					| val :
						res.tJump := DevCPL68k.NewLabel;
						res.fJump := DevCPL68k.NewLabel;
						Expr( node.left, res );
						IF res.typ.comp = DynArr THEN
							DevCPC68k.GetDynArrVal( res, res.typ.untagged );
							HALT(42);
						END;
						res.typ := node.typ; res.acttyp := node.typ;
				END; (* CASE *)
			| Ndop :
				CASE node.subcl OF
					times :
						Expr( node.left, expression );
						Expr( node.right, res );
						DevCPC68k.Mul( node.typ, expression, res );
						DevCPL68k.Free(expression);
					| slash :
						Expr( node.left, res );
						Expr( node.right, expression );
						DevCPC68k.Divide( node.typ, expression, res );
						DevCPL68k.Free(expression);
					| div :
						Expr( node.left, res );
						Expr( node.right, expression );
						DevCPC68k.Div( expression, res );
						DevCPL68k.Free(expression);
					| mod :
						Expr( node.left, res );
						Expr( node.right, expression );
						DevCPC68k.Mod( expression, res );
						DevCPL68k.Free(expression);
					| and :
						savedRegs := DevCPL68k.usedRegs;
						expression.tJump := DevCPL68k.NewLabel;
						expression.fJump := res.fJump;
						Expr( node.left, expression );
						DevCPC68k.FalseJump( expression, expression.fJump );
						DevCPL68k.usedRegs := savedRegs;
						Expr( node.right, res );
						DevCPC68k.Test( res );
						res.fJump := DevCPL68k.MergedLinks( expression.fJump, res.fJump );
					| plus :
						IF node.typ.form IN {String8, String16} THEN
							DevCPM.err(265)	(* impl restr !!! *)
						ELSE
							Expr( node.left, res );
							Expr( node.right, expression );
							DevCPC68k.Plus( node.typ, expression, res );
							DevCPL68k.Free(expression);
						END
					| minus :
						Expr( node.left, res );
						Expr( node.right, expression );
						DevCPC68k.Minus( node.typ, expression, res );
						DevCPL68k.Free(expression);
					| min, max:
						Expr( node.left, res );
						Expr( node.right, expression );
						(* get min of expression & res !!! *)
						DevCPC68k.MinMax(expression, res, node.subcl = min);
						DevCPL68k.Free(expression);
					| or : 
						savedRegs := DevCPL68k.usedRegs;
						expression.tJump := res.tJump;
						expression.fJump := DevCPL68k.NewLabel;
						Expr( node.left, expression );
						DevCPC68k.TrueJump( expression, expression.tJump );
						DevCPL68k.usedRegs := savedRegs;
						Expr( node.right, res );
						DevCPC68k.Test( res );
						res.tJump := DevCPL68k.MergedLinks( expression.tJump, res.tJump );
					| eql, neq, lss, leq, gtr, geq :
						expr1.tJump := DevCPL68k.NewLabel;
						expr1.fJump := DevCPL68k.NewLabel;
						expr2.tJump := DevCPL68k.NewLabel;
						expr2.fJump := DevCPL68k.NewLabel;
						Expr( node.left, expr1 );
						DevCPC68k.LoadCC( expr1 );
						Expr( node.right, expr2 );
						DevCPC68k.Compare( node.subcl, expr1, expr2, res );
						DevCPL68k.Free(expr2); DevCPL68k.Free(expr1);
					| in :
						Expr( node.left, element );
						Expr( node.right, set );
						DevCPC68k.In( element, set, res );
						DevCPL68k.Free(set);
					| ash :
						Expr( node.left, res );
						Expr( node.right, expression );
						DevCPC68k.Shift( ASh, expression, res );
						DevCPL68k.Free(expression);
					| msk :
						Expr( node.left, res );
						DevCPC68k.Mask( -node.right.conval.intval-1, res );
					| len :
						IF node.left.typ.form IN {String8, String16} THEN
							Expr(node.left, res);
							(* get dynamic string length of res (without 0X) !!! ok *)
							DevCPC68k.StringLen(FALSE,  res);
						ELSE
							Designator( node.left, NIL, arr );
							DevCPC68k.MakeLen( arr, arr.typ.n-node.right.conval.intval, res )
						END
					| bit :
						Expr( node.left, expr1 );
						Expr( node.right, expr2 );
						DevCPC68k.SYSBit( expr1, expr2, res );
					| lsh :
						Expr( node.left, res );
						Expr( node.right, expression );
						DevCPC68k.Shift( LSh, expression, res );
						DevCPL68k.Free(expression);
					| rot :
						Expr( node.left, res );
						Expr( node.right, expression );
						DevCPC68k.Shift( ROt, expression, res );
						DevCPL68k.Free(expression);
				END; (* CASE *)
			| Ncall :
				IF node.left.obj.mode IN {XProc, LProc, IProc, TProc, CProc} THEN
					restyp := node.left.typ;
				ELSE
					IF node.left.typ.form # ProcTyp THEN HALT(43); END;
					restyp := node.left.typ.BaseTyp;
				END;
				IF restyp.form IN realSet THEN
					INC(DevCPL68k.xrealstack); DevCPC68k.AddToSP(-12);
				END;
				IF restyp.form IN realSet THEN
					resadr.mode := 1; resadr.reg := DevCPL68k.GetAdrReg();
					resadr.typ := DevCPT.sysptrtyp; resadr.acttyp := DevCPT.sysptrtyp;
					sp.mode := 1; sp.reg := 15;
					sp.typ := DevCPT.sysptrtyp; sp.acttyp := DevCPT.sysptrtyp;
					DevCPL68k.Move(sp, resadr);
					savedRegs := DevCPL68k.usedRegs - DevCPL68k.reservedRegs - {resadr.reg};
				ELSE
					savedRegs := DevCPL68k.usedRegs - DevCPL68k.reservedRegs;
				END;
				DevCPC68k.PushRegs(savedRegs);
				IF restyp.form IN realSet THEN
					DevCPC68k.PushStack(resadr);
					DevCPL68k.Free(resadr);
				ELSE
					IF restyp.size = 1 THEN
						DevCPC68k.AddToSP(- 2);
					ELSE
						DevCPC68k.AddToSP(- restyp.size);
					END;
				END;
				DevCPL68k.usedRegs := DevCPL68k.reservedRegs;
				PushParams( node.obj, node.right );
				DevCPC68k.WriteStaticLink( node.left.obj );
				IF node.right # NIL THEN
					Designator( node.left, node.right.obj, procItem );
				ELSE
					Designator( node.left, NIL, procItem );
				END;
				DevCPC68k.Call( procItem, node.left.obj );
				DevCPL68k.usedRegs := savedRegs + DevCPL68k.reservedRegs;
				DevCPC68k.GetResult( restyp, savedRegs, res );
				DevCPC68k.PopRegs( savedRegs );
			| Ncomp:
				CompStat(node.left); Expr(node.right, res); CompRelease(node.left);
		ELSE
			Designator( node, NIL, res );
		END; (* CASE *)
		res.typ := node.typ;
	END Expr;

	PROCEDURE Checkpc;
	BEGIN
		DevCPE.OutSourceRef(DevCPM.errpos);	(* bh *)
	END Checkpc;

	PROCEDURE StatSeq( node : DevCPT.Node );
	(* Generates code for a statement sequence. *)
		VAR proc : DevCPT.Object;
				designator, expression, sourceAdr, destAdr, procItem, reg, tag : DevCPL68k.Item;
				begLabel, savedLoopEnd : DevCPL68k.Label;
				psize : INTEGER;
		
		PROCEDURE CaseStatement(node : DevCPT.Node);
		(* Generates code for a case statement. *)
			VAR
				expression, c : DevCPL68k.Item;
				lab: DevCPT.Node;
				lo, hi, tab: INTEGER;
				else, end: DevCPL68k.Label;
		BEGIN (* CaseStatement *)
			else := DevCPL68k.NewLabel; end := DevCPL68k.NewLabel;
			Expr( node.left, expression );
			node := node.right;
			lo := node.conval.intval; hi := node.conval.intval2;
			DevCPC68k.Convert(expression, DevCPT.int32typ);
			DevCPC68k.MakeIntConst(lo, DevCPT.int32typ, c); DevCPL68k.Load(expression); DevCPC68k.Minus(DevCPT.int32typ, c, expression);
			DevCPL68k.Jump(LT, else);
			DevCPC68k.MakeIntConst(hi-lo, DevCPT.int32typ, c); DevCPL68k.Cmp(c, expression);
			DevCPL68k.Jump(GT, else);
			DevCPL68k.GenCaseJump(expression, hi-lo+1, tab);
			DevCPL68k.Free(expression);
			DevCPL68k.SetLabel(else);
			IF node.conval.setval # {} THEN
				StatSeq(node.right);
				DevCPL68k.Jump(true, end);
			ELSE
				DevCPC68k.Trap(caseTrap); (* Checkpc *)
			END;
			DevCPL68k.EndStat;
			node := node.left;
			WHILE node # NIL DO
				lab := node.left;
				WHILE lab # NIL DO
					DevCPL68k.SetCaseEntry(tab, lab.conval.intval-lo, lab.conval.intval2-lo);
					lab := lab.link;
				END;
				StatSeq(node.right);
				DevCPL68k.Jump(true, end);
				node := node.link;
			END;
			DevCPL68k.SetLabel(end);
		END CaseStatement;

		PROCEDURE IfStatement( node : DevCPT.Node; trap : BOOLEAN );
		(* Generates code for an IF-Statement. If trap is true, a Trap is generated in the ELSE-Case. *)
			VAR endLabel : DevCPL68k.Label;
					curNode : DevCPT.Node;
					expression : DevCPL68k.Item;
		BEGIN (* IfStatement *)
			endLabel := DevCPL68k.NewLabel;
			curNode := node.left;
			WHILE curNode # NIL DO
(*				DevCPL68k.FreeAllRegs; *)
				DevCPL68k.BegStat;
				expression.tJump := DevCPL68k.NewLabel;
				expression.fJump := DevCPL68k.NewLabel;
				DevCPM.errpos := curNode.conval.intval; Checkpc;
				Expr( curNode.left, expression );
				DevCPC68k.FalseJump( expression, expression.fJump );
				DevCPL68k.Free(expression);
				DevCPL68k.EndStat;
				StatSeq( curNode.right );
				IF ( curNode.link # NIL ) OR ( node.right # NIL ) OR trap THEN
				(* last ELSIF part with no ELSE following *)
					DevCPL68k.Jump( true, endLabel );
				END;
				DevCPL68k.SetLabel( expression.fJump );
				curNode := curNode.link;
			END; (* WHILE *)
			IF trap THEN
				DevCPM.errpos := node.conval.intval; Checkpc;
				DevCPC68k.Trap( withTrap );
			ELSE
				StatSeq( node.right );
			END; (* IF *)
			DevCPL68k.SetLabel( endLabel );
		END IfStatement;

(*		PROCEDURE Size( typ : DevCPT.Struct; node : DevCPT.Node; VAR res : DevCPL68k.Item );
		(* Returns an item that denotes the size of the memory space in bytes that has to be allocated for a dynamic array. *)
			VAR dim, offsetItem : DevCPL68k.Item;
					noflen : INTEGER;
		BEGIN (* Size *)
			Expr( node, res );
			noflen := 1;
			node := node.link;
			typ := typ.BaseTyp.BaseTyp;
			WHILE node # NIL DO
				Expr( node, dim );
				INC( noflen );
				DevCPC68k.Mul( DevCPT.int32typ, dim, res );
				node := node.link;
				typ := typ.BaseTyp;
			END; (* WHILE *)
			IF typ.size > 1 THEN
				DevCPC68k.MakeIntConst( typ.size, DevCPT.int32typ, dim );
				DevCPC68k.Mul( DevCPT.int32typ, dim, res );
			END; (* IF *)
			DevCPC68k.MakeIntConst( 4 * noflen, DevCPT.int32typ, offsetItem );
			DevCPC68k.Plus( DevCPT.int32typ, offsetItem, res );
		END Size;*)

(*		PROCEDURE EnterLengths( VAR item : DevCPL68k.Item; node : DevCPT.Node );
		(* Writes the lengths in node to the address in item. Used for NEW( p, len1, len2, ... ). *)
			VAR length, adr : DevCPL68k.Item;
		BEGIN (* EnterLengths *)
			adr := item;
			DevCPC68k.DeRef( DevCPT.sysptrtyp, adr );
			WHILE node # NIL DO
				Expr( node, length );
				DevCPC68k.Convert( length, DevCPT.int32typ );
				DevCPL68k.Move( length, adr );
				INC( adr.bd, 4 );
				node := node.link;
			END; (* WHILE *)
		END EnterLengths; *)

	BEGIN (* StatSeq *)
		WHILE ( node # NIL ) & DevCPM.noerr DO
			DevCPM.errpos := node.conval.intval;
			DevCPL68k.BegStat;
			CASE node.class OF
					Ninittd:
					| Nassign :
					Checkpc;
					CASE node.subcl OF
						assign :
							Assign(node.left, node.right);
(*							IF node.right.typ.form IN {String8, String16} THEN
								(* string concatenation !!! ok*)
								StringCopy(node.left, node.right)
							ELSE
								expression.tJump := DevCPL68k.NewLabel;
								expression.fJump := DevCPL68k.NewLabel;
								Expr( node.right, expression );
								DevCPC68k.LoadCC( expression );
								Designator( node.left, NIL, designator );
								DevCPC68k.Assign( expression, designator );
								DevCPL68k.Free(designator); DevCPL68k.Free(expression);
								DevCPL68k.AssignTo(node.left.obj);
							END;*)
						| newfn :
							NewFunc(node); 
							DevCPL68k.AssignTo(node.left.obj);
						| incfn :
							Expr( node.right, expression );
							Designator( node.left, NIL, designator );
							DevCPL68k.LoadAdr( designator );
							DevCPC68k.Increment( designator, expression );
							DevCPL68k.Free(expression); DevCPL68k.Free(designator);
							DevCPL68k.AssignTo(node.left.obj);
						| decfn :
							Expr( node.right, expression );
							Designator( node.left, NIL, designator );
							DevCPL68k.LoadAdr( designator );
							DevCPC68k.Decrement( designator, expression );
							DevCPL68k.Free(expression); DevCPL68k.Free(designator);
							DevCPL68k.AssignTo(node.left.obj);
						| inclfn :
							Expr( node.right, expression );
							Designator( node.left, NIL, designator );
							DevCPL68k.LoadAdr( designator );
							DevCPC68k.Include( designator, expression );
							DevCPL68k.Free(expression); DevCPL68k.Free(designator);
							DevCPL68k.AssignTo(node.left.obj);
						| exclfn :
							Expr( node.right, expression );
							Designator( node.left, NIL, designator );
							DevCPL68k.LoadAdr( designator );
							DevCPC68k.Exclude( designator, expression );
							DevCPL68k.Free(expression); DevCPL68k.Free(designator);
							DevCPL68k.AssignTo(node.left.obj);
						| copyfn :
							Expr( node.right, expression );
							Designator( node.left, NIL, designator );
							reg.mode := -1;
							DevCPC68k.Copy( expression, designator, reg, FALSE, TRUE );
							DevCPL68k.Free(expression); DevCPL68k.Free(designator); DevCPL68k.Free(reg);
							DevCPL68k.AssignTo(node.left.obj);
						| getfn :
							Expr( node.right, sourceAdr );
							Designator( node.left, NIL, designator );
							DevCPL68k.LoadAdr( designator );
							DevCPC68k.SYSGet( sourceAdr, designator );
							DevCPL68k.AssignTo(node.left.obj);
						| putfn :
							Expr( node.left, destAdr );
							Expr( node.right, expression );
							DevCPC68k.SYSPut( expression, destAdr );
						| getrfn :
							DevCPC68k.MakeConst( node.obj, node.right.conval, DevCPT.int16typ, reg );
							INCL(DevCPL68k.usedRegs, reg.bd);
							Designator( node.left, NIL, designator );
							DevCPL68k.LoadAdr( designator );
							DevCPC68k.SYSGetReg( designator, reg );
							EXCL(DevCPL68k.usedRegs, reg.bd);
							DevCPL68k.AssignTo(node.left.obj);
						| putrfn :
							DevCPC68k.MakeConst( node.obj, node.left.conval, DevCPT.int16typ, reg );
							Expr( node.right, expression );
							DevCPC68k.SYSPutReg( expression, reg );
(*						| sysnewfn : (* % *)
							Designator( node.left, designator );
							DevCPL68k.LoadAdr( designator );
							Expr( node.right, expression );
							DevCPC68k.SYSNew( designator, expression );*)
						| movefn :
							Expr( node.left, sourceAdr );
							Expr( node.right, destAdr );
							Expr( node.right.link, expression );
							DevCPC68k.SYSMove( destAdr, sourceAdr, expression );
					END; (* CASE *)
				| Ncall :
					Checkpc;
					PushParams( node.obj, node.right );
					DevCPC68k.WriteStaticLink( node.left.obj );
					IF node.right # NIL THEN
						Designator( node.left, node.right.obj, procItem );
					ELSE
						Designator( node.left, NIL, procItem );
					END;
					DevCPC68k.Call( procItem, node.left.obj );
				| Nifelse :
					IF ( node^.subcl # assertfn ) OR assert THEN IfStatement( node, FALSE ); END;
				| Ncase :
					Checkpc;
					CaseStatement( node );
				| Nwhile :
					Checkpc;
					begLabel := DevCPL68k.NewLabel;
					DevCPL68k.SetLabel( begLabel );
					expression.tJump := DevCPL68k.NewLabel;
					expression.fJump := DevCPL68k.NewLabel;
					Expr( node.left, expression );
					DevCPC68k.FalseJump( expression, expression.fJump );
					StatSeq( node.right );
					DevCPL68k.Jump( true, begLabel );
					DevCPL68k.SetLabel( expression.fJump );
				| Nrepeat :
					expression.tJump := DevCPL68k.NewLabel;
					expression.fJump := DevCPL68k.NewLabel;
					DevCPL68k.SetLabel( expression.fJump );
					StatSeq( node.left );
					DevCPM.errpos := node.conval.intval; Checkpc;
(*					DevCPL68k.BegStat; *)
					Expr( node.right, expression );
					DevCPC68k.FalseJump( expression, expression.fJump );
				| Nloop :
					savedLoopEnd := loopEnd;
					begLabel := DevCPL68k.NewLabel;
					loopEnd := DevCPL68k.NewLabel;
					DevCPL68k.SetLabel( begLabel );
					StatSeq( node.left );
					DevCPL68k.Jump( true, begLabel );
					DevCPL68k.SetLabel( loopEnd );
					loopEnd := savedLoopEnd;
				| Nexit :
					Checkpc;
					DevCPL68k.Jump( true, loopEnd );
				| Nreturn :
					IF node.left # NIL THEN
						Checkpc;
						expression.tJump := DevCPL68k.NewLabel;
						expression.fJump := DevCPL68k.NewLabel;
						Expr( node.left, expression )
					END;
					DevCPC68k.Return( node.obj, node.left # NIL, expression );
					DevCPL68k.Free(expression);
				| Nwith :
					IfStatement( node, node.subcl = 0 );
				| Ntrap :
					Checkpc;
					DevCPC68k.Trap( SHORT( node.right.conval.intval ) );
				| Ncomp:
					CompStat(node.left); StatSeq(node.right); CompRelease(node.left);
			END; (* CASE *)
			DevCPL68k.EndStat; (* Checkpc; *)
			node := node.link;
		END; (* WHILE *)
	END StatSeq;

	PROCEDURE procs(n: DevCPT.Node);
		VAR proc, type: DevCPT.Object; i, j: INTEGER; end: DevCPL68k.Label;
			ch: SHORTCHAR; name: ARRAY 64 OF SHORTCHAR;
			dummy: DevCPL68k.Item;
	BEGIN
		INC(DevCPL68k.level);
		WHILE (n # NIL) & DevCPM.noerr DO
			proc := n^.obj; 
			procs(n^.left);
			DevCPC68k.EnterProc(proc);
			StatSeq(n^.right);
			DevCPM.errpos := n.conval.intval2; Checkpc;
			IF proc^.typ # DevCPT.notyp THEN DevCPC68k.Trap(funcTrap); (* Checkpc *) END;
			DevCPC68k.Return(proc, FALSE, dummy);
			(* MacsBug-Strings require RTD, RTS or JMP(A0) at end *)
			IF proc^.mode = TProc THEN
				type := proc^.link^.typ^.strobj;
				i := 0; ch := type^.name[0];
				WHILE ch # 0X DO name[i] := ch; INC(i); ch := type^.name[i];  END ;
				name[i] := "."; INC(i); j := 0; ch := proc^.name[0];
				WHILE (ch # 0X) & (i < 63) DO name[i] := ch; INC(i); INC(j); ch := proc^.name[j] END ;
				name[i] := 0X;
			ELSE name := proc^.name^$
			END;
			IF macsbug THEN
				i := 0; WHILE name[i] # 0X DO INC(i); END;
				DevCPL68k.GenCh(SHORT(CHR(80H + i + snsz + 1)));
				i := 0;
				WHILE DevCPT.SelfName[i]  # 0X DO DevCPL68k.GenCh(DevCPT.SelfName[i]); INC(i); END;
				DevCPL68k.GenCh(".");
				i := 0; WHILE name[i] # 0X DO DevCPL68k.GenCh(name[i]); INC(i); END;
				IF ODD(DevCPE.pc) THEN DevCPL68k.GenCh(0X); END;
				DevCPL68k.GenCh(0X); DevCPL68k.GenCh(0X);
			END;
			DevCPE.OutRefName(name); DevCPE.OutRefs(proc^.scope^.right);
			n := n.link
		END;
		DEC(DevCPL68k.level)
	END procs;

	PROCEDURE Module*( prog : DevCPT.Node );
		VAR dummy: DevCPL68k.Item; name: ARRAY 64 OF SHORTCHAR; i: SHORTINT;
	BEGIN
		(* converts LONGINT ops to REAL ops *)
		DevCPH.UseReals(prog, {DevCPH.longMop, DevCPH.longDop});
		DevCPM.NewObj(DevCPT.SelfName);
		IF DevCPM.noerr THEN
			DevCPE.OutHeader;
			DevCPC68k.EnterMod;
			StatSeq(prog^.right);
			DevCPM.errpos := prog.conval.intval2; Checkpc;
			DevCPC68k.Return(NIL, FALSE, dummy);
			IF macsbug THEN
				snsz := 0; WHILE DevCPT.SelfName[snsz] # 0X DO INC(snsz) END;
				name := "[open]";
				i := 0; WHILE name[i] # 0X DO INC(i); END;
				DevCPL68k.GenCh(SHORT(CHR(80H + i + snsz + 1)));
				i := 0;
				WHILE DevCPT.SelfName[i]  # 0X DO DevCPL68k.GenCh(DevCPT.SelfName[i]); INC(i); END;
				DevCPL68k.GenCh(".");
				i := 0; WHILE name[i] # 0X DO DevCPL68k.GenCh(name[i]); INC(i); END;
				IF ODD(DevCPE.pc) THEN DevCPL68k.GenCh(0X); END;
				DevCPL68k.GenCh(0X); DevCPL68k.GenCh(0X);
			END;
			IF prog.link # NIL THEN	(* close section *)
				DevCPL68k.SetLabel(DevCPE.closeLbl);
				DevCPC68k.EnterMod;
				StatSeq(prog^.right);
				DevCPM.errpos := SHORT(ENTIER(prog.conval.realval)); Checkpc;
				DevCPC68k.Return(NIL, FALSE, dummy);
				IF macsbug THEN
					name := "[close]";
					i := 0; WHILE name[i] # 0X DO INC(i); END;
					DevCPL68k.GenCh(SHORT(CHR(80H + i + snsz + 1)));
					i := 0;
					WHILE DevCPT.SelfName[i]  # 0X DO DevCPL68k.GenCh(DevCPT.SelfName[i]); INC(i); END;
					DevCPL68k.GenCh(".");
					i := 0; WHILE name[i] # 0X DO DevCPL68k.GenCh(name[i]); INC(i); END;
					IF ODD(DevCPE.pc) THEN DevCPL68k.GenCh(0X); END;
					DevCPL68k.GenCh(0X); DevCPL68k.GenCh(0X);
				END;
			END;
			name := "$$"; DevCPE.OutRefName(name); DevCPE.OutRefs(DevCPT.topScope);
			procs(prog^.left);
			IF DevCPM.noerr THEN DevCPL68k.Prepare; DevCPE.OutCode END;
			IF ~DevCPM.noerr THEN DevCPM.DeleteObj END
		END
	END Module;

END DevCPV68k.