MODULE DevM68CPC;
(* code generator for MC68020: based on OPL Samuel Urech 26.2.93 *)
(* ip  *)

(* to do: static calls of final methods (obj.conval.setval * {absAttr, empAttr, extAttr} = {}) *)

(* 
	bh	24.2.98	pointer handling for rt gc
	bh	6.2.98	changes for Reals via Library
	bh	17.12.97	Java frontend extensions 
	ip	26.11.97	StringLen: moved the creation of the item back to prevent trashing of the types
	bh	27.10.97	CPE version
	cp	13-Oct-97	MakeTag changed according to bh
	ip	2.9.97	fixed Mod for results of 0 with different signs
	ip	2.9.97	MinMax finished
	ip	12.8.97	Copy: uses dest.acttyp instead of dest.typ
	ip	10.8.97	Imports renamed to Dev[Host]CPx
	bh	14.5.97	MakeTag: final types
	ip	10.4.97	Fixed MOD for Int64 (Test Bit #7 instead of #8 !!!!)
	ip	9.4.97	Decrement handle Int64
	ip	6.4.97	Increment handles Int64
	ip	5.3.97	XRealForm: Char16 entered as one of the forms
	ip	27.2.97	Copy: changes to enable concatenation
	ip	25.2.97	Copy: traps if dest too small in case of assign (additional parameter from OPV)
	ip	25.2.97	updated use of String16 according to String8 (needs to be revisited!!!)
	ip	24.2.97	updated use of Char16 according to Char8
	ip	24.2.97	added new structure forms, updated WordSet (for Convert)
	ip	19.2.97	Odd: Int64 correctly handled
	bh	11.1.97	Proc ReceiverOffset to avoid using parameter addresses 
	bh	26.11.96	Fix in Assign:  <pointer to X> := <X> works now
	bh	16.10.96	Copy corrected
	ip	22.7.96	LARGEINT
	ip	17.6.96	SANE-Einführung
	
	RELEASE 1.2
	ip	12.4.96	MakeField: setzt item.obj auf NIL
	ip	4.2.96	DeRef: Test mit InLastNIL
	ip	4.2.96	Call: OPL.ClearHints
	ip	1.2.96	Call: unterscheidet zwischen proc var und TProc, kein Test bei DeRef (TProc)
	ip	1.2.96	MakeProc: recv Parameter eingeführt, supercall verwendet aktuellen Receiver
	ip	1.2.95	DeRef: safe Parameter eingeführt
	ip	22.9.95	Call: an DLL angepasst

	RELEASE 1.1
	ip	11.8.95	EnterProc: HiddenPointer,... anf NIL initialisieren
	ip	12.7.95	Div* und Mod* an Oberon/L Spezifikation angepasst
	ip	7.7.95	EnterProc: FreeReg(source) entfernt
	ip	6.7.95	Include, Exclude: Laden der Adresse
	ip	6.7.95	MakeIndex: Check von dynamischen Arrays korrigiert
	ip	10.6.95	EnterProc: Felder in erweiterten Records
	ip	30.5.95	EnterProc: Procedurevariablen auf NIL initialisieren
	ip	24.5.95	FPCR wird so gesetzt, dass FPUExceptions generiert werden.
	ip	21.5.95	MakeIndex korrigiert (zu frühe Freigabe des sizeItem-Registers)
	ip	18.04.95	don't initialize entire stackframe
	ip	xx.03.95	free registers as soon as possible (Indexchecks)
*)

	IMPORT SYSTEM, DevCPT, DevHostCPL := DevM68CPL, DevCPE, DevCPM;

	CONST
		(* object modes *)
		Var = 1; VarPar = 2; Con = 3; Fld = 4; Typ = 5; LProc = 6; XProc = 7;
		SProc = 8; CProc = 9; IProc = 10; Head = 12; TProc = 13;

		(* accessibility of objects *)
		internal = 0; external = 1; externalR = 2; inPar = 3; outPar = 4;

		(* structure forms *)
		Undef = 0; Byte = 1; Bool = 2; Char8 = 3; Int8 = 4; Int16 = 5; Int32 = 6;
		Real = 7; LReal = 8; Set = 9; String8 = 10; NilTyp = 11; NoTyp = 12;
		Pointer = 13; ProcTyp = 14; Comp = 15;
		AnyPtr = 14; AnyRec = 15;	(* sym file only *)
		Char16 = 16; String16 = 17; Int64 = 18;

		(* composite structure forms *)
		Basic = 1; Array = 2; DynArr = 3; Record = 4;

		(* attribute flags (attr.adr, struct.attribute, proc.conval.setval) *)
		newAttr = 16; absAttr = 17; limAttr = 18; empAttr = 19; extAttr = 20;

		IntSet = { Int8 .. Int32 };
		RealSet = { Real, LReal, Int64 };
		ByteSet = { Int8, Byte, Char8, Bool };
		WordSet = { Int16, Char16 };
		LongSet = { Int32, Set, Pointer, ProcTyp };

		(* item modes *)
		dreg = 0; areg = 1; freg = 2; postinc = 3; predec = 4; regx = 5; absL = 7;
		imm = 8; immL = 9; pcx = 10; coc = 12; fcoc = 13; abs = 14; xreal = 15;
		heap = -1;

		(* procedure flags *)
		hasBody = 1; isRedef = 2; doStackCheck = 24;
		
		(* fixup types *)
		absolute = 100; relative = 101; copy = 102; table = 103; tableend = 104; deref = 105;
		
		(* sizes *)
		byte = 0; word = 1; long = 2;

		(* opcodes *)
		ADD = 13; AND = 12; oR = 8; SUB = 9;
		BCHG = 1; BCLR = 2; BSET = 3; BTST = 0;
		ADDI = 6; ANDI = 2; CMPI = 12; EORI = 10; ORI = 0; SUBI = 4;
		ADDQ = 0; SUBQ = 1;
		CLR = 2; NEG = 4; NEGX = 0; NOT = 6; TST = 10;
		BFCHG = 10; BFCLR = 12; BFSET = 14; BFTST = 8;
		DIVS = 81C0H; DIVU = 80C0H; MULS = 0C1C0H; MULU = 0C0C0H;
		ASh = 0; LSh = 1; ROt = 3; ROX = 2;
		JMP = 3BH; JSR = 3AH; PEA = 21H; NBCD = 20H; TAS = 2BH;

		(* Coprocessor opcodes *)
		FABS = 18H; FACOS = 1CH; FADD = 22H; FASIN = 0CH; FATAN = 0AH; FATANH = 0DH; FCMP = 38H;
		FCOS = 1DH; FCOSH = 19H; FDIV = 20H; FETOX = 10H; FETOXM1 = 8; FGETEXP = 1EH; FGETMAN = 1FH;
		FINT = 1; FINTRZ = 3; FLOG10 = 15H; FLOG2 = 16H; FLOGN = 14H; FLOGNP1 = 6; FMOD = 21H; FMOVE = 0;
		FMUL = 23H; FNEG = 1AH; FREM = 25H; FSCALE = 26H; FSGLDIV = 24H; FSGLMUL = 27H; FSIN = 0EH;
		FSINH = 2; FSQRT = 4; FSUB = 28H; FTAN = 0FH; FTANH = 9; FTENTOX = 12H; FTST = 3AH; FTWOTOX = 11H;

		(* Compare kinds *)
		eql = 9; neq = 10; lss = 11; leq = 12; gtr = 13; geq = 14;

		(* Condition Codes *)
		CC = 4; CS = 5; EQ = 7; false = 1; GE = 12; GT = 14; HI = 2; LE = 15;
		LS = 3; LT = 13; MI = 11; NE = 6; PL = 10; true = 0; VC = 8; VS = 9;

		(* Floating Point Condition Codes *)
		FEQ = 1; FNE = 0EH; FGT = 12H; FNGT = 1DH; FGE = 13H; FNGE = 1CH; FLT = 14H; FNLT = 1BH; FLE = 15H;
		FNLE = 1AH; Ffalse = 0; Ftrue = 0FH;

		(* Floating Point Control Registers *)
		FPCR = 4; FPSR = 2; FPIAR = 1;

		(* Traps *)
		withTrap = 129; caseTrap = 130; funcTrap = 131; typTrap = 132;
		recTrap = 133; ranTrap = 134; inxTrap = 135; copyTrap = 136;
		stackTrap = 137;
		NILTrap = 201;
		
		(* Offsets *)
		ParOff = 8;
		
		super = 1;
		None = -1;
		
	VAR
		stackLimit*: DevCPT.Object;
		FP, SP : DevHostCPL.Item;
		indexCheck, rangeCheck, ptrinit, realLib, rtGC, stackChk : BOOLEAN;
		shortString16typ*: DevCPT.Struct;
		untgPtr: DevCPT.Struct;
			

PROCEDURE [1] Debugger 0A9H, 0FFH;

	PROCEDURE Init*(opt: SET);
		CONST chk = 0; achk = 1; reallib = 7; rtgc = 8; stack = 9;
	BEGIN
		indexCheck := chk IN opt; rangeCheck := achk IN opt; ptrinit := chk IN opt;
		stackChk := stack IN opt;
		realLib := reallib IN opt; rtGC := rtgc IN opt;
		stackLimit := NIL
	END Init;

	PROCEDURE MakeLen*( VAR arr : DevHostCPL.Item; n : INTEGER; VAR item : DevHostCPL.Item );
	(* Makes an item that denotes the length in the n-th innermost dimension of a dynamic array. Starting at 0. *)
	BEGIN (* MakeLen *)
		item := arr;
		IF item.nolen = 0 THEN
			item.bd := arr.bd + 4*(n + 1);
		ELSE
			item.bd := arr.bd + 4*n + 12; (* Kernel.HeadSize for DynArr*)
		END; (* IF *)
		item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
	END MakeLen;

	PROCEDURE MakeIntConst*( val : INTEGER; typ : DevCPT.Struct; VAR item : DevHostCPL.Item );
	(* Makes an immediate item of a given type from a number. *)
	BEGIN (* MakeIntConst *)
		item.mode := imm;
		item.typ := typ; item.acttyp := typ;
		item.bd := val;
	END MakeIntConst;
	
	PROCEDURE MakeRealConst(val: REAL; typ: DevCPT.Struct; VAR item: DevHostCPL.Item);
	TYPE LR = ARRAY 2 OF INTEGER;
	VAR
		lr: LR; real: SHORTREAL;
	BEGIN
		item.mode := imm;
		item.typ := typ; item.acttyp := typ;
		IF typ.form = Real THEN
			real := SHORT(val);
			item.bd := SYSTEM.VAL(INTEGER, real);
		ELSE
			lr := SYSTEM.VAL(LR, val);
			IF DevCPM.LEHost THEN 
				item.bd := lr[1]; item.ext := lr[0]
			ELSE 
				item.bd := lr[0]; item.ext := lr[1]
			END
		END;
	END MakeRealConst;
	
	PROCEDURE MakeLargeIntConst(const: DevCPT.Const; VAR item: DevHostCPL.Item);
	VAR r: REAL;
	BEGIN
		item.mode := imm;
		DevCPE.GetLongWords(const, item.bd, item.ext)
	END MakeLargeIntConst;

	PROCEDURE GetVarBase( obj : DevCPT.Object ) : SHORTINT;
	(* Returns the register to which the given variable is relative. *)
		VAR diff, reg : SHORTINT;
				source, dest : DevHostCPL.Item;
	BEGIN (* GetVarBase *)
		diff := SHORT(DevHostCPL.level - obj.mnolev);
		IF diff = 0 THEN
			reg := FP.reg;
		ELSE (* follow static link *)
			reg := DevHostCPL.GetAdrReg( );
			source.mode := regx;
			source.typ := untgPtr; source.acttyp := untgPtr;
			source.reg := FP.reg;
			source.bd := 8;
			source.inxReg := None;
			source.offsReg := None;
			dest.mode := areg;
			dest.typ := untgPtr; dest.acttyp := untgPtr;
			dest.reg := reg;
			DevHostCPL.Move( source, dest );
			source.reg := reg;
			WHILE diff > 1 DO
				DevHostCPL.Move( source, dest );
				DEC( diff );
			END; (* WHILE *)
		END; (* IF *)
		RETURN reg
	END GetVarBase;

	PROCEDURE MakeVar*( obj : DevCPT.Object; VAR item : DevHostCPL.Item );
	(* Makes an item from a variable. *)
		VAR aregItem : DevHostCPL.Item;
	BEGIN (* MakeVar *)
		IF ( obj.mode = VarPar ) & ( obj.typ.comp # DynArr ) THEN
			item.mode := regx;
			item.reg := GetVarBase( obj );
			item.typ := untgPtr; item.acttyp := untgPtr;
			item.bd := obj.adr;
			item.inxReg := None;
			aregItem.mode := areg;
			aregItem.reg := DevHostCPL.GetAdrReg( );
			DevHostCPL.Move( item, aregItem );
			DevHostCPL.FreeReg( item );
			item.mode := regx;
			item.reg := aregItem.reg;
			item.bd := 0;
		ELSIF obj.mnolev <= 0 THEN (* imported or global variable -> GenLinked zum Ansprechen *)
			item.mode := absL;
			item.reg := absolute;
			IF obj.mnolev < 0 THEN item.bd := 0 ELSE item.bd := obj.adr END;
		ELSE (* local variable *)
			item.mode := regx;
			item.reg := GetVarBase( obj );
			item.bd := obj.adr;
		END; (* IF *)
		item.obj := obj;
		item.typ := obj.typ; item.acttyp := obj.typ;
		item.inxReg := None;
		item.offsReg := None;
		item.nolen := 0;
		item.tmode := 0
	END MakeVar;

	PROCEDURE DeRef*( typ : DevCPT.Struct; VAR item : DevHostCPL.Item; safe: BOOLEAN);
	(* Makes a dereferentiation of an item. *)
		VAR
			aregItem : DevHostCPL.Item;
			inxReg : SHORTINT;
			handle: BOOLEAN;
	BEGIN (* DeRef *)
		handle := item.typ.sysflag = 2;
		aregItem.mode := areg;
		IF (item.reg >= 8) & (item.reg < 12) THEN
			aregItem.reg := item.reg;
		ELSE
			aregItem.reg := DevHostCPL.GetAdrReg( );
		END;
		aregItem.typ := DevCPT.sysptrtyp; aregItem.acttyp := DevCPT.sysptrtyp;
		item.typ := DevCPT.sysptrtyp; item.acttyp := DevCPT.sysptrtyp;
		inxReg := item.offsReg;
		DevHostCPL.Move( item, aregItem );
		IF ~(safe OR (DevHostCPL.InLastNIL(item.obj) >= 0)) THEN
			DevHostCPL.Test(aregItem);
			DevHostCPL.Trapcc(EQ, NILTrap);
			DevHostCPL.SetLastNIL(item.obj);
		END;
		IF handle THEN
			item.mode := regx;
			item.reg := aregItem.reg;
			item.bd := 0; item.inxReg := None;
			DevHostCPL.Move(item, aregItem);
			IF ~safe THEN
				DevHostCPL.Test(aregItem);
				DevHostCPL.Trapcc(EQ, NILTrap);
			END;
		END;
		item.mode := regx;
		item.typ := typ; item.acttyp := typ;
		item.reg := aregItem.reg;
		IF (typ.comp = Array) & ~typ.untagged THEN
			item.bd := 12; (* pointer to array hat einen Header von 12 Bytes *)
		ELSE
			item.bd := 0;
		END;
		item.inxReg := inxReg; (* xsize and scale keep their values *)
		IF typ.comp = DynArr THEN
			item.nolen := SHORT(SHORT( typ.n ) + 1);
		ELSE
			item.nolen := 0;
		END; (* IF *)
		item.tmode := heap
	END DeRef;

	PROCEDURE StaticTag*( typ : DevCPT.Struct; VAR tag : DevHostCPL.Item );
	(* Returns the type tag of a type. *)
	BEGIN (* StaticTag *)
		tag.mode := immL;
		tag.reg := absolute;
		tag.typ := untgPtr; tag.acttyp := untgPtr;
		tag.bd := 0;
		tag.obj := typ.strobj;
	END StaticTag;

	PROCEDURE MakeTag*( obj : DevCPT.Object; typ : DevCPT.Struct; VAR item, tag : DevHostCPL.Item );
	(* Makes an item that denotes the type tag of the given object and item. *)
	VAR sreg, dreg: DevHostCPL.Item; t: DevCPT.Struct;
	BEGIN (* MakeTag *)
		IF typ.untagged THEN DevCPM.err(999) END;
		t := typ; IF t.form = Pointer THEN t := t.BaseTyp END;
		IF (t.attribute = 0) (* OR (t.attribute = limAttr)*) THEN	(* final type *)
			StaticTag(t, tag)
		ELSIF item.typ.form = Pointer THEN
			tag := item;
			IF (item.reg >= 8) &(item.reg < 12) THEN
				sreg.mode := areg; dreg.mode := areg;
				sreg.typ := DevCPT.sysptrtyp; sreg.acttyp := DevCPT.sysptrtyp;
				dreg.typ := DevCPT.sysptrtyp; dreg.acttyp := DevCPT.sysptrtyp;
				sreg.reg := item.reg; dreg.reg := DevHostCPL.GetAdrReg();
				DevHostCPL.Move(sreg, dreg);
				tag.reg := dreg.reg;
			END;
			DeRef( DevCPT.sysptrtyp, tag, FALSE );
			tag.bd := -4;
			tag.typ := untgPtr; tag.acttyp := untgPtr
		ELSIF ( obj # NIL ) & ( obj.mode = VarPar ) THEN
			tag.mode := regx;
			tag.typ := untgPtr; tag.acttyp := untgPtr;
			tag.reg := GetVarBase(obj);
			tag.bd := obj.adr + 4;
			tag.inxReg := None;
			tag.offsReg := None;
		ELSIF item.tmode = heap THEN
			tag := item;
			tag.typ := untgPtr; tag.acttyp := untgPtr;
			tag.tmode := 0;
			INC(tag.bd, -4)
		ELSE
			StaticTag( typ, tag );
		END; (* IF *)
	END MakeTag;

	PROCEDURE MakeConst*( obj : DevCPT.Object; const : DevCPT.Const; typ : DevCPT.Struct; VAR item : DevHostCPL.Item );
	(* Makes an item from a constant. *)
		VAR realval : SHORTREAL;
	BEGIN (* MakeConst *)
		item.typ := typ; item.acttyp := typ;
		CASE typ.form OF
			Set :
				MakeIntConst( SYSTEM.VAL( INTEGER, const.setval ), typ, item );
			| String8 :
				DevHostCPL.AllocConst(item, const, typ.form);
			| String16:
				DevHostCPL.AllocConst(item, const, typ.form);
			| LReal, Real:
				MakeRealConst(const.realval, typ, item);
			| Int64:
				MakeLargeIntConst(const, item);
		ELSE
			MakeIntConst( const.intval, typ, item );
		END; (* CASE *)
	END MakeConst;

	PROCEDURE BaseTypSize( VAR arr, size : DevHostCPL.Item; VAR scale : SHORTINT );
	(* Returns the size of the base type of a dynamic array if the base type is a dynamic array itself. *)
		VAR i : INTEGER;
				typ : DevCPT.Struct;
				len : DevHostCPL.Item;
	BEGIN (* BaseTypSize *)
		typ := arr.typ.BaseTyp;
		WHILE typ.comp = DynArr DO
			typ := typ.BaseTyp;
		END; (* WHILE *)
		IF ( typ.size = 1 ) OR ( typ.size = 2 ) OR ( typ.size = 4 ) OR ( typ.size = 8 ) THEN
			scale := SHORT( typ.size );
			MakeLen(arr, arr.typ.n-1, size);
		ELSE
			scale := 1;
			MakeIntConst( typ.size, DevCPT.int32typ, size );
			MakeLen(arr, arr.typ.n-1, len);
			DevHostCPL.Format12( MULS, len, size );
			DevHostCPL.FreeReg( len );
		END; (* IF *)
		FOR i := 0 TO arr.typ.n-2 DO
			MakeLen( arr, i, len );
			DevHostCPL.Format12( MULS, len, size );
			DevHostCPL.FreeReg( len );
		END; (* FOR *)
	END BaseTypSize;

	PROCEDURE Size( VAR arr, size : DevHostCPL.Item; VAR scale : SHORTINT );
	(* Returns the size of a dynamic array and a scale factor. *)
		VAR
			len : DevHostCPL.Item;
			btyp : DevCPT.Struct;
	BEGIN (* Size *)
		MakeLen(arr,arr.typ.n, len);
		btyp := arr.typ.BaseTyp;
		IF btyp.comp = DynArr THEN
			BaseTypSize( arr, size, scale );
			DevHostCPL.Format12( MULS, len, size );
			DevHostCPL.FreeReg( len );
		ELSE
			IF ( btyp.size = 1 ) OR ( btyp.size = 2 ) OR ( btyp.size = 4 ) OR ( btyp.size = 8 ) THEN
				scale := SHORT( btyp.size );
				size := len;
			ELSE
				scale := 1;
				MakeIntConst( btyp.size, DevCPT.int32typ, size );
				DevHostCPL.Format12( MULS, len, size );
				DevHostCPL.FreeReg( len );
			END; (* IF *)
		END; (* IF *)
	END Size;

	PROCEDURE ElimIndex( VAR item : DevHostCPL.Item );
	(* Eliminates the index register in the item. *)
		VAR newReg : SHORTINT; freeReg: DevHostCPL.Item;
	BEGIN (* ElimIndex *)
		IF item.inxReg # None THEN (* load old address *)
			IF (item.mode = regx) & ~(item.reg IN DevHostCPL.reservedRegs) THEN
				newReg := item.reg;
			ELSE
				newReg := DevHostCPL.GetAdrReg( );
			END;
			DevHostCPL.Lea( item, newReg );
			item.mode := regx;
			item.bd := 0;
			item.reg := newReg;
			freeReg.reg := item.inxReg;
			freeReg.mode := dreg;
			DevHostCPL.FreeReg(freeReg);
			item.inxReg := None;
		END; (* IF *)
	END ElimIndex;

	PROCEDURE SetElem*( VAR item : DevHostCPL.Item );
	(* Makes a set-element from an integer element and sets the corresponding bit. *)
		VAR source : DevHostCPL.Item;
	BEGIN (* SetElem *)
		source :=  item;
		item.mode := dreg;
		item.typ := DevCPT.settyp; item.acttyp := DevCPT.settyp;
		item.reg := DevHostCPL.GetReg( );
		DevHostCPL.Format7( CLR, item );
		DevHostCPL.Format5( BSET, source, item );
	END SetElem;

	PROCEDURE PushAdrStack*(VAR item : DevHostCPL.Item );
	BEGIN
		DevHostCPL.Pea(item);
	END PushAdrStack;

	PROCEDURE ^ PushStack*(VAR item : DevHostCPL.Item );

	PROCEDURE AddToSP*( data : INTEGER );
	(* Subtracts the immediate value 'data' from the stack pointer. *)
		VAR source : DevHostCPL.Item;
	BEGIN (* AddToSP *)
		IF data > 0 THEN
			IF data <= 8 THEN
				DevHostCPL.Format1( ADDQ, SHORT( data ), SP );
			ELSE
				MakeIntConst( data, DevCPT.int32typ, source );
				DevHostCPL.Format3( ADD, source, SP.reg );
			END; (* IF *)
		ELSIF data < 0 THEN
			data := -data;
			IF data <= 8 THEN
				DevHostCPL.Format1( SUBQ, SHORT( data ), SP );
			ELSE
				MakeIntConst( data, DevCPT.int32typ, source );
				DevHostCPL.Format3( SUB, source, SP.reg );
			END; (* IF *)
		END; (* IF *)
	END AddToSP;

	PROCEDURE RoundZero;
	(* Sets the rounding mode of the coprocessor to -inf. *)
		VAR env, temp : DevHostCPL.Item;
	BEGIN (* RoundDown *)
		AddToSP(-2);
		env.mode := regx; env.reg := 15; env.bd := 0; env.inxReg := None;
		env.obj := NIL; env.typ := DevCPT.int16typ; env.acttyp := DevCPT.int16typ;
		MakeIntConst(00003H, DevCPT.int16typ, temp);
		PushAdrStack(env); PushStack(temp);
		DevHostCPL.FP68K;
		DevHostCPL.Format6(ANDI, 09FFH, env);
		DevHostCPL.Format6(ORI, 06000H, env);
		MakeIntConst(00001H, DevCPT.int16typ, temp);
		PushAdrStack(env); PushStack(temp);
		DevHostCPL.FP68K;
		AddToSP(2);
(*		MakeIntConst( 07420H, DevCPT.int32typ, temp );
		DevHostCPL.FMovecr( temp, 0, FPCR );*)
	END RoundZero;

	PROCEDURE RoundDown;
	(* Sets the rounding mode of the coprocessor to -inf. *)
		VAR env, temp : DevHostCPL.Item;
	BEGIN (* RoundDown *)
		AddToSP(-2);
		env.mode := regx; env.reg := 15; env.bd := 0; env.inxReg := None;
		env.obj := NIL; env.typ := DevCPT.int16typ; env.acttyp := DevCPT.int16typ;
		MakeIntConst(00003H, DevCPT.int16typ, temp);
		PushAdrStack(env); PushStack(temp);
		DevHostCPL.FP68K;
		DevHostCPL.Format6(ANDI, 09FFH, env);
		DevHostCPL.Format6(ORI, 04000H, env);
		MakeIntConst(00001H, DevCPT.int16typ, temp);
		PushAdrStack(env); PushStack(temp);
		DevHostCPL.FP68K;
		AddToSP(2);
(*		MakeIntConst( 07420H, DevCPT.int32typ, temp );
		DevHostCPL.FMovecr( temp, 0, FPCR );*)
	END RoundDown;

	PROCEDURE RoundNearest;
	(* Sets the rounding mode of the coprocessor to nearest. *)
		VAR env, temp : DevHostCPL.Item;
	BEGIN (* RoundNearest *)
		AddToSP(-2);
		env.mode := regx; env.reg := 15; env.bd := 0; env.inxReg := None;
		env.obj := NIL; env.typ := DevCPT.int16typ; env.acttyp := DevCPT.int16typ;
		MakeIntConst(00003H, DevCPT.int16typ, temp);
		PushAdrStack(env); PushStack(temp);
		DevHostCPL.FP68K;
		DevHostCPL.Format6(ANDI, 09FFH, env);
		MakeIntConst(00001H, DevCPT.int16typ, temp);
		PushAdrStack(env); PushStack(temp);
		DevHostCPL.FP68K;
		AddToSP(2);
(*		MakeIntConst( 07400H, DevCPT.int32typ, temp );
		DevHostCPL.FMovecr( temp, 0, FPCR );*)
	END RoundNearest;

	PROCEDURE XRealForm(form: BYTE; size: INTEGER): SHORTINT;
	BEGIN
		CASE form OF
		Real, Set: RETURN 01000H;
		| LReal: RETURN 00800H;
		| Int16, Char16: RETURN 02000H;
		| Int32: RETURN 02800H;
		| Int64: RETURN 03000H;
		ELSE
			CASE size OF
			4: RETURN 01000H;
			| 8: RETURN 00800H;
			END;
		END;
	END XRealForm;
	
	PROCEDURE ToXReal(VAR item, dest: DevHostCPL.Item);
	VAR t, src: DevHostCPL.Item;
	BEGIN
		ASSERT(dest.mode = regx, 20); ASSERT(item.mode # xreal);
		MakeIntConst(XRealForm(item.acttyp.form, item.acttyp.size) + 0000EH, DevCPT.int16typ, t);
		src := item;
		IF item.mode IN {dreg, areg, imm, freg} THEN
			src.typ := src.acttyp;
			PushStack(src); IF dest.reg = 15 THEN INC(dest.bd, src.typ.size); END;
			src.mode := regx; src.reg := 15; src.bd := 0; src.inxReg := None;
			src.obj := NIL;
		END;
		PushAdrStack(src); IF dest.reg = 15 THEN INC(dest.bd, 4); END;
		PushAdrStack(dest); PushStack(t);
		DevHostCPL.FP68K;
		IF dest.reg = 15 THEN DEC(dest.bd, 4); END;
		IF item.mode IN {dreg, areg, imm, freg} THEN
			AddToSP(src.typ.size); DEC(dest.bd, src.typ.size);
		END;
	END ToXReal;
	
	PROCEDURE FromXReal(VAR source, item: DevHostCPL.Item);
	VAR t, dst: DevHostCPL.Item;
	BEGIN
		ASSERT(source.mode = regx, 20);
		MakeIntConst(XRealForm(item.typ.form, item.acttyp.size) +00010H, DevCPT.int16typ, t);
		IF (item.mode = regx) OR (item.mode = absL) THEN
			dst := item;
		ELSIF item.mode = predec THEN
			AddToSP(-item.typ.size); INC(source.bd, item.typ.size);
			dst.mode := regx; dst.reg := 15; dst.bd := 0; dst.inxReg := None; dst.obj := NIL;
		ELSE HALT(21);
		END;
		PushAdrStack(source); IF dst.reg = 15 THEN INC(dst.bd, 4) END;
		PushAdrStack(dst); PushStack(t);
		DevHostCPL.FP68K;
		IF dst.reg = 15 THEN DEC(dst.bd, 4) END;
		IF item.mode = predec THEN
			t.mode := regx; t.reg := 15; t.bd := 0; t.inxReg := None;
			t.obj := NIL;
			CASE item.typ.size OF
			2: t.typ := DevCPT.int16typ;
			| 4, 8: t.typ := DevCPT.int32typ;
			END;
			t.acttyp := t.typ;
			dst := t; dst.bd := 12;
			DevHostCPL.Move(t, dst);
			IF item.typ.size = 8 THEN
				INC(t.bd, 4); INC(dst.bd, 4);
				DevHostCPL.Move(t, dst);
			END;
		END;
	END FromXReal;
	
	PROCEDURE Convert*( VAR source : DevHostCPL.Item; desttyp : DevCPT.Struct );
	(* Converts the given item to desttyp. *)
		VAR sf, sc, df, dc : BYTE;
				dest, shift : DevHostCPL.Item;
	BEGIN (* Convert *)
		sf := source.typ.form;
		sc := source.typ.comp;
		df := desttyp.form;
		dc := desttyp.comp;
		IF df = ProcTyp THEN
			source.typ := desttyp; source.acttyp := desttyp;
			RETURN
		END;
		IF sf # Pointer THEN
			IF ~realLib & (df IN {Real, LReal}) THEN
				IF sf IN ByteSet THEN
					Convert(source, DevCPT.int32typ);
				ELSIF (df = Int64) & (sf IN {Real, LReal}) THEN
					IF source.mode # xreal THEN
						INC(DevHostCPL.xrealstack); AddToSP(-12);
						dest.mode := regx; dest.reg := 15; dest.bd := 0; dest.inxReg := None;
						dest.obj := NIL; dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
						ToXReal(source, dest);
						DevHostCPL.FreeReg(source);
					END;
					RoundDown;
					source.mode := regx; source.reg := 15; source.bd := 0; source.inxReg := None;
					source.obj := NIL; source.typ := DevCPT.int32typ; source.acttyp := DevCPT.int32typ;
					MakeIntConst(00014H, DevCPT.int16typ, dest);
					PushAdrStack(source); PushStack(dest);
					DevHostCPL.FP68K;
					RoundNearest;
					source.mode := xreal; source.reg := DevHostCPL.xrealstack;
				END;
			ELSIF ( sf # df ) OR ( sc # dc ) THEN
				IF df = Int64 THEN	(* bh *)
					IF sf IN ByteSet + WordSet THEN
						Convert(source, DevCPT.int32typ)
					ELSIF source.mode # dreg THEN
						DevHostCPL.Load(source)
					ELSE
						DevHostCPL.Test(source)
					END;
					dest.mode := dreg; dest.reg := DevHostCPL.GetReg();
					dest.typ := DevCPT.int8typ; dest.acttyp := DevCPT.int8typ;
					DevCPE.GenShort(5BC0H + dest.reg);	(* SMI dest *)
					DevHostCPL.Ext(dest, long);
					source.inxReg := source.reg; source.reg := dest.reg
				ELSIF df IN LongSet THEN
					IF sf IN {Char8, Char16} THEN
						dest.mode := dreg;
						dest.typ := desttyp; dest.acttyp := desttyp;
						dest.reg := DevHostCPL.GetReg( );
						DevHostCPL.Format7( CLR, dest );
						DevHostCPL.Move( source, dest );
						source := dest;
					ELSIF sf IN ByteSet + WordSet THEN
						IF source.mode # imm THEN
							DevHostCPL.Ext( source, long );
						END;
					ELSIF realLib & (sf = Int64) THEN	(* bh *)
						IF source.mode = dreg THEN 
							dest.mode := dreg; dest.reg := source.reg;
							dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
							DevHostCPL.Free(dest);
							source.reg := source.inxReg; source.inxReg := -1
						ELSIF source.mode IN {regx, abs, absL, pcx} THEN
							INC(source.bd, 4)
						ELSE HALT(0)
						END
					ELSIF sf IN RealSet THEN
						IF source.mode # xreal THEN
							INC(DevHostCPL.xrealstack); AddToSP(-12);
							dest.mode := regx; dest.reg := 15; dest.bd := 0; dest.inxReg := None;
							dest.obj := NIL; dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
							ToXReal(source, dest);
							DevHostCPL.FreeReg(source);
						END;
						AddToSP(-desttyp.size);
						source.mode := regx; source.reg  := 15; source.bd := desttyp.size; source.inxReg := None;
						source.obj := NIL; source.typ := DevCPT.int32typ; source.acttyp := DevCPT.int32typ;
						dest.mode := regx; dest.reg  := 15; dest.bd := 0; dest.inxReg := None;
						dest.obj := NIL; dest.typ := desttyp; dest.acttyp := desttyp;
						RoundDown;
						FromXReal(source, dest);
						RoundNearest;
						source.mode := postinc; source.obj := NIL; source.typ := desttyp; source.acttyp := desttyp;
						DevHostCPL.Load(source);
						AddToSP(12); DEC(DevHostCPL.xrealstack);
						(*DevHostCPL.FLoad( source );
						RoundDown;
						source.typ := desttyp; source.acttyp := desttyp;
						DevHostCPL.Load( source );
						RoundNearest;*)
					END; (* IF *)
				ELSIF df IN WordSet THEN
					IF sf IN LongSet THEN
						DevHostCPL.Load( source );
					ELSIF sf = Char8 THEN
						dest.mode := dreg;
						dest.typ := desttyp; dest.acttyp := desttyp;
						dest.reg := DevHostCPL.GetReg( );
						DevHostCPL.Format7( CLR, dest );
						DevHostCPL.Move( source, dest );
						source := dest;
					ELSIF sf IN ByteSet THEN
						IF source.mode # imm THEN
							DevHostCPL.Ext( source, word );
						END;
					ELSIF realLib & (sf = Int64) THEN	(* bh *)
						Convert(source, DevCPT.int32typ);
						Convert(source, desttyp)
					ELSIF sf IN RealSet THEN
						IF source.mode # xreal THEN
							INC(DevHostCPL.xrealstack); AddToSP(-12);
							dest.mode := regx; dest.reg := 15; dest.bd := 0; dest.inxReg := None;
							dest.obj := NIL; dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
							ToXReal(source, dest);
							DevHostCPL.FreeReg(source);
						END;
						AddToSP(-desttyp.size);
						source.mode := regx; source.reg  := 15; source.bd := desttyp.size; source.inxReg := None;
						source.obj := NIL; source.typ := DevCPT.int32typ; source.acttyp := DevCPT.int32typ;
						dest.mode := regx; dest.reg  := 15; dest.bd := 0; dest.inxReg := None;
						dest.obj := NIL; dest.typ := desttyp; dest.acttyp := desttyp;
						RoundDown;
						FromXReal(source, dest);
						RoundNearest;
						source.mode := postinc;  source.obj := NIL; source.typ := desttyp; source.acttyp := desttyp;
						DevHostCPL.Load(source);
						AddToSP(12); DEC(DevHostCPL.xrealstack);
(*						DevHostCPL.FLoad( source );
						RoundDown;
						source.typ := desttyp; source.acttyp := desttyp;
						DevHostCPL.Load( source );
						RoundNearest;*)
					END; (* IF *)
				ELSIF df IN ByteSet THEN
					IF sf IN WordSet + LongSet THEN
						DevHostCPL.Load( source );
					ELSIF sf IN RealSet THEN
						Convert(source, DevCPT.int16typ);
						Convert(source, desttyp);
(*						DevHostCPL.FLoad( source );
						RoundDown;
						source.typ := desttyp; source.acttyp := desttyp;
						DevHostCPL.Load( source );
						RoundNearest;*)
					END; (* IF *)
(*				ELSIF df IN RealSet THEN
					DevHostCPL.FLoad( source ); *)
				END; (* IF *)
				source.acttyp := desttyp;
			END; (* IF *)
			source.typ := desttyp;
		END;
	END Convert;

	PROCEDURE GetDynArrVal*( VAR item : DevHostCPL.Item; untagged: BOOLEAN );
	(* Returns an item containing the actual value of a dynamic array. *)
	BEGIN (* GetDynArrVal *)
		IF item.nolen = 0 THEN
			DeRef( DevCPT.sysptrtyp, item, FALSE );
		ELSE
			IF ~untagged THEN
				INC(item.bd, LONG( item.nolen ) * 4  + 12);
			END;
			item.nolen := 0;
			item.typ := untgPtr; item.acttyp := untgPtr;
			item.inxReg := item.offsReg;
			item.offsReg := None;
		END; (* IF *)
	END GetDynArrVal;

	PROCEDURE GetDynArrAdr( VAR item, adr : DevHostCPL.Item );
	(* Returns an item containing the address of a dynamic array. *)
		VAR adrReg : DevHostCPL.Item;
	BEGIN (* GetDynArrAdr *)
		adr.typ := untgPtr; adr.acttyp := untgPtr;
		adr.nolen := 0;
		IF item.nolen = 0 THEN
			IF item.offsReg # None THEN
				DeRef( DevCPT.sysptrtyp, item, FALSE );
				adr.mode := areg;
				adr.reg := DevHostCPL.GetAdrReg( );
				DevHostCPL.Lea( item, adr.reg );
			ELSE
				adr.mode := item.mode;
				adr.reg := item.reg;
				adr.bd := item.bd;
				adr.inxReg := None;
				adr.offsReg := None;
			END;
		ELSE (* Pointer to DynArr *)
			adr.mode := item.mode;
			adr.reg := item.reg;
			IF item.typ.untagged THEN
				adr.bd := item.bd;
			ELSE
				adr.bd := item.bd + item.nolen * 4 + 12; (* HeaderSize von Kernel. Felder last, actual und first *)
			END;
			adr.inxReg := item.offsReg;
			adr.xsize := item.xsize;
			adr.scale := item.scale;
			adr.offsReg := None;
			adrReg.mode := areg;
			adrReg.typ := untgPtr; adrReg.acttyp := untgPtr;
			adrReg.reg := DevHostCPL.GetAdrReg( );
			DevHostCPL.Lea( adr, adrReg.reg );
			adr := adrReg;
		END; (* IF *)
	END GetDynArrAdr;

	PROCEDURE MakeField*( VAR item : DevHostCPL.Item; offset : INTEGER; typ : DevCPT.Struct );
	(* Increments the address of item by offset and sets its type to typ. *)
	BEGIN (* MakeField *)
		DevHostCPL.LoadExternal( item );
		INC( item.bd, offset );
		item.obj := NIL;
		item.typ := typ; item.acttyp := typ;
		item.tmode := 0
	END MakeField;

	PROCEDURE MakeIndex*( VAR index, res : DevHostCPL.Item );
	(* Makes an indexed item from an item and an index. res := res[ index ].
		The generated item has always got an index register or an offset register. *)
		VAR baseTyp : DevCPT.Struct;
				sizeItem, chkItem, offset : DevHostCPL.Item;
				size : INTEGER;
				scale : SHORTINT;
	BEGIN (* MakeIndex *)
		baseTyp := res.typ.BaseTyp;
		size := baseTyp.size;
		DevHostCPL.LoadExternal( res );
		IF ( res.typ.comp # DynArr ) & ( index.mode = imm ) THEN
			INC( res.bd, size * index.bd );
		ELSE
			ElimIndex( res );
			IF index.typ.form = Int8  THEN Convert( index, DevCPT.int16typ ); END;
			IF ( ( index.mode # imm ) OR ( index.bd # 0 ) ) & indexCheck THEN
				IF (res.typ.comp = DynArr) & ~(res.typ.untagged) THEN
					MakeLen(res, res.typ.n, chkItem);
					DevHostCPL.Load(chkItem);
					DevHostCPL.Format1(SUBQ, 1, chkItem);
					Convert( index, DevCPT.int32typ );
				ELSIF ~res.typ.untagged THEN
					MakeIntConst( res.typ.n - 1, index.typ, chkItem );
				END; (* IF *)
				IF ~res.typ.untagged THEN
					DevHostCPL.Chk( index, chkItem );
					DevHostCPL.FreeReg( chkItem );
				END;
			END; (* IF *)
			DevHostCPL.Load( index );
			IF baseTyp.comp # Basic THEN
				IF baseTyp.comp = DynArr THEN
					IF ~baseTyp.untagged THEN
						Convert( index, DevCPT.int32typ );
						BaseTypSize( res, sizeItem, scale );
						DevHostCPL.Format12( MULS, sizeItem, index );
						IF (sizeItem.mode # regx) OR (sizeItem.reg # res.reg) THEN
							DevHostCPL.FreeReg( sizeItem );
						END;
					ELSE
						DevCPM.err(998);
						scale := 1; (* avoid case trap *)
					END;
				ELSE
					IF ( size = 1 ) OR ( size = 2 ) OR ( size = 4 ) OR ( size = 8 ) THEN
						scale := SHORT( size );
					ELSE
						scale := 1;
						IF (res.typ.n * size > MAX(SHORTINT)) & (index.typ.form # Int32) THEN
							Convert(index, DevCPT.int32typ);
						END;
						MakeIntConst( size, index.typ, sizeItem );
						IF index.typ.form = Int32 THEN
							DevHostCPL.Format12( MULS, sizeItem, index );
						ELSE
							DevHostCPL.Format11( MULS, sizeItem, index );
						END; (* IF *)
						DevHostCPL.FreeReg( sizeItem );
					END; (* IF *)
				END; (* IF *)
				size := 1;
			ELSE scale := SHORT( size );
			END; (* IF *)
			IF baseTyp.comp = DynArr THEN
				IF res.offsReg # None THEN
					offset.mode := dreg;
					offset.typ := DevCPT.int32typ; offset.acttyp := DevCPT.int32typ;
					offset.reg := res.offsReg;
					DevHostCPL.Format2( ADD, offset, index );
				END; (* IF *)
				res.offsReg := index.reg;
			ELSE
				IF res.typ.comp = DynArr THEN
					GetDynArrVal( res, res.typ.untagged );
					ElimIndex( res );
				END; (* IF *)
				res.inxReg := index.reg;
			END; (* IF *)
			IF index.typ.form = Int32 THEN
				res.xsize := 1;
			ELSE
				res.xsize := 0;
			END; (* IF *)
			CASE scale OF
				1 : res.scale := 0;
				| 2 : res.scale := 1;
				| 4 : res.scale := 2;
				| 8 : res.scale := 3;
			END; (* CASE *)
		END; (* IF *)
		res.typ := baseTyp; res.acttyp := baseTyp;
		res.tmode := 0
	END MakeIndex;
	
	PROCEDURE ReceiverOffset (proc: DevCPT.Object): INTEGER;
		(* must correspond to DevOPV.Parameters *)
		VAR padr: INTEGER; par: DevCPT.Object; typ: DevCPT.Struct;
	BEGIN
(*	
		RETURN proc.link.adr - ParOff	(* parameter address of imported procedures not known ! *)
*)
		padr := 0;
		IF proc.mnolev > 0 THEN INC(padr, 4) END;
		par := proc.link.link;	(* don't count receiver itself *)
		WHILE par # NIL DO
			typ := par.typ;
			IF (par.mode = VarPar) & (typ.comp # DynArr) THEN
				IF (typ.comp = Record) & ~typ.untagged THEN INC(padr, 8) ELSE INC(padr, 4) END
			ELSIF (typ.form = Comp) & (typ.comp # DynArr) & (typ.size > 4) THEN
				INC(padr, 4)
			ELSE
				INC(padr, typ.size); INC(padr, padr MOD 2)
			END;
			par := par.link
		END;
		RETURN padr
	END ReceiverOffset;

	PROCEDURE MakeProc*( obj, recv : DevCPT.Object; subcl : BYTE; VAR item : DevHostCPL.Item );
	(* Makes an item from a procedure object. *)
	VAR btyp: DevCPT.Struct;
	BEGIN (* MakeProc *)
		IF obj.mode = TProc THEN (* receiver is obj.link *)
			IF obj.link.mode = VarPar THEN
				item.mode := regx;
				item.typ := untgPtr; item.acttyp := untgPtr;
				item.reg := SP.reg;
				item.bd := ReceiverOffset(obj) + 4; (* tag  is pushed before recordptr *)
				item.inxReg := None;
				item.offsReg := None;
			ELSE
				item.mode := regx;
				item.typ := untgPtr; item.acttyp := untgPtr;
				item.reg := SP.reg;
				item.bd := ReceiverOffset(obj);
				item.inxReg := None;
				item.offsReg := None;
				DeRef( DevCPT.sysptrtyp, item, FALSE );
				item.bd := -4;
			END; (* IF *)
			DeRef( DevCPT.sysptrtyp, item, TRUE );
			IF subcl = super THEN
				IF obj.link.typ.form = Pointer THEN
					btyp := recv.typ.BaseTyp.BaseTyp;
				ELSE
					btyp := recv.typ.BaseTyp;
				END;
				item.bd := DevHostCPL.BaseTypeOffs + 4 * btyp.extlev;
				DeRef( DevCPT.sysptrtyp, item, TRUE );
			ELSIF subcl = 2 THEN (* static call *)
				btyp := obj.link.typ;
				IF btyp.form = Pointer THEN btyp := btyp.BaseTyp END;
				item.bd := DevHostCPL.BaseTypeOffs + 4 * btyp.extlev;
				DeRef( DevCPT.sysptrtyp, item, TRUE );
			ELSIF subcl = 3 THEN (* interface method call *)
				DevCPM.err(200)
			END; (* IF *)
			item.bd := - 4 * (obj.num + 1 );
		ELSE
			item.mode := immL;
			item.reg := absolute;
			item.bd := 0;
			item.inxReg := None;
			item.offsReg := None;
			item.obj := obj;
		END; (* IF *)
		item.tmode := 0
	END MakeProc;

	PROCEDURE MakePostInc( typ : DevCPT.Struct; VAR item : DevHostCPL.Item );
	(* Makes a post-increment item from the given item. *)
		VAR reg : SHORTINT;
	BEGIN (* MakePostInc *)
		IF item.mode # postinc THEN
			IF~(( item.mode = regx ) & ( item.bd = 0 ) & ( item.inxReg = None ) & ~ ( item.reg IN { FP.reg, SP.reg } )) THEN
				reg := DevHostCPL.GetAdrReg( );
				DevHostCPL.Lea( item, reg );
				DevHostCPL.FreeReg(item);
				item.reg := reg;
			END;
			item.mode := postinc;
		END;
		item.typ := typ; item.acttyp := typ;
	END MakePostInc;

	PROCEDURE MakeSPPredec*( VAR res : DevHostCPL.Item );
	(* Makes a pre-decrement item with the stack pointer. *)
	BEGIN (* MakeSPPredec *)
		res.mode := predec;
		res.reg := SP.reg;
		res.typ := SP.typ; res.acttyp := SP.typ;
	END MakeSPPredec;

	PROCEDURE MakeSPPostinc*(VAR res: DevHostCPL.Item);
	BEGIN
		res.mode := postinc;
		res.reg := SP.reg;
		res.typ := SP.typ; res.acttyp := SP.typ;
	END MakeSPPostinc;

	PROCEDURE MakeCocItem*( trueCond : SHORTINT; VAR res : DevHostCPL.Item );
	(* Makes a coc item with the true-condition trueCond. *)
	BEGIN (* MakeCocItem *)
		res.mode := coc;
		res.typ := DevCPT.booltyp; res.acttyp := DevCPT.booltyp;
		res.bd := DevHostCPL.TFConds( trueCond );
		(* leave tJump and fJump unchanged! *)
	END MakeCocItem;

	PROCEDURE MakeFCocItem*( trueCond : SHORTINT; VAR res : DevHostCPL.Item );
	(* Makes an fcoc item with the true-condition trueCond. *)
	BEGIN (* MakeFCocItem *)
		res.mode := fcoc;
		res.typ := DevCPT.booltyp; res.acttyp := DevCPT.booltyp;
		res.bd := DevHostCPL.TFFConds( trueCond );
		(* leave tJump and fJump unchanged! *)
	END MakeFCocItem;

	PROCEDURE Swap( x : SET ) : SHORTINT;
	(* Writes bits 15 to 0 to the positions 0 to 15 of the result. Used for MOVEM. *)
		VAR y : SET;
				i : INTEGER;
	BEGIN (* Swap *)
		y := { };
		FOR i := 0 TO 15 DO
			IF i IN x THEN INCL( y, 15 - i ); END;
		END; (* FOR *)
		RETURN SHORT( SYSTEM.VAL( INTEGER, y ) )
	END Swap;

	PROCEDURE SwappedFloats( x : SET ) : SHORTINT;
	(* Writes bits 23 to 16 to the positions 0 to 7 of the result. Used for FMOVEM. *)
		VAR y : SET;
				i : INTEGER;
	BEGIN (* SwappedFloats *)
		y := { };
		FOR i := 16 TO 23 DO
			IF i IN x THEN INCL( y, 23 - i ); END;
		END; (* FOR *)
		RETURN SHORT( SYSTEM.VAL( INTEGER, y ) )
	END SwappedFloats;

	PROCEDURE Floats( x : SET ) : SHORTINT;
	(* Writes bits 16 to 23 to the positions 0 to 7 of the result. Used for FMOVEM. *)
		VAR y : SET;
				i : INTEGER;
	BEGIN (* Floats *)
		y := { };
		FOR i := 16 TO 23 DO
			IF i IN x THEN INCL( y, i - 16 ); END;
		END; (* FOR *)
		RETURN SHORT( SYSTEM.VAL( INTEGER, y ) )
	END Floats;

	PROCEDURE PushRegs*( regs : SET );
	(* Pushes the given registers onto the stack. *)
		VAR sppredec : DevHostCPL.Item;
				regList : SHORTINT;
	BEGIN (* PushRegs *)
		MakeSPPredec( sppredec );
		regList := Swap( regs );
		IF regList # 0 THEN
			DevHostCPL.Movem( 0, regList, sppredec );
		END; (* IF *)
		regList := Floats( regs );
(*		IF regList # 0 THEN
			DevHostCPL.FMovem( 0, regList , sppredec );
		END; (* IF *)*)
	END PushRegs;

	PROCEDURE PopRegs*( regs : SET );
	(* Pops the given registers from the stack. *)
		VAR sppostinc : DevHostCPL.Item;
				regList : SHORTINT;
	BEGIN (* PopRegs *)
		MakeSPPostinc(sppostinc);
		regList := SwappedFloats( regs );
(*		IF regList # 0 THEN
			DevHostCPL.FMovem( 1, regList, sppostinc );
		END; (* IF *)*)
		regList := SHORT( SYSTEM.VAL( INTEGER, regs ) );
		IF regList # 0 THEN
			DevHostCPL.Movem( 1, regList, sppostinc );
		END; (* IF *)
	END PopRegs;

	PROCEDURE TrueJump*( VAR expression : DevHostCPL.Item; VAR label : DevHostCPL.Label );
	(* Generates a conditional branch to the given label with the true condition. *)
	BEGIN (* TrueJump *)
		IF expression.mode = imm THEN
			IF expression.bd # 0  THEN
				DevHostCPL.Jump( true, label );
			END; (* IF *)
		ELSIF expression.mode = coc THEN
			DevHostCPL.Jump( SHORT( expression.bd DIV 10000H ), label );
(*		ELSIF expression.mode = fcoc THEN
			DevHostCPL.FJump( SHORT( expression.bd DIV 10000H ), label );*)
		ELSE
			DevHostCPL.Load( expression );
			DevHostCPL.Format7( TST, expression );
			DevHostCPL.Jump( NE, label );
		END; (* IF *)
		DevHostCPL.SetLabel( expression.fJump );
	END TrueJump;

	PROCEDURE FalseJump*( VAR expression : DevHostCPL.Item; VAR label : DevHostCPL.Label );
	(* Generates a conditional branch to the given label with the false condition. *)
	BEGIN (* FalseJump *)
		IF expression.mode = imm THEN
			IF expression.bd = 0 THEN
				DevHostCPL.Jump( true, label );
			END; (* IF *)
		ELSIF expression.mode = coc THEN
			DevHostCPL.Jump( SHORT( expression.bd MOD 10000H ), label );
(*		ELSIF expression.mode = fcoc THEN
			DevHostCPL.FJump( SHORT( expression.bd MOD 10000H ), label );*)
		ELSE
			DevHostCPL.Load( expression );
			DevHostCPL.Format7( TST, expression );
			DevHostCPL.Jump( EQ, label );
		END; (* IF *)
		DevHostCPL.SetLabel( expression.tJump );
	END FalseJump;

	PROCEDURE MoveBlock( scale : SHORTINT; VAR size, source, dest : DevHostCPL.Item; opt: BOOLEAN );
	(* Moves a block of data of length size from source to dest. *)
		VAR i : INTEGER;
				losize : DevHostCPL.Item;
				label : DevHostCPL.Label;
	BEGIN (* MoveBlock *)
		IF opt & (size.mode = imm) THEN
			size.bd := size.bd * scale; scale := 1;
			IF size.bd MOD 8 = 0 THEN
				scale := 8; size.bd := size.bd DIV 8;
			ELSIF size.bd MOD 4 = 0 THEN
				scale := 4; size.bd := size.bd DIV 4;
			ELSIF size.bd MOD 2 = 0 THEN
				scale := 2; size.bd := size.bd DIV 2;
			END;
		END;
		IF scale = 1 THEN
			MakePostInc( DevCPT.int8typ, source );
			MakePostInc( DevCPT.int8typ, dest );
		ELSIF scale = 2 THEN
			MakePostInc( DevCPT.int16typ, source );
			MakePostInc( DevCPT.int16typ, dest );
		ELSE
			MakePostInc( DevCPT.int32typ, source );
			MakePostInc( DevCPT.int32typ, dest );
		END; (* IF *)
		IF ( size.mode = imm ) & ( size.bd <= 6 ) THEN
			i := 0;
			WHILE i < size.bd DO
				DevHostCPL.Move( source, dest );
				IF scale = 8 THEN DevHostCPL.Move(source, dest); END;
				INC( i );
			END; (* WHILE *)
		ELSE
			IF size.mode = imm THEN
				DEC( size.bd );
			ELSE
				DevHostCPL.Load( size );
				DevHostCPL.Format1( SUBQ, 1, size );
			END; (* IF *)
			IF ( ( size.mode = imm ) & ( size.bd <= MAX( SHORTINT ) ) ) OR ( size.typ # DevCPT.int32typ ) THEN
				DevHostCPL.Load( size );
				Convert( size, DevCPT.int16typ );
				label := DevHostCPL.NewLabel;
				DevHostCPL.SetLabel( label );
				DevHostCPL.Move( source, dest );
				IF scale = 8 THEN DevHostCPL.Move( source, dest ); END;
				DevHostCPL.DBcc( false, size.reg, label );
			ELSE
				DevHostCPL.Load( size );
				losize.mode := dreg;
				losize.typ := DevCPT.int16typ; losize.acttyp := DevCPT.int16typ;
				losize.reg := DevHostCPL.GetReg( );
				DevHostCPL.Move( size, losize );
				DevHostCPL.Swap( size );
				label := DevHostCPL.NewLabel;
				DevHostCPL.SetLabel( label );
				DevHostCPL.Move( source, dest );
				IF scale = 8 THEN DevHostCPL.Move( source, dest ); END;
				DevHostCPL.DBcc( false, losize.reg, label );
				DevHostCPL.DBcc( false, size.reg, label );
				DevHostCPL.FreeReg(losize);
			END; (* IF *)
		END; (* IF *)
	END MoveBlock;

	(* pointer handling for rt gc *)
	
	PROCEDURE ContainsPtrs (typ: DevCPT.Struct): BOOLEAN;
		VAR fld: DevCPT.Object;
	BEGIN
		WHILE typ.comp IN {DynArr, Array} DO typ := typ.BaseTyp END;
		IF (typ.form = Pointer) & ~typ.untagged THEN RETURN TRUE
		ELSIF typ.comp = Record THEN
			REPEAT
				fld := typ.link;
				WHILE (fld # NIL) & (fld.mode = Fld) DO
					IF (fld.name^ = DevCPM.HdPtrName) OR ContainsPtrs(fld.typ) THEN RETURN TRUE END;
					fld := fld.link
				END;
				typ := typ.BaseTyp
			UNTIL typ = NIL
		END;
		RETURN FALSE
	END ContainsPtrs;
	
	PROCEDURE MarkPtr (VAR x: DevHostCPL.Item);
		VAR p: DevHostCPL.Item; lbl: DevHostCPL.Label;
	BEGIN
		p.mode := areg; p.typ := x.typ; p.reg := 13 (* A5 *) (* DevHostCPL.GetAdrReg() *);
		DevHostCPL.Move(x, p);
		DevHostCPL.Free(x); x := p;
		DevHostCPL.Format7( TST, x );
		(* lbl := DevHostCPL.NewLabel; *)
		(* DevHostCPL.Jump( EQ, lbl ); *)
		DevCPE.GenShort(6704H);	(* BEQ lbl *)
		p.mode := regx; p.reg := x.reg; p.bd := -8; p.inxReg := -1; p.typ := DevCPT.booltyp;
		DevHostCPL.Format7( CLR, p );	(* 4 bytes *)
		(* DevHostCPL.SetLabel( lbl ); *)
	END MarkPtr;
	
	PROCEDURE MovePtrBlock (scale : SHORTINT; VAR size, source, dest : DevHostCPL.Item; typ: DevCPT.Struct);
		VAR reg, losize: DevHostCPL.Item; fld: DevCPT.Object; off: INTEGER;
			x: DevHostCPL.Item; b: DevCPT.Struct; label: DevHostCPL.Label;
	BEGIN
		IF typ.comp = Record THEN
			ASSERT(size.mode = imm);
			ASSERT(size.bd * scale = typ.size);
			fld := typ.link; off := 0;
			WHILE (fld # NIL) & (fld.mode = Fld) DO
				IF (fld.name^ = DevCPM.HdPtrName) OR (fld.typ.form = Pointer) & ~fld.typ.untagged THEN
					IF fld.adr > off THEN
						size.bd := fld.adr - off;
						MoveBlock(1, size, source, dest, TRUE)
					END;
					MakePostInc( DevCPT.int32typ, source );
					MakePostInc( DevCPT.int32typ, dest );
					x := source; MarkPtr(x);
					DevHostCPL.Move(x, dest);
					off := fld.adr + 4
				ELSIF ContainsPtrs(fld.typ) THEN DevCPM.err(275)
				END;
				fld := fld.link
			END;
			IF typ.size > off THEN
				size.bd := typ.size - off;
				MoveBlock(1, size, source, dest, TRUE)
			END
		ELSE	(* array *)
			b := typ.BaseTyp; WHILE b.comp IN {Array, DynArr} DO b := b.BaseTyp END;
			IF b.form = Pointer THEN
				ASSERT(~typ.untagged);
				MakePostInc( DevCPT.int32typ, source );
				MakePostInc( DevCPT.int32typ, dest );
				IF typ.comp = DynArr THEN
					ASSERT(scale = 4);
					DevHostCPL.Load( size );
					DevHostCPL.Format1( SUBQ, 1, size );
				ELSE
					ASSERT(typ.size MOD 4 = 0);
					ASSERT(size.mode = imm);
					size.bd := typ.size DIV 4 - 1;
				END;
				label := DevHostCPL.NewLabel;
				IF ( ( size.mode = imm ) & ( size.bd <= MAX( SHORTINT ) ) ) OR ( size.typ # DevCPT.int32typ ) THEN
					DevHostCPL.Load( size );
					Convert( size, DevCPT.int16typ );
					DevHostCPL.SetLabel( label );
					x := source; MarkPtr(x);
					DevHostCPL.Move(x, dest);
					IF scale = 8 THEN DevHostCPL.Move( source, dest ); END;
					DevHostCPL.DBcc( false, size.reg, label );
				ELSE
					DevHostCPL.Load( size );
					losize.mode := dreg;
					losize.typ := DevCPT.int16typ; losize.acttyp := DevCPT.int16typ;
					losize.reg := DevHostCPL.GetReg( );
					DevHostCPL.Move( size, losize );
					DevHostCPL.Swap( size );
					DevHostCPL.SetLabel( label );
					x := source; MarkPtr(x);
					DevHostCPL.Move(x, dest);
					DevHostCPL.DBcc( false, losize.reg, label );
					DevHostCPL.DBcc( false, size.reg, label );
					DevHostCPL.FreeReg(losize);
				END
			ELSE DevCPM.err(275)
			END
		END
	END MovePtrBlock;
	
	
	PROCEDURE Assign*( VAR source, dest : DevHostCPL.Item );
	(* Generates code for the assignment dest := source.  *)
		VAR size : INTEGER;
				length, src, dst : DevHostCPL.Item;
				label : DevHostCPL.Label;
				scale : SHORTINT;
				typ: DevCPT.Struct;
	BEGIN (* Assign *)
		IF source.mode = xreal THEN
			IF source.reg # DevHostCPL.xrealstack THEN DevCPM.err(-811); END;
			src.mode := regx; src.reg := 15; src.bd := 0; src.inxReg := None;
			src.obj := NIL; src.typ := DevCPT.int32typ; src.acttyp := DevCPT.int32typ;
			IF dest.typ.form = Int64 THEN
				RoundDown;
			END;
			FromXReal(src, dest);
			IF dest.typ.form = Int64 THEN
				RoundNearest;
			END
		ELSE
			Convert( source, dest.typ );
			size := source.typ.size;
			DevHostCPL.LoadAdr( dest );
(*			IF source.mode = freg THEN
				DevHostCPL.FMove( source, dest );
			ELS*)
			IF source.typ.form IN RealSet THEN
				IF source.acttyp.form # dest.typ.form THEN
					INC(DevHostCPL.xrealstack); AddToSP(-12);
					dst.mode := regx; dst.reg := 15; dst.bd := 0; dst.inxReg := -1;
					dst.obj := NIL; dst.typ := DevCPT.int32typ; dst.acttyp := DevCPT.int32typ;
					ToXReal(source, dst);
					DevHostCPL.FreeReg(source);
					source.mode := xreal; source.reg := DevHostCPL.xrealstack;
					src.mode := regx; src.reg := 15; src.bd := 0; src.inxReg := None;
					src.obj := NIL; src.typ := DevCPT.int32typ; src.acttyp := DevCPT.int32typ;
					IF dest.typ.form = Int64 THEN
						RoundDown;
					END;
					FromXReal(src, dest);
					IF dest.typ.form = Int64 THEN
						RoundNearest;
					END
				ELSIF source.typ.form = Real THEN
					DevHostCPL.Move( source, dest );
				ELSE (* LReal, Int64 *)
					typ := dest.typ;
					IF (source.mode = imm) OR (source.mode = dreg) THEN	(* bh *)
						DevHostCPL.LoadExternal(dest);
						source.typ := DevCPT.int32typ; source.acttyp := DevCPT.int32typ;
						dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
						DevHostCPL.Move(source, dest);
						INC(dest.bd, 4);
						size := source.bd; source.bd := source.ext;
						scale := source.reg; source.reg := source.inxReg;
						DevHostCPL.Move(source, dest);
						DEC(dest.bd, 4);
						source.bd := size; source.reg := scale;
					ELSE
						DevHostCPL.LoadAdr( source );
						DevHostCPL.LoadExternal( source );
						DevHostCPL.LoadExternal( dest );
						source.typ := DevCPT.int32typ; source.acttyp := DevCPT.int32typ;
						dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
						DevHostCPL.Move( source, dest );
						INC(source.bd, 4);
						INC(dest.bd, 4);
						DevHostCPL.Move( source, dest );
						DEC(source.bd, 4);
						DEC(dest.bd, 4);
					END;
					source.typ := typ; source.acttyp := typ;
					dest.typ := typ; dest.acttyp := typ;
				END;
			ELSIF source.mode IN { coc, fcoc } THEN
				src.mode := imm;
				src.typ := DevCPT.booltyp; src.acttyp := DevCPT.booltyp;
				label := DevHostCPL.NewLabel;
				IF source.mode = coc THEN
					DevHostCPL.Jump( SHORT( source.bd MOD 10000H ), source.fJump );
(*				ELSE
					DevHostCPL.FJump( SHORT( source.bd MOD 10000H ), source.fJump );*)
				END;
				DevHostCPL.SetLabel( source.tJump );
				src.bd := 1;
				DevHostCPL.Move( src, dest );
				DevHostCPL.Jump( true, label );
				DevHostCPL.SetLabel( source.fJump );
				src.bd := 0;
				DevHostCPL.Move( src, dest );
				DevHostCPL.SetLabel( label );
			ELSIF (dest.typ.form = Pointer) & (source.typ.form IN {String8, String16, Comp}) THEN
				DevHostCPL.LoadAdr(source);
				DevHostCPL.Move( source, dest );
			ELSIF (source.typ.form = Pointer) & (source.mode # imm) & ~source.typ.untagged & rtGC THEN
				MarkPtr(source);
				DevHostCPL.Move( source, dest );
			ELSIF ( size = 1 ) OR ( size = 2 ) OR ( size = 4 ) THEN
				DevHostCPL.Move( source, dest );
			ELSE (* complex data structure *)
				IF source.typ.comp = DynArr THEN
					Size( source, length, scale );
					GetDynArrVal( source, source.typ.untagged );
				ELSE
					IF size MOD 4 = 0 THEN
						scale := 4;
						MakeIntConst( size DIV 4, DevCPT.int32typ, length );
					ELSIF size MOD 2 = 0 THEN
						scale := 2;
						MakeIntConst( size DIV 2, DevCPT.int32typ, length );
					ELSE
						scale := 1;
						MakeIntConst( size, DevCPT.int32typ, length );
					END; (* IF *)
				END; (* IF *)
				IF rtGC & ContainsPtrs(source.typ) THEN MovePtrBlock(scale, length, source, dest, source.typ)
				ELSE MoveBlock( scale, length, source, dest, TRUE )
				END
			END; (* IF *)
		END;
	END Assign;

	PROCEDURE PushStack*(VAR item : DevHostCPL.Item );
		VAR dest : DevHostCPL.Item; typ: DevCPT.Struct; temp: INTEGER; r: SHORTINT;
	BEGIN
		dest.mode := predec;
		dest.reg := SP.reg;
		IF (item.mode # xreal) & ((item.typ.form = LReal) & (item.acttyp.form = LReal) OR (item.typ.form = Int64) & (item.acttyp.form = Int64)) THEN
			typ := item.typ;
			IF (item.mode = imm) OR (item.mode = dreg) THEN	(* bh *)
				dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
				item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
				temp := item.bd; item.bd := item.ext;
				r := item.reg; item.reg := item.inxReg;
				Assign(item, dest);
				item.bd := temp; item.reg := r;
				Assign(item, dest);
				dest.typ := typ; dest.acttyp := typ;
				item.typ := typ; item.acttyp := typ;
(*			ELSIF item.mode = freg THEN
				DevHostCPL.FMove(item, dest);*)
			ELSE
				dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
				item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
				INC(item.bd, 4);
				Assign(item, dest);
				DEC(item.bd, 4);
				Assign(item, dest);
				dest.typ := typ; dest.acttyp := typ;
				item.typ := typ; item.acttyp := typ;
			END;
		ELSE
			dest.typ := item.typ; dest.acttyp := item.typ;
			Assign( item, dest );
		END;
	END PushStack;

	PROCEDURE StringLen*(include0X: BOOLEAN; VAR item: DevHostCPL.Item);
	VAR
		len, scratch: DevHostCPL.Item;
		typ: DevCPT.Struct;
		loop: DevHostCPL.Label;
	BEGIN
		len.mode := dreg; len.reg := DevHostCPL.GetReg(); len.bd := 0; len.inxReg := None;
		len.obj := NIL; len.typ := DevCPT.int32typ; len.acttyp := DevCPT.int32typ;
		IF include0X THEN
			DevHostCPL.Moveq(0, len.reg);
		ELSE
			DevHostCPL.Moveq(-1, len.reg);
		END;
		IF (item.acttyp.form = String16) OR ((item.acttyp.form = Comp) & ((item.acttyp.comp = Array) OR (item.acttyp.comp = DynArr)) & (item.acttyp.BaseTyp.form = Char16)) THEN
			typ := DevCPT.char16typ;
		ELSE
			typ := DevCPT.char8typ;
		END;
		IF item.acttyp.comp = DynArr THEN GetDynArrVal(item, item.acttyp.untagged) END;
		MakePostInc(typ, item);
		scratch.mode := dreg; scratch.reg := DevHostCPL.GetReg(); scratch.bd := 0; scratch.inxReg := None;
		scratch.obj := NIL; scratch.typ := typ; scratch.acttyp := typ;
		loop := DevHostCPL.NewLabel;
		DevHostCPL.SetLabel(loop);
		DevHostCPL.Format1(ADDQ, 1, len);
		DevHostCPL.Move(item, scratch);
		DevHostCPL.Jump(NE, loop);
		DevHostCPL.Free(item); DevHostCPL.Free(scratch);
		item := len;
	END StringLen;

	PROCEDURE PushDynArrStack*( formalTyp : DevCPT.Struct; (* offset : LONGINT; *) VAR item : DevHostCPL.Item );
	(* Moves the address and the length(s) of the given item to (offset, A7). *)
		VAR source, adr, length, len1 : DevHostCPL.Item;
				typ : DevCPT.Struct;
				dim : INTEGER;
				lengthMade : BOOLEAN;
	BEGIN (* MoveDynArrStack *)
		IF ~item.typ.untagged THEN
			dim := formalTyp.n;
			typ := item.typ;
			WHILE ( typ.comp = DynArr ) & (dim > 0) DO
				MakeLen(item, dim, length);
				PushStack(length);
				DEC( dim );
				typ := typ.BaseTyp;
				formalTyp := formalTyp.BaseTyp;
			END; (* WHILE *)
			WHILE dim > 0 DO
				IF typ.form IN {String8, String16} THEN
					MakeIntConst( item.nolen, DevCPT.int32typ, length );
				ELSE
					MakeIntConst( typ.n, DevCPT.int32typ, length );
				END; (* IF *)
				PushStack(length);
				DEC( dim );
				typ := typ.BaseTyp;
				formalTyp := formalTyp.BaseTyp;
			END; (* WHILE *)
			IF ( formalTyp.comp = DynArr ) & ( formalTyp.BaseTyp = DevCPT.bytetyp ) THEN
			(*  a big chunk of unecessary stuff folded away, still here for possible future reference.  *)
			ELSIF typ.comp = DynArr THEN
				MakeLen(item, dim, length);
			ELSIF typ.form IN {String8, String16} THEN
				(* if variable string -> get dynamic string length (incl 0X) !!! *)
				length := item;
				StringLen(TRUE, length);
			ELSE
				MakeIntConst( typ.n, DevCPT.int32typ, length );
			END; (* IF *)
		ELSE
			MakeIntConst(item.typ.size, DevCPT.int32typ, length);
		END;
		PushStack(length);
		IF item.acttyp.comp = DynArr THEN
			source := item;
			GetDynArrAdr( source, adr );
			PushStack(adr);
		ELSE
			DevHostCPL.Pea(item);
		END;
	END PushDynArrStack;

	PROCEDURE Copy*(VAR source, dest, destlen : DevHostCPL.Item; checklen, last: BOOLEAN);
	(* Generates code for COPY( source, dest ). dest may not be bigger than 32kB. *)
		VAR loop, lastchar, end : DevHostCPL.Label;
				temp, dbuf, sbuf : DevHostCPL.Item;
				sourcetyp, desttyp, sourceacttyp: DevCPT.Struct;
	BEGIN (* Copy *)
		sourceacttyp := source.acttyp;
		IF (sourceacttyp.form = String16)
			OR ((sourceacttyp.comp = Array) OR (sourceacttyp.comp = DynArr))
				& (sourceacttyp.BaseTyp.form = Char16) THEN
			sourcetyp := DevCPT.char16typ;
		ELSE
			sourcetyp := DevCPT.char8typ;
		END;
		IF sourceacttyp.comp = DynArr THEN GetDynArrVal( source, sourceacttyp.untagged ) END;
		IF destlen.mode = None THEN
			IF (dest.acttyp.form = String16)
				OR ((dest.acttyp.comp = Array) OR (dest.acttyp.comp = DynArr))
					& (dest.acttyp.BaseTyp.form = Char16) THEN
				desttyp := DevCPT.char16typ;
			ELSE
				desttyp := DevCPT.char8typ;
			END;
			IF dest.acttyp.comp = DynArr THEN
				IF dest.acttyp.untagged THEN
					MakeIntConst(0FFFFH, DevCPT.int32typ, destlen);
				ELSE
					MakeLen(dest, 0, destlen)
				END;
				GetDynArrVal( dest, dest.acttyp.untagged );
				DevHostCPL.Load( destlen );
				IF ~dest.acttyp.untagged THEN DevHostCPL.Format1( SUBQ, 1, destlen ) END
			ELSE
				MakeIntConst( dest.acttyp.n - 1, DevCPT.int32typ, destlen );
				DevHostCPL.Load( destlen )
			END;
			MakePostInc(desttyp, dest);
		END;
		MakePostInc(sourcetyp, source);
		loop := DevHostCPL.NewLabel;
		IF sourcetyp = dest.typ THEN
			IF sourceacttyp # shortString16typ THEN
				DevHostCPL.SetLabel( loop );
				DevHostCPL.Move( source, dest );
				DevHostCPL.DBcc( EQ, destlen.reg, loop );
			ELSE
				dbuf.mode := dreg; dbuf.reg := DevHostCPL.GetReg(); dbuf.bd := 0; dbuf.inxReg := None;
				dbuf.obj := NIL; dbuf.typ := DevCPT.char16typ; dbuf.acttyp := DevCPT.char16typ;
				DevHostCPL.SetLabel(loop);
				DevHostCPL.Move(source, dbuf);
				DevHostCPL.Format6(ANDI, 0FFH, dbuf);
				DevHostCPL.Move(dbuf, dest);
				DevHostCPL.DBcc(EQ, destlen.reg, loop);
				DevHostCPL.Free(dbuf);
			END;
		ELSE
			dbuf.mode := dreg; dbuf.reg := DevHostCPL.GetReg(); dbuf.bd := 0; dbuf.inxReg := None;
			dbuf.obj := NIL; dbuf.typ := dest.typ; dbuf.acttyp := dest.typ;
			sbuf := dbuf; sbuf.typ := sourcetyp; sbuf.acttyp := sourcetyp;
			IF sourcetyp = DevCPT.char8typ THEN
				DevHostCPL.Clr(dbuf);
			END;
			DevHostCPL.SetLabel(loop);
			DevHostCPL.Move(source, sbuf);
			IF sourcetyp = DevCPT.char16typ THEN
				lastchar := DevHostCPL.NewLabel;
				DevHostCPL.Jump(EQ, lastchar);
				DevHostCPL.Move(dbuf, dest);
				DevHostCPL.DBcc(false, destlen.reg, loop);
				end := DevHostCPL.NewLabel;
				DevHostCPL.Jump(true, end);
				DevHostCPL.SetLabel(lastchar);
				DevHostCPL.Move(dbuf, dest);
				DevHostCPL.SetLabel(end);
			ELSE (* no special testing and branching needed as upper half stays zero during the loop *)
				DevHostCPL.Move(dbuf, dest);
				DevHostCPL.DBcc(EQ, destlen.reg, loop);
			END;
			DevHostCPL.Free(dbuf);
		END;
		IF checklen THEN
			DevHostCPL.Trapcc(NE, inxTrap);
			IF ~last THEN
				temp.mode := areg; temp.reg := dest.reg; temp.bd := 0; temp.inxReg := None;
				temp.obj := NIL; temp.typ := DevCPT.int32typ; temp.acttyp := DevCPT.int32typ;
				DevHostCPL.Format1(SUBQ, SHORT(dest.typ.size), temp);
			END;
		ELSE
			dest.mode := predec;
			MakeIntConst( 0, dest.typ, temp );
			DevHostCPL.Move( temp, dest )
		END;
	END Copy;
	
	PROCEDURE Decrement*( VAR designator, expression : DevHostCPL.Item );
	(* Decrements the value of designator by expression *)
	VAR
		low, high, lowe, highe, addr, temp: DevHostCPL.Item;
		label: DevHostCPL.Label;
	BEGIN (* Decrement *)
		IF designator.typ.form = Int64 THEN
			IF ~realLib & ((expression.mode = xreal) OR ((expression.mode = regx) & (expression.typ.form = Int64) & (expression.acttyp.form = Int64))) THEN (* handle subtraction in memory *)
				temp := designator; INC(temp.bd, 8);
				high.mode := predec; high.reg := DevHostCPL.GetAdrReg(); high.typ := DevCPT.int32typ; high.acttyp := DevCPT.int32typ;
				DevHostCPL.Lea(temp, high.reg);
				highe.mode := predec; highe.reg := DevHostCPL.GetAdrReg(); highe.typ := DevCPT.int32typ; highe.acttyp := DevCPT.int32typ;
				IF expression.mode = xreal THEN
					AddToSP(-8); (* make room for Int64 on stack *)
					ASSERT(expression.reg = DevHostCPL.xrealstack, 20);
					addr.mode := regx; addr.reg := 15; addr.bd := 8; addr.inxReg := None;
					addr.typ := NIL; addr.acttyp := NIL;
					temp.mode := regx; temp.reg := 15; temp.bd := 0; temp.inxReg := None;
					temp.typ := DevCPT.int64typ; temp.acttyp := DevCPT.int64typ;
					FromXReal(addr, temp);
				ELSE
					temp := expression;
				END;
				INC(temp.bd, 8);
				DevHostCPL.Lea(temp, highe.reg);
				temp.mode := dreg; temp.reg := 0; temp.typ := DevCPT.int16typ; temp.acttyp := DevCPT.int16typ;
				DevHostCPL.Format6(ADDI, 0, temp); (* clear the X bit in the condition codes *)
				DevHostCPL.SubX(highe, high);
				DevHostCPL.SubX(highe, high);
				DevHostCPL.Free(highe); DevHostCPL.Free(high);
				IF expression.mode = xreal THEN
					AddToSP(8);
				END;
			ELSE (* load operands, subtract, store *)
				addr := designator; addr.typ := DevCPT.int32typ; addr.acttyp := DevCPT.int32typ;
				low := addr; INC(low.bd, 4);
				(* low.mode := dreg; low.reg := DevHostCPL.GetReg(); low.typ := DevCPT.int32typ;
				highe.mode := dreg; highe.reg := DevHostCPL.GetReg(); highe.typ := DevCPT.int32typ;
				INC(addr.bd, 4);
				DevHostCPL.Move(addr, low); *)
				IF expression.typ.form # Int64 THEN Convert(expression, DevCPT.int64typ) END;
				IF expression.mode = imm THEN
					ASSERT(expression.typ.form = Int64, 20);
					highe.mode := dreg; highe.reg := DevHostCPL.GetReg(); highe.typ := DevCPT.int32typ;
					IF (expression.bd >= MIN(BYTE)) & (expression.bd <= MAX(BYTE)) THEN
						DevHostCPL.Moveq(SHORT(expression.bd), highe.reg);
					ELSE
						MakeIntConst(expression.bd, DevCPT.int32typ, temp);
						DevHostCPL.Move(temp, highe);
					END;
					IF (expression.ext > 0) & (expression.ext <= 8) THEN
						DevHostCPL.Format1(SUBQ, SHORT(expression.ext), low);
					ELSE
						DevHostCPL.Format6(SUBI, expression.ext, low);
					END;
				ELSIF expression.mode = dreg THEN	(* bh *)
					highe.mode := dreg; highe.typ := DevCPT.int32typ; highe.reg := expression.reg;
					lowe.mode := dreg; lowe.typ := DevCPT.int32typ; lowe.reg := expression.inxReg;
					DevHostCPL.Format2(SUB, lowe, low);
				ELSIF expression.mode = regx THEN	(* bh *)
					highe := expression; highe.typ := DevCPT.int32typ; DevHostCPL.Load(highe);
					lowe := expression; lowe.typ := DevCPT.int32typ; INC(lowe.bd, 4); DevHostCPL.Load(lowe);
					DevHostCPL.Format2(SUB, lowe, low);
					DevHostCPL.Free(lowe);
				(* ELSIF (expression.mode = regx) OR (expression.mode = dreg) THEN
					DevHostCPL.Moveq(0, highe.reg);
					lowe := expression; lowe.typ := lowe.acttyp;
					IF lowe.mode = regx THEN
						DevHostCPL.Load(lowe);
					ELSIF lowe.typ.form = Int32 THEN
						DevHostCPL.Test(lowe);
					END;
					IF lowe.typ.form # Int32 THEN
						Convert(lowe, DevCPT.int32typ);
					END;
					label := DevHostCPL.NewLabel;
					DevHostCPL.Jump(PL, label);
					DevHostCPL.Moveq(-1, highe.reg);
					DevHostCPL.SetLabel(label);
					DevHostCPL.Format2(SUB, lowe, low);
					IF expression.mode # dreg THEN
						DevHostCPL.Free(lowe);
					END; *)
				ELSE (* other modes should not appear, either the compiler or my understanding of it is broken *)
					HALT(0);
				END;
				high.mode := dreg; high.reg := DevHostCPL.GetReg(); high.typ := DevCPT.int32typ;
				DevHostCPL.Move(addr, high);
				DevHostCPL.SubX(highe, high);
				DevHostCPL.Free(highe);
				(* DevHostCPL.Move(low, addr);
				DEC(addr.bd, 4); *)
				DevHostCPL.Move(high, addr);
				DevHostCPL.Free(high); (* DevHostCPL.Free(low); *)
			END
		ELSE
			IF expression.mode = imm THEN
				IF ( expression.bd > 0 ) & ( expression.bd <= 8 ) THEN
					DevHostCPL.Format1( SUBQ, SHORT( expression.bd ), designator );
				ELSIF ( expression.bd < 0 ) & ( expression.bd >= -8 ) THEN
					DevHostCPL.Format1( ADDQ, SHORT( -expression.bd ), designator );
				ELSIF expression.bd # 0 THEN
					DevHostCPL.Format6( SUBI, expression.bd, designator );
				END; (* IF *)
			ELSE
				DevHostCPL.Format2( SUB, expression, designator );
			END; (* IF *)
		END;
	END Decrement;

	PROCEDURE Increment*( VAR designator, expression : DevHostCPL.Item );
	(* Increments the value of designator by expression *)
	VAR
		low, high, lowe, highe, addr, temp: DevHostCPL.Item;
		label: DevHostCPL.Label;
	BEGIN (* Increment *)
		IF designator.typ.form = Int64 THEN
			IF ~realLib & ((expression.mode = xreal) OR ((expression.mode = regx) & (expression.typ.form = Int64) & (expression.acttyp.form = Int64))) THEN (* handle addition in memory *)
				temp := designator; INC(temp.bd, 8);
				high.mode := predec; high.reg := DevHostCPL.GetAdrReg(); high.typ := DevCPT.int32typ; high.acttyp := DevCPT.int32typ;
				DevHostCPL.Lea(temp, high.reg);
				highe.mode := predec; highe.reg := DevHostCPL.GetAdrReg(); highe.typ := DevCPT.int32typ; highe.acttyp := DevCPT.int32typ;
				IF expression.mode = xreal THEN
					AddToSP(-8); (* make room for Int64 on stack *)
					ASSERT(expression.reg = DevHostCPL.xrealstack, 20);
					addr.mode := regx; addr.reg := 15; addr.bd := 8; addr.inxReg := None;
					addr.typ := NIL; addr.acttyp := NIL;
					temp.mode := regx; temp.reg := 15; temp.bd := 0; temp.inxReg := None;
					temp.typ := DevCPT.int64typ; temp.acttyp := DevCPT.int64typ;
					FromXReal(addr, temp);
				ELSE
					temp := expression;
				END;
				INC(temp.bd, 8);
				DevHostCPL.Lea(temp, highe.reg);
				temp.mode := dreg; temp.reg := 0; temp.typ := DevCPT.int16typ; temp.acttyp := DevCPT.int16typ;
				DevHostCPL.Format6(ADDI, 0, temp); (* clear the X bit in the condition codes *)
				DevHostCPL.AddX(highe, high);
				DevHostCPL.AddX(highe, high);
				IF expression.mode = xreal THEN
					AddToSP(8);
				END;
			ELSE (* load operands, add, store *)
				addr := designator; addr.typ := DevCPT.int32typ; addr.acttyp := DevCPT.int32typ;
				low := addr; INC(low.bd, 4);
				(* low.mode := dreg; low.reg := DevHostCPL.GetReg(); low.typ := DevCPT.int32typ;
				highe.mode := dreg; highe.reg := DevHostCPL.GetReg(); highe.typ := DevCPT.int32typ;
				INC(addr.bd, 4);
				DevHostCPL.Move(addr, low); *)
				IF expression.typ.form # Int64 THEN Convert(expression, DevCPT.int64typ) END;
				IF expression.mode = imm THEN
					ASSERT(expression.typ.form = Int64, 20);
					highe.mode := dreg; highe.reg := DevHostCPL.GetReg(); highe.typ := DevCPT.int32typ;
					IF (expression.bd >= MIN(BYTE)) & (expression.bd <= MAX(BYTE)) THEN
						DevHostCPL.Moveq(SHORT(expression.bd), highe.reg);
					ELSE
						MakeIntConst(expression.bd, DevCPT.int32typ, temp);
						DevHostCPL.Move(temp, highe);
					END;
					IF (expression.ext > 0) & (expression.ext <= 8) THEN
						DevHostCPL.Format1(ADDQ, SHORT(expression.ext), low);
					ELSE
						DevHostCPL.Format6(ADDI, expression.ext, low);
					END;
				ELSIF expression.mode = dreg THEN	(* bh *)
					highe.mode := dreg; highe.typ := DevCPT.int32typ; highe.reg := expression.reg;
					lowe.mode := dreg; lowe.typ := DevCPT.int32typ; lowe.reg := expression.inxReg;
					DevHostCPL.Format2(ADD, lowe, low);
				ELSIF expression.mode = regx THEN	(* bh *)
					highe := expression; highe.typ := DevCPT.int32typ; DevHostCPL.Load(highe);
					lowe := expression; lowe.typ := DevCPT.int32typ; INC(lowe.bd, 4); DevHostCPL.Load(lowe);
					DevHostCPL.Format2(ADD, lowe, low);
					DevHostCPL.Free(lowe);
				(* ELSIF (expression.mode = regx) OR (expression.mode = dreg) THEN
					DevHostCPL.Moveq(0, highe.reg);
					lowe := expression; lowe.typ := lowe.acttyp;
					IF lowe.mode = regx THEN
						DevHostCPL.Load(lowe);
					ELSIF lowe.typ.form = Int32 THEN
						DevHostCPL.Test(lowe);
					END;
					IF lowe.typ.form # Int32 THEN
						Convert(lowe, DevCPT.int32typ);
					END;
					label := DevHostCPL.NewLabel;
					DevHostCPL.Jump(PL, label);
					DevHostCPL.Moveq(-1, highe.reg);
					DevHostCPL.SetLabel(label);
					DevHostCPL.Format2(ADD, lowe, low);
					IF expression.mode # dreg THEN
						DevHostCPL.Free(lowe);
					END; *)
				ELSE (* other modes should not appear, either the compiler or my understanding of it is broken *)
					HALT(0);
				END;
				high.mode := dreg; high.reg := DevHostCPL.GetReg(); high.typ := DevCPT.int32typ;
				DevHostCPL.Move(addr, high);
				DevHostCPL.AddX(highe, high);
				DevHostCPL.Free(highe);
				(* DevHostCPL.Move(low, addr);
				DEC(addr.bd, 4); *)
				DevHostCPL.Move(high, addr);
				DevHostCPL.Free(high); (* DevHostCPL.Free(low); *)
			END
		ELSE
			IF expression.mode = imm THEN
				IF ( expression.bd > 0 ) & ( expression.bd <= 8 ) THEN
					DevHostCPL.Format1( ADDQ, SHORT( expression.bd ), designator );
				ELSIF ( expression.bd < 0 ) & ( expression.bd >= -8 ) THEN
					DevHostCPL.Format1( SUBQ, SHORT( -expression.bd ), designator );
				ELSIF expression.bd # 0 THEN
					DevHostCPL.Format6( ADDI, expression.bd, designator );
				END; (* IF *)
			ELSE
				DevHostCPL.Format2( ADD, expression, designator );
			END; (* IF *)
		END;
	END Increment;

	PROCEDURE Include*( VAR set, element : DevHostCPL.Item );
	(* set := set + { element } *)
		VAR temp : DevHostCPL.Item;
	BEGIN (* Include *)
		temp.reg := DevHostCPL.GetAdrReg();
		DevHostCPL.Lea(set, temp.reg);
		DevHostCPL.FreeReg(set);
		set.mode := regx; set.reg := temp.reg; set.bd := 0; set.inxReg := None; set.offsReg := None;
		temp := set;
		IF element.mode = imm THEN
			DevHostCPL.Format4( BSET, element.bd, temp );
		ELSE
			DevHostCPL.Format5( BSET, element, temp );
		END; (* IF *)
		DevHostCPL.Move( temp, set );
	END Include;

	PROCEDURE Exclude*( VAR set, element : DevHostCPL.Item );
	(* set := set - { element } *)
		VAR temp : DevHostCPL.Item;
	BEGIN (* Exclude *)
		temp.reg := DevHostCPL.GetAdrReg();
		DevHostCPL.Lea(set, temp.reg);
		DevHostCPL.FreeReg(set);
		set.mode := regx; set.reg := temp.reg; set.bd := 0; set.inxReg := None; set.offsReg := None;
		temp := set;
		IF element.mode = imm THEN
			DevHostCPL.Format4( BCLR, element.bd, temp );
		ELSE
			DevHostCPL.Format5( BCLR, element, temp );
		END; (* IF *)
		DevHostCPL.Move( temp, set );
	END Exclude;

	PROCEDURE EnterMod*;
	(* Generates code for the entry into the module. *)
	BEGIN (* EnterMod *)
		DevHostCPL.Enter(0);
	END EnterMod;

	PROCEDURE CopyDynArrs( par : DevCPT.Object; stackCheck: BOOLEAN );
	(* Copys the dynamic arrays which are value-parameters to the stack. *)
		VAR source, dest, ptr, size, negsize, newSP, limit : DevHostCPL.Item;
				scale : SHORTINT;
	BEGIN (* CopyDynArrs *)
		WHILE par # NIL DO
			IF ( par.typ.comp = DynArr ) & ( par.mode = Var ) THEN
				MakeVar( par, source );
				Size( source, size, scale );
				DevHostCPL.Load( size );
				GetDynArrVal( source, FALSE );
				IF scale = 1 THEN (* align size to 4 bytes *)
					DevHostCPL.Format1( ADDQ, 3, size );
					DevHostCPL.Format13( ASh, -2, size );
					scale := 4;
				ELSIF scale = 2 THEN
					DevHostCPL.Format1( ADDQ, 1, size );
					DevHostCPL.Format13( ASh, -1, size );
					scale := 4;
				END; (* IF *)
				negsize.mode := dreg;
				negsize.typ := DevCPT.int32typ; negsize.acttyp := DevCPT.int32typ;
				negsize.reg := DevHostCPL.GetReg( );
				DevHostCPL.Move( size, negsize );
				DevHostCPL.Format7( NEG, negsize );
				newSP.mode := regx;
				newSP.typ := untgPtr; newSP.acttyp := untgPtr;
				newSP.reg := SP.reg;
				newSP.bd := 0;
				newSP.inxReg := negsize.reg;
				IF size.typ.form = Int32 THEN newSP.xsize := 1; ELSE newSP.xsize := 0; END;
				newSP.scale := DevHostCPL.Scale( scale );
				dest.mode := areg;
				dest.typ := untgPtr; dest.acttyp := untgPtr;
				dest.reg := DevHostCPL.GetAdrReg( );
				DevHostCPL.Lea( newSP, dest.reg );
				IF stackCheck THEN
					MakeVar(stackLimit, limit);
					DevHostCPL.Cmp(limit, dest);
					DevHostCPL.Trapcc(LE, stackTrap)
				END;
				DevHostCPL.Move( dest, SP );
				dest.mode := regx;
				dest.typ := par.typ; dest.acttyp := par.typ;
				dest.bd := 0;
				dest.inxReg := None;
				IF rtGC & ContainsPtrs(par.typ) THEN MovePtrBlock(scale, size, source, dest, par.typ)
				ELSE MoveBlock( scale, size, source, dest, TRUE )
				END;
				ptr.mode := regx;
				ptr.typ := untgPtr; ptr.acttyp := untgPtr;
				ptr.reg := FP.reg;
				ptr.bd := par.adr;
				ptr.inxReg := None;
				DevHostCPL.Move( SP, ptr );
				DevHostCPL.FreeReg(source); DevHostCPL.FreeReg(dest); DevHostCPL.FreeReg(ptr);
				DevHostCPL.FreeReg(size); DevHostCPL.FreeReg(negsize); DevHostCPL.FreeReg(newSP);
			ELSIF (par.typ.form = Comp) & (par.mode = Var) & (par.typ.size > 4) THEN
				MakeIntConst(par.typ.size, DevCPT.int32typ, size);
				source.mode := regx;
				source.reg := FP.reg;
				source.typ := untgPtr; source.acttyp := untgPtr;
				source.bd := par.num;
				source.inxReg := None;
				dest.mode := areg;
				dest.typ := untgPtr; dest.acttyp := untgPtr;
				dest.reg := DevHostCPL.GetAdrReg();
				DevHostCPL.Move(source, dest);
				source.mode := regx;
				source.reg := dest.reg;
				source.bd := 0; source.inxReg := None;
				dest.mode := regx;
				dest.typ := untgPtr; dest.acttyp := untgPtr;
				dest.reg := FP.reg;
				dest.bd := par.adr;
				dest.inxReg := None;
				IF rtGC & ContainsPtrs(par.typ) THEN MovePtrBlock(1, size, source, dest, par.typ)
				ELSE MoveBlock(1, size, source, dest, TRUE )
				END;
				DevHostCPL.FreeReg(source); DevHostCPL.FreeReg(dest); DevHostCPL.FreeReg(size);
			END; (* IF *)
			par := par.link;
		END; (* WHILE *)
	END CopyDynArrs;

	PROCEDURE EnterProc*( proc : DevCPT.Object );
	(* Generates code for the entry into a procedure. If ptrinit is set, the whole local variable area is initialized. *)
		VAR dest, losize, hisize, adrReg, sp, spr, applLimit : DevHostCPL.Item;
				dsize, i, sysflag : INTEGER;
				label, break : DevHostCPL.Label;
				callback: BOOLEAN;
				var, fld: DevCPT.Object;
				typ: DevCPT.Struct;
				ptrs: RECORD
						count: BYTE;
						offs: ARRAY 12 OF INTEGER;
					END;
	BEGIN (* EnterProc *)
		DevHostCPL.SetLabel(proc.adr); (* DevHostCPL.ClearHints implizit *)
		dsize := -proc.conval.intval2;
		sysflag := proc.sysflag;
		callback := 1 IN SYSTEM.VAL(SET, sysflag);
IF proc.sysflag = 3 THEN HALT(120) END;
		IF doStackCheck IN proc.conval.setval THEN
			MakeVar(stackLimit, applLimit);
			IF dsize > 256 THEN
				DevHostCPL.Enter(0);
				sp.reg := DevHostCPL.GetReg(); sp.mode := dreg;
				sp.typ := untgPtr; sp.acttyp := untgPtr;
				DevHostCPL.Move(SP, sp);
				DevHostCPL.Format6(SUBI, dsize, sp);
				DevHostCPL.Cmp(applLimit, sp);
				DevHostCPL.Trapcc(LE, stackTrap);
				DevHostCPL.Move(sp, SP)
			ELSE
				DevHostCPL.Enter(-dsize);
				DevHostCPL.Cmp(applLimit, SP);
				DevHostCPL.Trapcc(LE, stackTrap)
			END
		ELSE
			DevHostCPL.Enter(-dsize)
		END;
		IF callback THEN
			PushRegs(DevHostCPL.calleeSavedRegs);
			spr.reg := DevHostCPL.GetReg(); spr.mode := dreg;
			spr.typ := untgPtr; spr.acttyp := untgPtr;
			DevHostCPL.Move(SP, spr);
		END;
(*
		IF ~(2 IN SYSTEM.VAL(SET, sysflag)) THEN
			sp.reg := DevHostCPL.GetReg(); sp.mode := dreg;
			sp.typ := untgPtr; sp.acttyp := untgPtr;
			DevHostCPL.Move(SP, sp); DevHostCPL.Format6(SUBI, 2048, sp);
			applLimit.mode := abs; applLimit.bd := 0130H;
			applLimit.typ := untgPtr; applLimit.acttyp := untgPtr;
			DevHostCPL.Cmp(applLimit, sp);
			DevHostCPL.FreeReg(sp);
			break := DevHostCPL.NewLabel; DevHostCPL.Jump(GT, break);
			DevHostCPL.Trapcc(true, stackTrap);
			DevHostCPL.SetLabel(break);
		END;
*)
		IF ptrinit THEN
			var := proc.scope.scope; ptrs.count := 0;
			WHILE (var # NIL) & (ptrs.count < LEN(ptrs.offs)) DO
				IF var.typ.form IN {Pointer, ProcTyp} THEN
					ptrs.offs[ptrs.count] := var.adr; INC(ptrs.count);
				ELSIF (var.typ.form = Comp) & (var.typ.comp = Record) THEN
					typ := var.typ;
					WHILE (typ # NIL) & (ptrs.count < LEN(ptrs.offs)) DO
						fld := typ.link;
						WHILE (fld # NIL) & (fld.mode = Fld) & (ptrs.count < LEN(ptrs.offs)) DO
							IF (fld.typ.form IN {Pointer, ProcTyp}) OR (fld.name^ = DevCPM.HdPtrName)
								OR (fld.name^ = DevCPM.HdProcName) OR (fld.name^ = DevCPM.HdUtPtrName) THEN
								ptrs.offs[ptrs.count] := var.adr + fld.adr; INC(ptrs.count);
							ELSIF fld.typ.form = Comp THEN
								ptrs.count := LEN(ptrs.offs);
							END;
							fld := fld.link;
						END;
						typ := typ.BaseTyp;
					END;
				ELSIF (var.typ.form = Comp) & (var.typ.comp = Array) THEN
					IF ~(var.typ.BaseTyp.form IN {String8, String16, Set, LReal, Real, Int64, Int32, Int16, Int8, Char8, Char16, Bool, Byte}) THEN
						ptrs.count := LEN(ptrs.offs);
					END;
				END;
				var := var.link;
			END;
			IF ptrs.count < LEN(ptrs.offs) THEN
				dest.mode := regx;
				dest.reg := FP.reg;
				dest.typ := untgPtr; dest.acttyp := untgPtr;
				dest.inxReg := None;
				dest.offsReg := None;
				WHILE ptrs.count > 0 DO
					DEC(ptrs.count);
					dest.bd := ptrs.offs[ptrs.count];
					DevHostCPL.Clr(dest);
				END;
			ELSIF dsize > 24 THEN
				adrReg.mode := areg;
				adrReg.typ := untgPtr; adrReg.acttyp := untgPtr;
				adrReg.reg := DevHostCPL.GetAdrReg( );
				DevHostCPL.Move( SP, adrReg );
				IF callback THEN
					MakeIntConst((8+5)*4, DevCPT.int32typ, hisize);
					DevHostCPL.Format3(ADD, hisize, adrReg.reg);
					DevHostCPL.FreeReg(hisize);
				END;
				adrReg.mode := postinc;
				IF dsize > 4 * MAX( SHORTINT ) THEN
					MakeIntConst( ( dsize DIV 4 - 1 ) DIV 10000H, DevCPT.int16typ, hisize );
					DevHostCPL.Load( hisize );
					MakeIntConst( ( dsize DIV 4 - 1 ) MOD 10000H, DevCPT.int16typ, losize );
					DevHostCPL.Load( losize );
					label := DevHostCPL.NewLabel;
					DevHostCPL.SetLabel( label );
					DevHostCPL.Clr(adrReg);
					DevHostCPL.DBcc( false, losize.reg, label );
					DevHostCPL.DBcc( false, hisize.reg, label );
					DevHostCPL.FreeReg(hisize);
				ELSE
					MakeIntConst( dsize DIV 4 - 1, DevCPT.int16typ, losize );
					DevHostCPL.Load( losize );
					label := DevHostCPL.NewLabel;
					DevHostCPL.SetLabel( label );
					DevHostCPL.Clr(adrReg);
					DevHostCPL.DBcc( false, losize.reg, label );
				END; (* IF *)
				IF dsize MOD 4 # 0 THEN (* varsize in 2 Byte-Chunks alloziert *)
					adrReg.typ := DevCPT.int16typ; adrReg.acttyp := DevCPT.int16typ;
					DevHostCPL.Clr(adrReg);
				END;
				DevHostCPL.FreeReg(adrReg); DevHostCPL.FreeReg(losize);
			ELSE
				dest.mode := regx;
				dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
				dest.reg := SP.reg;
				IF callback THEN
					dest.bd := (8+5)*4;
				ELSE
					dest.bd := 0;
				END;
				dest.inxReg := None;
				dest.offsReg := None;
				FOR i := 1 TO dsize DIV 4 DO
					DevHostCPL.Clr(dest);
					INC( dest.bd, 4 );
				END; (* FOR *)
				IF dsize MOD 4 # 0 THEN
					i := dsize MOD 4;
					IF i >= 2 THEN
						dest.typ := DevCPT.int16typ; dest.acttyp := DevCPT.int16typ;
						DevHostCPL.Clr(dest);
						INC(dest.bd, 2);
					END;
					IF ODD(i) THEN
						dest.typ := DevCPT.int8typ; dest.typ := DevCPT.int8typ;
						DevHostCPL.Clr(dest);
					END;
				END;
				DevHostCPL.FreeReg(dest);
			END; (* IF *)
		END; (* IF *)
		CopyDynArrs( proc.link, doStackCheck IN proc.conval.setval );
		IF callback THEN
			MakeSPPredec(sp);
			DevHostCPL.Move(spr, sp);
			DevHostCPL.FreeReg(spr);
		END;
	END EnterProc;

	PROCEDURE Return*( proc : DevCPT.Object; withRes : BOOLEAN;  VAR result : DevHostCPL.Item );
	(* Generates code for returning from a procedure or a module (proc = NIL). 
		result contains the value that has to be returned in a reserved area on the stack, if withRes is TRUE. *)
		VAR sysflag: INTEGER; resadr, item : DevHostCPL.Item;
	BEGIN (* Return *)
		IF withRes THEN
			IF ~realLib & (proc.typ.form IN RealSet) THEN
				item.mode := regx; item.reg := FP.reg; item.bd := proc.conval.intval; item.inxReg := None;
				item.obj := NIL; item.typ := untgPtr; item.acttyp := untgPtr;
				resadr.mode := areg; resadr.reg := DevHostCPL.GetAdrReg();
				resadr.obj := NIL; resadr.typ := untgPtr; resadr.acttyp := untgPtr;
				DevHostCPL.Move(item, resadr);
				item.mode := regx; item.reg := resadr.reg; item.bd := 0; item.inxReg := None;
				IF result.mode # xreal THEN
					item.obj := NIL; item.typ := proc.typ; item.acttyp := proc.typ;
					ToXReal(result, item);
				ELSE
					item.obj := NIL; item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
					resadr.mode := regx; resadr.reg := 15; resadr.bd := 0; resadr.inxReg := None;
					resadr.obj := NIL; resadr.typ := DevCPT.int32typ; resadr.acttyp := DevCPT.int32typ;
					DevHostCPL.Move(resadr, item);
					INC(resadr.bd, 4); INC(item.bd, 4); DevHostCPL.Move(resadr, item);
					INC(resadr.bd, 4); INC(item.bd, 4); DevHostCPL.Move(resadr, item);
				END;
				DevHostCPL.FreeReg(item);
			ELSE
				item.mode := regx; item.reg := FP.reg; item.bd := proc.conval.intval; item.inxReg := None;
				item.obj := NIL; item.typ := proc.typ; item.acttyp := proc.typ;
				Assign(result, item);
				DevHostCPL.FreeReg(result);
			END;
		END; (* IF *)
		IF proc # NIL THEN
			sysflag := proc.sysflag;
			IF 1 IN SYSTEM.VAL(SET, sysflag) THEN
				MakeSPPostinc(item);
				DevHostCPL.Move(item, SP);
				PopRegs(DevHostCPL.calleeSavedRegs);
			END;
			IF withRes & ~realLib & (proc.typ.form IN RealSet) THEN
				DevHostCPL.Return(proc.conval.intval - ParOff + DevCPT.sysptrtyp.size);
			ELSE
				DevHostCPL.Return(proc.conval.intval - ParOff);
			END;
		ELSE
			DevHostCPL.Return(0);
		END;
	END Return;
	
	PROCEDURE WriteStaticLink*( obj : DevCPT.Object );
	(* Writes the static link of the given object to (A7) if necessary. *)
		VAR source, dest : DevHostCPL.Item;
				diff : SHORTINT;
	BEGIN (* WriteStaticLink *)
		IF ( obj # NIL ) & ( obj.mnolev > 0 ) & ( obj.mode = LProc ) THEN (* static link needed *)
			diff := SHORT(DevHostCPL.level - obj.mnolev);
			IF diff = 0 THEN (* local procedure *)
				source := FP;
			ELSE
				source.mode := regx;
				source.typ := untgPtr; source.acttyp := untgPtr;
				source.reg := FP.reg;
				source.bd := 8;
				source.inxReg := None;
				source.offsReg := None;
				IF diff > 1 THEN
					dest.mode := areg;
					dest.typ := untgPtr; dest.acttyp := untgPtr;
					dest.reg := DevHostCPL.GetAdrReg( );
					DevHostCPL.Move( source, dest );
					source.reg := dest.reg;
					WHILE diff > 2 DO
						DevHostCPL.Move( source, dest );
						DEC( diff );
					END; (* WHILE *)
				END; (* IF *)
			END; (* IF *)
			PushStack(source);
		END; (* IF *)
	END WriteStaticLink;

	PROCEDURE Call*( VAR item : DevHostCPL.Item; obj : DevCPT.Object );
	(* Calls the given procedure. *)
	VAR mod: DevCPT.Object;
	BEGIN (* Call *)
		IF ( obj # NIL ) & ( obj.mode = CProc ) THEN
			DevHostCPL.WriteCProc( obj^.conval^.ext );
		ELSIF item.mode = imm THEN
			HALT(44);
		ELSIF (obj # NIL) & (obj.mnolev < 0 ) & (item.mode = immL) THEN (* external proc *)
			item.mode := absL;
			IF (-obj.mnolev >= 0) & (-obj.mnolev < LEN(DevCPT.GlbMod)) THEN
				mod := DevCPT.GlbMod[-obj.mnolev];
			ELSE
				mod := NIL;
			END;
			IF (mod = NIL) OR (mod.library = NIL) THEN (* Procedure exported from Oberon *)
				item.reg := deref;
			ELSE (* Procedure from DLL *)
				item.reg := absolute;
			END;
			IF obj.mode = Var THEN (* external proc var *)
				item.reg := absolute;
				DeRef(DevCPT.sysptrtyp, item, FALSE);
			END; 
			DevHostCPL.Format15( JSR, item );
		ELSIF item.mode = immL THEN
			IF obj = NIL THEN
				obj := item.obj;
			END;
			DevHostCPL.Bsr(obj.adr);
		ELSE (* proc var OR TProc *)
			DeRef( DevCPT.sysptrtyp, item, (obj # NIL) & (obj.mode = TProc) );
			DevHostCPL.Format15( JSR, item );
		END; (* IF *)
		DevHostCPL.ClearHints;
	END Call;

	PROCEDURE GetResult*( typ : DevCPT.Struct; savedRegs: SET; VAR res : DevHostCPL.Item );
	(* Returns the result of a function call. *)
		VAR item : DevHostCPL.Item; reg: SHORTINT;
	BEGIN (* GetResult *)
		IF ~realLib & (typ.form IN RealSet) THEN
			res.mode := xreal; res.reg := DevHostCPL.xrealstack;
(*			res.mode := freg; res.typ := typ; res.acttyp := typ; res.reg := DevHostCPL.GetFReg( );
			DevHostCPL.FMove( item, res ); *)
		ELSE
			IF FALSE (*savedRegs = {}*) THEN
				res.mode := postinc; res.reg := SP.reg; res.typ := typ; res.acttyp := typ;
			ELSIF typ.size = 8 THEN	(* bh *)
				item.mode := postinc; item.reg := SP.reg; item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
				res.mode := dreg; res.typ := DevCPT.int32typ; res.acttyp := DevCPT.int32typ; res.reg := DevHostCPL.GetReg( );
				DevHostCPL.Move( item, res );
				reg := res.reg; res.reg := DevHostCPL.GetReg( );
				DevHostCPL.Move( item, res );
				res.typ := typ; res.acttyp := typ; res.inxReg := res.reg; res.reg := reg;
			ELSE
				item.mode := postinc; item.reg := SP.reg; item.typ := typ; item.acttyp := typ;
				res.mode := dreg; res.typ := typ; res.acttyp := typ; res.reg := DevHostCPL.GetReg( );
				DevHostCPL.Move( item, res );
			END;
		END; (* IF *)
		IF res.mode # dreg THEN
			res.inxReg := None; res.offsReg := None	(* necessary if next operation is Deref *)
		END
	END GetResult;

	PROCEDURE TypeTest*( VAR item : DevHostCPL.Item; typ : DevCPT.Struct; guard, equal : BOOLEAN );
	(* Generates code for a type test. If equal is true, the two types have to be equal, if guard is true, a Trap is generated
		if the test fails. If both are false, only the condition codes are set. *)
		VAR tag : DevHostCPL.Item;
				savedRegs : SET;
	BEGIN (* TypeTest *)
		savedRegs := DevHostCPL.usedRegs;
		IF ~ equal THEN
			DeRef( DevCPT.sysptrtyp, item, FALSE );
			INC( item.bd, DevHostCPL.BaseTypeOffs + 4 * typ.extlev) ;
		END; (* IF *)
		DevHostCPL.Load( item );
		StaticTag( typ, tag );
		DevHostCPL.Cmp( tag, item );
		IF equal THEN
			DevHostCPL.Trapcc( NE, recTrap);
		ELSIF guard THEN
			DevHostCPL.Trapcc( NE, typTrap);
		ELSE
			MakeCocItem( EQ, item );
		END; (* IF *)
		DevHostCPL.usedRegs := savedRegs;
	END TypeTest;

	PROCEDURE Case*( VAR expression : DevHostCPL.Item; lo, hi : INTEGER; VAR label : DevHostCPL.Label; VAR jtAdr : INTEGER );
	(* Generates the initializing part of a case statement and allocates the jump table.
		label denotes the else part of the case statement, jtAdr is the address of the jump table. *)
		VAR loItem, jumpTabEntry, jumpAddress : DevHostCPL.Item;
	BEGIN (* Case *)
		DevHostCPL.Load( expression );
		IF expression.typ.form IN ByteSet THEN Convert( expression, DevCPT.int16typ ); END;
		MakeIntConst( lo, expression.typ, loItem );
		DevHostCPL.Format2( SUB, loItem, expression );
		DevHostCPL.Format6( CMPI, hi - lo, expression );
		DevHostCPL.Jump( HI, label );
(*		DevHostCPL.AllocBytes( jumpTab, 2 * ( hi - lo + 1 ), jtAdr );
		jumpTabEntry.mode := pcx;
		jumpTabEntry.typ := DevCPT.int16typ; jumpTabEntry.acttyp := DevCPT.int16typ;
		jumpTabEntry.bd := jtAdr - DevHostCPL.ConstSize - DevHostCPL.dsize; *)
		jumpTabEntry.inxReg := expression.reg;
		IF expression.typ.size = 4 THEN
			jumpTabEntry.xsize := 1;
		ELSE
			jumpTabEntry.xsize := 0;
			Convert( expression, DevCPT.int16typ );
		END; (* IF *)
		jumpTabEntry.scale := 1; (* 2 bytes *)
		DevHostCPL.Load( jumpTabEntry );
		jumpAddress.mode := pcx;
		jumpAddress.typ := untgPtr; jumpAddress.acttyp := untgPtr;
		jumpAddress.bd := 0;
		jumpAddress.inxReg := jumpTabEntry.reg;
		jumpAddress.xsize := 0; (* word *)
		jumpAddress.scale := 1; (* *2 *)
		DevHostCPL.Format15( JMP, jumpAddress );
	END Case;

	PROCEDURE Test*( VAR item : DevHostCPL.Item );
	(* Tests a boolean item and makes a coc item. fcoc items are left unchanged. *)
	BEGIN (* Test *)
		IF ( item.mode # coc ) & ( item.mode # fcoc ) THEN
			DevHostCPL.Load( item );
			DevHostCPL.Format7( TST, item );
			MakeCocItem( NE, item );
		END; (* IF *)
	END Test;

	PROCEDURE UpTo*( VAR low, high, res : DevHostCPL.Item );
	(* set constructor res := { low .. high }. *)
		VAR chkItem, leftShift, rightShift : DevHostCPL.Item;
	BEGIN (* UpTo *)
		res.mode := dreg;
		res.typ := DevCPT.settyp; res.acttyp := DevCPT.settyp;
		res.reg := DevHostCPL.GetReg( );
		IF rangeCheck THEN
			MakeIntConst( DevCPM.MaxSet, high.typ, chkItem );
			IF low.mode # imm THEN DevHostCPL.Chk( low, chkItem ); END;
			IF high.mode # imm THEN DevHostCPL.Chk( high, chkItem ); END;
		END; (* IF *)
		rightShift.mode := dreg;
		rightShift.typ := high.typ; rightShift.acttyp := high.typ;
		rightShift.reg := DevHostCPL.GetReg( );
		leftShift.mode := dreg;
		leftShift.typ := high.typ; leftShift.acttyp := high.typ;
		leftShift.reg := DevHostCPL.GetReg( );
		DevHostCPL.Moveq( DevCPM.MaxSet, rightShift.reg );
		DevHostCPL.Format2( SUB, high, rightShift );
		DevHostCPL.Move( rightShift, leftShift );
		DevHostCPL.Format2( ADD, low, leftShift );
		DevHostCPL.Moveq( -1, res.reg );
		DevHostCPL.Format14( LSh, 1, leftShift, res );
		DevHostCPL.Format14( LSh, 0, rightShift, res );
		DevHostCPL.FreeReg( high );
		DevHostCPL.FreeReg( low );
		DevHostCPL.FreeReg( leftShift );
		DevHostCPL.FreeReg( rightShift );
	END UpTo;

	PROCEDURE Abs*( VAR item : DevHostCPL.Item );
	(* Generates code for the calculation of the absolute value of the given item. *)
		VAR dst: DevHostCPL.Item; label : DevHostCPL.Label;
	BEGIN (* Abs *)
		IF item.typ.form IN RealSet THEN
			IF item.mode # xreal THEN
				INC(DevHostCPL.xrealstack); AddToSP(-12);
				dst.mode := regx; dst.reg := 15; dst.bd := 0; dst.inxReg := None;
				dst.obj := NIL; dst.typ := DevCPT.int32typ; dst.acttyp := DevCPT.int32typ;
				ToXReal(item, dst);
				DevHostCPL.FreeReg(item);
			END;
			item.mode := regx; item.reg := 15; item.bd := 0; item.inxReg := None;
			item.obj := NIL; item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
			MakeIntConst(0000FH, DevCPT.int16typ, dst);
			PushAdrStack(item); PushStack(dst);
			DevHostCPL.FP68K;
			item.mode := xreal; item.reg := DevHostCPL.xrealstack;
(*			DevHostCPL.Format8( FABS, item, item ); *)
		ELSE
			DevHostCPL.Load( item );
			label := DevHostCPL.NewLabel;
			DevHostCPL.Format7( TST, item );
			DevHostCPL.Jump( GE, label );
			DevHostCPL.Format7( NEG, item );
			DevHostCPL.SetLabel( label );
		END; (* IF *)
	END Abs;

	PROCEDURE Adr*( VAR item : DevHostCPL.Item );
	(* Generates code for the calculation of the address of the given item. *)
	VAR
		reg : SHORTINT;
		adr : DevHostCPL.Item;
	BEGIN (* Adr *)
		IF item.typ.comp = DynArr THEN
			GetDynArrAdr( item, adr );
			item := adr;
		ELSIF item.mode IN { regx, pcx } THEN
			reg := DevHostCPL.GetAdrReg( );
			DevHostCPL.Lea( item, reg );
			item.mode := areg;
			item.reg := reg;
		ELSIF item.mode = absL THEN
			item.mode := immL;
		ELSIF item.mode = immL THEN
			(* do nothing *)
		ELSE
			HALT( 94 );
		END; (* IF *)
		item.typ := untgPtr; item.acttyp := untgPtr;
	END Adr;

	PROCEDURE Cap*( VAR item : DevHostCPL.Item );
	(* Generates code for the calculation of CAP( item ). For characters only. *)
	BEGIN (* Cap *)
		DevHostCPL.Load( item );
		DevHostCPL.Format4( BCLR, 5, item );
	END Cap;

	PROCEDURE Neg*( VAR item : DevHostCPL.Item );
	(* Generates code for the calculation of -item. *)
	VAR dst: DevHostCPL.Item;
	BEGIN (* Neg *)
		IF item.typ.form IN RealSet THEN
			IF item.mode # xreal THEN
				INC(DevHostCPL.xrealstack); AddToSP(-12);
				dst.mode := regx; dst.reg := 15; dst.bd := 0; dst.inxReg := None;
				dst.obj := NIL; dst.typ := DevCPT.int32typ; dst.acttyp := DevCPT.int32typ;
				ToXReal(item, dst);
				DevHostCPL.FreeReg(item);
			END;
			item.mode := regx; item.reg := 15; item.bd := 0; item.inxReg := None;
			item.obj := NIL; item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
			MakeIntConst(0000DH, DevCPT.int16typ, dst);
			PushAdrStack(item); PushStack(dst);
			DevHostCPL.FP68K;
			item.mode := xreal; item.reg := DevHostCPL.xrealstack;
(*			DevHostCPL.Format8( FNEG, item, item ); *)
		ELSIF item.typ.form = Set THEN
			DevHostCPL.Load( item );
			DevHostCPL.Format7( NOT, item );
		ELSE
			DevHostCPL.Load( item );
			DevHostCPL.Format7( NEG, item );
		END; (* IF *)
	END Neg;

	PROCEDURE Not*( VAR item : DevHostCPL.Item );
	(* Generates code for the calculation of ~ item. For Booleans only. *)
		VAR tcond, fcond : INTEGER;
	BEGIN (* Not *)
		IF ( item.mode = coc ) OR ( item.mode = fcoc ) THEN
			tcond := item.bd DIV 10000H;
			fcond := item.bd MOD 10000H;
			item.bd := 10000H * fcond + tcond;
		ELSE
			DevHostCPL.Load( item );
			DevHostCPL.Format7( TST, item );
			MakeCocItem( EQ, item );
		END; (* IF *)
	END Not;

	PROCEDURE Odd*( VAR item : DevHostCPL.Item );
	(* Generates code for the calculation of ODD( item ). *)
	VAR temp: DevHostCPL.Item;
	BEGIN (* Odd *)
		IF item.typ.form =  Int64 THEN
			IF item.mode = xreal THEN
				IF item.reg # DevHostCPL.xrealstack THEN DevCPM.err(-811); END;
				AddToSP(-DevCPT.int64typ.size);
				item.mode := regx; item.reg  := 15; item.bd := DevCPT.int64typ.size; item.inxReg := None;
				item.obj := NIL; item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
				temp.mode := regx; temp.reg  := 15; temp.bd := 0; temp.inxReg := None;
				temp.obj := NIL; temp.typ := DevCPT.int64typ; temp.acttyp := DevCPT.int64typ;
				RoundDown;
				FromXReal(item, temp);
				RoundNearest;
				item.mode := regx; item.reg  := 15; item.bd := 4; item.inxReg := None;
				item.obj := NIL; item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
				DevHostCPL.Load(item);
				DevHostCPL.Format4(BTST, 0, item);
				MakeCocItem(NE, item);
				AddToSP(12+DevCPT.int64typ.size); DEC(DevHostCPL.xrealstack);
			ELSE
				IF item.mode IN {dreg, regx, abs, absL}  THEN
					item.typ := DevCPT.int32typ;
					IF item.mode = dreg THEN DevHostCPL.Free(item); item.reg := item.inxReg
					ELSE INC(item.bd, 4); DevHostCPL.Load(item)
					END;
					DevHostCPL.Format4(BTST, 0, item);
					MakeCocItem(NE, item);
				ELSE
					HALT(0);
				END;
			END;
		ELSE
			DevHostCPL.Load( item );
			DevHostCPL.Format4( BTST, 0, item );
			MakeCocItem( NE, item );
		END;
	END Odd;

	PROCEDURE LoadCC*( VAR item : DevHostCPL.Item );
	(* If item.mode is coc or fcoc, the item is loaded into a data register. *)
		VAR temp : DevHostCPL.Item;
	BEGIN (* LoadCC *)
		IF item.mode IN { coc, fcoc } THEN
			temp := item;
			item.mode := dreg;
			item.typ := DevCPT.booltyp; item.acttyp := DevCPT.booltyp;
			item.reg := DevHostCPL.GetReg( );
			Assign( temp, item );
		END; (* IF *)
	END LoadCC;

	PROCEDURE RealDop(op: SHORTINT; swap: BOOLEAN; VAR source, dest: DevHostCPL.Item);
	VAR d, s, t: DevHostCPL.Item;
	BEGIN
		dest.typ := dest.acttyp; source.typ := source.acttyp;
		IF (dest.mode = xreal) & (source.mode = xreal) THEN
			IF ~((source.reg = DevHostCPL.xrealstack) & (dest.reg = DevHostCPL.xrealstack-1) OR
				(source.reg = DevHostCPL.xrealstack-1) & (dest.reg = DevHostCPL.xrealstack)) THEN DevCPM.err(-812) END;
			IF (source.reg = DevHostCPL.xrealstack) OR swap THEN
				d.mode := regx; d.reg := 15; d.bd := 12; d.inxReg := -1;
				d.obj := NIL; d.typ := DevCPT.int32typ; d.acttyp := DevCPT.int32typ;
				s.mode := regx; s.reg := 15; s.bd := 0; s.inxReg := -1;
				s.obj := NIL; s.typ := DevCPT.int32typ; s.acttyp := DevCPT.int32typ;
			ELSE
				INC(DevHostCPL.xrealstack); AddToSP(-12);
				d.mode := regx; d.reg := 15; d.bd := 0; d.inxReg := None;
				d.obj := NIL; d.typ := DevCPT.int32typ; d.acttyp := DevCPT.int32typ;
				s := d; s.bd := 24; DevHostCPL.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevHostCPL.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevHostCPL.Move(s, d);
				d.bd := 24; s.bd := 12; DevHostCPL.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevHostCPL.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevHostCPL.Move(s, d);
				d.bd := 12; s.bd := 0; DevHostCPL.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevHostCPL.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevHostCPL.Move(s, d);
				AddToSP(12); DEC(DevHostCPL.xrealstack);
				d.bd := 12; s.bd := 0;
			END;
			dest.reg := SHORT(DevHostCPL.xrealstack-1); source.reg := DevHostCPL.xrealstack;
		ELSIF dest.mode = xreal THEN
			d.mode := regx; d.reg := 15; d.bd := 0; d.inxReg := -1;
			d.obj := NIL; d.typ := DevCPT.int32typ; d.acttyp := DevCPT.int32typ;
			s := source;
		ELSIF source.mode = xreal THEN
			IF swap THEN
				t := source; source := dest; dest := t;
				d.mode := regx; d.reg := 15; d.bd := 0; d.inxReg := -1;
				d.obj := NIL; d.typ := DevCPT.int32typ; d.acttyp := DevCPT.int32typ;
				s := source;
			ELSE
				INC(DevHostCPL.xrealstack); AddToSP(-12);
				d.mode := regx; d.reg := 15; d.bd := 0; d.inxReg := None;
				d.obj := NIL; d.typ := DevCPT.int32typ; d.acttyp := DevCPT.int32typ;
				s := d; s.bd := 12; DevHostCPL.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevHostCPL.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevHostCPL.Move(s, d);
				d.bd := 12; s.bd := 0;
				ToXReal(dest, d);
				DevHostCPL.FreeReg(dest);
				dest.mode := xreal; dest.reg := SHORT(DevHostCPL.xrealstack - 1);
				dest.typ := NIL; dest.acttyp := NIL;
				source.reg := DevHostCPL.xrealstack;
			END;
		ELSE
			INC(DevHostCPL.xrealstack); AddToSP(-12);
			d.mode := regx; d.reg := 15; d.bd := 0; d.inxReg := -1; d.obj := NIL;
			d.obj := NIL; d.typ := DevCPT.int32typ; d.acttyp := DevCPT.int32typ;
			ToXReal(dest, d);
			DevHostCPL.FreeReg(dest);
			dest.mode := xreal; dest.reg := DevHostCPL.xrealstack;
			dest.typ := NIL; dest.acttyp := NIL;
			s := source;
		END;
		IF source.mode IN {dreg, areg, imm, freg} THEN
			INC(DevHostCPL.xrealstack); AddToSP(-12); INC(d.bd, 12);
			s.mode := regx; s.reg := 15; s.bd := 0; s.inxReg := None;
			s.obj := NIL; s.typ := DevCPT.int32typ; s.acttyp := DevCPT.int32typ;
			ToXReal(source, s);
			DevHostCPL.FreeReg(source);
			source.mode := xreal; source.reg := DevHostCPL.xrealstack;
		END;
		IF source.mode = xreal THEN
			MakeIntConst(op, DevCPT.int16typ, t);
		ELSE
			MakeIntConst(XRealForm(source.typ.form, source.acttyp.size) + op, DevCPT.int16typ, t);
		END;
		PushAdrStack(s); IF d.reg = 15 THEN INC(d.bd, 4) END;
		PushAdrStack(d); PushStack(t);
		DevHostCPL.FP68K;
	END RealDop;
	
	PROCEDURE Compare*( kind : BYTE; VAR left, right, res : DevHostCPL.Item );
	(* Compares left and right and generates a coc- or fcoc-item. *)
		VAR tCond : SHORTINT;
				lbuf, rbuf, alias : DevHostCPL.Item;
				begLabel, endLabel : DevHostCPL.Label;
				lefttyp, righttyp, leftacttyp, rightacttyp: DevCPT.Struct;
	BEGIN (* Compare *)
		IF (left.mode = xreal) OR (left.typ.form IN RealSet) THEN
			RealDop(8, (kind = eql) OR (kind = neq), right, left);
			CASE kind OF
				eql : tCond := EQ;
				| neq : tCond := NE;
				| lss : tCond := LT;
				| leq : tCond := LE;
				| gtr : tCond := GT;
				| geq : tCond := GE;
			END; (* CASE *)
			MakeCocItem( tCond, res );
			IF (left.mode # xreal) OR (left.reg # DevHostCPL.xrealstack) THEN DevCPM.err(-813) END;
			AddToSP(12); DEC(DevHostCPL.xrealstack); left.mode := None;
(*			DevHostCPL.Format8( FCMP, right, left );
			CASE kind OF
				eql : tCond := FEQ;
				| neq : tCond := FNE;
				| lss : tCond := FLT;
				| leq : tCond := FLE;
				| gtr : tCond := FGT;
				| geq : tCond := FGE;
			END; (* CASE *)
			MakeFCocItem( tCond, res );*)
		ELSE	(* must handle strings !!! *)
			(* Oh well, more dancing on the hands required. How about dancing on the bald spot? *)
			IF ( left.typ.comp IN { Array, DynArr } ) OR ( left.typ.form IN {String8, String16} ) THEN
				leftacttyp := left.acttyp;
				IF (leftacttyp.form = String16) OR ((leftacttyp.comp IN {Array, DynArr}) & (leftacttyp.BaseTyp.form = Char16)) THEN
					lefttyp := DevCPT.char16typ;
				ELSE
					lefttyp := DevCPT.char8typ;
				END;
				IF leftacttyp.comp = DynArr THEN
					GetDynArrVal( left, leftacttyp.untagged );
				END;
				MakePostInc( lefttyp, left );
				rightacttyp := right.acttyp;
				IF (rightacttyp.form = String16) OR ((rightacttyp.comp IN {Array, DynArr}) & (rightacttyp.BaseTyp.form = Char16)) THEN
					righttyp := DevCPT.char16typ;
				ELSE
					righttyp := DevCPT.char8typ;
				END;
				IF rightacttyp.comp = DynArr THEN
					GetDynArrVal( right, rightacttyp.untagged );
				END;
				MakePostInc( righttyp, right );
				lbuf.mode := dreg; lbuf.typ := lefttyp; lbuf.acttyp := lefttyp;
				lbuf.reg := DevHostCPL.GetReg( );
				rbuf.mode := dreg; rbuf.typ := lefttyp; rbuf.acttyp := lefttyp;
				rbuf.reg := DevHostCPL.GetReg( );
				begLabel := DevHostCPL.NewLabel;
				endLabel := DevHostCPL.NewLabel;
				IF lefttyp = righttyp THEN
					DevHostCPL.SetLabel( begLabel );
					DevHostCPL.Move( left, lbuf );
					DevHostCPL.Move( right, rbuf );
					IF leftacttyp = shortString16typ THEN
						DevHostCPL.Format6(ANDI, 0FFH, lbuf);
					END;
					IF rightacttyp = shortString16typ THEN
						DevHostCPL.Format6(ANDI, 0FFH, rbuf);
					END;
					DevHostCPL.Cmp( rbuf, lbuf );
					DevHostCPL.Jump( NE, endLabel );
					DevHostCPL.Format7( TST, lbuf );
					DevHostCPL.Jump( NE, begLabel );
					DevHostCPL.SetLabel( endLabel );
					DevHostCPL.Cmp( rbuf, lbuf );
				ELSE
					alias.mode := dreg; alias.bd := 0; alias.inxReg := None;
					alias.obj := NIL; alias.typ := DevCPT.char16typ; alias.acttyp := DevCPT.char16typ;
					IF lefttyp = DevCPT.char8typ THEN
						alias.reg := lbuf.reg;
					ELSE
						alias.reg := rbuf.reg
					END;
					DevHostCPL.Clr(alias);
					DevHostCPL.SetLabel(begLabel);
					DevHostCPL.Move(left, lbuf);
					DevHostCPL.Move(right, rbuf);
					IF lefttyp = DevCPT.char8typ THEN
						DevHostCPL.Cmp(rbuf, alias);
					ELSE
						DevHostCPL.Cmp(alias, lbuf);
					END;
					DevHostCPL.Jump(NE, endLabel);
					DevHostCPL.Format7(TST, alias);
					DevHostCPL.Jump(NE, begLabel);
					DevHostCPL.SetLabel(endLabel);
					IF lefttyp = DevCPT.char8typ THEN
						DevHostCPL.Cmp(rbuf, alias);
					ELSE
						DevHostCPL.Cmp(alias, lbuf);
					END;
				END;
				DevHostCPL.Free(lbuf); DevHostCPL.Free(rbuf);
			ELSE
				IF right.typ = DevCPT.niltyp THEN Convert( right, DevCPT.sysptrtyp ); END;
				LoadCC( left );
				LoadCC( right );
				DevHostCPL.Cmp( right, left );
			END; (* IF *)
			IF ( left.typ.form IN {Char8, Char16} ) OR ( left.typ.comp IN { Array, DynArr } ) OR ( left.typ.form IN {String8, String16} ) THEN
				CASE kind OF
					eql : tCond := EQ;
					| neq : tCond := NE;
					| lss : tCond := CS;
					| leq : tCond := LS;
					| gtr : tCond := HI;
					| geq : tCond := CC;
				END; (* CASE *)
			ELSE
				CASE kind OF
					eql : tCond := EQ;
					| neq : tCond := NE;
					| lss : tCond := LT;
					| leq : tCond := LE;
					| gtr : tCond := GT;
					| geq : tCond := GE;
				END; (* CASE *)
			END; (* IF *)
			MakeCocItem( tCond, res );
		END; (* IF *)
	END Compare; 

	PROCEDURE MinMax*(VAR item, res: DevHostCPL.Item; min: BOOLEAN);
	VAR
		cmp, keep, top: DevHostCPL.Item;
		label: DevHostCPL.Label;
	BEGIN
		IF res.typ.form IN RealSet THEN
			keep.mode := regx; keep.reg := 15; keep.bd := 0; keep.inxReg := None;
			keep.obj := NIL; keep.typ := DevCPT.int32typ; keep.acttyp := DevCPT.int32typ;
			top := keep;
			IF (res.mode = xreal) & (item.mode = xreal) THEN
				ASSERT(((res.reg = DevHostCPL.xrealstack) & (item.reg = DevHostCPL.xrealstack-1)) OR ((res.reg = DevHostCPL.xrealstack-1) & (item.reg = DevHostCPL.xrealstack)), 20);
			ELSIF res.mode = xreal THEN
				ASSERT(res.reg = DevHostCPL.xrealstack, 21);
				INC(DevHostCPL.xrealstack); AddToSP(-12);
				ToXReal(item, top);
				DevHostCPL.FreeReg(item);
				item.mode := xreal; item.reg := DevHostCPL.xrealstack;
				item.typ := NIL; item.acttyp := NIL; item.obj := NIL;
			ELSIF item.mode = xreal THEN
				ASSERT(item.reg = DevHostCPL.xrealstack, 22);
				INC(DevHostCPL.xrealstack); AddToSP(-12);
				ToXReal(res, top);
				DevHostCPL.FreeReg(res);
				res.mode := xreal; res.reg := DevHostCPL.xrealstack;
				res.typ := NIL; res.acttyp := NIL; res.obj := NIL;
			ELSE (* neither item is on the xrealstack *)
				INC(DevHostCPL.xrealstack); AddToSP(-12);
				ToXReal(res, keep);
				DevHostCPL.FreeReg(res);
				res.mode := xreal; res.reg := DevHostCPL.xrealstack;
				res.typ := NIL; res.acttyp := NIL; res.obj := NIL;
				INC(DevHostCPL.xrealstack); AddToSP(-12);
				ToXReal(item, top);
				DevHostCPL.FreeReg(item);
				item.mode := xreal; item.reg := DevHostCPL.xrealstack;
				item.typ := NIL; item.acttyp := NIL; item.obj := NIL;
			END;
			RealDop(8, FALSE, item, res);
			label := DevHostCPL.NewLabel;
			IF min THEN
				MakeCocItem(GE, cmp);
			ELSE
				MakeCocItem(LE, cmp);
			END;
			cmp.tJump := DevHostCPL.NewLabel;
			FalseJump(cmp, label);
			keep.bd := 12;
			DevHostCPL.Move(top, keep); INC(top.bd, 4); INC(keep.bd, 4);
			DevHostCPL.Move(top, keep); INC(top.bd, 4); INC(keep.bd, 4);
			DevHostCPL.Move(top, keep);
			DevHostCPL.SetLabel(label);
			AddToSP(12); DEC(DevHostCPL.xrealstack);
			item.mode := None;
		ELSE
			DevHostCPL.Load(res);
			IF min THEN
				Compare(gtr, res, item, cmp);
			ELSE
				Compare(leq, res, item, cmp);
			END;
			label := DevHostCPL.NewLabel;
			cmp.tJump := DevHostCPL.NewLabel;
			FalseJump(cmp, label);
			DevHostCPL.Move(item, res);
			DevHostCPL.SetLabel(label);
		END;
	END MinMax;
	
	PROCEDURE Plus*( typ : DevCPT.Struct; VAR source, dest : DevHostCPL.Item );
	(* Generates code for the addition dest := dest + source. *)
	BEGIN (* Plus *)
		IF typ.form IN RealSet THEN
			RealDop(0, TRUE, source, dest);
		ELSE
			DevHostCPL.AssertDestReg( typ, source, dest );
			IF typ.form = Set THEN
				DevHostCPL.Format2( oR, source, dest );
(*			ELSIF typ.form IN RealSet THEN
				DevHostCPL.Format8( FADD, source, dest );*)
			ELSE
				IF source.mode = imm THEN
					IF (source.bd > 0) & (source.bd <= 8) THEN
						DevHostCPL.Format1(ADDQ, SHORT(source.bd), dest);
					ELSIF (source.bd < 0) & (source.bd >= -8) THEN
						DevHostCPL.Format1(SUBQ, SHORT(-source.bd), dest);
					ELSIF source.bd # 0 THEN
						DevHostCPL.Format6( ADDI, source.bd, dest);
					END;
				ELSE
					DevHostCPL.Format2( ADD, source, dest );
				END;
			END; (* IF *)
		END;
	END Plus;

	PROCEDURE Minus*( typ : DevCPT.Struct; VAR source, dest : DevHostCPL.Item );
	(* Generates code for the subtraktion dest := dest - source. *)
	BEGIN (* Minus *)
		IF typ.form IN RealSet THEN
			RealDop(2, FALSE, source, dest);
		ELSE
			IF typ.form = Set THEN
				DevHostCPL.Load( dest );
				DevHostCPL.Load( source );
				DevHostCPL.Format7( NOT, source );
				DevHostCPL.Format2( AND, source, dest );
(*			ELSIF typ.form IN RealSet THEN
				DevHostCPL.FLoad( dest );
				DevHostCPL.Format8( FSUB, source, dest );*)
			ELSE
				DevHostCPL.Load( dest );
				IF source.mode = imm THEN
					IF (source.bd > 0) & (source.bd <= 8) THEN
						DevHostCPL.Format1(SUBQ, SHORT(source.bd), dest);
					ELSIF (source.bd < 0) & (source.bd >= -8) THEN
						DevHostCPL.Format1(ADDQ, SHORT(-source.bd), dest);
					ELSIF source.bd # 0 THEN
						DevHostCPL.Format6( SUBI, source.bd, dest);
					END;
				ELSE
					DevHostCPL.Format2( SUB, source, dest );
				END;
			END; (* IF *)
		END;
(*		DevHostCPL.FreeReg( source ); *)
	END Minus;

	PROCEDURE Mul*( typ : DevCPT.Struct; VAR source, dest : DevHostCPL.Item );
	(* Generates code for the multiplication dest := dest * source. *)
	VAR d, s, t: DevHostCPL.Item;
	BEGIN (* Mul *)
		IF typ.form IN RealSet THEN
			RealDop(4, TRUE, source, dest);
		ELSE
			DevHostCPL.AssertDestReg( typ, source, dest );
			IF typ.form = Set THEN
				DevHostCPL.Format2( AND, source, dest );
(*			ELSIF typ.form IN RealSet THEN
				DevHostCPL.Format8( FMUL, source, dest );*)
			ELSIF typ.form = Int8 THEN
				Convert( source, DevCPT.int16typ );
				Convert( dest, DevCPT.int16typ );
				DevHostCPL.Format11( MULS, source, dest );
			ELSIF typ.form = Int16 THEN
				DevHostCPL.Format11( MULS, source, dest );
			ELSIF typ.form = Int32 THEN
				Convert( source, DevCPT.int32typ );
				Convert( dest, DevCPT.int32typ );
				DevHostCPL.Format12( MULS, source, dest );
			END; (* IF *)
		END;
	END Mul;

	PROCEDURE Divide*( typ : DevCPT.Struct; VAR source, dest : DevHostCPL.Item );
	(* Generates code for the division dest := dest / source. *)
	BEGIN (* Divide *)
		IF typ.form IN RealSet THEN
			RealDop(6, FALSE, source, dest);
		ELSE
			IF typ.form = Set THEN
				DevHostCPL.Load( dest );
				DevHostCPL.Eor( source, dest );
(*			ELSE
				DevHostCPL.Format8( FDIV, source, dest );*)
			END; (* IF *)
		END;
	END Divide;

	PROCEDURE Div*( VAR source, dest : DevHostCPL.Item );
	(* Generates code for the integer division dest := dest DIV source. *)
		VAR corr, end : DevHostCPL.Label;
				remainder, dst, item: DevHostCPL.Item;
	BEGIN (* Div *)
		IF ~(dest.typ.form IN RealSet) THEN
			DevHostCPL.Load( dest );
			Convert( dest, DevCPT.int32typ );
			corr := DevHostCPL.NewLabel;
			end := DevHostCPL.NewLabel;
			IF source.typ.form = Int32 THEN
				remainder.mode := dreg;
				remainder.reg := DevHostCPL.GetReg( );
				remainder.typ := DevCPT.int32typ; remainder.acttyp := DevCPT.int32typ;
				DevHostCPL.Move(source, remainder);
				DevHostCPL.Eor(dest, remainder);
				DevHostCPL.Format4(BTST, 31, remainder);
				DevHostCPL.Jump(NE, corr);
				DevHostCPL.Divsl( source, remainder, dest );
				DevHostCPL.Jump(true, end);
				DevHostCPL.SetLabel(corr);
				DevHostCPL.Divsl( source, remainder, dest );
				DevHostCPL.Test(remainder);
				DevHostCPL.Jump(EQ, end);
				DevHostCPL.Format1( SUBQ, 1, dest );
				DevHostCPL.SetLabel(end);
				DevHostCPL.FreeReg(remainder);
			ELSE
				Convert( source, DevCPT.int16typ );
				remainder.mode := dreg;
				remainder.reg := DevHostCPL.GetReg( );
				remainder.typ := DevCPT.int16typ; remainder.acttyp := DevCPT.int16typ;
				DevHostCPL.Move(source, remainder);
				Convert(remainder, DevCPT.int32typ);
				DevHostCPL.Eor(dest, remainder);
				DevHostCPL.Format4(BTST, 31, remainder);
				DevHostCPL.Jump(NE, corr);
				DevHostCPL.Format11( DIVS, source, dest );
				DevHostCPL.Jump(true, end);
				DevHostCPL.SetLabel(corr);
				DevHostCPL.Format11( DIVS, source, dest );
				DevHostCPL.Move(dest, remainder);
				DevHostCPL.Swap(remainder);
				remainder.typ := DevCPT.int16typ; remainder.acttyp := DevCPT.int16typ;
				DevHostCPL.Test(remainder);
				DevHostCPL.Jump(EQ, end);
				DevHostCPL.Format1( SUBQ, 1, dest );
				DevHostCPL.SetLabel(end);
				DevHostCPL.FreeReg(remainder);
			END; (* IF *)
		ELSE
			RealDop(6, FALSE, source, dest);
			RoundDown;
			dst.mode := regx; dst.reg := 15; dst.bd := (DevHostCPL.xrealstack - dest.reg) * 12; dst.inxReg := None;
			dst.obj := NIL; dst.typ := DevCPT.int32typ; dst.acttyp := DevCPT.int32typ;
			MakeIntConst(00014H, DevCPT.int16typ, item);
			PushAdrStack(dst); PushStack(item);
			DevHostCPL.FP68K;
			RoundNearest;
		END;
	END Div;

	PROCEDURE Mod*( VAR source, dest : DevHostCPL.Item );
	(* Generates code for the remainder dest := dest MOD source.*)
		VAR corr, end : DevHostCPL.Label;
				remainder, zero, src, shift : DevHostCPL.Item;
	BEGIN (* Mod *)
		IF ~(dest.typ.form IN RealSet) THEN
			DevHostCPL.Load( source ); (* because it is used twice and may be a pc-item. *)
			DevHostCPL.Load( dest );
			Convert( dest, DevCPT.int32typ );
			corr := DevHostCPL.NewLabel;
			end := DevHostCPL.NewLabel;
			IF source.typ.form = Int32 THEN
				remainder.mode := dreg;
				remainder.reg := DevHostCPL.GetReg( );
				remainder.typ := DevCPT.int32typ; remainder.acttyp := DevCPT.int32typ;
				DevHostCPL.Move(source, remainder);
				DevHostCPL.Eor(dest, remainder);
				DevHostCPL.Format4(BTST, 31, remainder);
				DevHostCPL.Jump(NE, corr);
				DevHostCPL.Divsl( source, remainder, dest );
				DevHostCPL.Jump(true, end);
				DevHostCPL.SetLabel(corr);
				DevHostCPL.Divsl( source, remainder, dest );
				DevHostCPL.Test(remainder);
				DevHostCPL.Jump(EQ, end);
				DevHostCPL.Format2( ADD, source, remainder );
				DevHostCPL.SetLabel(end);
				DevHostCPL.FreeReg(dest);
				dest := remainder;
			ELSE
				Convert( source, DevCPT.int16typ );
				remainder.mode := dreg;
				remainder.reg := DevHostCPL.GetReg( );
				remainder.typ := DevCPT.int16typ; remainder.acttyp := DevCPT.int16typ;
				DevHostCPL.Move(source, remainder);
				Convert(remainder, DevCPT.int32typ);
				DevHostCPL.Eor(dest, remainder);
				DevHostCPL.Format4(BTST, 31, remainder);
				DevHostCPL.FreeReg(remainder);
				DevHostCPL.Jump(NE, corr);
				DevHostCPL.Format11( DIVS, source, dest );
				DevHostCPL.Swap( dest );
				DevHostCPL.Jump(true, end);
				DevHostCPL.SetLabel(corr);
				DevHostCPL.Format11( DIVS, source, dest );
				DevHostCPL.Swap( dest );
				remainder := dest;
				remainder.typ := DevCPT.int16typ; remainder.acttyp := DevCPT.int16typ;
				DevHostCPL.Test(remainder);
				DevHostCPL.Jump(EQ, end);
				DevHostCPL.Format2( ADD, source, dest );
				DevHostCPL.SetLabel(end);
			END; (* IF *)
			DevHostCPL.FreeReg( source );
		ELSE
			RealDop(12, FALSE, source, dest);
			end := DevHostCPL.NewLabel;
			remainder.mode := regx; remainder.reg := 15; remainder.bd := (DevHostCPL.xrealstack - dest.reg) * 12;
			remainder.inxReg := None; remainder.typ := DevCPT.int32typ; remainder.acttyp := DevCPT.int32typ;
			zero.mode := dreg; zero.reg := DevHostCPL.GetReg(); zero.bd := 0;
			zero.inxReg := None; zero.typ := DevCPT.int32typ; zero.acttyp := DevCPT.int32typ;
			DevHostCPL.Move(remainder, zero);
			DevHostCPL.Format6(ANDI, 07FFFFFFFH, zero);
			INC(remainder.bd, 4);
			DevHostCPL.Format2(oR, remainder, zero);
			INC(remainder.bd, 4); remainder.typ := DevCPT.int16typ; remainder.acttyp := DevCPT.int16typ;
			zero.typ := DevCPT.int16typ; zero.typ := DevCPT.int16typ;
			DevHostCPL.Format2(oR, remainder, zero);
			zero.typ := DevCPT.int32typ; zero.typ := DevCPT.int32typ;
			DevHostCPL.Test(zero);
			DevHostCPL.FreeReg(zero);
			DevHostCPL.Jump(EQ, end);
			remainder.mode := regx; remainder.reg := 15; remainder.bd := (DevHostCPL.xrealstack - dest.reg) * 12;
			remainder.inxReg := None; remainder.typ := DevCPT.int8typ; remainder.acttyp := DevCPT.int8typ;
			DevHostCPL.Load(remainder);
			IF source.mode = xreal THEN
				src.mode := regx; src.reg := 15; src.bd := (DevHostCPL.xrealstack - source.reg) * 12;
				src.inxReg := None; src.typ := DevCPT.int8typ; src.acttyp := DevCPT.int8typ;
			ELSIF source.acttyp.form = Int64 THEN
				src := source;
				src.typ := DevCPT.int8typ; src.acttyp := DevCPT.int8typ;
				DevHostCPL.Load(src);
			ELSE
				source.typ := source.acttyp;
				src := source;
				(* The following code has been replaced by a DevHostCPL.Load, as source will never be in a register.
				 Otherwise source would have been converted to xreal in RealDop:
				src.mode := dreg; src.reg := DevHostCPL.GetReg();
				DevHostCPL.Move(source, src); *)
				DevHostCPL.Load(src);
				IF source.typ.size = 4 THEN
					MakeIntConst(24, DevCPT.int16typ, shift);
					DevHostCPL.Format14(LSh, 0, shift, src);
				ELSIF source.typ.size = 2 THEN
					MakeIntConst(8, DevCPT.int16typ, shift);
					DevHostCPL.Format14(LSh, 0, shift, src);
				ELSIF source.typ.size # 1 THEN
					HALT(0);
				END;
			END;
			DevHostCPL.Eor(src, remainder);
			DevHostCPL.FreeReg(src);
			DevHostCPL.Format4(BTST, 7, remainder);
			DevHostCPL.Jump(EQ, end);
			RealDop(0, TRUE, source, dest);
			DevHostCPL.SetLabel(end);
		END;
	END Mod;

	PROCEDURE Mask*( mask : INTEGER; VAR dest : DevHostCPL.Item );
	(* Generates code for the calculation of dest := dest & ~mask. Used for MOD. *)
	BEGIN (* Mask *)
		DevHostCPL.Load( dest );
		DevHostCPL.Format6( ANDI, mask, dest );
	END Mask;

	PROCEDURE In*( VAR element, set, dest : DevHostCPL.Item );
	(* Generates code for the calculation of dest := element IN set. *)
	BEGIN (* In *)
		IF element.mode = imm THEN
			DevHostCPL.Format4( BTST, element.bd, set );
		ELSE
			DevHostCPL.Format5( BTST, element, set );
		END; (* IF *)
		MakeCocItem( NE, dest );
	END In;

	PROCEDURE Shift*( opcode : SHORTINT; VAR shift, dest : DevHostCPL.Item );
	(* Generates code for the calculation of ASH( dest, shift ), SYSTEM.LSH( dest, shift ) and SYSTEM.ROT( dest, shift ). *)
		VAR elseLabel, endLabel : DevHostCPL.Label;
	BEGIN (* Shift *)
		IF shift.mode = imm THEN
			IF shift.bd # 0 THEN
				IF ( shift.bd >= -8 ) & ( shift.bd <= 8 ) THEN
					DevHostCPL.Format13( opcode, SHORT( shift.bd ), dest );
				ELSE
					IF shift.bd < 0 THEN
						MakeIntConst( -shift.bd, DevCPT.int16typ, shift );
						DevHostCPL.Format14( opcode, 0, shift, dest );
					ELSE
						MakeIntConst( shift.bd, DevCPT.int16typ, shift );
						DevHostCPL.Format14( opcode, 1, shift, dest );
					END; (* IF *)
				END; (* IF *)
			END; (* IF *)
		ELSE (* shift must be tested, because the machine instructions only take positive shifts. *)
			elseLabel := DevHostCPL.NewLabel;
			endLabel := DevHostCPL.NewLabel;
			DevHostCPL.Load( shift );
			DevHostCPL.Load( dest );
			DevHostCPL.Format7( TST, shift );
			DevHostCPL.Jump( LT, elseLabel );
			DevHostCPL.Format14( opcode, 1, shift, dest );
			DevHostCPL.Jump( true, endLabel );
			DevHostCPL.SetLabel( elseLabel );
			DevHostCPL.Format7( NEG, shift );
			DevHostCPL.Format14( opcode, 0, shift, dest );
			DevHostCPL.SetLabel( endLabel );
		END; (* IF *)
	END Shift;

	PROCEDURE Trap*( nr : SHORTINT );
	(* Generates code for a trap. *)
	BEGIN (* Trap *)
		DevHostCPL.Trapcc( true, nr );
	END Trap;

	PROCEDURE SYSMove*( VAR sourceAdr, destAdr, length : DevHostCPL.Item );
	(* Generates code for SYSTEM.MOVE( sourceAdr, destAdr, length ). *)
		VAR source, dest : DevHostCPL.Item;
	BEGIN (* SYSMove *)
		source.mode := areg;
		source.typ := DevCPT.int32typ; source.acttyp := DevCPT.int32typ;
		source.reg := DevHostCPL.GetAdrReg( );
		Convert( sourceAdr, DevCPT.int32typ );
		DevHostCPL.Move( sourceAdr, source );
		source.mode := postinc;
		source.typ := DevCPT.int8typ; source.acttyp := DevCPT.int8typ;
		dest.mode := areg;
		dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
		dest.reg := DevHostCPL.GetAdrReg( );
		Convert( destAdr, DevCPT.int32typ );
		DevHostCPL.Move( destAdr, dest );
		dest.mode := postinc;
		dest.typ := DevCPT.int8typ; dest.acttyp := DevCPT.int8typ;
		MoveBlock( 1, length, source, dest, FALSE );
	END SYSMove;

	PROCEDURE SYSGet*( VAR adr, dest : DevHostCPL.Item );
	(* Generates code for SYSTEM.GET( adr, dest ). *)
		VAR adrReg : DevHostCPL.Item;
	BEGIN (* SYSGet *)
		adrReg.mode := areg;
		adrReg.typ := DevCPT.int32typ; adrReg.acttyp := DevCPT.int32typ;
		adrReg.reg := DevHostCPL.GetAdrReg( );
		DevHostCPL.Move( adr, adrReg );
		adrReg.mode := regx;
		adrReg.bd := 0;
		adrReg.typ := dest.typ; adrReg.acttyp := dest.typ;
		adrReg.inxReg := None;
		Assign( adrReg, dest );
	END SYSGet;

	PROCEDURE SYSPut*( VAR source, address : DevHostCPL.Item );
	(* Generates code for SYSTEM.PUT( source, address ). *)
		VAR adrReg : DevHostCPL.Item;
	BEGIN (* SYSPut *)
		adrReg.mode := areg;
		adrReg.typ := DevCPT.int32typ; adrReg.acttyp := DevCPT.int32typ;
		adrReg.reg := DevHostCPL.GetAdrReg( );
		address.typ := untgPtr; address.acttyp := untgPtr;
		DevHostCPL.Move( address, adrReg );
		adrReg.mode := regx;
		adrReg.typ := source.typ; adrReg.acttyp := source.typ;
		adrReg.bd := 0;
		adrReg.inxReg := None;
		Assign( source, adrReg );
	END SYSPut;

	PROCEDURE SYSGetReg*( VAR dest, sourceReg : DevHostCPL.Item );
	(* Generates code for SYSTEM.GETREG( sourceReg, dest ). *)
	BEGIN (* SYSGetReg *)
		sourceReg.reg := SHORT( sourceReg.bd );
		sourceReg.typ := dest.typ; sourceReg.acttyp := dest.typ;
		IF ( sourceReg.reg >= 0 ) & ( sourceReg.reg <= 7 ) THEN
			sourceReg.mode := dreg;
			DevHostCPL.Move( sourceReg, dest );
		ELSIF ( sourceReg.reg >= 8 ) & ( sourceReg.reg <= 15 ) THEN
			sourceReg.mode := areg;
			DevHostCPL.Move( sourceReg, dest );
		ELSIF ( sourceReg.reg >= 16 ) & ( sourceReg.reg <= 23 ) THEN
			sourceReg.mode := freg;
			DevHostCPL.FMove( sourceReg, dest );
		ELSE
			DevCPM.err( 220 );
		END; (* IF *)
	END SYSGetReg;

	PROCEDURE SYSPutReg*( VAR source, destReg : DevHostCPL.Item );
	(* Generates code for SYSTEM.PUTREG( destReg, source ). *)
	BEGIN (* SYSPutReg *)
		destReg.reg := SHORT( destReg.bd );
		IF ( destReg.bd >= 0 ) & ( destReg.bd <= 7 ) THEN
			destReg.mode := dreg;
			DevHostCPL.Move( source, destReg );
		ELSIF ( destReg.bd >= 8 ) & ( destReg.bd <= 15 ) THEN
			destReg.mode := areg;
			DevHostCPL.Move( source, destReg );
		ELSIF ( destReg.bd >= 16 ) & ( destReg.bd <= 23 ) THEN
			destReg.mode := freg;
			DevHostCPL.FMove( source, destReg );
		ELSE
			DevCPM.err( 220 );
		END; (* IF *)
	END SYSPutReg;

	PROCEDURE SYSBit*( VAR adr, bitnr, res : DevHostCPL.Item );
	(* Generates code for SYSTEM.BIT( adr, bitnr ). *)
		VAR adrItem : DevHostCPL.Item;
	BEGIN (* SYSBit *)
		adrItem.mode := areg;
		adrItem.reg := DevHostCPL.GetAdrReg( );
		adrItem.typ := untgPtr; adrItem.acttyp := untgPtr;
		adr.typ := untgPtr; adr.acttyp := untgPtr;
		DevHostCPL.Move( adr, adrItem );
		adrItem.mode := regx;
		adrItem.bd := 0;
		adrItem.inxReg := None;
		IF bitnr.mode = imm THEN
			DevHostCPL.Format4( BTST, bitnr.bd, adrItem );
		ELSE
			DevHostCPL.Format5( BTST, bitnr, adrItem );
		END; (* IF *)
		MakeCocItem( NE, res );
	END SYSBit;

BEGIN (* OPC *)
	untgPtr := DevCPT.int32typ;
	FP.mode := areg;
	FP.typ := untgPtr; FP.acttyp := untgPtr;
	FP.reg := 14;
	SP.mode := areg;
	SP.typ := untgPtr; SP.acttyp := untgPtr;
	SP.reg := 15;
	NEW(shortString16typ); shortString16typ^ := DevCPT.string16typ^;
END DevM68CPC.