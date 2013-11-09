MODULE DevCPC68k;
(* code generator for MC68020: based on OPL Samuel Urech 26.2.93 *)
(* ip 6 Aug 1998 *)

(* to do: static calls of final methods (obj.conval.setval * {absAttr, empAttr, extAttr} = {}) *)

(* 
	ip	22-Jul-98	Convert: checks for conversion to Int64 from {Real, LReal} and rounds down
	ip	3-Jun-98	Assign: checks for assignments of floating-point values to Int64 and rounds accordingly

RELEASE 1.3
	cp	26-Feb-97	MinMax changed according to bh
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
	ip	17.6.96	SANE-Einführung

RELEASE 1.2
	ip	12.4.96	MakeField: setzt item.obj auf NIL
	ip	4.2.96	DeRef: Test mit InLastNIL
	ip	4.2.96	Call: OPL.ClearHints
	ip	1.2.96	Call: unterscheidet zwischen proc var und TProc, kein Test bei DeRef (TProc)
	ip	1.2.96	MakeProc: recv Parameter eingeführt, supercall verwendet aktuellen Receiver
	ip	1.2.95	DeRef: safe Parameter eingeführt
	ip	22.9.95	Call: an DLL angepasst

RELEASE 1.1
	ip	11.8.95	EnterProc: HiddenPointer,... anf NIL initialisieren
	ip	12.7.95	Div* und Mod* an Oberon/L Spezifikation angepasst
	ip	7.7.95	EnterProc: FreeReg(source) entfernt
	ip	6.7.95	Include, Exclude: Laden der Adresse
	ip	6.7.95	MakeIndex: Check von dynamischen Arrays korrigiert
	ip	10.6.95	EnterProc: Felder in erweiterten Records
	ip	30.5.95	EnterProc: Procedurevariablen auf NIL initialisieren
	ip	24.5.95	FPCR wird so gesetzt, dass FPUExceptions generiert werden.
	ip	21.5.95	MakeIndex korrigiert (zu frühe Freigabe des sizeItem-Registers)
	ip	18.04.95	don't initialize entire stackframe
	ip	xx.03.95	free registers as soon as possible (Indexchecks)
*)

	IMPORT SYSTEM, DevCPT, DevCPL68k, DevCPE, DevCPM;

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

	VAR FP, SP : DevCPL68k.Item;
			indexCheck, rangeCheck, ptrinit : BOOLEAN;
			shortString16typ*: DevCPT.Struct;

PROCEDURE [1] Debugger 0A9H, 0FFH;

	PROCEDURE Init*(opt: SET);
		CONST chk = 0; achk = 1;
	BEGIN
		indexCheck := chk IN opt; rangeCheck := achk IN opt; ptrinit := chk IN opt;
	END Init;

	PROCEDURE MakeLen*( VAR arr : DevCPL68k.Item; n : INTEGER; VAR item : DevCPL68k.Item );
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

	PROCEDURE MakeIntConst*( val : INTEGER; typ : DevCPT.Struct; VAR item : DevCPL68k.Item );
	(* Makes an immediate item of a given type from a number. *)
	BEGIN (* MakeIntConst *)
		item.mode := imm;
		item.typ := typ; item.acttyp := typ;
		item.bd := val;
	END MakeIntConst;
	
	PROCEDURE MakeRealConst(val: REAL; typ: DevCPT.Struct; VAR item: DevCPL68k.Item);
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
			item.bd := lr[0]; item.ext := lr[1];
		END;
	END MakeRealConst;
	
	PROCEDURE MakeLargeIntConst(const: DevCPT.Const; VAR item: DevCPL68k.Item);
	VAR r: REAL;
	BEGIN
		item.mode := imm;
		item.bd := SHORT(ENTIER((const.realval + const.intval)/4294967296.0));
		r := const.realval + const.intval - item.bd * 4294967296.0;
		IF r > MAX(INTEGER) THEN r := r - 4294967296.0 END;
		item.ext := SHORT(ENTIER(r));
	END MakeLargeIntConst;

	PROCEDURE GetVarBase( obj : DevCPT.Object ) : SHORTINT;
	(* Returns the register to which the given variable is relative. *)
		VAR diff, reg : SHORTINT;
				source, dest : DevCPL68k.Item;
	BEGIN (* GetVarBase *)
		diff := SHORT(DevCPL68k.level - obj.mnolev);
		IF diff = 0 THEN
			reg := FP.reg;
		ELSE (* follow static link *)
			reg := DevCPL68k.GetAdrReg( );
			source.mode := regx;
			source.typ := DevCPT.sysptrtyp; source.acttyp := DevCPT.sysptrtyp;
			source.reg := FP.reg;
			source.bd := 8;
			source.inxReg := None;
			source.offsReg := None;
			dest.mode := areg;
			dest.typ := DevCPT.sysptrtyp; dest.acttyp := DevCPT.sysptrtyp;
			dest.reg := reg;
			DevCPL68k.Move( source, dest );
			source.reg := reg;
			WHILE diff > 1 DO
				DevCPL68k.Move( source, dest );
				DEC( diff );
			END; (* WHILE *)
		END; (* IF *)
		RETURN reg
	END GetVarBase;

	PROCEDURE MakeVar*( obj : DevCPT.Object; VAR item : DevCPL68k.Item );
	(* Makes an item from a variable. *)
		VAR aregItem : DevCPL68k.Item;
	BEGIN (* MakeVar *)
		IF ( obj.mode = VarPar ) & ( obj.typ.comp # DynArr ) THEN
			item.mode := regx;
			item.reg := GetVarBase( obj );
			item.typ := DevCPT.sysptrtyp; item.acttyp := DevCPT.sysptrtyp;
			item.bd := obj.adr;
			item.inxReg := None;
			aregItem.mode := areg;
			aregItem.reg := DevCPL68k.GetAdrReg( );
			DevCPL68k.Move( item, aregItem );
			DevCPL68k.FreeReg( item );
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

	PROCEDURE DeRef*( typ : DevCPT.Struct; VAR item : DevCPL68k.Item; safe: BOOLEAN);
	(* Makes a dereferentiation of an item. *)
		VAR
			aregItem : DevCPL68k.Item;
			inxReg : SHORTINT;
			handle: BOOLEAN;
	BEGIN (* DeRef *)
		handle := item.typ.sysflag = 2;
		aregItem.mode := areg;
		IF (item.reg >= 8) & (item.reg < 12) THEN
			aregItem.reg := item.reg;
		ELSE
			aregItem.reg := DevCPL68k.GetAdrReg( );
		END;
		aregItem.typ := DevCPT.sysptrtyp; aregItem.acttyp := DevCPT.sysptrtyp;
		item.typ := DevCPT.sysptrtyp; item.acttyp := DevCPT.sysptrtyp;
		inxReg := item.offsReg;
		DevCPL68k.Move( item, aregItem );
		IF ~(safe OR (DevCPL68k.InLastNIL(item.obj) >= 0)) THEN
			DevCPL68k.Test(aregItem);
			DevCPL68k.Trapcc(EQ, NILTrap);
			DevCPL68k.SetLastNIL(item.obj);
		END;
		IF handle THEN
			item.mode := regx;
			item.reg := aregItem.reg;
			item.bd := 0; item.inxReg := None;
			DevCPL68k.Move(item, aregItem);
			IF ~safe THEN
				DevCPL68k.Test(aregItem);
				DevCPL68k.Trapcc(EQ, NILTrap);
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

	PROCEDURE StaticTag*( typ : DevCPT.Struct; VAR tag : DevCPL68k.Item );
	(* Returns the type tag of a type. *)
	BEGIN (* StaticTag *)
		tag.mode := immL;
		tag.reg := absolute;
		tag.typ := DevCPT.sysptrtyp; tag.acttyp := DevCPT.sysptrtyp;
		tag.bd := 0;
		tag.obj := typ.strobj;
	END StaticTag;

	PROCEDURE MakeTag*( obj : DevCPT.Object; typ : DevCPT.Struct; VAR item, tag : DevCPL68k.Item );
	(* Makes an item that denotes the type tag of the given object and item. *)
	VAR sreg, dreg: DevCPL68k.Item; t: DevCPT.Struct;
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
				sreg.reg := item.reg; dreg.reg := DevCPL68k.GetAdrReg();
				DevCPL68k.Move(sreg, dreg);
				tag.reg := dreg.reg;
			END;
			DeRef( DevCPT.sysptrtyp, tag, FALSE );
			tag.bd := -4;
		ELSIF ( obj # NIL ) & ( obj.mode = VarPar ) THEN
			tag.mode := regx;
			tag.typ := DevCPT.sysptrtyp; tag.acttyp := DevCPT.sysptrtyp;
			tag.reg := GetVarBase(obj);
			tag.bd := obj.adr + 4;
			tag.inxReg := None;
			tag.offsReg := None;
		ELSIF item.tmode = heap THEN
			tag := item;
			tag.typ := DevCPT.sysptrtyp; tag.acttyp := DevCPT.sysptrtyp;
			tag.tmode := 0;
			INC(tag.bd, -4)
		ELSE
			StaticTag( typ, tag );
		END; (* IF *)
	END MakeTag;

	PROCEDURE MakeConst*( obj : DevCPT.Object; const : DevCPT.Const; typ : DevCPT.Struct; VAR item : DevCPL68k.Item );
	(* Makes an item from a constant. *)
		VAR realval : SHORTREAL;
	BEGIN (* MakeConst *)
		item.typ := typ; item.acttyp := typ;
		CASE typ.form OF
			Set :
				MakeIntConst( SYSTEM.VAL( INTEGER, const.setval ), typ, item );
			| String8 :
				DevCPL68k.AllocConst(item, const, typ.form);
			| String16:
				DevCPL68k.AllocConst(item, const, typ.form);
			| LReal, Real:
				MakeRealConst(const.realval, typ, item);
			| Int64:
				MakeLargeIntConst(const, item);
		ELSE
			MakeIntConst( const.intval, typ, item );
		END; (* CASE *)
	END MakeConst;

	PROCEDURE BaseTypSize( VAR arr, size : DevCPL68k.Item; VAR scale : SHORTINT );
	(* Returns the size of the base type of a dynamic array if the base type is a dynamic array itself. *)
		VAR i : INTEGER;
				typ : DevCPT.Struct;
				len : DevCPL68k.Item;
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
			DevCPL68k.Format12( MULS, len, size );
			DevCPL68k.FreeReg( len );
		END; (* IF *)
		FOR i := 0 TO arr.typ.n-2 DO
			MakeLen( arr, i, len );
			DevCPL68k.Format12( MULS, len, size );
			DevCPL68k.FreeReg( len );
		END; (* FOR *)
	END BaseTypSize;

	PROCEDURE Size( VAR arr, size : DevCPL68k.Item; VAR scale : SHORTINT );
	(* Returns the size of a dynamic array and a scale factor. *)
		VAR
			len : DevCPL68k.Item;
			btyp : DevCPT.Struct;
	BEGIN (* Size *)
		MakeLen(arr,arr.typ.n, len);
		btyp := arr.typ.BaseTyp;
		IF btyp.comp = DynArr THEN
			BaseTypSize( arr, size, scale );
			DevCPL68k.Format12( MULS, len, size );
			DevCPL68k.FreeReg( len );
		ELSE
			IF ( btyp.size = 1 ) OR ( btyp.size = 2 ) OR ( btyp.size = 4 ) OR ( btyp.size = 8 ) THEN
				scale := SHORT( btyp.size );
				size := len;
			ELSE
				scale := 1;
				MakeIntConst( btyp.size, DevCPT.int32typ, size );
				DevCPL68k.Format12( MULS, len, size );
				DevCPL68k.FreeReg( len );
			END; (* IF *)
		END; (* IF *)
	END Size;

	PROCEDURE ElimIndex( VAR item : DevCPL68k.Item );
	(* Eliminates the index register in the item. *)
		VAR newReg : SHORTINT; freeReg: DevCPL68k.Item;
	BEGIN (* ElimIndex *)
		IF item.inxReg # None THEN (* load old address *)
			IF (item.mode = regx) & ~(item.reg IN DevCPL68k.reservedRegs) THEN
				newReg := item.reg;
			ELSE
				newReg := DevCPL68k.GetAdrReg( );
			END;
			DevCPL68k.Lea( item, newReg );
			item.mode := regx;
			item.bd := 0;
			item.reg := newReg;
			freeReg.reg := item.inxReg;
			freeReg.mode := dreg;
			DevCPL68k.FreeReg(freeReg);
			item.inxReg := None;
		END; (* IF *)
	END ElimIndex;

	PROCEDURE SetElem*( VAR item : DevCPL68k.Item );
	(* Makes a set-element from an integer element and sets the corresponding bit. *)
		VAR source : DevCPL68k.Item;
	BEGIN (* SetElem *)
		source :=  item;
		item.mode := dreg;
		item.typ := DevCPT.settyp; item.acttyp := DevCPT.settyp;
		item.reg := DevCPL68k.GetReg( );
		DevCPL68k.Format7( CLR, item );
		DevCPL68k.Format5( BSET, source, item );
	END SetElem;

	PROCEDURE PushAdrStack*(VAR item : DevCPL68k.Item );
	BEGIN
		DevCPL68k.Pea(item);
	END PushAdrStack;

	PROCEDURE ^ PushStack*(VAR item : DevCPL68k.Item );

	PROCEDURE AddToSP*( data : INTEGER );
	(* Subtracts the immediate value 'data' from the stack pointer. *)
		VAR source : DevCPL68k.Item;
	BEGIN (* AddToSP *)
		IF data > 0 THEN
			IF data < 8 THEN
				DevCPL68k.Format1( ADDQ, SHORT( data ), SP );
			ELSE
				MakeIntConst( data, DevCPT.int32typ, source );
				DevCPL68k.Format3( ADD, source, SP.reg );
			END; (* IF *)
		ELSIF data < 0 THEN
			data := -data;
			IF data < 8 THEN
				DevCPL68k.Format1( SUBQ, SHORT( data ), SP );
			ELSE
				MakeIntConst( data, DevCPT.int32typ, source );
				DevCPL68k.Format3( SUB, source, SP.reg );
			END; (* IF *)
		END; (* IF *)
	END AddToSP;

	PROCEDURE RoundZero;
	(* Sets the rounding mode of the coprocessor to -inf. *)
		VAR env, temp : DevCPL68k.Item;
	BEGIN (* RoundDown *)
		AddToSP(-2);
		env.mode := regx; env.reg := 15; env.bd := 0; env.inxReg := None;
		env.obj := NIL; env.typ := DevCPT.int16typ; env.acttyp := DevCPT.int16typ;
		MakeIntConst(00003H, DevCPT.int16typ, temp);
		PushAdrStack(env); PushStack(temp);
		DevCPL68k.FP68K;
		DevCPL68k.Format6(ANDI, 09FFH, env);
		DevCPL68k.Format6(ORI, 06000H, env);
		MakeIntConst(00001H, DevCPT.int16typ, temp);
		PushAdrStack(env); PushStack(temp);
		DevCPL68k.FP68K;
		AddToSP(2);
(*		MakeIntConst( 07420H, DevCPT.int32typ, temp );
		DevCPL68k.FMovecr( temp, 0, FPCR );*)
	END RoundZero;

	PROCEDURE RoundDown;
	(* Sets the rounding mode of the coprocessor to -inf. *)
		VAR env, temp : DevCPL68k.Item;
	BEGIN (* RoundDown *)
		AddToSP(-2);
		env.mode := regx; env.reg := 15; env.bd := 0; env.inxReg := None;
		env.obj := NIL; env.typ := DevCPT.int16typ; env.acttyp := DevCPT.int16typ;
		MakeIntConst(00003H, DevCPT.int16typ, temp);
		PushAdrStack(env); PushStack(temp);
		DevCPL68k.FP68K;
		DevCPL68k.Format6(ANDI, 09FFH, env);
		DevCPL68k.Format6(ORI, 04000H, env);
		MakeIntConst(00001H, DevCPT.int16typ, temp);
		PushAdrStack(env); PushStack(temp);
		DevCPL68k.FP68K;
		AddToSP(2);
(*		MakeIntConst( 07420H, DevCPT.int32typ, temp );
		DevCPL68k.FMovecr( temp, 0, FPCR );*)
	END RoundDown;

	PROCEDURE RoundNearest;
	(* Sets the rounding mode of the coprocessor to nearest. *)
		VAR env, temp : DevCPL68k.Item;
	BEGIN (* RoundNearest *)
		AddToSP(-2);
		env.mode := regx; env.reg := 15; env.bd := 0; env.inxReg := None;
		env.obj := NIL; env.typ := DevCPT.int16typ; env.acttyp := DevCPT.int16typ;
		MakeIntConst(00003H, DevCPT.int16typ, temp);
		PushAdrStack(env); PushStack(temp);
		DevCPL68k.FP68K;
		DevCPL68k.Format6(ANDI, 09FFH, env);
		MakeIntConst(00001H, DevCPT.int16typ, temp);
		PushAdrStack(env); PushStack(temp);
		DevCPL68k.FP68K;
		AddToSP(2);
(*		MakeIntConst( 07400H, DevCPT.int32typ, temp );
		DevCPL68k.FMovecr( temp, 0, FPCR );*)
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
	
	PROCEDURE ToXReal(VAR item, dest: DevCPL68k.Item);
	VAR t, src: DevCPL68k.Item;
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
		DevCPL68k.FP68K;
		IF dest.reg = 15 THEN DEC(dest.bd, 4); END;
		IF item.mode IN {dreg, areg, imm, freg} THEN
			AddToSP(src.typ.size); DEC(dest.bd, src.typ.size);
		END;
	END ToXReal;
	
	PROCEDURE FromXReal(VAR source, item: DevCPL68k.Item);
	VAR t, dst: DevCPL68k.Item;
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
		DevCPL68k.FP68K;
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
			DevCPL68k.Move(t, dst);
			IF item.typ.size = 8 THEN
				INC(t.bd, 4); INC(dst.bd, 4);
				DevCPL68k.Move(t, dst);
			END;
		END;
	END FromXReal;
	
	PROCEDURE Convert*( VAR source : DevCPL68k.Item; desttyp : DevCPT.Struct );
	(* Converts the given item to desttyp. *)
		VAR sf, sc, df, dc : BYTE;
				dest : DevCPL68k.Item;
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
			IF df IN RealSet THEN
				IF sf IN ByteSet THEN
					Convert(source, DevCPT.int32typ);
				ELSIF (df = Int64) & (sf IN {Real, LReal}) THEN
					IF source.mode # xreal THEN
						INC(DevCPL68k.xrealstack); AddToSP(-12);
						dest.mode := regx; dest.reg := 15; dest.bd := 0; dest.inxReg := None;
						dest.obj := NIL; dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
						ToXReal(source, dest);
						DevCPL68k.FreeReg(source);
					END;
					RoundDown;
					source.mode := regx; source.reg := 15; source.bd := 0; source.inxReg := None;
					source.obj := NIL; source.typ := DevCPT.int32typ; source.acttyp := DevCPT.int32typ;
					MakeIntConst(00014H, DevCPT.int16typ, dest);
					PushAdrStack(source); PushStack(dest);
					DevCPL68k.FP68K;
					RoundNearest;
					source.mode := xreal; source.reg := DevCPL68k.xrealstack;
				END;
			ELSIF ( sf # df ) OR ( sc # dc ) THEN
				IF df IN LongSet THEN
					IF sf IN {Char8, Char16} THEN
						dest.mode := dreg;
						dest.typ := desttyp; dest.acttyp := desttyp;
						dest.reg := DevCPL68k.GetReg( );
						DevCPL68k.Format7( CLR, dest );
						DevCPL68k.Move( source, dest );
						source := dest;
					ELSIF sf IN ByteSet + WordSet THEN
						IF source.mode # imm THEN
							DevCPL68k.Ext( source, long );
						END;
					ELSIF sf IN RealSet THEN
						IF source.mode # xreal THEN
							INC(DevCPL68k.xrealstack); AddToSP(-12);
							dest.mode := regx; dest.reg := 15; dest.bd := 0; dest.inxReg := None;
							dest.obj := NIL; dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
							ToXReal(source, dest);
							DevCPL68k.FreeReg(source);
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
						DevCPL68k.Load(source);
						AddToSP(12); DEC(DevCPL68k.xrealstack);
						(*DevCPL68k.FLoad( source );
						RoundDown;
						source.typ := desttyp; source.acttyp := desttyp;
						DevCPL68k.Load( source );
						RoundNearest;*)
					END; (* IF *)
				ELSIF df IN WordSet THEN
					IF sf IN LongSet THEN
						DevCPL68k.Load( source );
					ELSIF sf = Char8 THEN
						dest.mode := dreg;
						dest.typ := desttyp; dest.acttyp := desttyp;
						dest.reg := DevCPL68k.GetReg( );
						DevCPL68k.Format7( CLR, dest );
						DevCPL68k.Move( source, dest );
						source := dest;
					ELSIF sf IN ByteSet THEN
						IF source.mode # imm THEN
							DevCPL68k.Ext( source, word );
						END;
					ELSIF sf IN RealSet THEN
						IF source.mode # xreal THEN
							INC(DevCPL68k.xrealstack); AddToSP(-12);
							dest.mode := regx; dest.reg := 15; dest.bd := 0; dest.inxReg := None;
							dest.obj := NIL; dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
							ToXReal(source, dest);
							DevCPL68k.FreeReg(source);
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
						DevCPL68k.Load(source);
						AddToSP(12); DEC(DevCPL68k.xrealstack);
(*						DevCPL68k.FLoad( source );
						RoundDown;
						source.typ := desttyp; source.acttyp := desttyp;
						DevCPL68k.Load( source );
						RoundNearest;*)
					END; (* IF *)
				ELSIF df IN ByteSet THEN
					IF sf IN WordSet + LongSet THEN
						DevCPL68k.Load( source );
					ELSIF sf IN RealSet THEN
						Convert(source, DevCPT.int16typ);
						Convert(source, desttyp);
(*						DevCPL68k.FLoad( source );
						RoundDown;
						source.typ := desttyp; source.acttyp := desttyp;
						DevCPL68k.Load( source );
						RoundNearest;*)
					END; (* IF *)
(*				ELSIF df IN RealSet THEN
					DevCPL68k.FLoad( source ); *)
				END; (* IF *)
				source.acttyp := desttyp;
			END; (* IF *)
			source.typ := desttyp;
		END;
	END Convert;

	PROCEDURE GetDynArrVal*( VAR item : DevCPL68k.Item; untagged: BOOLEAN );
	(* Returns an item containing the actual value of a dynamic array. *)
	BEGIN (* GetDynArrVal *)
		IF item.nolen = 0 THEN
			DeRef( DevCPT.sysptrtyp, item, FALSE );
		ELSE
			IF ~untagged THEN
				INC(item.bd, LONG( item.nolen ) * 4  + 12);
			END;
			item.nolen := 0;
			item.typ := DevCPT.sysptrtyp; item.acttyp := DevCPT.sysptrtyp;
			item.inxReg := item.offsReg;
			item.offsReg := None;
		END; (* IF *)
	END GetDynArrVal;

	PROCEDURE GetDynArrAdr( VAR item, adr : DevCPL68k.Item );
	(* Returns an item containing the address of a dynamic array. *)
		VAR adrReg : DevCPL68k.Item;
	BEGIN (* GetDynArrAdr *)
		adr.typ := DevCPT.sysptrtyp; adr.acttyp := DevCPT.sysptrtyp;
		adr.nolen := 0;
		IF item.nolen = 0 THEN
			IF item.offsReg # None THEN
				DeRef( DevCPT.sysptrtyp, item, FALSE );
				adr.mode := areg;
				adr.reg := DevCPL68k.GetAdrReg( );
				DevCPL68k.Lea( item, adr.reg );
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
			adrReg.typ := DevCPT.sysptrtyp; adrReg.acttyp := DevCPT.sysptrtyp;
			adrReg.reg := DevCPL68k.GetAdrReg( );
			DevCPL68k.Lea( adr, adrReg.reg );
			adr := adrReg;
		END; (* IF *)
	END GetDynArrAdr;

	PROCEDURE MakeField*( VAR item : DevCPL68k.Item; offset : INTEGER; typ : DevCPT.Struct );
	(* Increments the address of item by offset and sets its type to typ. *)
	BEGIN (* MakeField *)
		DevCPL68k.LoadExternal( item );
		INC( item.bd, offset );
		item.obj := NIL;
		item.typ := typ; item.acttyp := typ;
		item.tmode := 0
	END MakeField;

	PROCEDURE MakeIndex*( VAR index, res : DevCPL68k.Item );
	(* Makes an indexed item from an item and an index. res := res[ index ].
		The generated item has always got an index register or an offset register. *)
		VAR baseTyp : DevCPT.Struct;
				sizeItem, chkItem, offset : DevCPL68k.Item;
				size : INTEGER;
				scale : SHORTINT;
	BEGIN (* MakeIndex *)
		baseTyp := res.typ.BaseTyp;
		size := baseTyp.size;
		DevCPL68k.LoadExternal( res );
		IF ( res.typ.comp # DynArr ) & ( index.mode = imm ) THEN
			INC( res.bd, size * index.bd );
		ELSE
			ElimIndex( res );
			IF index.typ.form = Int8  THEN Convert( index, DevCPT.int16typ ); END;
			IF ( ( index.mode # imm ) OR ( index.bd # 0 ) ) & indexCheck THEN
				IF (res.typ.comp = DynArr) & ~(res.typ.untagged) THEN
					MakeLen(res, res.typ.n, chkItem);
					DevCPL68k.Load(chkItem);
					DevCPL68k.Format1(SUBQ, 1, chkItem);
					Convert( index, DevCPT.int32typ );
				ELSIF ~res.typ.untagged THEN
					MakeIntConst( res.typ.n - 1, index.typ, chkItem );
				END; (* IF *)
				IF ~res.typ.untagged THEN
					DevCPL68k.Chk( index, chkItem );
					DevCPL68k.FreeReg( chkItem );
				END;
			END; (* IF *)
			DevCPL68k.Load( index );
			IF baseTyp.comp # Basic THEN
				IF baseTyp.comp = DynArr THEN
					IF ~baseTyp.untagged THEN
						Convert( index, DevCPT.int32typ );
						BaseTypSize( res, sizeItem, scale );
						DevCPL68k.Format12( MULS, sizeItem, index );
						IF (sizeItem.mode # regx) OR (sizeItem.reg # res.reg) THEN
							DevCPL68k.FreeReg( sizeItem );
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
							DevCPL68k.Format12( MULS, sizeItem, index );
						ELSE
							DevCPL68k.Format11( MULS, sizeItem, index );
						END; (* IF *)
						DevCPL68k.FreeReg( sizeItem );
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
					DevCPL68k.Format2( ADD, offset, index );
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

	PROCEDURE MakeProc*( obj, recv : DevCPT.Object; subcl : BYTE; VAR item : DevCPL68k.Item );
	(* Makes an item from a procedure object. *)
	VAR btyp: DevCPT.Struct;
	BEGIN (* MakeProc *)
		IF obj.mode = TProc THEN (* receiver is obj.link *)
			IF obj.link.mode = VarPar THEN
				item.mode := regx;
				item.typ := DevCPT.sysptrtyp; item.acttyp := DevCPT.sysptrtyp;
				item.reg := SP.reg;
				item.bd := ReceiverOffset(obj) + 4; (* tag  is pushed before recordptr *)
				item.inxReg := None;
				item.offsReg := None;
			ELSE
				item.mode := regx;
				item.typ := DevCPT.sysptrtyp; item.acttyp := DevCPT.sysptrtyp;
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
				item.bd := DevCPL68k.BaseTypeOffs + 4 * btyp.extlev;
				DeRef( DevCPT.sysptrtyp, item, TRUE );
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

	PROCEDURE MakePostInc( typ : DevCPT.Struct; VAR item : DevCPL68k.Item );
	(* Makes a post-increment item from the given item. *)
		VAR dest : DevCPL68k.Item;
	BEGIN (* MakePostInc *)
		IF item.mode # postinc THEN
			IF ( item.mode = regx ) & ( item.bd = 0 ) & ( item.inxReg = None ) & ~ ( item.reg IN { FP.reg, SP.reg } ) THEN
				item.mode := postinc;
				item.typ := typ; item.acttyp := typ;
			ELSE
				dest.mode := postinc;
				dest.typ := typ; dest.acttyp := typ;
				dest.reg := DevCPL68k.GetAdrReg( );
				DevCPL68k.Lea( item, dest.reg );
				DevCPL68k.FreeReg(item);
				item := dest;
			END;
		END;
	END MakePostInc;

	PROCEDURE MakeSPPredec*( VAR res : DevCPL68k.Item );
	(* Makes a pre-decrement item with the stack pointer. *)
	BEGIN (* MakeSPPredec *)
		res.mode := predec;
		res.reg := SP.reg;
		res.typ := SP.typ; res.acttyp := SP.typ;
	END MakeSPPredec;

	PROCEDURE MakeSPPostinc*(VAR res: DevCPL68k.Item);
	BEGIN
		res.mode := postinc;
		res.reg := SP.reg;
		res.typ := SP.typ; res.acttyp := SP.typ;
	END MakeSPPostinc;

	PROCEDURE MakeCocItem*( trueCond : SHORTINT; VAR res : DevCPL68k.Item );
	(* Makes a coc item with the true-condition trueCond. *)
	BEGIN (* MakeCocItem *)
		res.mode := coc;
		res.typ := DevCPT.booltyp; res.acttyp := DevCPT.booltyp;
		res.bd := DevCPL68k.TFConds( trueCond );
		(* leave tJump and fJump unchanged! *)
	END MakeCocItem;

	PROCEDURE MakeFCocItem*( trueCond : SHORTINT; VAR res : DevCPL68k.Item );
	(* Makes an fcoc item with the true-condition trueCond. *)
	BEGIN (* MakeFCocItem *)
		res.mode := fcoc;
		res.typ := DevCPT.booltyp; res.acttyp := DevCPT.booltyp;
		res.bd := DevCPL68k.TFFConds( trueCond );
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
		VAR sppredec : DevCPL68k.Item;
				regList : SHORTINT;
	BEGIN (* PushRegs *)
		MakeSPPredec( sppredec );
		regList := Swap( regs );
		IF regList # 0 THEN
			DevCPL68k.Movem( 0, regList, sppredec );
		END; (* IF *)
		regList := Floats( regs );
(*		IF regList # 0 THEN
			DevCPL68k.FMovem( 0, regList , sppredec );
		END; (* IF *)*)
	END PushRegs;

	PROCEDURE PopRegs*( regs : SET );
	(* Pops the given registers from the stack. *)
		VAR sppostinc : DevCPL68k.Item;
				regList : SHORTINT;
	BEGIN (* PopRegs *)
		MakeSPPostinc(sppostinc);
		regList := SwappedFloats( regs );
(*		IF regList # 0 THEN
			DevCPL68k.FMovem( 1, regList, sppostinc );
		END; (* IF *)*)
		regList := SHORT( SYSTEM.VAL( INTEGER, regs ) );
		IF regList # 0 THEN
			DevCPL68k.Movem( 1, regList, sppostinc );
		END; (* IF *)
	END PopRegs;

	PROCEDURE TrueJump*( VAR expression : DevCPL68k.Item; VAR label : DevCPL68k.Label );
	(* Generates a conditional branch to the given label with the true condition. *)
	BEGIN (* TrueJump *)
		IF expression.mode = imm THEN
			IF expression.bd # 0  THEN
				DevCPL68k.Jump( true, label );
			END; (* IF *)
		ELSIF expression.mode = coc THEN
			DevCPL68k.Jump( SHORT( expression.bd DIV 10000H ), label );
(*		ELSIF expression.mode = fcoc THEN
			DevCPL68k.FJump( SHORT( expression.bd DIV 10000H ), label );*)
		ELSE
			DevCPL68k.Load( expression );
			DevCPL68k.Format7( TST, expression );
			DevCPL68k.Jump( NE, label );
		END; (* IF *)
		DevCPL68k.SetLabel( expression.fJump );
	END TrueJump;

	PROCEDURE FalseJump*( VAR expression : DevCPL68k.Item; VAR label : DevCPL68k.Label );
	(* Generates a conditional branch to the given label with the false condition. *)
	BEGIN (* FalseJump *)
		IF expression.mode = imm THEN
			IF expression.bd = 0 THEN
				DevCPL68k.Jump( true, label );
			END; (* IF *)
		ELSIF expression.mode = coc THEN
			DevCPL68k.Jump( SHORT( expression.bd MOD 10000H ), label );
(*		ELSIF expression.mode = fcoc THEN
			DevCPL68k.FJump( SHORT( expression.bd MOD 10000H ), label );*)
		ELSE
			DevCPL68k.Load( expression );
			DevCPL68k.Format7( TST, expression );
			DevCPL68k.Jump( EQ, label );
		END; (* IF *)
		DevCPL68k.SetLabel( expression.tJump );
	END FalseJump;

	PROCEDURE MoveBlock( scale : SHORTINT; VAR size, source, dest : DevCPL68k.Item; opt: BOOLEAN );
	(* Moves a block of data of length size from source to dest. *)
		VAR i : INTEGER;
				losize : DevCPL68k.Item;
				label : DevCPL68k.Label;
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
				DevCPL68k.Move( source, dest );
				IF scale = 8 THEN DevCPL68k.Move(source, dest); END;
				INC( i );
			END; (* WHILE *)
		ELSE
			IF size.mode = imm THEN
				DEC( size.bd );
			ELSE
				DevCPL68k.Load( size );
				DevCPL68k.Format1( SUBQ, 1, size );
			END; (* IF *)
			IF ( ( size.mode = imm ) & ( size.bd <= MAX( SHORTINT ) ) ) OR ( size.typ # DevCPT.int32typ ) THEN
				DevCPL68k.Load( size );
				Convert( size, DevCPT.int16typ );
				label := DevCPL68k.NewLabel;
				DevCPL68k.SetLabel( label );
				DevCPL68k.Move( source, dest );
				IF scale = 8 THEN DevCPL68k.Move( source, dest ); END;
				DevCPL68k.DBcc( false, size.reg, label );
			ELSE
				DevCPL68k.Load( size );
				losize.mode := dreg;
				losize.typ := DevCPT.int16typ; losize.acttyp := DevCPT.int16typ;
				losize.reg := DevCPL68k.GetReg( );
				DevCPL68k.Move( size, losize );
				DevCPL68k.Swap( size );
				label := DevCPL68k.NewLabel;
				DevCPL68k.SetLabel( label );
				DevCPL68k.Move( source, dest );
				IF scale = 8 THEN DevCPL68k.Move( source, dest ); END;
				DevCPL68k.DBcc( false, losize.reg, label );
				DevCPL68k.DBcc( false, size.reg, label );
				DevCPL68k.FreeReg(losize);
			END; (* IF *)
		END; (* IF *)
	END MoveBlock;

	PROCEDURE Assign*( VAR source, dest : DevCPL68k.Item );
	(* Generates code for the assignment dest := source.  *)
		VAR size : INTEGER;
				length, src, dst : DevCPL68k.Item;
				label : DevCPL68k.Label;
				scale : SHORTINT;
				typ: DevCPT.Struct;
	BEGIN (* Assign *)
		IF source.mode = xreal THEN
			IF source.reg # DevCPL68k.xrealstack THEN DevCPM.err(-811); END;
			src.mode := regx; src.reg := 15; src.bd := 0; src.inxReg := None;
			src.obj := NIL; src.typ := DevCPT.int32typ; src.acttyp := DevCPT.int32typ;
			IF dest.typ.form = Int64 THEN
				RoundDown;
			END;
			FromXReal(src, dest);
			IF dest.typ.form = Int64 THEN
				RoundNearest;
			END;
		ELSE
			Convert( source, dest.typ );
			size := source.typ.size;
			DevCPL68k.LoadAdr( dest );
(*			IF source.mode = freg THEN
				DevCPL68k.FMove( source, dest );
			ELS*)
			IF source.typ.form IN RealSet THEN
				IF source.acttyp.form # dest.typ.form THEN
					INC(DevCPL68k.xrealstack); AddToSP(-12);
					dst.mode := regx; dst.reg := 15; dst.bd := 0; dst.inxReg := -1;
					dst.obj := NIL; dst.typ := DevCPT.int32typ; dst.acttyp := DevCPT.int32typ;
					ToXReal(source, dst);
					DevCPL68k.FreeReg(source);
					source.mode := xreal; source.reg := DevCPL68k.xrealstack;
					src.mode := regx; src.reg := 15; src.bd := 0; src.inxReg := None;
					src.obj := NIL; src.typ := DevCPT.int32typ; src.acttyp := DevCPT.int32typ;
					IF dest.typ.form = Int64 THEN
						RoundDown;
					END;
					FromXReal(src, dest);
					IF dest.typ.form = Int64 THEN
						RoundNearest;
					END;
				ELSIF source.typ.form = Real THEN
					DevCPL68k.Move( source, dest );
				ELSE (* LReal, Int64 *)
					typ := dest.typ;
					IF source.mode = imm THEN
						DevCPL68k.LoadExternal(dest);
						source.typ := DevCPT.int32typ; source.acttyp := DevCPT.int32typ;
						dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
						DevCPL68k.Move(source, dest);
						INC(dest.bd, 4);
						size := source.bd; source.bd := source.ext;
						DevCPL68k.Move(source, dest);
						DEC(dest.bd, 4);
						source.bd := size;
						source.typ := typ; source.acttyp := typ;
						dest.typ := typ; dest.acttyp := typ;
					ELSE
						DevCPL68k.LoadAdr( source );
						DevCPL68k.LoadExternal( source );
						DevCPL68k.LoadExternal( dest );
						source.typ := DevCPT.int32typ; source.acttyp := DevCPT.int32typ;
						dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
						DevCPL68k.Move( source, dest );
						INC(source.bd, 4);
						INC(dest.bd, 4);
						DevCPL68k.Move( source, dest );
						DEC(source.bd, 4);
						DEC(dest.bd, 4);
						source.typ := typ; source.acttyp := typ;
						dest.typ := typ; dest.acttyp := typ;
					END;
				END;
			ELSIF source.mode IN { coc, fcoc } THEN
				src.mode := imm;
				src.typ := DevCPT.booltyp; src.acttyp := DevCPT.booltyp;
				label := DevCPL68k.NewLabel;
				IF source.mode = coc THEN
					DevCPL68k.Jump( SHORT( source.bd MOD 10000H ), source.fJump );
(*				ELSE
					DevCPL68k.FJump( SHORT( source.bd MOD 10000H ), source.fJump );*)
				END;
				DevCPL68k.SetLabel( source.tJump );
				src.bd := 1;
				DevCPL68k.Move( src, dest );
				DevCPL68k.Jump( true, label );
				DevCPL68k.SetLabel( source.fJump );
				src.bd := 0;
				DevCPL68k.Move( src, dest );
				DevCPL68k.SetLabel( label );
			ELSIF (dest.typ.form = Pointer) & (source.typ.form IN {String8, String16, Comp}) THEN
				DevCPL68k.LoadAdr(source);
				DevCPL68k.Move( source, dest );
			ELSIF ( size = 1 ) OR ( size = 2 ) OR ( size = 4 ) THEN
				DevCPL68k.Move( source, dest );
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
				MoveBlock( scale, length, source, dest, TRUE );
			END; (* IF *)
		END;
	END Assign;

	PROCEDURE PushStack*(VAR item : DevCPL68k.Item );
		VAR dest : DevCPL68k.Item; typ: DevCPT.Struct; temp: INTEGER;
	BEGIN
		dest.mode := predec;
		dest.reg := SP.reg;
		IF (item.mode # xreal) & ((item.typ.form = LReal) & (item.acttyp.form = LReal) OR (item.typ.form = Int64) & (item.acttyp.form = Int64)) THEN
			typ := item.typ;
			IF item.mode = imm THEN
				dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
				item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
				temp := item.bd; item.bd := item.ext;
				Assign(item, dest);
				item.bd := temp;
				Assign(item, dest);
				dest.typ := typ; dest.acttyp := typ;
				item.typ := typ; item.acttyp := typ;
(*			ELSIF item.mode = freg THEN
				DevCPL68k.FMove(item, dest);*)
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

	PROCEDURE StringLen*(include0X: BOOLEAN; VAR item: DevCPL68k.Item);
	VAR
		len, scratch: DevCPL68k.Item;
		typ: DevCPT.Struct;
		loop: DevCPL68k.Label;
	BEGIN
		len.mode := dreg; len.reg := DevCPL68k.GetReg(); len.bd := 0; len.inxReg := None;
		len.obj := NIL; len.typ := DevCPT.int32typ; len.acttyp := DevCPT.int32typ;
		IF include0X THEN
			DevCPL68k.Moveq(0, len.reg);
		ELSE
			DevCPL68k.Moveq(-1, len.reg);
		END;
		IF (item.acttyp.form = String16) OR ((item.acttyp.form = Comp) & ((item.acttyp.comp = Array) OR (item.acttyp.comp = DynArr)) & (item.acttyp.BaseTyp.form = Char16)) THEN
			typ := DevCPT.char16typ;
		ELSE
			typ := DevCPT.char8typ;
		END;
		IF item.acttyp.comp = DynArr THEN GetDynArrVal(item, item.acttyp.untagged) END;
		MakePostInc(typ, item);
		scratch.mode := dreg; scratch.reg := DevCPL68k.GetReg(); scratch.bd := 0; scratch.inxReg := None;
		scratch.obj := NIL; scratch.typ := typ; scratch.acttyp := typ;
		loop := DevCPL68k.NewLabel;
		DevCPL68k.SetLabel(loop);
		DevCPL68k.Format1(ADDQ, 1, len);
		DevCPL68k.Move(item, scratch);
		DevCPL68k.Jump(NE, loop);
		DevCPL68k.Free(item); DevCPL68k.Free(scratch);
		item := len;
	END StringLen;

	PROCEDURE PushDynArrStack*( formalTyp : DevCPT.Struct; (* offset : LONGINT; *) VAR item : DevCPL68k.Item );
	(* Moves the address and the length(s) of the given item to (offset, A7). *)
		VAR source, adr, length, len1 : DevCPL68k.Item;
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
			(*  a big chunk of unecessary stuff folded away, still here for possible future reference.  *)
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
			DevCPL68k.Pea(item);
		END;
	END PushDynArrStack;

	PROCEDURE Copy*(VAR source, dest, destlen : DevCPL68k.Item; checklen, last: BOOLEAN);
	(* Generates code for COPY( source, dest ). dest may not be bigger than 32kB. *)
		VAR loop, lastchar, end : DevCPL68k.Label;
				temp, dbuf, sbuf : DevCPL68k.Item;
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
				DevCPL68k.Load( destlen );
				IF ~dest.acttyp.untagged THEN DevCPL68k.Format1( SUBQ, 1, destlen ) END
			ELSE
				MakeIntConst( dest.acttyp.n - 1, DevCPT.int32typ, destlen );
				DevCPL68k.Load( destlen )
			END;
			MakePostInc(desttyp, dest);
		END;
		MakePostInc(sourcetyp, source);
		loop := DevCPL68k.NewLabel;
		IF sourcetyp = dest.typ THEN
			IF sourceacttyp # shortString16typ THEN
				DevCPL68k.SetLabel( loop );
				DevCPL68k.Move( source, dest );
				DevCPL68k.DBcc( EQ, destlen.reg, loop );
			ELSE
				dbuf.mode := dreg; dbuf.reg := DevCPL68k.GetReg(); dbuf.bd := 0; dbuf.inxReg := None;
				dbuf.obj := NIL; dbuf.typ := DevCPT.char16typ; dbuf.acttyp := DevCPT.char16typ;
				DevCPL68k.SetLabel(loop);
				DevCPL68k.Move(source, dbuf);
				DevCPL68k.Format6(ANDI, 0FFH, dbuf);
				DevCPL68k.Move(dbuf, dest);
				DevCPL68k.DBcc(EQ, destlen.reg, loop);
				DevCPL68k.Free(dbuf);
			END;
		ELSE
			dbuf.mode := dreg; dbuf.reg := DevCPL68k.GetReg(); dbuf.bd := 0; dbuf.inxReg := None;
			dbuf.obj := NIL; dbuf.typ := dest.typ; dbuf.acttyp := dest.typ;
			sbuf := dbuf; sbuf.typ := sourcetyp; sbuf.acttyp := sourcetyp;
			IF sourcetyp = DevCPT.char8typ THEN
				DevCPL68k.Clr(dbuf);
			END;
			DevCPL68k.SetLabel(loop);
			DevCPL68k.Move(source, sbuf);
			IF sourcetyp = DevCPT.char16typ THEN
				lastchar := DevCPL68k.NewLabel;
				DevCPL68k.Jump(EQ, lastchar);
				DevCPL68k.Move(dbuf, dest);
				DevCPL68k.DBcc(false, destlen.reg, loop);
				end := DevCPL68k.NewLabel;
				DevCPL68k.Jump(true, end);
				DevCPL68k.SetLabel(lastchar);
				DevCPL68k.Move(dbuf, dest);
				DevCPL68k.SetLabel(end);
			ELSE (* no special testing and branching needed as upper half stays zero during the loop *)
				DevCPL68k.Move(dbuf, dest);
				DevCPL68k.DBcc(EQ, destlen.reg, loop);
			END;
			DevCPL68k.Free(dbuf);
		END;
		IF checklen THEN
			DevCPL68k.Trapcc(NE, inxTrap);
			IF ~last THEN
				temp.mode := areg; temp.reg := dest.reg; temp.bd := 0; temp.inxReg := None;
				temp.obj := NIL; temp.typ := DevCPT.int32typ; temp.acttyp := DevCPT.int32typ;
				DevCPL68k.Format1(SUBQ, SHORT(dest.typ.size), temp);
			END;
		ELSE
			dest.mode := predec;
			MakeIntConst( 0, dest.typ, temp );
			DevCPL68k.Move( temp, dest )
		END;
	END Copy;
	
	PROCEDURE Decrement*( VAR designator, expression : DevCPL68k.Item );
	(* Decrements the value of designator by expression *)
	VAR
		low, high, lowe, highe, addr, temp: DevCPL68k.Item;
		label: DevCPL68k.Label;
	BEGIN (* Decrement *)
		IF designator.typ.form = Int64 THEN
			IF (expression.mode = xreal) OR ((expression.mode = regx) & (expression.typ.form = Int64) & (expression.acttyp.form = Int64)) THEN (* handle subtraction in memory *)
				temp := designator; INC(temp.bd, 8);
				high.mode := predec; high.reg := DevCPL68k.GetAdrReg(); high.typ := DevCPT.int32typ; high.acttyp := DevCPT.int32typ;
				DevCPL68k.Lea(temp, high.reg);
				highe.mode := predec; highe.reg := DevCPL68k.GetAdrReg(); highe.typ := DevCPT.int32typ; highe.acttyp := DevCPT.int32typ;
				IF expression.mode = xreal THEN
					AddToSP(-8); (* make room for Int64 on stack *)
					ASSERT(expression.reg = DevCPL68k.xrealstack, 20);
					addr.mode := regx; addr.reg := 15; addr.bd := 8; addr.inxReg := None;
					addr.typ := NIL; addr.acttyp := NIL;
					temp.mode := regx; temp.reg := 15; temp.bd := 0; temp.inxReg := None;
					temp.typ := DevCPT.int64typ; temp.acttyp := DevCPT.int64typ;
					FromXReal(addr, temp);
				ELSE
					temp := expression;
				END;
				INC(temp.bd, 8);
				DevCPL68k.Lea(temp, highe.reg);
				temp.mode := dreg; temp.reg := 0; temp.typ := DevCPT.int16typ; temp.acttyp := DevCPT.int16typ;
				DevCPL68k.Format6(ADDI, 0, temp); (* clear the X bit in the condition codes *)
				DevCPL68k.SubX(highe, high);
				DevCPL68k.SubX(highe, high);
				DevCPL68k.Free(highe); DevCPL68k.Free(high);
				IF expression.mode = xreal THEN
					AddToSP(8);
				END;
			ELSE (* load operands, subtract, store *)
				addr := designator; addr.typ := DevCPT.int32typ; addr.acttyp := DevCPT.int32typ;
				high.mode := dreg; high.reg := DevCPL68k.GetReg(); high.typ := DevCPT.int32typ;
				low.mode := dreg; low.reg := DevCPL68k.GetReg(); low.typ := DevCPT.int32typ;
				highe.mode := dreg; highe.reg := DevCPL68k.GetReg(); highe.typ := DevCPT.int32typ;
				DevCPL68k.Move(addr, high);
				INC(addr.bd, 4);
				DevCPL68k.Move(addr, low);
				IF expression.mode = imm THEN
					ASSERT(expression.typ.form = Int64, 20);
					IF (expression.bd >= MIN(BYTE)) & (expression.bd <= MAX(BYTE)) THEN
						DevCPL68k.Moveq(SHORT(expression.bd), highe.reg);
					ELSE
						MakeIntConst(expression.bd, DevCPT.int32typ, temp);
						DevCPL68k.Move(temp, highe);
					END;
					IF (expression.ext > 0) & (expression.ext <= 8) THEN
						DevCPL68k.Format1(SUBQ, SHORT(expression.ext), low);
					ELSE
						DevCPL68k.Format6(SUBI, expression.ext, low);
					END;
				ELSIF (expression.mode = regx) OR (expression.mode = dreg) THEN
					DevCPL68k.Moveq(0, highe.reg);
					lowe := expression; lowe.typ := lowe.acttyp;
					IF lowe.mode = regx THEN
						DevCPL68k.Load(lowe);
					ELSIF lowe.typ.form = Int32 THEN
						DevCPL68k.Test(lowe);
					END;
					IF lowe.typ.form # Int32 THEN
						Convert(lowe, DevCPT.int32typ);
					END;
					label := DevCPL68k.NewLabel;
					DevCPL68k.Jump(PL, label);
					DevCPL68k.Moveq(-1, highe.reg);
					DevCPL68k.SetLabel(label);
					DevCPL68k.Format2(SUB, lowe, low);
					IF expression.mode # dreg THEN
						DevCPL68k.Free(lowe);
					END;
				ELSE (* other modes should not appear, either the compiler or my understanding of it is broken *)
					HALT(0);
				END;
				DevCPL68k.SubX(highe, high);
				DevCPL68k.Free(highe);
				DevCPL68k.Move(low, addr);
				DEC(addr.bd, 4);
				DevCPL68k.Move(high, addr);
				DevCPL68k.Free(high); DevCPL68k.Free(low);
			END;
		ELSE
			IF expression.mode = imm THEN
				IF ( expression.bd >= 0 ) & ( expression.bd <= 8 ) THEN
					DevCPL68k.Format1( SUBQ, SHORT( expression.bd ), designator );
				ELSE
					DevCPL68k.Format6( SUBI, expression.bd, designator );
				END; (* IF *)
			ELSE
				DevCPL68k.Format2( SUB, expression, designator );
			END; (* IF *)
		END;
	END Decrement;

	PROCEDURE Increment*( VAR designator, expression : DevCPL68k.Item );
	(* Increments the value of designator by expression *)
	VAR
		low, high, lowe, highe, addr, temp: DevCPL68k.Item;
		label: DevCPL68k.Label;
	BEGIN (* Increment *)
		IF designator.typ.form = Int64 THEN
			IF (expression.mode = xreal) OR ((expression.mode = regx) & (expression.typ.form = Int64) & (expression.acttyp.form = Int64)) THEN (* handle addition in memory *)
				temp := designator; INC(temp.bd, 8);
				high.mode := predec; high.reg := DevCPL68k.GetAdrReg(); high.typ := DevCPT.int32typ; high.acttyp := DevCPT.int32typ;
				DevCPL68k.Lea(temp, high.reg);
				highe.mode := predec; highe.reg := DevCPL68k.GetAdrReg(); highe.typ := DevCPT.int32typ; highe.acttyp := DevCPT.int32typ;
				IF expression.mode = xreal THEN
					AddToSP(-8); (* make room for Int64 on stack *)
					ASSERT(expression.reg = DevCPL68k.xrealstack, 20);
					addr.mode := regx; addr.reg := 15; addr.bd := 8; addr.inxReg := None;
					addr.typ := NIL; addr.acttyp := NIL;
					temp.mode := regx; temp.reg := 15; temp.bd := 0; temp.inxReg := None;
					temp.typ := DevCPT.int64typ; temp.acttyp := DevCPT.int64typ;
					FromXReal(addr, temp);
				ELSE
					temp := expression;
				END;
				INC(temp.bd, 8);
				DevCPL68k.Lea(temp, highe.reg);
				temp.mode := dreg; temp.reg := 0; temp.typ := DevCPT.int16typ; temp.acttyp := DevCPT.int16typ;
				DevCPL68k.Format6(ADDI, 0, temp); (* clear the X bit in the condition codes *)
				DevCPL68k.AddX(highe, high);
				DevCPL68k.AddX(highe, high);
				IF expression.mode = xreal THEN
					AddToSP(8);
				END;
			ELSE (* load operands, add, store *)
				addr := designator; addr.typ := DevCPT.int32typ; addr.acttyp := DevCPT.int32typ;
				high.mode := dreg; high.reg := DevCPL68k.GetReg(); high.typ := DevCPT.int32typ;
				low.mode := dreg; low.reg := DevCPL68k.GetReg(); low.typ := DevCPT.int32typ;
				highe.mode := dreg; highe.reg := DevCPL68k.GetReg(); highe.typ := DevCPT.int32typ;
				DevCPL68k.Move(addr, high);
				INC(addr.bd, 4);
				DevCPL68k.Move(addr, low);
				IF expression.mode = imm THEN
					ASSERT(expression.typ.form = Int64, 20);
					IF (expression.bd >= MIN(BYTE)) & (expression.bd <= MAX(BYTE)) THEN
						DevCPL68k.Moveq(SHORT(expression.bd), highe.reg);
					ELSE
						MakeIntConst(expression.bd, DevCPT.int32typ, temp);
						DevCPL68k.Move(temp, highe);
					END;
					IF (expression.ext > 0) & (expression.ext <= 8) THEN
						DevCPL68k.Format1(ADDQ, SHORT(expression.ext), low);
					ELSE
						DevCPL68k.Format6(ADDI, expression.ext, low);
					END;
				ELSIF (expression.mode = regx) OR (expression.mode = dreg) THEN
					DevCPL68k.Moveq(0, highe.reg);
					lowe := expression; lowe.typ := lowe.acttyp;
					IF lowe.mode = regx THEN
						DevCPL68k.Load(lowe);
					ELSIF lowe.typ.form = Int32 THEN
						DevCPL68k.Test(lowe);
					END;
					IF lowe.typ.form # Int32 THEN
						Convert(lowe, DevCPT.int32typ);
					END;
					label := DevCPL68k.NewLabel;
					DevCPL68k.Jump(PL, label);
					DevCPL68k.Moveq(-1, highe.reg);
					DevCPL68k.SetLabel(label);
					DevCPL68k.Format2(ADD, lowe, low);
					IF expression.mode # dreg THEN
						DevCPL68k.Free(lowe);
					END;
				ELSE (* other modes should not appear, either the compiler or my understanding of it is broken *)
					HALT(0);
				END;
				DevCPL68k.AddX(highe, high);
				DevCPL68k.Free(highe);
				DevCPL68k.Move(low, addr);
				DEC(addr.bd, 4);
				DevCPL68k.Move(high, addr);
				DevCPL68k.Free(high); DevCPL68k.Free(low);
			END;
		ELSE
			IF expression.mode = imm THEN
				IF ( expression.bd > 0 ) & ( expression.bd <= 8 ) THEN
					DevCPL68k.Format1( ADDQ, SHORT( expression.bd ), designator );
				ELSE
					DevCPL68k.Format6( ADDI, expression.bd, designator );
				END; (* IF *)
			ELSE
				DevCPL68k.Format2( ADD, expression, designator );
			END; (* IF *)
		END;
	END Increment;

	PROCEDURE Include*( VAR set, element : DevCPL68k.Item );
	(* set := set + { element } *)
		VAR temp : DevCPL68k.Item;
	BEGIN (* Include *)
		temp.reg := DevCPL68k.GetAdrReg();
		DevCPL68k.Lea(set, temp.reg);
		DevCPL68k.FreeReg(set);
		set.mode := regx; set.reg := temp.reg; set.bd := 0; set.inxReg := None; set.offsReg := None;
		temp := set;
		IF element.mode = imm THEN
			DevCPL68k.Format4( BSET, element.bd, temp );
		ELSE
			DevCPL68k.Format5( BSET, element, temp );
		END; (* IF *)
		DevCPL68k.Move( temp, set );
	END Include;

	PROCEDURE Exclude*( VAR set, element : DevCPL68k.Item );
	(* set := set - { element } *)
		VAR temp : DevCPL68k.Item;
	BEGIN (* Exclude *)
		temp.reg := DevCPL68k.GetAdrReg();
		DevCPL68k.Lea(set, temp.reg);
		DevCPL68k.FreeReg(set);
		set.mode := regx; set.reg := temp.reg; set.bd := 0; set.inxReg := None; set.offsReg := None;
		temp := set;
		IF element.mode = imm THEN
			DevCPL68k.Format4( BCLR, element.bd, temp );
		ELSE
			DevCPL68k.Format5( BCLR, element, temp );
		END; (* IF *)
		DevCPL68k.Move( temp, set );
	END Exclude;

	PROCEDURE EnterMod*;
	(* Generates code for the entry into the module. *)
	BEGIN (* EnterMod *)
		DevCPL68k.Enter(0);
	END EnterMod;

	PROCEDURE CopyDynArrs( par : DevCPT.Object );
	(* Copys the dynamic arrays which are value-parameters to the stack. *)
		VAR source, dest, ptr, size, negsize, newSP : DevCPL68k.Item;
				scale : SHORTINT;
	BEGIN (* CopyDynArrs *)
		WHILE par # NIL DO
			IF ( par.typ.comp = DynArr ) & ( par.mode = Var ) THEN
				MakeVar( par, source );
				Size( source, size, scale );
				DevCPL68k.Load( size );
				GetDynArrVal( source, FALSE );
				IF scale = 1 THEN (* align size to 4 bytes *)
					DevCPL68k.Format1( ADDQ, 3, size );
					DevCPL68k.Format13( ASh, -2, size );
					scale := 4;
				ELSIF scale = 2 THEN
					DevCPL68k.Format1( ADDQ, 1, size );
					DevCPL68k.Format13( ASh, -1, size );
					scale := 4;
				END; (* IF *)
				negsize.mode := dreg;
				negsize.typ := DevCPT.int32typ; negsize.acttyp := DevCPT.int32typ;
				negsize.reg := DevCPL68k.GetReg( );
				DevCPL68k.Move( size, negsize );
				DevCPL68k.Format7( NEG, negsize );
				newSP.mode := regx;
				newSP.typ := DevCPT.sysptrtyp; newSP.acttyp := DevCPT.sysptrtyp;
				newSP.reg := SP.reg;
				newSP.bd := 0;
				newSP.inxReg := negsize.reg;
				IF size.typ.form = Int32 THEN newSP.xsize := 1; ELSE newSP.xsize := 0; END;
				newSP.scale := DevCPL68k.Scale( scale );
				DevCPL68k.Lea( newSP, SP.reg );
				dest.mode := areg;
				dest.typ := DevCPT.sysptrtyp; dest.acttyp := DevCPT.sysptrtyp;
				dest.reg := DevCPL68k.GetAdrReg( );
				DevCPL68k.Move( SP, dest );
				dest.mode := regx;
				dest.typ := par.typ; dest.acttyp := par.typ;
				dest.bd := 0;
				dest.inxReg := None;
				MoveBlock( scale, size, source, dest, TRUE );
				ptr.mode := regx;
				ptr.typ := DevCPT.sysptrtyp; ptr.acttyp := DevCPT.sysptrtyp;
				ptr.reg := FP.reg;
				ptr.bd := par.adr;
				ptr.inxReg := None;
				DevCPL68k.Move( SP, ptr );
				DevCPL68k.FreeReg(source); DevCPL68k.FreeReg(dest); DevCPL68k.FreeReg(ptr);
				DevCPL68k.FreeReg(size); DevCPL68k.FreeReg(negsize); DevCPL68k.FreeReg(newSP);
			ELSIF (par.typ.form = Comp) & (par.mode = Var) & (par.typ.size > 4) THEN
				MakeIntConst(par.typ.size, DevCPT.int32typ, size);
				source.mode := regx;
				source.reg := FP.reg;
				source.typ := DevCPT.sysptrtyp; source.acttyp := DevCPT.sysptrtyp;
				source.bd := par.num;
				source.inxReg := None;
				dest.mode := areg;
				dest.typ := DevCPT.sysptrtyp; dest.acttyp := DevCPT.sysptrtyp;
				dest.reg := DevCPL68k.GetAdrReg();
				DevCPL68k.Move(source, dest);
				source.mode := regx;
				source.reg := dest.reg;
				source.bd := 0; source.inxReg := None;
				dest.mode := regx;
				dest.typ := DevCPT.sysptrtyp; dest.acttyp := DevCPT.sysptrtyp;
				dest.reg := FP.reg;
				dest.bd := par.adr;
				dest.inxReg := None;
				MoveBlock(1, size, source, dest, TRUE);
				DevCPL68k.FreeReg(source); DevCPL68k.FreeReg(dest); DevCPL68k.FreeReg(size);
			END; (* IF *)
			par := par.link;
		END; (* WHILE *)
	END CopyDynArrs;

	PROCEDURE EnterProc*( proc : DevCPT.Object );
	(* Generates code for the entry into a procedure. If ptrinit is set, the whole local variable area is initialized. *)
		VAR dest, losize, hisize, adrReg, sp, spr, applLimit : DevCPL68k.Item;
				dsize, i, sysflag : INTEGER;
				label, break : DevCPL68k.Label;
				callback: BOOLEAN;
				var, fld: DevCPT.Object;
				typ: DevCPT.Struct;
				ptrs: RECORD
						count: BYTE;
						offs: ARRAY 12 OF INTEGER;
					END;
	BEGIN (* EnterProc *)
		DevCPL68k.SetLabel(proc.adr); (* DevCPL68k.ClearHints implizit *)
		dsize := -proc.conval.intval2;
		DevCPL68k.Enter(-dsize);
		sysflag := proc.sysflag;
		callback := 1 IN SYSTEM.VAL(SET, sysflag);
IF proc.sysflag = 3 THEN HALT(120) END;
		IF callback THEN
			PushRegs(DevCPL68k.calleeSavedRegs);
			spr.reg := DevCPL68k.GetReg(); spr.mode := dreg;
			spr.typ := DevCPT.sysptrtyp; spr.acttyp := DevCPT.sysptrtyp;
			DevCPL68k.Move(SP, spr);
		END;
		IF ~(2 IN SYSTEM.VAL(SET, sysflag)) THEN
			sp.reg := DevCPL68k.GetReg(); sp.mode := dreg;
			sp.typ := DevCPT.sysptrtyp; sp.acttyp := DevCPT.sysptrtyp;
			DevCPL68k.Move(SP, sp); DevCPL68k.Format6(SUBI, 2048, sp);
			applLimit.mode := abs; applLimit.bd := 0130H;
			applLimit.typ := DevCPT.sysptrtyp; applLimit.acttyp := DevCPT.sysptrtyp;
			DevCPL68k.Cmp(applLimit, sp);
			DevCPL68k.FreeReg(sp);
			break := DevCPL68k.NewLabel; DevCPL68k.Jump(GT, break);
			DevCPL68k.Trapcc(true, stackTrap);
			DevCPL68k.SetLabel(break);
		END;
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
				dest.typ := DevCPT.sysptrtyp; dest.acttyp := DevCPT.sysptrtyp;
				dest.inxReg := None;
				dest.offsReg := None;
				WHILE ptrs.count > 0 DO
					DEC(ptrs.count);
					dest.bd := ptrs.offs[ptrs.count];
					DevCPL68k.Clr(dest);
				END;
			ELSIF dsize > 24 THEN
				adrReg.mode := areg;
				adrReg.typ := DevCPT.sysptrtyp; adrReg.acttyp := DevCPT.sysptrtyp;
				adrReg.reg := DevCPL68k.GetAdrReg( );
				DevCPL68k.Move( SP, adrReg );
				IF callback THEN
					MakeIntConst((8+5)*4, DevCPT.int32typ, hisize);
					DevCPL68k.Format3(ADD, hisize, adrReg.reg);
					DevCPL68k.FreeReg(hisize);
				END;
				adrReg.mode := postinc;
				IF dsize > 4 * MAX( SHORTINT ) THEN
					MakeIntConst( ( dsize DIV 4 - 1 ) DIV 10000H, DevCPT.int16typ, hisize );
					DevCPL68k.Load( hisize );
					MakeIntConst( ( dsize DIV 4 - 1 ) MOD 10000H, DevCPT.int16typ, losize );
					DevCPL68k.Load( losize );
					label := DevCPL68k.NewLabel;
					DevCPL68k.SetLabel( label );
					DevCPL68k.Clr(adrReg);
					DevCPL68k.DBcc( false, losize.reg, label );
					DevCPL68k.DBcc( false, hisize.reg, label );
					DevCPL68k.FreeReg(hisize);
				ELSE
					MakeIntConst( dsize DIV 4 - 1, DevCPT.int16typ, losize );
					DevCPL68k.Load( losize );
					label := DevCPL68k.NewLabel;
					DevCPL68k.SetLabel( label );
					DevCPL68k.Clr(adrReg);
					DevCPL68k.DBcc( false, losize.reg, label );
				END; (* IF *)
				IF dsize MOD 4 # 0 THEN (* varsize in 2 Byte-Chunks alloziert *)
					adrReg.typ := DevCPT.int16typ; adrReg.acttyp := DevCPT.int16typ;
					DevCPL68k.Clr(adrReg);
				END;
				DevCPL68k.FreeReg(adrReg); DevCPL68k.FreeReg(losize);
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
					DevCPL68k.Clr(dest);
					INC( dest.bd, 4 );
				END; (* FOR *)
				IF dsize MOD 4 # 0 THEN
					i := dsize MOD 4;
					IF i >= 2 THEN
						dest.typ := DevCPT.int16typ; dest.acttyp := DevCPT.int16typ;
						DevCPL68k.Clr(dest);
						INC(dest.bd, 2);
					END;
					IF ODD(i) THEN
						dest.typ := DevCPT.int8typ; dest.typ := DevCPT.int8typ;
						DevCPL68k.Clr(dest);
					END;
				END;
				DevCPL68k.FreeReg(dest);
			END; (* IF *)
		END; (* IF *)
		CopyDynArrs( proc.link );
		IF callback THEN
			MakeSPPredec(sp);
			DevCPL68k.Move(spr, sp);
			DevCPL68k.FreeReg(spr);
		END;
	END EnterProc;

	PROCEDURE Return*( proc : DevCPT.Object; withRes : BOOLEAN;  VAR result : DevCPL68k.Item );
	(* Generates code for returning from a procedure or a module (proc = NIL). 
		result contains the value that has to be returned in a reserved area on the stack, if withRes is TRUE. *)
		VAR sysflag: INTEGER; resadr, item : DevCPL68k.Item;
	BEGIN (* Return *)
		IF withRes THEN
			IF proc.typ.form IN RealSet THEN
				item.mode := regx; item.reg := FP.reg; item.bd := proc.conval.intval; item.inxReg := None;
				item.obj := NIL; item.typ := DevCPT.sysptrtyp; item.acttyp := DevCPT.sysptrtyp;
				resadr.mode := areg; resadr.reg := DevCPL68k.GetAdrReg();
				resadr.obj := NIL; resadr.typ := DevCPT.sysptrtyp; resadr.acttyp := DevCPT.sysptrtyp;
				DevCPL68k.Move(item, resadr);
				item.mode := regx; item.reg := resadr.reg; item.bd := 0; item.inxReg := None;
				IF result.mode # xreal THEN
					item.obj := NIL; item.typ := proc.typ; item.acttyp := proc.typ;
					ToXReal(result, item);
				ELSE
					item.obj := NIL; item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
					resadr.mode := regx; resadr.reg := 15; resadr.bd := 0; resadr.inxReg := None;
					resadr.obj := NIL; resadr.typ := DevCPT.int32typ; resadr.acttyp := DevCPT.int32typ;
					DevCPL68k.Move(resadr, item);
					INC(resadr.bd, 4); INC(item.bd, 4); DevCPL68k.Move(resadr, item);
					INC(resadr.bd, 4); INC(item.bd, 4); DevCPL68k.Move(resadr, item);
				END;
				DevCPL68k.FreeReg(item);
			ELSE
				item.mode := regx; item.reg := FP.reg; item.bd := proc.conval.intval; item.inxReg := None;
				item.obj := NIL; item.typ := proc.typ; item.acttyp := proc.typ;
				Assign(result, item);
				DevCPL68k.FreeReg(result);
			END;
		END; (* IF *)
		IF proc # NIL THEN
			sysflag := proc.sysflag;
			IF 1 IN SYSTEM.VAL(SET, sysflag) THEN
				MakeSPPostinc(item);
				DevCPL68k.Move(item, SP);
				PopRegs(DevCPL68k.calleeSavedRegs);
			END;
			IF withRes & (proc.typ.form IN RealSet) THEN
				DevCPL68k.Return(proc.conval.intval - ParOff + DevCPT.sysptrtyp.size);
			ELSE
				DevCPL68k.Return(proc.conval.intval - ParOff);
			END;
		ELSE
			DevCPL68k.Return(0);
		END;
	END Return;
	
	PROCEDURE WriteStaticLink*( obj : DevCPT.Object );
	(* Writes the static link of the given object to (A7) if necessary. *)
		VAR source, dest : DevCPL68k.Item;
				diff : SHORTINT;
	BEGIN (* WriteStaticLink *)
		IF ( obj # NIL ) & ( obj.mnolev > 0 ) & ( obj.mode = LProc ) THEN (* static link needed *)
			diff := SHORT(DevCPL68k.level - obj.mnolev);
			IF diff = 0 THEN (* local procedure *)
				source := FP;
			ELSE
				source.mode := regx;
				source.typ := DevCPT.sysptrtyp; source.acttyp := DevCPT.sysptrtyp;
				source.reg := FP.reg;
				source.bd := 8;
				source.inxReg := None;
				source.offsReg := None;
				IF diff > 1 THEN
					dest.mode := areg;
					dest.typ := DevCPT.sysptrtyp; dest.acttyp := DevCPT.sysptrtyp;
					dest.reg := DevCPL68k.GetAdrReg( );
					DevCPL68k.Move( source, dest );
					source.reg := dest.reg;
					WHILE diff > 2 DO
						DevCPL68k.Move( source, dest );
						DEC( diff );
					END; (* WHILE *)
				END; (* IF *)
			END; (* IF *)
			PushStack(source);
		END; (* IF *)
	END WriteStaticLink;

	PROCEDURE Call*( VAR item : DevCPL68k.Item; obj : DevCPT.Object );
	(* Calls the given procedure. *)
	VAR mod: DevCPT.Object;
	BEGIN (* Call *)
		IF ( obj # NIL ) & ( obj.mode = CProc ) THEN
			DevCPL68k.WriteCProc( obj^.conval^.ext );
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
			DevCPL68k.Format15( JSR, item );
		ELSIF item.mode = immL THEN
			IF obj = NIL THEN
				obj := item.obj;
			END;
			DevCPL68k.Bsr(obj.adr);
		ELSE (* proc var OR TProc *)
			DeRef( DevCPT.sysptrtyp, item, (obj # NIL) & (obj.mode = TProc) );
			DevCPL68k.Format15( JSR, item );
		END; (* IF *)
		DevCPL68k.ClearHints;
	END Call;

	PROCEDURE GetResult*( typ : DevCPT.Struct; savedRegs: SET; VAR res : DevCPL68k.Item );
	(* Returns the result of a function call. *)
		VAR item : DevCPL68k.Item;
	BEGIN (* GetResult *)
		IF typ.form IN RealSet THEN
			res.mode := xreal; res.reg := DevCPL68k.xrealstack;
(*			res.mode := freg; res.typ := typ; res.acttyp := typ; res.reg := DevCPL68k.GetFReg( );
			DevCPL68k.FMove( item, res ); *)
		ELSE
			IF FALSE (*savedRegs = {}*) THEN
				res.mode := postinc; res.reg := SP.reg; res.typ := typ; res.acttyp := typ;
			ELSE
				item.mode := postinc; item.reg := SP.reg; item.typ := typ; item.acttyp := typ;
				res.mode := dreg; res.typ := typ; res.acttyp := typ; res.reg := DevCPL68k.GetReg( );
				DevCPL68k.Move( item, res );
			END;
		END; (* IF *)
		res.inxReg := None; res.offsReg := None	(* necessary if next operation is Deref *)
	END GetResult;

	PROCEDURE TypeTest*( VAR item : DevCPL68k.Item; typ : DevCPT.Struct; guard, equal : BOOLEAN );
	(* Generates code for a type test. If equal is true, the two types have to be equal, if guard is true, a Trap is generated
		if the test fails. If both are false, only the condition codes are set. *)
		VAR tag : DevCPL68k.Item;
				savedRegs : SET;
	BEGIN (* TypeTest *)
		savedRegs := DevCPL68k.usedRegs;
		IF ~ equal THEN
			DeRef( DevCPT.sysptrtyp, item, FALSE );
			INC( item.bd, DevCPL68k.BaseTypeOffs + 4 * typ.extlev) ;
		END; (* IF *)
		DevCPL68k.Load( item );
		StaticTag( typ, tag );
		DevCPL68k.Cmp( tag, item );
		IF equal THEN
			DevCPL68k.Trapcc( NE, recTrap);
		ELSIF guard THEN
			DevCPL68k.Trapcc( NE, typTrap);
		ELSE
			MakeCocItem( EQ, item );
		END; (* IF *)
		DevCPL68k.usedRegs := savedRegs;
	END TypeTest;

	PROCEDURE Case*( VAR expression : DevCPL68k.Item; lo, hi : INTEGER; VAR label : DevCPL68k.Label; VAR jtAdr : INTEGER );
	(* Generates the initializing part of a case statement and allocates the jump table.
		label denotes the else part of the case statement, jtAdr is the address of the jump table. *)
		VAR loItem, jumpTabEntry, jumpAddress : DevCPL68k.Item;
	BEGIN (* Case *)
		DevCPL68k.Load( expression );
		IF expression.typ.form IN ByteSet THEN Convert( expression, DevCPT.int16typ ); END;
		MakeIntConst( lo, expression.typ, loItem );
		DevCPL68k.Format2( SUB, loItem, expression );
		DevCPL68k.Format6( CMPI, hi - lo, expression );
		DevCPL68k.Jump( HI, label );
(*		DevCPL68k.AllocBytes( jumpTab, 2 * ( hi - lo + 1 ), jtAdr );
		jumpTabEntry.mode := pcx;
		jumpTabEntry.typ := DevCPT.int16typ; jumpTabEntry.acttyp := DevCPT.int16typ;
		jumpTabEntry.bd := jtAdr - DevCPL68k.ConstSize - DevCPL68k.dsize; *)
		jumpTabEntry.inxReg := expression.reg;
		IF expression.typ.size = 4 THEN
			jumpTabEntry.xsize := 1;
		ELSE
			jumpTabEntry.xsize := 0;
			Convert( expression, DevCPT.int16typ );
		END; (* IF *)
		jumpTabEntry.scale := 1; (* 2 bytes *)
		DevCPL68k.Load( jumpTabEntry );
		jumpAddress.mode := pcx;
		jumpAddress.typ := DevCPT.sysptrtyp; jumpAddress.acttyp := DevCPT.sysptrtyp;
		jumpAddress.bd := 0;
		jumpAddress.inxReg := jumpTabEntry.reg;
		jumpAddress.xsize := 0; (* word *)
		jumpAddress.scale := 1; (* *2 *)
		DevCPL68k.Format15( JMP, jumpAddress );
	END Case;

	PROCEDURE Test*( VAR item : DevCPL68k.Item );
	(* Tests a boolean item and makes a coc item. fcoc items are left unchanged. *)
	BEGIN (* Test *)
		IF ( item.mode # coc ) & ( item.mode # fcoc ) THEN
			DevCPL68k.Load( item );
			DevCPL68k.Format7( TST, item );
			MakeCocItem( NE, item );
		END; (* IF *)
	END Test;

	PROCEDURE UpTo*( VAR low, high, res : DevCPL68k.Item );
	(* set constructor res := { low .. high }. *)
		VAR chkItem, leftShift, rightShift : DevCPL68k.Item;
	BEGIN (* UpTo *)
		res.mode := dreg;
		res.typ := DevCPT.settyp; res.acttyp := DevCPT.settyp;
		res.reg := DevCPL68k.GetReg( );
		IF rangeCheck THEN
			MakeIntConst( DevCPM.MaxSet, high.typ, chkItem );
			IF low.mode # imm THEN DevCPL68k.Chk( low, chkItem ); END;
			IF high.mode # imm THEN DevCPL68k.Chk( high, chkItem ); END;
		END; (* IF *)
		rightShift.mode := dreg;
		rightShift.typ := high.typ; rightShift.acttyp := high.typ;
		rightShift.reg := DevCPL68k.GetReg( );
		leftShift.mode := dreg;
		leftShift.typ := high.typ; leftShift.acttyp := high.typ;
		leftShift.reg := DevCPL68k.GetReg( );
		DevCPL68k.Moveq( DevCPM.MaxSet, rightShift.reg );
		DevCPL68k.Format2( SUB, high, rightShift );
		DevCPL68k.Move( rightShift, leftShift );
		DevCPL68k.Format2( ADD, low, leftShift );
		DevCPL68k.Moveq( -1, res.reg );
		DevCPL68k.Format14( LSh, 1, leftShift, res );
		DevCPL68k.Format14( LSh, 0, rightShift, res );
		DevCPL68k.FreeReg( high );
		DevCPL68k.FreeReg( low );
		DevCPL68k.FreeReg( leftShift );
		DevCPL68k.FreeReg( rightShift );
	END UpTo;

	PROCEDURE Abs*( VAR item : DevCPL68k.Item );
	(* Generates code for the calculation of the absolute value of the given item. *)
		VAR dst: DevCPL68k.Item; label : DevCPL68k.Label;
	BEGIN (* Abs *)
		IF item.typ.form IN RealSet THEN
			IF item.mode # xreal THEN
				INC(DevCPL68k.xrealstack); AddToSP(-12);
				dst.mode := regx; dst.reg := 15; dst.bd := 0; dst.inxReg := None;
				dst.obj := NIL; dst.typ := DevCPT.int32typ; dst.acttyp := DevCPT.int32typ;
				ToXReal(item, dst);
				DevCPL68k.FreeReg(item);
			END;
			item.mode := regx; item.reg := 15; item.bd := 0; item.inxReg := None;
			item.obj := NIL; item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
			MakeIntConst(0000FH, DevCPT.int16typ, dst);
			PushAdrStack(item); PushStack(dst);
			DevCPL68k.FP68K;
			item.mode := xreal; item.reg := DevCPL68k.xrealstack;
(*			DevCPL68k.Format8( FABS, item, item ); *)
		ELSE
			DevCPL68k.Load( item );
			label := DevCPL68k.NewLabel;
			DevCPL68k.Format7( TST, item );
			DevCPL68k.Jump( GE, label );
			DevCPL68k.Format7( NEG, item );
			DevCPL68k.SetLabel( label );
		END; (* IF *)
	END Abs;

	PROCEDURE Adr*( VAR item : DevCPL68k.Item );
	(* Generates code for the calculation of the address of the given item. *)
	VAR
		reg : SHORTINT;
		adr : DevCPL68k.Item;
	BEGIN (* Adr *)
		IF item.typ.comp = DynArr THEN
			GetDynArrAdr( item, adr );
			item := adr;
		ELSIF item.mode IN { regx, pcx } THEN
			reg := DevCPL68k.GetAdrReg( );
			DevCPL68k.Lea( item, reg );
			item.mode := areg;
			item.reg := reg;
		ELSIF item.mode = absL THEN
			item.mode := immL;
		ELSIF item.mode = immL THEN
			(* do nothing *)
		ELSE
			HALT( 94 );
		END; (* IF *)
		item.typ := DevCPT.sysptrtyp; item.acttyp := DevCPT.sysptrtyp;
	END Adr;

	PROCEDURE Cap*( VAR item : DevCPL68k.Item );
	(* Generates code for the calculation of CAP( item ). For characters only. *)
	BEGIN (* Cap *)
		DevCPL68k.Load( item );
		DevCPL68k.Format4( BCLR, 5, item );
	END Cap;

	PROCEDURE Neg*( VAR item : DevCPL68k.Item );
	(* Generates code for the calculation of -item. *)
	VAR dst: DevCPL68k.Item;
	BEGIN (* Neg *)
		IF item.typ.form IN RealSet THEN
			IF item.mode # xreal THEN
				INC(DevCPL68k.xrealstack); AddToSP(-12);
				dst.mode := regx; dst.reg := 15; dst.bd := 0; dst.inxReg := None;
				dst.obj := NIL; dst.typ := DevCPT.int32typ; dst.acttyp := DevCPT.int32typ;
				ToXReal(item, dst);
				DevCPL68k.FreeReg(item);
			END;
			item.mode := regx; item.reg := 15; item.bd := 0; item.inxReg := None;
			item.obj := NIL; item.typ := DevCPT.int32typ; item.acttyp := DevCPT.int32typ;
			MakeIntConst(0000DH, DevCPT.int16typ, dst);
			PushAdrStack(item); PushStack(dst);
			DevCPL68k.FP68K;
			item.mode := xreal; item.reg := DevCPL68k.xrealstack;
(*			DevCPL68k.Format8( FNEG, item, item ); *)
		ELSIF item.typ.form = Set THEN
			DevCPL68k.Load( item );
			DevCPL68k.Format7( NOT, item );
		ELSE
			DevCPL68k.Load( item );
			DevCPL68k.Format7( NEG, item );
		END; (* IF *)
	END Neg;

	PROCEDURE Not*( VAR item : DevCPL68k.Item );
	(* Generates code for the calculation of ~ item. For Booleans only. *)
		VAR tcond, fcond : INTEGER;
	BEGIN (* Not *)
		IF ( item.mode = coc ) OR ( item.mode = fcoc ) THEN
			tcond := item.bd DIV 10000H;
			fcond := item.bd MOD 10000H;
			item.bd := 10000H * fcond + tcond;
		ELSE
			DevCPL68k.Load( item );
			DevCPL68k.Format7( TST, item );
			MakeCocItem( EQ, item );
		END; (* IF *)
	END Not;

	PROCEDURE Odd*( VAR item : DevCPL68k.Item );
	(* Generates code for the calculation of ODD( item ). *)
	VAR temp: DevCPL68k.Item;
	BEGIN (* Odd *)
		IF item.typ.form =  Int64 THEN
			IF item.mode = xreal THEN
				IF item.reg # DevCPL68k.xrealstack THEN DevCPM.err(-811); END;
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
				DevCPL68k.Load(item);
				DevCPL68k.Format4(BTST, 0, item);
				MakeCocItem(NE, item);
				AddToSP(12+DevCPT.int64typ.size); DEC(DevCPL68k.xrealstack);
			ELSE
				IF item.mode IN {regx, abs, absL}  THEN
					item.typ := DevCPT.int32typ;
					INC(item.bd, 4);
					DevCPL68k.Load(item);
					DevCPL68k.Format4(BTST, 0, item);
					MakeCocItem(NE, item);
				ELSE
					HALT(0);
				END;
			END;
		ELSE
			DevCPL68k.Load( item );
			DevCPL68k.Format4( BTST, 0, item );
			MakeCocItem( NE, item );
		END;
	END Odd;

	PROCEDURE LoadCC*( VAR item : DevCPL68k.Item );
	(* If item.mode is coc or fcoc, the item is loaded into a data register. *)
		VAR temp : DevCPL68k.Item;
	BEGIN (* LoadCC *)
		IF item.mode IN { coc, fcoc } THEN
			temp := item;
			item.mode := dreg;
			item.typ := DevCPT.booltyp; item.acttyp := DevCPT.booltyp;
			item.reg := DevCPL68k.GetReg( );
			Assign( temp, item );
		END; (* IF *)
	END LoadCC;

	PROCEDURE RealDop(op: SHORTINT; swap: BOOLEAN; VAR source, dest: DevCPL68k.Item);
	VAR d, s, t: DevCPL68k.Item;
	BEGIN
		dest.typ := dest.acttyp; source.typ := source.acttyp;
		IF (dest.mode = xreal) & (source.mode = xreal) THEN
			IF ~((source.reg = DevCPL68k.xrealstack) & (dest.reg = DevCPL68k.xrealstack-1) OR
				(source.reg = DevCPL68k.xrealstack-1) & (dest.reg = DevCPL68k.xrealstack)) THEN DevCPM.err(-812) END;
			IF (source.reg = DevCPL68k.xrealstack) OR swap THEN
				d.mode := regx; d.reg := 15; d.bd := 12; d.inxReg := -1;
				d.obj := NIL; d.typ := DevCPT.int32typ; d.acttyp := DevCPT.int32typ;
				s.mode := regx; s.reg := 15; s.bd := 0; s.inxReg := -1;
				s.obj := NIL; s.typ := DevCPT.int32typ; s.acttyp := DevCPT.int32typ;
			ELSE
				INC(DevCPL68k.xrealstack); AddToSP(-12);
				d.mode := regx; d.reg := 15; d.bd := 0; d.inxReg := None;
				d.obj := NIL; d.typ := DevCPT.int32typ; d.acttyp := DevCPT.int32typ;
				s := d; s.bd := 24; DevCPL68k.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevCPL68k.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevCPL68k.Move(s, d);
				d.bd := 24; s.bd := 12; DevCPL68k.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevCPL68k.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevCPL68k.Move(s, d);
				d.bd := 12; s.bd := 0; DevCPL68k.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevCPL68k.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevCPL68k.Move(s, d);
				AddToSP(12); DEC(DevCPL68k.xrealstack);
				d.bd := 12; s.bd := 0;
			END;
			dest.reg := SHORT(DevCPL68k.xrealstack-1); source.reg := DevCPL68k.xrealstack;
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
				INC(DevCPL68k.xrealstack); AddToSP(-12);
				d.mode := regx; d.reg := 15; d.bd := 0; d.inxReg := None;
				d.obj := NIL; d.typ := DevCPT.int32typ; d.acttyp := DevCPT.int32typ;
				s := d; s.bd := 12; DevCPL68k.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevCPL68k.Move(s, d);
				INC(d.bd, 4); INC(s.bd, 4); DevCPL68k.Move(s, d);
				d.bd := 12; s.bd := 0;
				ToXReal(dest, d);
				DevCPL68k.FreeReg(dest);
				dest.mode := xreal; dest.reg := SHORT(DevCPL68k.xrealstack - 1);
				dest.typ := NIL; dest.acttyp := NIL;
				source.reg := DevCPL68k.xrealstack;
			END;
		ELSE
			INC(DevCPL68k.xrealstack); AddToSP(-12);
			d.mode := regx; d.reg := 15; d.bd := 0; d.inxReg := -1; d.obj := NIL;
			d.obj := NIL; d.typ := DevCPT.int32typ; d.acttyp := DevCPT.int32typ;
			ToXReal(dest, d);
			DevCPL68k.FreeReg(dest);
			dest.mode := xreal; dest.reg := DevCPL68k.xrealstack;
			dest.typ := NIL; dest.acttyp := NIL;
			s := source;
		END;
		IF source.mode IN {dreg, areg, imm, freg} THEN
			INC(DevCPL68k.xrealstack); AddToSP(-12); INC(d.bd, 12);
			s.mode := regx; s.reg := 15; s.bd := 0; s.inxReg := None;
			s.obj := NIL; s.typ := DevCPT.int32typ; s.acttyp := DevCPT.int32typ;
			ToXReal(source, s);
			DevCPL68k.FreeReg(source);
			source.mode := xreal; source.reg := DevCPL68k.xrealstack;
		END;
		IF source.mode = xreal THEN
			MakeIntConst(op, DevCPT.int16typ, t);
		ELSE
			MakeIntConst(XRealForm(source.typ.form, source.acttyp.size) + op, DevCPT.int16typ, t);
		END;
		PushAdrStack(s); IF d.reg = 15 THEN INC(d.bd, 4) END;
		PushAdrStack(d); PushStack(t);
		DevCPL68k.FP68K;
	END RealDop;
	
	PROCEDURE Compare*( kind : BYTE; VAR left, right, res : DevCPL68k.Item );
	(* Compares left and right and generates a coc- or fcoc-item. *)
		VAR tCond : SHORTINT;
				lbuf, rbuf, alias : DevCPL68k.Item;
				begLabel, endLabel : DevCPL68k.Label;
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
			IF (left.mode # xreal) OR (left.reg # DevCPL68k.xrealstack) THEN DevCPM.err(-813) END;
			AddToSP(12); DEC(DevCPL68k.xrealstack); left.mode := None;
(*			DevCPL68k.Format8( FCMP, right, left );
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
				lbuf.reg := DevCPL68k.GetReg( );
				rbuf.mode := dreg; rbuf.typ := lefttyp; rbuf.acttyp := lefttyp;
				rbuf.reg := DevCPL68k.GetReg( );
				begLabel := DevCPL68k.NewLabel;
				endLabel := DevCPL68k.NewLabel;
				IF lefttyp = righttyp THEN
					DevCPL68k.SetLabel( begLabel );
					DevCPL68k.Move( left, lbuf );
					DevCPL68k.Move( right, rbuf );
					IF leftacttyp = shortString16typ THEN
						DevCPL68k.Format6(ANDI, 0FFH, lbuf);
					END;
					IF rightacttyp = shortString16typ THEN
						DevCPL68k.Format6(ANDI, 0FFH, rbuf);
					END;
					DevCPL68k.Cmp( rbuf, lbuf );
					DevCPL68k.Jump( NE, endLabel );
					DevCPL68k.Format7( TST, lbuf );
					DevCPL68k.Jump( NE, begLabel );
					DevCPL68k.SetLabel( endLabel );
					DevCPL68k.Cmp( rbuf, lbuf );
				ELSE
					alias.mode := dreg; alias.bd := 0; alias.inxReg := None;
					alias.obj := NIL; alias.typ := DevCPT.char16typ; alias.acttyp := DevCPT.char16typ;
					IF lefttyp = DevCPT.char8typ THEN
						alias.reg := lbuf.reg;
					ELSE
						alias.reg := rbuf.reg
					END;
					DevCPL68k.Clr(alias);
					DevCPL68k.SetLabel(begLabel);
					DevCPL68k.Move(left, lbuf);
					DevCPL68k.Move(right, rbuf);
					IF lefttyp = DevCPT.char8typ THEN
						DevCPL68k.Cmp(rbuf, alias);
					ELSE
						DevCPL68k.Cmp(alias, lbuf);
					END;
					DevCPL68k.Jump(NE, endLabel);
					DevCPL68k.Format7(TST, alias);
					DevCPL68k.Jump(NE, begLabel);
					DevCPL68k.SetLabel(endLabel);
					IF lefttyp = DevCPT.char8typ THEN
						DevCPL68k.Cmp(rbuf, alias);
					ELSE
						DevCPL68k.Cmp(alias, lbuf);
					END;
				END;
				DevCPL68k.Free(lbuf); DevCPL68k.Free(rbuf);
			ELSE
				IF right.typ = DevCPT.niltyp THEN Convert( right, DevCPT.sysptrtyp ); END;
				LoadCC( left );
				LoadCC( right );
				DevCPL68k.Cmp( right, left );
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

	PROCEDURE MinMax* (VAR item, res: DevCPL68k.Item; min: BOOLEAN);
	VAR
		cmp, keep, top: DevCPL68k.Item;
		label: DevCPL68k.Label;
	BEGIN
		IF res.typ.form IN RealSet THEN
			keep.mode := regx; keep.reg := 15; keep.bd := 0; keep.inxReg := None;
			keep.obj := NIL; keep.typ := DevCPT.int32typ; keep.acttyp := DevCPT.int32typ;
			top := keep;
			IF (res.mode = xreal) & (item.mode = xreal) THEN
				ASSERT(((res.reg = DevCPL68k.xrealstack) & (item.reg = DevCPL68k.xrealstack-1)) OR ((res.reg = DevCPL68k.xrealstack-1) & (item.reg = DevCPL68k.xrealstack)), 20)
			ELSIF res.mode = xreal THEN
				ASSERT(res.reg = DevCPL68k.xrealstack, 21);
				INC(DevCPL68k.xrealstack); AddToSP(-12);
				ToXReal(item, top);
				DevCPL68k.FreeReg(item);
				item.mode := xreal; item.reg := DevCPL68k.xrealstack;
				item.typ := NIL; item.acttyp := NIL; item.obj := NIL
			ELSIF item.mode = xreal THEN
				ASSERT(item.reg = DevCPL68k.xrealstack, 22);
				INC(DevCPL68k.xrealstack); AddToSP(-12);
				ToXReal(res, top);
				DevCPL68k.FreeReg(res);
				res.mode := xreal; res.reg := DevCPL68k.xrealstack;
				res.typ := NIL; res.acttyp := NIL; res.obj := NIL
			ELSE (* neither item is on the xrealstack *)
				INC(DevCPL68k.xrealstack); AddToSP(-12);
				ToXReal(res, keep);
				DevCPL68k.FreeReg(res);
				res.mode := xreal; res.reg := DevCPL68k.xrealstack;
				res.typ := NIL; res.acttyp := NIL; res.obj := NIL;
				INC(DevCPL68k.xrealstack); AddToSP(-12);
				ToXReal(item, top);
				DevCPL68k.FreeReg(item);
				item.mode := xreal; item.reg := DevCPL68k.xrealstack;
				item.typ := NIL; item.acttyp := NIL; item.obj := NIL
			END;
			RealDop(8, FALSE, item, res);
			label := DevCPL68k.NewLabel;
			IF min THEN
				MakeCocItem(GE, cmp)
			ELSE
				MakeCocItem(LE, cmp)
			END;
			cmp.tJump := DevCPL68k.NewLabel;
			FalseJump(cmp, label);
			keep.bd := 12;
			DevCPL68k.Move(top, keep); INC(top.bd, 4); INC(keep.bd, 4);
			DevCPL68k.Move(top, keep); INC(top.bd, 4); INC(keep.bd, 4);
			DevCPL68k.Move(top, keep);
			DevCPL68k.SetLabel(label);
			AddToSP(12); DEC(DevCPL68k.xrealstack);
			item.mode := None
		ELSE
			DevCPL68k.Load(res);
			IF min THEN
				Compare(gtr, res, item, cmp)
			ELSE
				Compare(leq, res, item, cmp)
			END;
			label := DevCPL68k.NewLabel;
			cmp.tJump := DevCPL68k.NewLabel;
			FalseJump(cmp, label);
			DevCPL68k.Move(item, res);
			DevCPL68k.SetLabel(label)
		END
	END MinMax;
	
	PROCEDURE Plus*( typ : DevCPT.Struct; VAR source, dest : DevCPL68k.Item );
	(* Generates code for the addition dest := dest + source. *)
	BEGIN (* Plus *)
		IF typ.form IN RealSet THEN
			RealDop(0, TRUE, source, dest);
		ELSE
			DevCPL68k.AssertDestReg( typ, source, dest );
			IF typ.form = Set THEN
				DevCPL68k.Format2( oR, source, dest );
(*			ELSIF typ.form IN RealSet THEN
				DevCPL68k.Format8( FADD, source, dest );*)
			ELSE
				IF source.mode = imm THEN
					IF (source.bd > 0) & (source.bd < 8) THEN
						DevCPL68k.Format1(ADDQ, SHORT(source.bd), dest);
					ELSIF (source.bd < 0) & (source.bd > -8) THEN
						DevCPL68k.Format1(SUBQ, SHORT(-source.bd), dest);
					ELSIF source.bd # 0 THEN
						DevCPL68k.Format6( ADDI, source.bd, dest);
					END;
				ELSE
					DevCPL68k.Format2( ADD, source, dest );
				END;
			END; (* IF *)
		END;
	END Plus;

	PROCEDURE Minus*( typ : DevCPT.Struct; VAR source, dest : DevCPL68k.Item );
	(* Generates code for the subtraktion dest := dest - source. *)
	BEGIN (* Minus *)
		IF typ.form IN RealSet THEN
			RealDop(2, FALSE, source, dest);
		ELSE
			IF typ.form = Set THEN
				DevCPL68k.Load( dest );
				DevCPL68k.Load( source );
				DevCPL68k.Format7( NOT, source );
				DevCPL68k.Format2( AND, source, dest );
(*			ELSIF typ.form IN RealSet THEN
				DevCPL68k.FLoad( dest );
				DevCPL68k.Format8( FSUB, source, dest );*)
			ELSE
				DevCPL68k.Load( dest );
				IF source.mode = imm THEN
					IF (source.bd > 0) & (source.bd < 8) THEN
						DevCPL68k.Format1(SUBQ, SHORT(source.bd), dest);
					ELSIF (source.bd < 0) & (source.bd > -8) THEN
						DevCPL68k.Format1(ADDQ, SHORT(-source.bd), dest);
					ELSIF source.bd # 0 THEN
						DevCPL68k.Format6( SUBI, source.bd, dest);
					END;
				ELSE
					DevCPL68k.Format2( SUB, source, dest );
				END;
			END; (* IF *)
		END;
(*		DevCPL68k.FreeReg( source ); *)
	END Minus;

	PROCEDURE Mul*( typ : DevCPT.Struct; VAR source, dest : DevCPL68k.Item );
	(* Generates code for the multiplication dest := dest * source. *)
	VAR d, s, t: DevCPL68k.Item;
	BEGIN (* Mul *)
		IF typ.form IN RealSet THEN
			RealDop(4, TRUE, source, dest);
		ELSE
			DevCPL68k.AssertDestReg( typ, source, dest );
			IF typ.form = Set THEN
				DevCPL68k.Format2( AND, source, dest );
(*			ELSIF typ.form IN RealSet THEN
				DevCPL68k.Format8( FMUL, source, dest );*)
			ELSIF typ.form = Int8 THEN
				Convert( source, DevCPT.int16typ );
				Convert( dest, DevCPT.int16typ );
				DevCPL68k.Format11( MULS, source, dest );
			ELSIF typ.form = Int16 THEN
				DevCPL68k.Format11( MULS, source, dest );
			ELSIF typ.form = Int32 THEN
				Convert( source, DevCPT.int32typ );
				Convert( dest, DevCPT.int32typ );
				DevCPL68k.Format12( MULS, source, dest );
			END; (* IF *)
		END;
	END Mul;

	PROCEDURE Divide*( typ : DevCPT.Struct; VAR source, dest : DevCPL68k.Item );
	(* Generates code for the division dest := dest / source. *)
	BEGIN (* Divide *)
		IF typ.form IN RealSet THEN
			RealDop(6, FALSE, source, dest);
		ELSE
			IF typ.form = Set THEN
				DevCPL68k.Load( dest );
				DevCPL68k.Eor( source, dest );
(*			ELSE
				DevCPL68k.Format8( FDIV, source, dest );*)
			END; (* IF *)
		END;
	END Divide;

	PROCEDURE Div*( VAR source, dest : DevCPL68k.Item );
	(* Generates code for the integer division dest := dest DIV source. *)
		VAR corr, end : DevCPL68k.Label;
				remainder, dst, item: DevCPL68k.Item;
	BEGIN (* Div *)
		IF ~(dest.typ.form IN RealSet) THEN
			DevCPL68k.Load( dest );
			Convert( dest, DevCPT.int32typ );
			corr := DevCPL68k.NewLabel;
			end := DevCPL68k.NewLabel;
			IF source.typ.form = Int32 THEN
				remainder.mode := dreg;
				remainder.reg := DevCPL68k.GetReg( );
				remainder.typ := DevCPT.int32typ; remainder.acttyp := DevCPT.int32typ;
				DevCPL68k.Move(source, remainder);
				DevCPL68k.Eor(dest, remainder);
				DevCPL68k.Format4(BTST, 31, remainder);
				DevCPL68k.Jump(NE, corr);
				DevCPL68k.Divsl( source, remainder, dest );
				DevCPL68k.Jump(true, end);
				DevCPL68k.SetLabel(corr);
				DevCPL68k.Divsl( source, remainder, dest );
				DevCPL68k.Test(remainder);
				DevCPL68k.Jump(EQ, end);
				DevCPL68k.Format1( SUBQ, 1, dest );
				DevCPL68k.SetLabel(end);
				DevCPL68k.FreeReg(remainder);
			ELSE
				Convert( source, DevCPT.int16typ );
				remainder.mode := dreg;
				remainder.reg := DevCPL68k.GetReg( );
				remainder.typ := DevCPT.int16typ; remainder.acttyp := DevCPT.int16typ;
				DevCPL68k.Move(source, remainder);
				Convert(remainder, DevCPT.int32typ);
				DevCPL68k.Eor(dest, remainder);
				DevCPL68k.Format4(BTST, 31, remainder);
				DevCPL68k.Jump(NE, corr);
				DevCPL68k.Format11( DIVS, source, dest );
				DevCPL68k.Jump(true, end);
				DevCPL68k.SetLabel(corr);
				DevCPL68k.Format11( DIVS, source, dest );
				DevCPL68k.Move(dest, remainder);
				DevCPL68k.Swap(remainder);
				remainder.typ := DevCPT.int16typ; remainder.acttyp := DevCPT.int16typ;
				DevCPL68k.Test(remainder);
				DevCPL68k.Jump(EQ, end);
				DevCPL68k.Format1( SUBQ, 1, dest );
				DevCPL68k.SetLabel(end);
				DevCPL68k.FreeReg(remainder);
			END; (* IF *)
		ELSE
			RealDop(6, FALSE, source, dest);
			RoundDown;
			dst.mode := regx; dst.reg := 15; dst.bd := (DevCPL68k.xrealstack - dest.reg) * 12; dst.inxReg := None;
			dst.obj := NIL; dst.typ := DevCPT.int32typ; dst.acttyp := DevCPT.int32typ;
			MakeIntConst(00014H, DevCPT.int16typ, item);
			PushAdrStack(dst); PushStack(item);
			DevCPL68k.FP68K;
			RoundNearest;
		END;
	END Div;

	PROCEDURE Mod*( VAR source, dest : DevCPL68k.Item );
	(* Generates code for the remainder dest := dest MOD source.*)
		VAR corr, end : DevCPL68k.Label;
				remainder, zero, src, shift : DevCPL68k.Item;
	BEGIN (* Mod *)
		IF ~(dest.typ.form IN RealSet) THEN
			DevCPL68k.Load( source ); (* because it is used twice and may be a pc-item. *)
			DevCPL68k.Load( dest );
			Convert( dest, DevCPT.int32typ );
			corr := DevCPL68k.NewLabel;
			end := DevCPL68k.NewLabel;
			IF source.typ.form = Int32 THEN
				remainder.mode := dreg;
				remainder.reg := DevCPL68k.GetReg( );
				remainder.typ := DevCPT.int32typ; remainder.acttyp := DevCPT.int32typ;
				DevCPL68k.Move(source, remainder);
				DevCPL68k.Eor(dest, remainder);
				DevCPL68k.Format4(BTST, 31, remainder);
				DevCPL68k.Jump(NE, corr);
				DevCPL68k.Divsl( source, remainder, dest );
				DevCPL68k.Jump(true, end);
				DevCPL68k.SetLabel(corr);
				DevCPL68k.Divsl( source, remainder, dest );
				DevCPL68k.Test(remainder);
				DevCPL68k.Jump(EQ, end);
				DevCPL68k.Format2( ADD, source, remainder );
				DevCPL68k.SetLabel(end);
				DevCPL68k.FreeReg(dest);
				dest := remainder;
			ELSE
				Convert( source, DevCPT.int16typ );
				remainder.mode := dreg;
				remainder.reg := DevCPL68k.GetReg( );
				remainder.typ := DevCPT.int16typ; remainder.acttyp := DevCPT.int16typ;
				DevCPL68k.Move(source, remainder);
				Convert(remainder, DevCPT.int32typ);
				DevCPL68k.Eor(dest, remainder);
				DevCPL68k.Format4(BTST, 31, remainder);
				DevCPL68k.FreeReg(remainder);
				DevCPL68k.Jump(NE, corr);
				DevCPL68k.Format11( DIVS, source, dest );
				DevCPL68k.Swap( dest );
				DevCPL68k.Jump(true, end);
				DevCPL68k.SetLabel(corr);
				DevCPL68k.Format11( DIVS, source, dest );
				DevCPL68k.Swap( dest );
				remainder := dest;
				remainder.typ := DevCPT.int16typ; remainder.acttyp := DevCPT.int16typ;
				DevCPL68k.Test(remainder);
				DevCPL68k.Jump(EQ, end);
				DevCPL68k.Format2( ADD, source, dest );
				DevCPL68k.SetLabel(end);
			END; (* IF *)
			DevCPL68k.FreeReg( source );
		ELSE
			RealDop(12, FALSE, source, dest);
			end := DevCPL68k.NewLabel;
			remainder.mode := regx; remainder.reg := 15; remainder.bd := (DevCPL68k.xrealstack - dest.reg) * 12;
			remainder.inxReg := None; remainder.typ := DevCPT.int32typ; remainder.acttyp := DevCPT.int32typ;
			zero.mode := dreg; zero.reg := DevCPL68k.GetReg(); zero.bd := 0;
			zero.inxReg := None; zero.typ := DevCPT.int32typ; zero.acttyp := DevCPT.int32typ;
			DevCPL68k.Move(remainder, zero);
			DevCPL68k.Format6(ANDI, 07FFFFFFFH, zero);
			INC(remainder.bd, 4);
			DevCPL68k.Format2(oR, remainder, zero);
			INC(remainder.bd, 4); remainder.typ := DevCPT.int16typ; remainder.acttyp := DevCPT.int16typ;
			zero.typ := DevCPT.int16typ; zero.typ := DevCPT.int16typ;
			DevCPL68k.Format2(oR, remainder, zero);
			zero.typ := DevCPT.int32typ; zero.typ := DevCPT.int32typ;
			DevCPL68k.Test(zero);
			DevCPL68k.FreeReg(zero);
			DevCPL68k.Jump(EQ, end);
			remainder.mode := regx; remainder.reg := 15; remainder.bd := (DevCPL68k.xrealstack - dest.reg) * 12;
			remainder.inxReg := None; remainder.typ := DevCPT.int8typ; remainder.acttyp := DevCPT.int8typ;
			DevCPL68k.Load(remainder);
			IF source.mode = xreal THEN
				src.mode := regx; src.reg := 15; src.bd := (DevCPL68k.xrealstack - source.reg) * 12;
				src.inxReg := None; src.typ := DevCPT.int8typ; src.acttyp := DevCPT.int8typ;
			ELSIF source.acttyp.form = Int64 THEN
				src := source;
				src.typ := DevCPT.int8typ; src.acttyp := DevCPT.int8typ;
				DevCPL68k.Load(src);
			ELSE
				source.typ := source.acttyp;
				src := source;
				(* The following code has been replaced by a DevCPL68k.Load, as source will never be in a register.
				 Otherwise source would have been converted to xreal in RealDop:
				src.mode := dreg; src.reg := DevCPL68k.GetReg();
				DevCPL68k.Move(source, src); *)
				DevCPL68k.Load(src);
				IF source.typ.size = 4 THEN
					MakeIntConst(24, DevCPT.int16typ, shift);
					DevCPL68k.Format14(LSh, 0, shift, src);
				ELSIF source.typ.size = 2 THEN
					MakeIntConst(8, DevCPT.int16typ, shift);
					DevCPL68k.Format14(LSh, 0, shift, src);
				ELSIF source.typ.size # 1 THEN
					HALT(0);
				END;
			END;
			DevCPL68k.Eor(src, remainder);
			DevCPL68k.FreeReg(src);
			DevCPL68k.Format4(BTST, 7, remainder);
			DevCPL68k.Jump(EQ, end);
			RealDop(0, TRUE, source, dest);
			DevCPL68k.SetLabel(end);
		END;
	END Mod;

	PROCEDURE Mask*( mask : INTEGER; VAR dest : DevCPL68k.Item );
	(* Generates code for the calculation of dest := dest & ~mask. Used for MOD. *)
	BEGIN (* Mask *)
		DevCPL68k.Load( dest );
		DevCPL68k.Format6( ANDI, mask, dest );
	END Mask;

	PROCEDURE In*( VAR element, set, dest : DevCPL68k.Item );
	(* Generates code for the calculation of dest := element IN set. *)
	BEGIN (* In *)
		IF element.mode = imm THEN
			DevCPL68k.Format4( BTST, element.bd, set );
		ELSE
			DevCPL68k.Format5( BTST, element, set );
		END; (* IF *)
		MakeCocItem( NE, dest );
	END In;

	PROCEDURE Shift*( opcode : SHORTINT; VAR shift, dest : DevCPL68k.Item );
	(* Generates code for the calculation of ASH( dest, shift ), SYSTEM.LSH( dest, shift ) and SYSTEM.ROT( dest, shift ). *)
		VAR elseLabel, endLabel : DevCPL68k.Label;
	BEGIN (* Shift *)
		IF shift.mode = imm THEN
			IF shift.bd # 0 THEN
				IF ( shift.bd >= -8 ) & ( shift.bd <= 8 ) THEN
					DevCPL68k.Format13( opcode, SHORT( shift.bd ), dest );
				ELSE
					IF shift.bd < 0 THEN
						MakeIntConst( -shift.bd, DevCPT.int16typ, shift );
						DevCPL68k.Format14( opcode, 0, shift, dest );
					ELSE
						MakeIntConst( shift.bd, DevCPT.int16typ, shift );
						DevCPL68k.Format14( opcode, 1, shift, dest );
					END; (* IF *)
				END; (* IF *)
			END; (* IF *)
		ELSE (* shift must be tested, because the machine instructions only take positive shifts. *)
			elseLabel := DevCPL68k.NewLabel;
			endLabel := DevCPL68k.NewLabel;
			DevCPL68k.Load( shift );
			DevCPL68k.Load( dest );
			DevCPL68k.Format7( TST, shift );
			DevCPL68k.Jump( LT, elseLabel );
			DevCPL68k.Format14( opcode, 1, shift, dest );
			DevCPL68k.Jump( true, endLabel );
			DevCPL68k.SetLabel( elseLabel );
			DevCPL68k.Format7( NEG, shift );
			DevCPL68k.Format14( opcode, 0, shift, dest );
			DevCPL68k.SetLabel( endLabel );
		END; (* IF *)
	END Shift;

	PROCEDURE Trap*( nr : SHORTINT );
	(* Generates code for a trap. *)
	BEGIN (* Trap *)
		DevCPL68k.Trapcc( true, nr );
	END Trap;

	PROCEDURE SYSMove*( VAR sourceAdr, destAdr, length : DevCPL68k.Item );
	(* Generates code for SYSTEM.MOVE( sourceAdr, destAdr, length ). *)
		VAR source, dest : DevCPL68k.Item;
	BEGIN (* SYSMove *)
		source.mode := areg;
		source.typ := DevCPT.int32typ; source.acttyp := DevCPT.int32typ;
		source.reg := DevCPL68k.GetAdrReg( );
		Convert( sourceAdr, DevCPT.int32typ );
		DevCPL68k.Move( sourceAdr, source );
		source.mode := postinc;
		source.typ := DevCPT.int8typ; source.acttyp := DevCPT.int8typ;
		dest.mode := areg;
		dest.typ := DevCPT.int32typ; dest.acttyp := DevCPT.int32typ;
		dest.reg := DevCPL68k.GetAdrReg( );
		Convert( destAdr, DevCPT.int32typ );
		DevCPL68k.Move( destAdr, dest );
		dest.mode := postinc;
		dest.typ := DevCPT.int8typ; dest.acttyp := DevCPT.int8typ;
		MoveBlock( 1, length, source, dest, FALSE );
	END SYSMove;

	PROCEDURE SYSGet*( VAR adr, dest : DevCPL68k.Item );
	(* Generates code for SYSTEM.GET( adr, dest ). *)
		VAR adrReg : DevCPL68k.Item;
	BEGIN (* SYSGet *)
		adrReg.mode := areg;
		adrReg.typ := DevCPT.int32typ; adrReg.acttyp := DevCPT.int32typ;
		adrReg.reg := DevCPL68k.GetAdrReg( );
		DevCPL68k.Move( adr, adrReg );
		adrReg.mode := regx;
		adrReg.bd := 0;
		adrReg.typ := dest.typ; adrReg.acttyp := dest.typ;
		adrReg.inxReg := None;
		Assign( adrReg, dest );
	END SYSGet;

	PROCEDURE SYSPut*( VAR source, address : DevCPL68k.Item );
	(* Generates code for SYSTEM.PUT( source, address ). *)
		VAR adrReg : DevCPL68k.Item;
	BEGIN (* SYSPut *)
		adrReg.mode := areg;
		adrReg.typ := DevCPT.int32typ; adrReg.acttyp := DevCPT.int32typ;
		adrReg.reg := DevCPL68k.GetAdrReg( );
		address.typ := DevCPT.sysptrtyp; address.acttyp := DevCPT.sysptrtyp;
		DevCPL68k.Move( address, adrReg );
		adrReg.mode := regx;
		adrReg.typ := source.typ; adrReg.acttyp := source.typ;
		adrReg.bd := 0;
		adrReg.inxReg := None;
		Assign( source, adrReg );
	END SYSPut;

	PROCEDURE SYSGetReg*( VAR dest, sourceReg : DevCPL68k.Item );
	(* Generates code for SYSTEM.GETREG( sourceReg, dest ). *)
	BEGIN (* SYSGetReg *)
		sourceReg.reg := SHORT( sourceReg.bd );
		sourceReg.typ := dest.typ; sourceReg.acttyp := dest.typ;
		IF ( sourceReg.reg >= 0 ) & ( sourceReg.reg <= 7 ) THEN
			sourceReg.mode := dreg;
			DevCPL68k.Move( sourceReg, dest );
		ELSIF ( sourceReg.reg >= 8 ) & ( sourceReg.reg <= 15 ) THEN
			sourceReg.mode := areg;
			DevCPL68k.Move( sourceReg, dest );
		ELSIF ( sourceReg.reg >= 16 ) & ( sourceReg.reg <= 23 ) THEN
			sourceReg.mode := freg;
			DevCPL68k.FMove( sourceReg, dest );
		ELSE
			DevCPM.err( 220 );
		END; (* IF *)
	END SYSGetReg;

	PROCEDURE SYSPutReg*( VAR source, destReg : DevCPL68k.Item );
	(* Generates code for SYSTEM.PUTREG( destReg, source ). *)
	BEGIN (* SYSPutReg *)
		destReg.reg := SHORT( destReg.bd );
		IF ( destReg.bd >= 0 ) & ( destReg.bd <= 7 ) THEN
			destReg.mode := dreg;
			DevCPL68k.Move( source, destReg );
		ELSIF ( destReg.bd >= 8 ) & ( destReg.bd <= 15 ) THEN
			destReg.mode := areg;
			DevCPL68k.Move( source, destReg );
		ELSIF ( destReg.bd >= 16 ) & ( destReg.bd <= 23 ) THEN
			destReg.mode := freg;
			DevCPL68k.FMove( source, destReg );
		ELSE
			DevCPM.err( 220 );
		END; (* IF *)
	END SYSPutReg;

	PROCEDURE SYSBit*( VAR adr, bitnr, res : DevCPL68k.Item );
	(* Generates code for SYSTEM.BIT( adr, bitnr ). *)
		VAR adrItem : DevCPL68k.Item;
	BEGIN (* SYSBit *)
		adrItem.mode := areg;
		adrItem.reg := DevCPL68k.GetAdrReg( );
		adrItem.typ := DevCPT.sysptrtyp; adrItem.acttyp := DevCPT.sysptrtyp;
		adr.typ := DevCPT.sysptrtyp; adr.acttyp := DevCPT.sysptrtyp;
		DevCPL68k.Move( adr, adrItem );
		adrItem.mode := regx;
		adrItem.bd := 0;
		adrItem.inxReg := None;
		IF bitnr.mode = imm THEN
			DevCPL68k.Format4( BTST, bitnr.bd, adrItem );
		ELSE
			DevCPL68k.Format5( BTST, bitnr, adrItem );
		END; (* IF *)
		MakeCocItem( NE, res );
	END SYSBit;

BEGIN (* OPC *)
	FP.mode := areg;
	FP.typ := DevCPT.sysptrtyp; FP.acttyp := DevCPT.sysptrtyp;
	FP.reg := 14;
	SP.mode := areg;
	SP.typ := DevCPT.sysptrtyp; SP.acttyp := DevCPT.sysptrtyp;
	SP.reg := 15;
	NEW(shortString16typ); shortString16typ^ := DevCPT.string16typ^;
END DevCPC68k.