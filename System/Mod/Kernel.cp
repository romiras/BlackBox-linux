MODULE Kernel;
(* bh 25.02.02 *)

	(* TODO: Stack overflow is not cought *)

	(* bh 20.12.95 termination and finalization for COM *)
	(* bh 7.1.95 ole initialization *)
	(* bh 9.1.96 MarkLocals corrected *)
	(* bh 15.1.96 AddRef, Release, interface ptr finalization *)
	(* bh 20.1.96 new desc for basetypes *)
	(* bh 10.2.96 finalization changed *)
	(* bh 11.2.96 trap handling adapted to new compiler *)
	(* bh 20.2.96 interface trap handler *)
	(* bh 29.2.96 trap handling changed *)
	(* bh 27.3.96 new interface handling *)
	(* bh 18.4.96 FastCollect *)
	(* bh 18.4.96 Next inline *)
	(* bh 18.4.96 only one free list *)
	(* bh 18.4.96 allocLimit in NewBlock *)
	(* bh 23.4.96 NewObj corrected *)
	(* bh 23.4.96 MarkInterfaces *)
	(* bh 6.1.97 SourcePos adapted *)
	(* bh 2.5.97 V1.3 changes *)
	(* bh 22.9.98 simple memory allocation for DLLs *)
	(* dg 23.9.98 fixed "Millenium Bug", Time() returns LONGINT *)
	(* dg 01.10.98 trapped cleaners are now removed *)
	(* dg 01.10.98 added Kernel.RemoveCleaner *)
	(* dg	29.10.98	ASSERT added in PushTrapCleaner - asserts that a TrapCleaner is not pushed twice *)
	(* dg	11.02.99	fixed Kernel.Time (interval = 2*(MAX(INTEGER)+1) = 100000000L *)
	(* dg	16.02.99	Finalizer calls eliminated in FastCollect *)
	(* dg	24.02.99	added type Hook *)
	(* dg	09.03.99	Loader Hook *)
	(* bh	14.04.99	silent trap *)
	(* bh	31.08.99	strict stack sweep *)
	(* cp	23.11.99	CallFinalizers: array handling corrected (according to bh) *)
	(* ww 4.12.00	Max. heap memory incresed from 0.5 to 1.5 GByte (AllocHeapMem) *)
	(* bh 8.2.01 InitFpu eliminated, delayed for 1.5 *)
	(* bh 8.2.01 stack overflow handling *)
	(* ww 12.2.01 separate treatment of TrapChecker (to not have to install Views.TrapCleanup as a TrapViewer) *)
	(* ww 13.2.01 when growing heap, merge new area with free block (if any) at the end of the old heap *)
	(* ww 14.2.01 (re-)introduced separate free lists for small blocks *)
	(* ww 16.2.01 slight performance improvement in OldBlock *)
	(* ww 22.3.01 AllocHeapMem aligns cluster to 16 bytes if dllMem is true. c.max contains actual address of allocated memory *)
	(* ww 22.4.01 Changed FastCollect to call MarkFinObj instead of CheckFinalizers preventing moving obj. to hotFinalizers *)
	(* ww 24.4.01 CallFinalizers catches traps in Finalizers *)
	(* ww 27.4.01 Introduced "WouldFinalize" to allow applications to call Collect after FastCollect detected objects
						being due for finalization *)


	IMPORT LinDl, LinLibc, SYSTEM;
	
	CONST
		dllMem = TRUE; (* should be a variable, but for easier memory managment it is always true. *)
	
		strictStackSweep = TRUE;
		
		nameLen* = 256;

		littleEndian* = TRUE;
		timeResolution* = 1000;	(* ticks per second *)
		
		processor* = 10;	(* i386 *)

		objType* = "ocf";	(* file types *)
		symType* = "osf";
		docType* = "odc";
		
		(* loader constants *)
		done* = 0;
		fileNotFound* = 1;
		syntaxError* = 2;
		objNotFound* = 3;
		illegalFPrint* = 4;
		cyclicImport* = 5;
		noMem* = 6;
		commNotFound* = 7;
		commSyntaxError* = 8;
		moduleNotFound* = 9;

		any = 1000000;
		
		CX = 1;
		SP = 4;	(* register number of stack pointer *)
		FP = 5;	(* register number of frame pointer *)
		ML = 3;	(* register which holds the module list at program start *)
		
		N = 128 DIV 16;	(* free lists *)
		
		(* kernel flags in module desc *)
		init = 16; dyn = 17; dll = 24; iptrs = 30;
		
		(* meta interface consts *)
		mConst = 1; mTyp = 2; mVar = 3; mProc = 4; mField = 5;
		
		debug = FALSE;
		
		trapReturn = 1; (* Return value for sigsetjmp given from siglongjmp *)

		(* constants for the message boxes *) 
		mbClose* = -1; mbOk* = 0; mbCancel* =1; mbRetry* = 2; mbIgnore* = 3; mbYes* = 4; mbNo* = 5;

	
	TYPE
		Name* = ARRAY nameLen OF SHORTCHAR;
		Command* = PROCEDURE;
		Module* = POINTER TO RECORD [untagged]
			next-: Module;
			opts-: SET;	(* 0..15: compiler opts, 16..31: kernel flags *)
			refcnt-: INTEGER;	(* <0: module invalidated *)
			compTime-, loadTime-: ARRAY 6 OF SHORTINT;
			ext-: INTEGER;	(* currently not used *)
			term-: Command;	(* terminator *)
			nofimps-, nofptrs-: INTEGER;
			csize-, dsize-, rsize-: INTEGER;
			code-, data-, refs-: INTEGER;
			procBase-, varBase-: INTEGER;	(* meta base addresses *)
			names-: POINTER TO ARRAY [untagged] OF SHORTCHAR;	(* names[0] = 0X *)
			ptrs-: POINTER TO ARRAY [untagged] OF INTEGER;
			imports-: POINTER TO ARRAY [untagged] OF Module;
			export-: Directory;	(* exported objects (name sorted) *)
			name-: Name
		END;
		Type* = POINTER TO RECORD [untagged]
			(* record: ptr to method n at offset - 4 * (n+1) *)
			size-: INTEGER;	(* record: size, array: #elem, dyn array: 0, proc: sigfp *)
			mod-: Module;
			id-: INTEGER;	(* name idx * 256 + lev * 16 + attr * 4 + form *)
			base-: ARRAY 16 OF Type;
			fields-: Directory;	(* new fields (declaration order) *)
			ptroffs-: ARRAY any OF INTEGER	(* array of any length *)
		END;
		Object* = POINTER TO ObjDesc;
		ObjDesc* = RECORD [untagged]
			fprint-: INTEGER;
			offs-: INTEGER;	(* pvfprint for record types *)
			id-: INTEGER;	(* name idx * 256 + vis * 16 + mode *)
			struct-: Type	(* id of basic type or pointer to typedesc *)
		END;
		Directory* = POINTER TO RECORD [untagged]
			num-: INTEGER;	(* number of entries *)
			obj-: ARRAY any OF ObjDesc	(* array of any length *)
		END;

		Handler* = PROCEDURE;
		
		Reducer* = POINTER TO ABSTRACT RECORD
			next: Reducer
		END;

		Identifier* = ABSTRACT RECORD
			typ*: INTEGER;
			obj-: ANYPTR
		END;
		
		TrapCleaner* = POINTER TO ABSTRACT RECORD
			next: TrapCleaner
		END;
		
		TryHandler* = PROCEDURE (a, b, c: INTEGER);
		
		
		(* meta extension suport *)
		
		ItemExt* = POINTER TO ABSTRACT RECORD END;
		
		ItemAttr* = RECORD
			obj*, vis*, typ*, adr*: INTEGER;
			mod*: Module;
			desc*: Type;
			ptr*: SYSTEM.PTR;
			ext*: ItemExt
		END;
	
		Hook* = POINTER TO ABSTRACT RECORD END;
		
		LoaderHook* = POINTER TO ABSTRACT RECORD (Hook) 
			res*: INTEGER;
			importing*, imported*, object*: ARRAY 256 OF CHAR
		END;

		GuiHook* = POINTER TO ABSTRACT RECORD (Hook) END; (* Implemented by HostGnome *)
		
		Block = POINTER TO RECORD [untagged]
			tag: Type;
			last: INTEGER;		(* arrays: last element *)
			actual: INTEGER;	(* arrays: used during mark phase *)
			first: INTEGER		(* arrays: first element *)
		END;
		FreeBlock = POINTER TO FreeDesc;
		FreeDesc = RECORD [untagged]
			tag: Type;		(* f.tag = ADR(f.size) *)
			size: INTEGER;
			next: FreeBlock
		END;
		Cluster = POINTER TO RECORD [untagged]
			size: INTEGER;	(* total size *)
			next: Cluster;
			max: INTEGER
			(* start of first block *)
		END;

		FList = POINTER TO RECORD
			next: FList;
			blk: Block;
			iptr, aiptr: BOOLEAN
		END;

		CList = POINTER TO RECORD
			next: CList;
			do: Command;
			trapped: BOOLEAN
		END;

	
		PtrType = RECORD v: SYSTEM.PTR END;	(* used for array of pointer *)
		Char8Type = RECORD v: SHORTCHAR END;
		Char16Type = RECORD v: CHAR END;
		Int8Type = RECORD v: BYTE END;
		Int16Type = RECORD v: SHORTINT END;
		Int32Type = RECORD v: INTEGER END;
		Int64Type = RECORD v: LONGINT END;
		BoolType = RECORD v: BOOLEAN END;
		SetType = RECORD v: SET END;
		Real32Type = RECORD v: SHORTREAL END;
		Real64Type = RECORD v: REAL END;
		ProcType = RECORD v: PROCEDURE END;
		UPtrType = RECORD v: INTEGER END;
(*		IntPtrType = RECORD p: COM.IUnknown END;	(* used for array of interface pointer *)*)
		
		StrPtr = POINTER TO ARRAY [untagged] OF SHORTCHAR;
		
(*		IntPtr = POINTER TO RECORD [untagged] p: COM.IUnknown END;*)

(*		ExcpFramePtr = POINTER TO RECORD (KERNEL32.ExcpFrm)
			par: INTEGER
		END;*)
(*		
		Interface = POINTER TO RECORD	(* COMPILER DEPENDENT *)
			vtab: INTEGER;
			ref: INTEGER;	(* must correspond to Block.actual *)
			unk: COM.IUnknown
		END;*)

		(* Linux specific boot loader info. Record must be identical to struct in the loader. *)
		BootInfo* = POINTER TO RECORD [untagged]
			modList: Module;
			argc-: INTEGER;
			argv-: LinLibc.StrArray
		END;

	VAR
		baseStack: INTEGER;	(* modList, root, and baseStack must be together for remote debugging *)
		root: Cluster;	(* cluster list *)
		modList-: Module; (* root of module list *)
		trapCount-: INTEGER;
		err-, pc-, sp-, fp-, stack-, val-: INTEGER;
		comSig-: INTEGER;	(* command signature *)
		
		free: ARRAY N OF FreeBlock;	(* free list *)
		sentinelBlock: FreeDesc;
		sentinel: FreeBlock;
		candidates: ARRAY 1024 OF INTEGER;
		nofcand: INTEGER;
		allocated: INTEGER;	(* bytes allocated on BlackBox heap *)
		total: INTEGER;	(* current total size of BlackBox heap *)
		used: INTEGER;	(* bytes allocated on system heap *)
		finalizers: FList;
		hotFinalizers: FList;
		cleaners: CList;
		reducers: Reducer;
		trapStack: TrapCleaner;
		actual: Module;	(* valid during module initialization *)
		
		res: INTEGER;	(* auxiliary global variables used for trap handling *)
		old: SET;
		
		trapViewer, trapChecker: Handler;
		trapped, guarded, secondTrap: BOOLEAN;
		interrupted: BOOLEAN;
		static, inDll, terminating: BOOLEAN;
		retAd: INTEGER;
		restart: Command;
		
(*		heap: LinLibc.PtrVoid;  (*heap: KERNEL32.Handle;*)*)
(*		excpPtr: KERNEL32.ExcpFrmPtr;*)
(*		mainThread: KERNEL32.Handle;*)
		
		told, shift: INTEGER; (* used in Time() *)
		
		loader: LoaderHook;
		loadres: INTEGER;
		
		wouldFinalize: BOOLEAN;
		
		watcher*: PROCEDURE (event: INTEGER);	(* for debug *)
		
		loopContext: LinLibc.sigjmp_buf; (* trap return context, if no Kernel.Try has been used. *)
		currentTryContext: POINTER TO LinLibc.sigjmp_buf; (* trap return context, if Kernel.Try has been used. *)
		
		guiHook: GuiHook;
		
		cmdLine-: ARRAY 1024 OF CHAR;
		(* !!! This variable has to be the last variable in the list.  !!! *)
		bootInfo-: BootInfo;

	
	(* code procedures for exception handling *)
		
	PROCEDURE [1] PushFP 055H;
	PROCEDURE [1] PopFP 05DH;
	PROCEDURE [1] PushBX 053H;
	PROCEDURE [1] PopBX 05BH;
	PROCEDURE [1] PushSI 056H;
	PROCEDURE [1] PopSI 05EH;
	PROCEDURE [1] PushDI 057H;
	PROCEDURE [1] PopDI 05FH;
	PROCEDURE [1] LdSP8 08DH, 065H, 0F8H;
	PROCEDURE [1] Return0 (ret: INTEGER) 0C3H;
	PROCEDURE [1] ReturnCX (ret: INTEGER) 05AH, 001H, 0CCH, 0FFH, 0E2H;	(* POP DX; ADD SP,CX; JP DX *)
	PROCEDURE [1] FPageWord (offs: INTEGER): INTEGER 64H, 8BH, 0H;	(* MOV EAX,FS:[EAX] *)
	
	(* code procedures for fpu *)
	
	PROCEDURE [1] FINIT 0DBH, 0E3H;
	PROCEDURE [1] FLDCW 0D9H, 06DH, 0FCH;	(* -4, FP *)
	PROCEDURE [1] FSTCW 0D9H, 07DH, 0FCH;	(* -4, FP *)
	
	(* code procedure for memory erase *)
	
	PROCEDURE [code] Erase (adr, words: INTEGER)	
		089H, 0C7H,	(* MOV EDI, EAX *)
		031H, 0C0H,	(* XOR EAX, EAX *)
		059H,			(* POP ECX *)
		0F2H, 0ABH;	(* REP STOS *)
		
	(* code procedure for stack allocate *)
	
	PROCEDURE [code] ALLOC (* argument in CX *)
(*
	PUSH	EAX
	ADD	ECX,-5
	JNS	L0
	XOR	ECX,ECX
L0: AND	ECX,-4	(n-8+3)/4*4
	MOV	EAX,ECX
	AND	EAX,4095
	SUB	ESP,EAX
	MOV	EAX,ECX
	SHR	EAX,12
	JEQ	L2
L1: PUSH	0
	SUB	ESP,4092
	DEC	EAX
	JNE	L1
L2: ADD	ECX,8
	MOV	EAX,[ESP,ECX,-4]
	PUSH	EAX
	MOV	EAX,[ESP,ECX,-4]
	SHR	ECX,2
	RET
*);
		
	(* code procedures for COM support *)
	
	PROCEDURE [code] ADDREF
(*
	MOV	ECX,[ESP,4]
	INC	[ECX,4]
	MOV	EAX,[ECX,8]
	OR	EAX,EAX
	JE	L1
	PUSH	EAX
	MOV	EAX,[EAX]
	CALL	[EAX,4]
	MOV	ECX,[ESP,4]
L1: MOV	EAX,[ECX,4]
	RET	4
*)
		08BH, 04CH, 024H, 004H,
		0FFH, 041H, 004H,
		08BH, 041H, 008H,
		009H, 0C0H,
		074H, 00AH,
		050H,
		08BH, 000H,
		0FFH, 050H, 004H,
		08BH, 04CH, 024H, 004H,
		08BH, 041H, 004H,
		0C2H, 004H, 000H;

	PROCEDURE [code] RELEASE
(*	
	MOV	ECX,[ESP,4]
	MOV	EAX,[ECX,8]
	OR	EAX,EAX
	JE	L1
	PUSH	EAX
	MOV	EAX,[EAX]
	CALL	[EAX,8]
	MOV	ECX,[ESP,4]
L1: DEC	[ECX,4]
	MOV	EAX,[ECX,4]
	RET	4
*)
		08BH, 04CH, 024H, 004H,
		08BH, 041H, 008H,
		009H, 0C0H,
		074H, 00AH,
		050H,
		08BH, 000H,
		0FFH, 050H, 008H,
		08BH, 04CH, 024H, 004H,
		0FFH, 049H, 004H,
		08BH, 041H, 004H,
		0C2H, 004H, 000H;
		
	PROCEDURE [code] CALLREL
(*
	MOV     EAX,[ESP,4]
	CMP     [EAX,4],1
	JNE     L1
	PUSH    ESI
	PUSH    EDI
	PUSH    EAX
	MOV	EAX,[EAX,-4]
	CALL    [EAX,-8]
	POP     EDI
	POP     ESI
L1:
*)
		08BH, 044H, 024H, 004H,
		083H, 078H, 004H, 001H,
		075H, 00BH,
		056H,
		057H,
		050H,
		08BH, 040H, 0FCH,
		0FFH, 050H, 0F8H,
		05FH,
		05EH;
	
	PROCEDURE (VAR id: Identifier) Identified* (): BOOLEAN,	NEW, ABSTRACT;
	PROCEDURE (r: Reducer) Reduce* (full: BOOLEAN),	NEW, ABSTRACT;
	PROCEDURE (c: TrapCleaner) Cleanup*,	NEW, EMPTY;
	
	
	(* meta extension suport *)
		
	PROCEDURE (e: ItemExt) Lookup* (name: ARRAY OF CHAR; VAR i: ANYREC), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) Index* (index: INTEGER; VAR elem: ANYREC), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) Deref* (VAR ref: ANYREC), NEW, ABSTRACT;
	
	PROCEDURE (e: ItemExt) Valid* (): BOOLEAN, NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) Size* (): INTEGER, NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) BaseTyp* (): INTEGER, NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) Len* (): INTEGER, NEW, ABSTRACT;
	
	PROCEDURE (e: ItemExt) Call* (OUT ok: BOOLEAN), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) BoolVal* (): BOOLEAN, NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) PutBoolVal* (x: BOOLEAN), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) CharVal* (): CHAR, NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) PutCharVal* (x: CHAR), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) IntVal* (): INTEGER, NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) PutIntVal* (x: INTEGER), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) LongVal* (): LONGINT, NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) PutLongVal* (x: LONGINT), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) RealVal* (): REAL, NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) PutRealVal* (x: REAL), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) SetVal* (): SET, NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) PutSetVal* (x: SET), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) PtrVal* (): ANYPTR, NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) PutPtrVal* (x: ANYPTR), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) GetSStringVal* (OUT x: ARRAY OF SHORTCHAR; OUT ok: BOOLEAN), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) PutSStringVal* (IN x: ARRAY OF SHORTCHAR; OUT ok: BOOLEAN), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) GetStringVal* (OUT x: ARRAY OF CHAR; OUT ok: BOOLEAN), NEW, ABSTRACT;
	PROCEDURE (e: ItemExt) PutStringVal* (IN x: ARRAY OF CHAR; OUT ok: BOOLEAN), NEW, ABSTRACT;
	
	
	(* -------------------- miscellaneous tools -------------------- *)
	
	
	PROCEDURE Msg (IN str: ARRAY OF CHAR);
		VAR ss: ARRAY 1024 OF SHORTCHAR; res, l: INTEGER;
	BEGIN
		ss := SHORT(str);
		l := LEN(ss$);
		ss[l] := 0AX; ss[l + 1] := 0X;
		res := LinLibc.printf(ss);
	END Msg;
	
	PROCEDURE Int (x: LONGINT);
		VAR j, k: INTEGER; ch: CHAR; a, s: ARRAY 32 OF CHAR;
	BEGIN
		IF x # MIN(LONGINT) THEN
			IF x < 0 THEN s[0] := "-"; k := 1; x := -x ELSE k := 0 END;
			j := 0; REPEAT a[j] := CHR(x MOD 10 + ORD("0")); x := x DIV 10; INC(j) UNTIL x = 0
		ELSE
			a := "8085774586302733229"; s[0] := "-"; k := 1;
			j := 0; WHILE a[j] # 0X DO INC(j) END
		END;
		ASSERT(k + j < LEN(s), 20);
		REPEAT DEC(j); ch := a[j]; s[k] := ch; INC(k) UNTIL j = 0;
		s[k] := 0X;
		Msg(s);
	END Int;
	
	PROCEDURE (h: GuiHook)  MessageBox* (title, msg: ARRAY OF CHAR; buttons: SET): INTEGER, NEW, ABSTRACT;
	PROCEDURE (h: GuiHook)  Beep*, NEW, ABSTRACT;
	
	(* Is extended by HostGnome to show dialogs. If no dialog is present or
	    if the dialog is not closed by using one button, then "mbClose" is returned *)
	PROCEDURE MessageBox* (title, msg: ARRAY OF CHAR; buttons: SET): INTEGER;
		VAR res: INTEGER;
	BEGIN
		IF guiHook # NIL THEN
			res := guiHook.MessageBox(title, msg, buttons)
		ELSE
			Msg(" ");
			Msg("****");
			Msg("* " + title);
			Msg("* " + msg);
			Msg("****");
			res := mbClose;
		END;
		RETURN res
	END MessageBox;
	
	PROCEDURE SetGuiHook* (hook: GuiHook);
	BEGIN
		guiHook := hook
	END SetGuiHook;

	PROCEDURE SplitName* (name: ARRAY OF CHAR; VAR head, tail: ARRAY OF CHAR);
		(* portable *)
		VAR i, j: INTEGER; ch, lch: CHAR;
	BEGIN
		i := 0; ch := name[0];
		REPEAT
			head[i] := ch; lch := ch; INC(i); ch := name[i]
		UNTIL (ch = 0X)
			OR ((ch >= "A") & (ch <= "Z") OR (ch >= "À") & (ch # "×") & (ch <= "Þ"))
				& ((lch < "A") OR (lch > "Z") & (lch < "À") OR (lch = "×") OR (lch > "Þ"));
		head[i] := 0X; j := 0;
		WHILE ch # 0X DO tail[j] := ch; INC(i); INC(j); ch := name[i] END;
		tail[j] := 0X;
		IF tail = "" THEN tail := head$; head := "" END
	END SplitName;
	
	PROCEDURE MakeFileName* (VAR name: ARRAY OF CHAR; type: ARRAY OF CHAR);
		VAR i, j: INTEGER; ext: ARRAY 8 OF CHAR; ch: CHAR;
	BEGIN
		i := 0;
		WHILE (name[i] # 0X) & (name[i] # ".") DO INC(i) END;
		IF name[i] = "." THEN
			IF name[i + 1] = 0X THEN name[i] := 0X END
		ELSIF i < LEN(name) - 4 THEN
			IF type = "" THEN ext := docType ELSE ext := type$ END;
			name[i] := "."; INC(i); j := 0; ch := ext[0];
			WHILE ch # 0X DO
				IF (ch >= "A") & (ch <= "Z") THEN
					ch := CHR(ORD(ch) + ORD("a") - ORD("A"))
				END;
				name[i] := ch; INC(i); INC(j); ch := ext[j]
			END;
			name[i] := 0X
		END
	END MakeFileName;
	
	PROCEDURE Time* (): LONGINT;
		VAR t: INTEGER;
	BEGIN
		t := LinLibc.clock() DIV (LinLibc.CLOCKS_PER_SECOND DIV 1000); (* processor time to milliseconds *)
		IF t < told THEN INC(shift) END;
		told := t;
		RETURN shift * 100000000L + t
	END Time;
	
	PROCEDURE Beep* ();
		VAR ss: ARRAY 2 OF SHORTCHAR;
	BEGIN
		IF guiHook # NIL THEN
			guiHook.Beep
		ELSE
			ss[0] := 007X; ss[1] := 0X;
			res := LinLibc.printf(ss); res := LinLibc.fflush(LinLibc.NULL)
		END
	END Beep;
	
	PROCEDURE SearchProcVar* (var: INTEGER; VAR m: Module; VAR adr: INTEGER);
	BEGIN
		adr := var; m := NIL;
		IF var # 0 THEN
			m := modList;
			WHILE (m # NIL) & ((var < m.code) OR (var >= m.code + m.csize)) DO m := m.next END;
			IF m # NIL THEN DEC(adr, m.code) END
		END
	END SearchProcVar;


	(* -------------------- system memory management --------------------- *)
	
	PROCEDURE GrowHeapMem (size: INTEGER; VAR c: Cluster);
		(* grow to at least size bytes, typically at least 256 kbytes are allocated *)
		CONST N = 262144;
		VAR adr, s: INTEGER;
	BEGIN
		ASSERT(size >= c.size, 100); 
		IF size <= c.max THEN
			s := (size + (N - 1)) DIV N * N;
(*			adr := KERNEL32.VirtualAlloc(SYSTEM.VAL(INTEGER, c), s, {12}, {6});	(* commit; exec, read, write *)*)
			adr := LinLibc.calloc(1, s);
			IF adr # 0 THEN
				INC(used, s - c.size); INC(total, s - c.size); c.size := s
			END
		END
		(* post: (c.size unchanged) OR (c.size >= size) *)
	END GrowHeapMem;

	PROCEDURE AllocHeapMem (size: INTEGER; VAR c: Cluster);
		(* allocate at least size bytes, typically at least 256 kbytes are allocated *)
		CONST M = 1536 * 100000H;	(* 1.5 GByte *)
		CONST N = 65536;	(* cluster size for dll *)
		VAR adr, s: INTEGER;
	BEGIN
		IF dllMem THEN
			INC(size, 16);
			ASSERT(size > 0, 100); adr := 0;
			(*
			IF size < N THEN adr := KERNEL32.HeapAlloc(heap, {0}, N) END;
			IF adr = 0 THEN adr := KERNEL32.HeapAlloc(heap, {0}, size) END;
			*)
			IF size < N THEN adr := LinLibc.calloc(1, N) END; 
			IF adr = 0 THEN adr := LinLibc.calloc(1, size)
			ELSE size := N
			END;
			
			IF adr = 0 THEN c := NIL
			ELSE
				c := SYSTEM.VAL(Cluster, ((adr + 15) DIV 16) * 16); c.max := adr;
(*				c.size := KERNEL32.HeapSize(heap, {0}, adr) - (SYSTEM.VAL(INTEGER, c) - adr);*)
				c.size := size - (SYSTEM.VAL(INTEGER, c) - adr);
				INC(used, c.size); INC(total, c.size)
			END;
		ELSE
			adr := 0; s := M;
			REPEAT
(*				adr := KERNEL32.VirtualAlloc(01000000H, s, {13}, {6});	(* reserve; exec, read, write *)*)
				IF adr = 0 THEN
(*					adr := KERNEL32.VirtualAlloc(0, s, {13}, {6})	(* reserve; exec, read, write *)*)
				END;
				s := s DIV 2
			UNTIL adr # 0;
			IF adr = 0 THEN c := NIL
			ELSE
(*				adr := KERNEL32.VirtualAlloc(adr, 1024, {12}, {6});	(* commit; exec, read, write *)*)
				c := SYSTEM.VAL(Cluster, adr);
				c.max := s * 2; c.size := 0; c.next := NIL;
				GrowHeapMem(size, c);
				IF c.size < size THEN c := NIL END
			END
		END 
		(* post: (c = NIL) OR (c MOD 16 = 0) & (c.size >= size) *)
	END AllocHeapMem;

	PROCEDURE FreeHeapMem (c: Cluster);
		VAR res: INTEGER;
	BEGIN
		DEC(used, c.size); DEC(total, c.size);
		IF dllMem THEN
(*			res := KERNEL32.HeapFree(heap, {0}, c.max)*)
			LinLibc.free(c.max)
		END
	END FreeHeapMem;
	
	PROCEDURE HeapFull (size: INTEGER): BOOLEAN;
(*		VAR ms: KERNEL32.MemStatus;*)
	BEGIN
		RETURN used + size > 4000000 (* TODO: Do this right!!!      Well, maybe not, since it isn't used for dllMem *)
	(*
		ms.size := SIZE(KERNEL32.MemStatus);
		ms.memLoad := -1;
		KERNEL32.GlobalMemoryStatus(ms);
		IF ms.memLoad >= 0 THEN
			RETURN used + size > ms.totPhys
		ELSE	(* old win32s *)
			RETURN used + size > 4000000
		END*)
	END HeapFull;
	
	PROCEDURE AllocModMem* (descSize, modSize: INTEGER; VAR descAdr, modAdr: INTEGER);
		VAR res: INTEGER;
	BEGIN
		(*
		descAdr := KERNEL32.VirtualAlloc(0, descSize, {12, 13}, {6});	(* reserve & commit; exec, read, write *)
		IF descAdr # 0 THEN
			modAdr := KERNEL32.VirtualAlloc(0, modSize, {12, 13}, {6});	(* reserve & commit; exec, read, write *)
			IF modAdr # 0 THEN INC(used, descSize + modSize)
			ELSE res := KERNEL32.VirtualFree(descAdr, 0, {15}); descAdr := 0
			END
		ELSE modAdr := 0
		END
		*)
		descAdr := LinLibc.calloc(1, descSize); 
		IF descAdr # LinLibc.NULL THEN
			modAdr := LinLibc.calloc(1, modSize);
			IF modAdr # LinLibc.NULL THEN INC(used, descSize + modSize)
			ELSE LinLibc.free(descAdr); descAdr := 0
			END
		ELSE modAdr := 0
		END
	END AllocModMem;
	
	PROCEDURE DeallocModMem* (descSize, modSize, descAdr, modAdr: INTEGER);
		VAR res: INTEGER;
	BEGIN
		DEC(used, descSize + modSize);
	(*	res := KERNEL32.VirtualFree(descAdr, 0, {15});	(* release *)
		res := KERNEL32.VirtualFree(modAdr, 0, {15})	(* release *)*)
		LinLibc.free(descAdr);
		LinLibc.free(modAdr)
	END DeallocModMem;
	
	PROCEDURE InvalModMem (modSize, modAdr: INTEGER);
		VAR res: INTEGER;
	BEGIN
		DEC(used, modSize);
(*		res := KERNEL32.VirtualFree(modAdr, modSize, {14})	(* decommit *)*)
		LinLibc.free(modAdr)
	END InvalModMem;
	
	PROCEDURE IsReadable* (from, to: INTEGER): BOOLEAN;
		(* check wether memory between from (incl.) and to (excl.) may be read *)
	BEGIN
(*		RETURN KERNEL32.IsBadReadPtr(from, to - from) = 0*)
		RETURN TRUE (* TODO: Do this correct!!! *)
	END IsReadable;
	

	(* --------------------- COM reference counting -------------------- *)
	
	PROCEDURE [noframe] AddRef* (p: INTEGER): INTEGER;	(* COMPILER DEPENDENT *)
	BEGIN
		ADDREF
(*
		INC(p.ref);
		IF p.unk # NIL THEN p.unk.AddRef() END;
		RETURN p.ref
*)
	END AddRef;
	
	PROCEDURE [noframe] Release* (p: INTEGER): INTEGER;	(* COMPILER DEPENDENT *)
	BEGIN
		RELEASE
(*
		IF p.unk # NIL THEN p.unk.Release() END;
		DEC(p.ref);
		RETURN p.ref
*)
	END Release;
	
	PROCEDURE [noframe] Release2* (p: INTEGER): INTEGER;	(* COMPILER DEPENDENT *)
	BEGIN
		CALLREL;
		RELEASE
(*
		IF p.ref = 1 THEN p.RELEASE END;
		IF p.unk # NIL THEN p.unk.Release() END;
		DEC(p.ref);
		RETURN p.ref
*)
	END Release2;
(*	
	PROCEDURE RecFinalizer (obj: ANYPTR);
		VAR i: INTEGER; type: Type; p: IntPtr;
	BEGIN
		SYSTEM.GET(SYSTEM.VAL(INTEGER, obj) - 4, type);
		i := 0;
		WHILE type.ptroffs[i] >= 0 DO INC(i) END;
		INC(i);
		WHILE type.ptroffs[i] >= 0 DO
			p := SYSTEM.VAL(IntPtr, SYSTEM.VAL(INTEGER, obj) + type.ptroffs[i]); INC(i);
			p.p := NIL	(* calls p.p.Release *)
		END
	END RecFinalizer;*)
	
(*	PROCEDURE ArrFinalizer (obj: SYSTEM.PTR);
		VAR last, adr, i, j: INTEGER; type: Type; p: IntPtr;
	BEGIN
		SYSTEM.GET(SYSTEM.VAL(INTEGER, obj) - 4, type);
		type := SYSTEM.VAL(Type, SYSTEM.VAL(INTEGER, type) - 2);	(* remove array flag *)
		SYSTEM.GET(SYSTEM.VAL(INTEGER, obj), last);
		SYSTEM.GET(SYSTEM.VAL(INTEGER, obj) + 8, adr);
		j := 0;
		WHILE type.ptroffs[j] >= 0 DO INC(j) END;
		INC(j);
		WHILE adr <= last DO
			i := j;
			WHILE type.ptroffs[i] >= 0 DO
				p := SYSTEM.VAL(IntPtr, adr + type.ptroffs[i]); INC(i);
				p.p := NIL	(* calls p.p.Release *)
			END;
			INC(adr, type.size)
		END
	END ArrFinalizer;*)
(*	
	PROCEDURE ReleaseIPtrs (mod: Module);
		VAR i: INTEGER; p: IntPtr;
	BEGIN
		IF iptrs IN mod.opts THEN
			EXCL(mod.opts, iptrs);
			i := mod.nofptrs;
			WHILE mod.ptrs[i] # -1 DO
				p := SYSTEM.VAL(IntPtr, mod.varBase + mod.ptrs[i]); INC(i);
				p.p := NIL	(* calls p.p.Release *)
			END
		END
	END ReleaseIPtrs;*)
	

	(* --------------------- NEW implementation (portable) -------------------- *)

	PROCEDURE^ NewBlock (size: INTEGER): Block;

	PROCEDURE NewRec* (typ: INTEGER): INTEGER;	(* implementation of NEW(ptr) *)
		VAR size: INTEGER; b: Block; tag: Type; l: FList;
	BEGIN
		IF ODD(typ) THEN	(* record contains interface pointers *)
			tag := SYSTEM.VAL(Type, typ - 1);
			b := NewBlock(tag.size);
			IF b = NIL THEN RETURN 0 END;
			b.tag := tag;
			l := SYSTEM.VAL(FList, NewRec(SYSTEM.TYP(FList)));	(* NEW(l) *)
			l.blk := b; l.iptr := TRUE; l.next := finalizers; finalizers := l;
			RETURN SYSTEM.ADR(b.last)
		ELSE
			tag := SYSTEM.VAL(Type, typ);
			b := NewBlock(tag.size);
			IF b = NIL THEN RETURN 0 END;
			b.tag := tag; SYSTEM.GET(typ - 4, size);
			IF size # 0 THEN	(* record uses a finalizer *)
				l := SYSTEM.VAL(FList, NewRec(SYSTEM.TYP(FList)));	(* NEW(l) *)
				l.blk := b; l.next := finalizers; finalizers := l
			END;
			RETURN SYSTEM.ADR(b.last)
		END
	END NewRec;
	
	PROCEDURE NewArr* (eltyp, nofelem, nofdim: INTEGER): INTEGER;	(* impl. of NEW(ptr, dim0, dim1, ...) *)
		VAR b: Block; size, headSize: INTEGER; t: Type; fin: BOOLEAN; l: FList;
	BEGIN
		headSize := 4 * nofdim + 12; fin := FALSE;
		CASE eltyp OF
(*		| -1: eltyp := SYSTEM.ADR(IntPtrType); fin := TRUE*)
		| 0: eltyp := SYSTEM.ADR(PtrType)
		| 1: eltyp := SYSTEM.ADR(Char8Type)
		| 2: eltyp := SYSTEM.ADR(Int16Type)
		| 3: eltyp := SYSTEM.ADR(Int8Type)
		| 4: eltyp := SYSTEM.ADR(Int32Type)
		| 5: eltyp := SYSTEM.ADR(BoolType)
		| 6: eltyp := SYSTEM.ADR(SetType)
		| 7: eltyp := SYSTEM.ADR(Real32Type)
		| 8: eltyp := SYSTEM.ADR(Real64Type)
		| 9: eltyp := SYSTEM.ADR(Char16Type)
		| 10: eltyp := SYSTEM.ADR(Int64Type)
		| 11: eltyp := SYSTEM.ADR(ProcType)
		| 12: eltyp := SYSTEM.ADR(UPtrType)
		ELSE	(* eltyp is desc *)
			IF ODD(eltyp) THEN DEC(eltyp); fin := TRUE END
		END;
		t := SYSTEM.VAL(Type, eltyp);
		size := headSize + nofelem * t.size;
		b := NewBlock(size);
		IF b = NIL THEN RETURN 0 END;
		b.tag := SYSTEM.VAL(Type, eltyp + 2);	(* tag + array mark *)
		b.last := SYSTEM.ADR(b.last) + size - t.size;	(* pointer to last elem *)
		b.first := SYSTEM.ADR(b.last) + headSize;	(* pointer to first elem *)
		IF fin THEN
			l := SYSTEM.VAL(FList, NewRec(SYSTEM.TYP(FList)));	(* NEW(l) *)
			l.blk := b; l.aiptr := TRUE; l.next := finalizers; finalizers := l
		END;
		RETURN SYSTEM.ADR(b.last)
	END NewArr;


	(* -------------------- handler installation (portable) --------------------- *)

	PROCEDURE ThisFinObj* (VAR id: Identifier): ANYPTR;
		VAR l: FList;
	BEGIN
		ASSERT(id.typ # 0, 100); ASSERT(hotFinalizers = NIL, 101);
		l := finalizers;
		WHILE l # NIL DO
			IF SYSTEM.VAL(INTEGER, l.blk.tag) = id.typ THEN
				id.obj := SYSTEM.VAL(ANYPTR, SYSTEM.ADR(l.blk.last));
				IF id.Identified() THEN RETURN id.obj END
			END;
			l := l.next
		END;
		RETURN NIL
	END ThisFinObj;
	
	PROCEDURE InstallReducer* (r: Reducer);
	BEGIN
		r.next := reducers; reducers := r
	END InstallReducer;

	PROCEDURE InstallTrapViewer* (h: Handler);
	BEGIN
		trapViewer := h
	END InstallTrapViewer;
	
	PROCEDURE InstallTrapChecker* (h: Handler);
	BEGIN
		trapChecker := h
	END InstallTrapChecker;
	
	PROCEDURE PushTrapCleaner* (c: TrapCleaner);
		VAR t: TrapCleaner;
	BEGIN
		t := trapStack; WHILE (t # NIL) & (t # c) DO t := t.next END;
		ASSERT(t = NIL, 20);
		c.next := trapStack; trapStack := c
	END PushTrapCleaner;
	
	PROCEDURE PopTrapCleaner* (c: TrapCleaner);
		VAR t: TrapCleaner;
	BEGIN
		t := NIL;
		WHILE (trapStack # NIL) & (t # c) DO
			t := trapStack; trapStack := trapStack.next
		END
	END PopTrapCleaner;
	
	PROCEDURE InstallCleaner* (p: Command);
		VAR c: CList;
	BEGIN
		c := SYSTEM.VAL(CList, NewRec(SYSTEM.TYP(CList)));	(* NEW(c) *)
		c.do := p; c.trapped := FALSE; c.next := cleaners; cleaners := c
	END InstallCleaner;
	
	PROCEDURE RemoveCleaner* (p: Command);
		VAR c0, c: CList;
	BEGIN
		c := cleaners; c0 := NIL;
		WHILE (c # NIL) & (c.do # p) DO c0 := c; c := c.next END;
		IF c # NIL THEN
			IF c0 = NIL THEN cleaners := cleaners.next ELSE c0.next := c.next END
		END
	END RemoveCleaner;
	
	PROCEDURE Cleanup*;
		VAR c, c0: CList;
	BEGIN
		c := cleaners; c0 := NIL;
		WHILE c # NIL DO
			IF ~c.trapped THEN
				c.trapped := TRUE; c.do; c.trapped := FALSE; c0 := c
			ELSE
				IF c0 = NIL THEN cleaners := cleaners.next
				ELSE c0.next := c.next
				END
			END;
			c := c.next
		END
	END Cleanup;

	(* -------------------- meta information (portable) --------------------- *)
	
	PROCEDURE (h: LoaderHook) ThisMod* (IN name: ARRAY OF SHORTCHAR): Module, NEW, ABSTRACT;
	
	PROCEDURE SetLoaderHook*(h: LoaderHook);
	BEGIN
		loader := h
	END SetLoaderHook;

	PROCEDURE InitModule (mod: Module);	(* initialize linked modules *)
		VAR body: Command;
	BEGIN
		IF ~(dyn IN mod.opts) & (mod.next # NIL) & ~(init IN mod.next.opts) THEN InitModule(mod.next) END;
		IF ~(init IN mod.opts) THEN
			body := SYSTEM.VAL(Command, mod.code);
			INCL(mod.opts, init);
			actual := mod; body(); actual := NIL
		END
	END InitModule;
	
	PROCEDURE ThisLoadedMod* (IN name: ARRAY OF SHORTCHAR): Module;	(* loaded modules only *)
		VAR m: Module;
	BEGIN
		loadres := done;
		m := modList;
		WHILE (m # NIL) & ((m.name # name) OR (m.refcnt < 0)) DO m := m.next END;
		IF (m # NIL) & ~(init IN m.opts) THEN InitModule(m) END;
		IF m = NIL THEN loadres := moduleNotFound END;
		RETURN m
	END ThisLoadedMod;
	
	PROCEDURE ThisMod* (IN name: ARRAY OF CHAR): Module;
		VAR n : Name;
	BEGIN
		n := SHORT(name$);
		IF loader # NIL THEN
			loader.res := done;
			RETURN loader.ThisMod(n)
		ELSE
			RETURN ThisLoadedMod(n)
		END
	END ThisMod;
	
	PROCEDURE LoadMod* (IN name: ARRAY OF CHAR);
		VAR m: Module;
	BEGIN
		m := ThisMod(name)
	END LoadMod;
	
	PROCEDURE GetLoaderResult* (OUT res: INTEGER; OUT importing, imported, object: ARRAY OF CHAR);
	BEGIN
		IF loader # NIL THEN
			res := loader.res;
			importing := loader.importing$;
			imported := loader.imported$;
			object := loader.object$
		ELSE
			res := loadres;
			importing := "";
			imported := "";
			object := ""
		END
	END GetLoaderResult;
		
	PROCEDURE ThisObject* (mod: Module; name: ARRAY OF SHORTCHAR): Object;
		VAR l, r, m: INTEGER; p: StrPtr;
	BEGIN
		l := 0; r := mod.export.num;
		WHILE l < r DO	(* binary search *)
			m := (l + r) DIV 2;
			p := SYSTEM.VAL(StrPtr, SYSTEM.ADR(mod.names[mod.export.obj[m].id DIV 256]));
			IF p^ = name THEN RETURN SYSTEM.VAL(Object, SYSTEM.ADR(mod.export.obj[m])) END;
			IF p^ < name THEN l := m + 1 ELSE r := m END
		END;
		RETURN NIL
	END ThisObject;
	
	PROCEDURE ThisDesc* (mod: Module; fprint: INTEGER): Object;
		VAR i, n: INTEGER;
	BEGIN
		i := 0; n := mod.export.num;
		WHILE (i < n) & (mod.export.obj[i].id DIV 256 = 0) DO 
			IF mod.export.obj[i].offs = fprint THEN RETURN SYSTEM.VAL(Object, SYSTEM.ADR(mod.export.obj[i])) END;
			INC(i)
		END;
		RETURN NIL
	END ThisDesc;
	
	PROCEDURE ThisField* (rec: Type; name: ARRAY OF SHORTCHAR): Object;
		VAR n: INTEGER; p: StrPtr; obj: Object; m: Module;
	BEGIN
		m := rec.mod;
		obj := SYSTEM.VAL(Object, SYSTEM.ADR(rec.fields.obj[0])); n := rec.fields.num;
		WHILE n > 0 DO
			p := SYSTEM.VAL(StrPtr, SYSTEM.ADR(m.names[obj.id DIV 256]));
			IF p^ = name THEN RETURN obj END;
			DEC(n); INC(SYSTEM.VAL(INTEGER, obj), 16)
		END;
		RETURN NIL
	END ThisField;

	PROCEDURE ThisCommand* (mod: Module; name: ARRAY OF SHORTCHAR): Command;
		VAR x: Object;
	BEGIN
		x := ThisObject(mod, name);
		IF (x # NIL) & (x.id MOD 16 = mProc) & (x.fprint = comSig) THEN
			RETURN SYSTEM.VAL(Command, mod.procBase + x.offs)
		ELSE
			RETURN NIL
		END
	END ThisCommand;

	PROCEDURE ThisType* (mod: Module; name: ARRAY OF SHORTCHAR): Type;
		VAR x: Object;
	BEGIN
		x := ThisObject(mod, name);
		IF (x # NIL) & (x.id MOD 16 = mTyp) & (SYSTEM.VAL(INTEGER, x.struct) DIV 256 # 0) THEN
			RETURN x.struct
		ELSE
			RETURN NIL
		END
	END ThisType;

	PROCEDURE TypeOf* (IN rec: ANYREC): Type;
	BEGIN
		RETURN SYSTEM.VAL(Type, SYSTEM.TYP(rec))
	END TypeOf;

	PROCEDURE LevelOf* (t: Type): SHORTINT;
	BEGIN
		RETURN SHORT(t.id DIV 16 MOD 16)
	END LevelOf;
	
	PROCEDURE NewObj* (VAR o: SYSTEM.PTR; t: Type);
		VAR i: INTEGER;
	BEGIN
		IF t.size = -1 THEN o := NIL
		ELSE
			i := 0; WHILE t.ptroffs[i] >= 0 DO INC(i) END;
			IF t.ptroffs[i+1] >= 0 THEN INC(SYSTEM.VAL(INTEGER, t)) END;	(* with interface pointers *)
			o := SYSTEM.VAL(SYSTEM.PTR, NewRec(SYSTEM.VAL(INTEGER, t)))	(* generic NEW *)
		END
	END NewObj;

	PROCEDURE GetObjName* (mod: Module; obj: Object; VAR name: Name);
		VAR p: StrPtr;
	BEGIN
		p := SYSTEM.VAL(StrPtr, SYSTEM.ADR(mod.names[obj.id DIV 256]));
		name := p^$
	END GetObjName;
	
	PROCEDURE GetTypeName* (t: Type; VAR name: Name);
		VAR p: StrPtr;
	BEGIN
		p := SYSTEM.VAL(StrPtr, SYSTEM.ADR(t.mod.names[t.id DIV 256]));
		name := p^$
	END GetTypeName;
	
	PROCEDURE RegisterMod* (mod: Module);
		VAR i: INTEGER;(* t: KERNEL32.SystemTime;*) obj: Object; s: SET; c: Command; str: Name;
			t: LinLibc.time_t; tm: LinLibc.tm;
	BEGIN
		mod.next := modList; modList := mod; mod.refcnt := 0; INCL(mod.opts, dyn); i := 0;
		WHILE i < mod.nofimps DO
			IF mod.imports[i] # NIL THEN INC(mod.imports[i].refcnt) END;
			INC(i)
		END;
		t := LinLibc.time(NIL);
		tm := LinLibc.localtime(t);
		mod.loadTime[0] := SHORT(tm.tm_year + 1900); (* Linux counts years from 1900 but BlackBox from 0000 *)
		mod.loadTime[1] := SHORT(tm.tm_mon + 1) (* Linux month range 0-11 but BB month range 1-12 *);
		mod.loadTime[2] := SHORT(tm.tm_mday);
		mod.loadTime[3] := SHORT(tm.tm_hour);
		mod.loadTime[4] := SHORT(tm.tm_min);
		mod.loadTime[5] := SHORT(tm.tm_sec); 
		tm := NIL;
		IF ~(init IN mod.opts) THEN InitModule(mod) END
	END RegisterMod;
	
	PROCEDURE^ Collect*;
	
	PROCEDURE UnloadMod* (mod: Module);
		VAR i: INTEGER; t: Command;
	BEGIN
		IF mod.refcnt = 0 THEN
			t := mod.term; mod.term := NIL;
			IF t # NIL THEN t() END;	(* terminate module *)
			i := 0;
			WHILE i < mod.nofptrs DO	(* release global pointers *)
				SYSTEM.PUT(mod.varBase + mod.ptrs[i], 0); INC(i)
			END;
(*			ReleaseIPtrs(mod);	(* release global interface pointers *)*)
			Collect;	(* call finalizers *)
			i := 0;
			WHILE i < mod.nofimps DO	(* release imported modules *)
				IF mod.imports[i] # NIL THEN DEC(mod.imports[i].refcnt) END;
				INC(i)
			END;
			mod.refcnt := -1;
			IF dyn IN mod.opts THEN	(* release memory *)
				InvalModMem(mod.data + mod.dsize - mod.refs, mod.refs)
			END
		END
	END UnloadMod;

	(* -------------------- reference information (portable) --------------------- *)

	PROCEDURE RefCh (VAR ref: INTEGER; VAR ch: SHORTCHAR);
	BEGIN
		SYSTEM.GET(ref, ch); INC(ref)
	END RefCh;
	
	PROCEDURE RefNum (VAR ref: INTEGER; VAR x: INTEGER);
		VAR s, n: INTEGER; ch: SHORTCHAR;
	BEGIN
		s := 0; n := 0; RefCh(ref, ch);
		WHILE ORD(ch) >= 128 DO INC(n, ASH(ORD(ch) - 128, s) ); INC(s, 7); RefCh(ref, ch) END;
		x := n + ASH(ORD(ch) MOD 64 - ORD(ch) DIV 64 * 64, s)
	END RefNum;
	
	PROCEDURE RefName (VAR ref: INTEGER; VAR n: Name);
		VAR i: INTEGER; ch: SHORTCHAR;
	BEGIN
		i := 0; RefCh(ref, ch);
		WHILE ch # 0X DO n[i] := ch; INC(i); RefCh(ref, ch) END;
		n[i] := 0X
	END RefName;
	
	PROCEDURE GetRefProc* (VAR ref: INTEGER; VAR adr: INTEGER; VAR name: Name);
		VAR ch: SHORTCHAR;
	BEGIN
		SYSTEM.GET(ref, ch);
		WHILE ch >= 0FDX DO	(* skip variables *)
			INC(ref); RefCh(ref, ch);
			IF ch = 10X THEN INC(ref, 4) END;
			RefNum(ref, adr); RefName(ref, name); SYSTEM.GET(ref, ch)
		END;
		WHILE (ch > 0X) & (ch < 0FCX) DO	(* skip source refs *)
			INC(ref); RefNum(ref, adr); SYSTEM.GET(ref, ch)
		END;
		IF ch = 0FCX THEN INC(ref); RefNum(ref, adr); RefName(ref, name)
		ELSE adr := 0
		END
	END GetRefProc;
	
	PROCEDURE GetRefVar* (VAR ref: INTEGER; VAR mode, form: SHORTCHAR; VAR desc: Type;
																VAR adr: INTEGER; VAR name: Name);
	BEGIN
		SYSTEM.GET(ref, mode); desc := NIL;
		IF mode >= 0FDX THEN
			mode := SHORT(CHR(ORD(mode) - 0FCH));
			INC(ref); RefCh(ref, form);
			IF form = 10X THEN
				SYSTEM.GET(ref, desc); INC(ref, 4); form := SHORT(CHR(16 + desc.id MOD 4))
			END;
			RefNum(ref, adr); RefName(ref, name)
		ELSE
			mode := 0X; form := 0X; adr := 0
		END
	END GetRefVar;
	
	PROCEDURE SourcePos* (mod: Module; codePos: INTEGER): INTEGER;
		VAR ref, pos, ad, d: INTEGER; ch: SHORTCHAR; name: Name;
	BEGIN
		ref := mod.refs; pos := 0; ad := 0; SYSTEM.GET(ref, ch);
		WHILE ch # 0X DO
			WHILE (ch > 0X) & (ch < 0FCX) DO
				INC(ad, ORD(ch)); INC(ref); RefNum(ref, d);
				IF ad > codePos THEN RETURN pos END;
				INC(pos, d); SYSTEM.GET(ref, ch) 
			END;
			IF ch = 0FCX THEN INC(ref); RefNum(ref, d); RefName(ref, name); SYSTEM.GET(ref, ch) END;
			WHILE ch >= 0FDX DO	(* skip variables *)
				INC(ref); RefCh(ref, ch);
				IF ch = 10X THEN INC(ref, 4) END;
				RefNum(ref, d); RefName(ref, name); SYSTEM.GET(ref, ch)
			END
		END;
		RETURN -1
	END SourcePos;
	
	(* -------------------- dynamic link libraries --------------------- *)
	
(*	PROCEDURE LoadDll* (IN name: ARRAY OF SHORTCHAR; VAR ok: BOOLEAN);
		VAR h: KERNEL32.Handle;
	BEGIN
		ok := FALSE;
		h := KERNEL32.LoadLibraryA(name);
		IF h # 0 THEN ok := TRUE END
	END LoadDll;
	
	PROCEDURE ThisDllObj* (mode, fprint: INTEGER; IN dll, name: ARRAY OF SHORTCHAR): INTEGER;
		VAR ad: INTEGER; h: KERNEL32.Handle;
	BEGIN
		ad := 0;
		IF mode = mProc THEN
			h := KERNEL32.GetModuleHandleA(dll);
			IF h # 0 THEN ad := KERNEL32.GetProcAddress(h, name) END
		END;
		RETURN ad
	END ThisDllObj;*)
	
	PROCEDURE LoadDll* (IN name: ARRAY OF SHORTCHAR; VAR ok: BOOLEAN);
		VAR h: LinDl.HANDLE;
	BEGIN
		ok := FALSE;
		h := LinDl.dlopen(name, LinDl.RTLD_LAZY +  LinDl.RTLD_GLOBAL);
		IF h # LinDl.NULL THEN ok := TRUE END
	END LoadDll;
	
	PROCEDURE ThisDllObj* (mode, fprint: INTEGER; IN dll, name: ARRAY OF SHORTCHAR): INTEGER;
		VAR ad: INTEGER; h: LinDl.HANDLE;
	BEGIN
		ad := 0;
		IF mode IN {mVar, mProc} THEN
			h := LinDl.dlopen(dll, LinDl.RTLD_LAZY+  LinDl.RTLD_GLOBAL);
			IF h # LinDl.NULL THEN
				ad := LinDl.dlsym(h, name);
			END
		END;
		RETURN ad
	END ThisDllObj;
	
	(* -------------------- garbage collector (portable) --------------------- *)
	
	PROCEDURE Mark (this: Block);
		VAR father, son: Block; tag: Type; flag, offset, actual: INTEGER;
	BEGIN
		IF ~ODD(SYSTEM.VAL(INTEGER, this.tag)) THEN
			father := NIL;
			LOOP
				INC(SYSTEM.VAL(INTEGER, this.tag));
				flag := SYSTEM.VAL(INTEGER, this.tag) MOD 4;
				tag := SYSTEM.VAL(Type, SYSTEM.VAL(INTEGER, this.tag) - flag);
				IF flag >= 2 THEN actual := this.first; this.actual := actual
				ELSE actual := SYSTEM.ADR(this.last)
				END;
				LOOP
					offset := tag.ptroffs[0];
					IF offset < 0 THEN
						INC(SYSTEM.VAL(INTEGER, tag), offset + 4);	(* restore tag *)
						IF (flag >= 2) & (actual < this.last) & (offset < -4) THEN	(* next array element *)
							INC(actual, tag.size); this.actual := actual
						ELSE	(* up *)
							this.tag := SYSTEM.VAL(Type, SYSTEM.VAL(INTEGER, tag) + flag);
							IF father = NIL THEN RETURN END;
							son := this; this := father;
							flag := SYSTEM.VAL(INTEGER, this.tag) MOD 4;
							tag := SYSTEM.VAL(Type, SYSTEM.VAL(INTEGER, this.tag) - flag);
							offset := tag.ptroffs[0];
							IF flag >= 2 THEN actual := this.actual ELSE actual := SYSTEM.ADR(this.last) END;
							SYSTEM.GET(actual + offset, father); SYSTEM.PUT(actual + offset, SYSTEM.ADR(son.last));
							INC(SYSTEM.VAL(INTEGER, tag), 4)
						END
					ELSE
						SYSTEM.GET(actual + offset, son);
						IF son # NIL THEN
							DEC(SYSTEM.VAL(INTEGER, son), 4);
							IF ~ODD(SYSTEM.VAL(INTEGER, son.tag)) THEN	(* down *)
								this.tag := SYSTEM.VAL(Type, SYSTEM.VAL(INTEGER, tag) + flag);
								SYSTEM.PUT(actual + offset, father); father := this; this := son;
								EXIT
							END
						END;
						INC(SYSTEM.VAL(INTEGER, tag), 4)
					END
				END
			END
		END
	END Mark;
	
	PROCEDURE MarkGlobals;
		VAR m: Module; i, p: INTEGER;
	BEGIN
		m := modList;
		WHILE m # NIL DO
			IF m.refcnt >= 0 THEN
				i := 0;
				WHILE i < m.nofptrs DO
					SYSTEM.GET(m.varBase + m.ptrs[i], p); INC(i);
					IF p # 0 THEN
						Mark(SYSTEM.VAL(Block, p - 4))
					END
				END
			END;
			m := m.next
		END
	END MarkGlobals;
(*
	PROCEDURE Next (b: Block): Block;	(* next block in same cluster *)
		VAR size: INTEGER;
	BEGIN
		SYSTEM.GET(SYSTEM.VAL(INTEGER, b.tag) DIV 4 * 4, size);
		IF ODD(SYSTEM.VAL(INTEGER, b.tag) DIV 2) THEN INC(size, b.last - SYSTEM.ADR(b.last)) END;
		RETURN SYSTEM.VAL(Block, SYSTEM.VAL(INTEGER, b) + (size + 19) DIV 16 * 16)
	END Next;
*)
	PROCEDURE [code] Next (b: Block): Block	(* next block in same cluster *)
(*
	MOV	ECX,[EAX]	b.tag
	AND	CL,0FCH	b.tag DIV * 4
	MOV	ECX,[ECX]	size
	TESTB	[EAX],02H	ODD(b.tag DIV 2)
	JE	L1
	ADD	ECX,[EAX,4]	size + b.last
	SUB	ECX,EAX
	SUB	ECX,4	size + b.last - ADR(b.last)
	L1:
	ADD	ECX,19	size + 19
	AND	CL,0F0H	(size + 19) DIV 16 * 16
	ADD	EAX,ECX	b + size
*)
	08BH, 008H,
	080H, 0E1H, 0FCH,
	08BH, 009H,
	0F6H, 000H, 002H,
	074H, 008H,
	003H, 048H, 004H,
	029H, 0C1H,
	083H, 0E9H, 004H,
	083H, 0C1H, 013H,
	080H, 0E1H, 0F0H,
	001H, 0C8H;
	
	PROCEDURE CheckCandidates;
	(* pre: nofcand > 0 *)
		VAR i, j, h, p, end: INTEGER; c: Cluster; blk, next: Block;
	BEGIN
		(* sort candidates (shellsort) *)
		h := 1; REPEAT h := h*3 + 1 UNTIL h > nofcand;
		REPEAT h := h DIV 3; i := h;
			WHILE i < nofcand DO p := candidates[i]; j := i;
				WHILE (j >= h) & (candidates[j-h] > p) DO
					candidates[j] := candidates[j-h]; j := j-h
				END;
				candidates[j] := p; INC(i)
			END
		UNTIL h = 1;
		(* sweep *)
		c := root; i := 0;
		WHILE c # NIL DO
			blk := SYSTEM.VAL(Block, SYSTEM.VAL(INTEGER, c) + 12);
			end := SYSTEM.VAL(INTEGER, blk) + (c.size - 12) DIV 16 * 16;
			WHILE candidates[i] < SYSTEM.VAL(INTEGER, blk) DO
				INC(i);
				IF i = nofcand THEN RETURN END
			END;
			WHILE SYSTEM.VAL(INTEGER, blk) < end DO
				next := Next(blk);
				IF candidates[i] < SYSTEM.VAL(INTEGER, next) THEN
					IF (SYSTEM.VAL(INTEGER, blk.tag) # SYSTEM.ADR(blk.last))	(* not a free block *)
							& (~strictStackSweep OR (candidates[i] = SYSTEM.ADR(blk.last))) THEN
						Mark(blk)
					END;
					REPEAT
						INC(i);
						IF i = nofcand THEN RETURN END
					UNTIL candidates[i] >= SYSTEM.VAL(INTEGER, next)
				END;
				IF (SYSTEM.VAL(INTEGER, blk.tag) MOD 4 = 0) & (SYSTEM.VAL(INTEGER, blk.tag) # SYSTEM.ADR(blk.last))
						& (blk.tag.base[0] = NIL) & (blk.actual > 0) THEN	(* referenced interface record *)
					Mark(blk)
				END;
				blk := next
			END;
			c := c.next
		END
	END CheckCandidates;

	PROCEDURE MarkLocals;
		VAR sp, p, min, max: INTEGER; c: Cluster;
	BEGIN
		SYSTEM.GETREG(FP, sp); nofcand := 0; c := root;
		WHILE c.next # NIL DO c := c.next END;
		min := SYSTEM.VAL(INTEGER, root); max := SYSTEM.VAL(INTEGER, c) + c.size;
		WHILE sp < baseStack DO
			SYSTEM.GET(sp, p);
			IF (p > min) & (p < max) & (~strictStackSweep OR (p MOD 16 = 0)) THEN
				candidates[nofcand] := p; INC(nofcand);
				IF nofcand = LEN(candidates) - 1 THEN CheckCandidates; nofcand := 0 END
			END;
			INC(sp, 4)
		END;
		candidates[nofcand] := max; INC(nofcand);	(* ensure complete scan for interface mark*)
		IF nofcand > 0 THEN CheckCandidates END
	END MarkLocals;
	
	PROCEDURE MarkFinObj;
		VAR f: FList;
	BEGIN
		wouldFinalize := FALSE;
		f := finalizers;
		WHILE f # NIL DO
			IF ~ODD(SYSTEM.VAL(INTEGER, f.blk.tag)) THEN wouldFinalize := TRUE END;
			Mark(f.blk);
			f := f.next
		END;
		f := hotFinalizers;
		WHILE f # NIL DO IF ~ODD(SYSTEM.VAL(INTEGER, f.blk.tag)) THEN wouldFinalize := TRUE END;
			Mark(f.blk);
			f := f.next
		END
	END MarkFinObj;

	PROCEDURE CheckFinalizers;
		VAR f, g, h, k: FList;
	BEGIN
		f := finalizers; g := NIL;
		(* hotFinalizers := NIL; k := NIL; *)
		IF hotFinalizers = NIL THEN k := NIL
		ELSE
			k := hotFinalizers;
			WHILE k.next # NIL DO k := k.next END
		END;
		WHILE f # NIL DO
			h := f; f := f.next;
			IF ~ODD(SYSTEM.VAL(INTEGER, h.blk.tag)) THEN
				IF g = NIL THEN finalizers := f ELSE g.next := f END;
				IF k = NIL THEN hotFinalizers := h ELSE k.next := h END;
				k := h; h.next := NIL
			ELSE g := h
			END
		END;
		h := hotFinalizers;
		WHILE h # NIL DO Mark(h.blk); h := h.next END
	END CheckFinalizers;

	PROCEDURE ExecFinalizer (a, b, c: INTEGER);
		VAR f: FList; fin: PROCEDURE(this: ANYPTR);
	BEGIN
		f := hotFinalizers; hotFinalizers := hotFinalizers.next;
		IF f.aiptr THEN (*ArrFinalizer(SYSTEM.VAL(ANYPTR, S.ADR(f.blk.last)))*)
		ELSE
			SYSTEM.GET(SYSTEM.VAL(INTEGER, f.blk.tag) - 4, fin);	(* method 0 *)
			IF fin # NIL THEN fin(SYSTEM.VAL(ANYPTR, SYSTEM.ADR(f.blk.last))) END;
(*			IF f.iptr THEN RecFinalizer(SYSTEM.VAL(ANYPTR, SYSTEM.ADR(f.blk.last))) END *)
		END
	END ExecFinalizer;
	
	PROCEDURE^ Try* (h: TryHandler; a, b, c: INTEGER);	(* COMPILER DEPENDENT *)

	PROCEDURE CallFinalizers;
		VAR f: FList;
	BEGIN
		WHILE hotFinalizers # NIL DO
			f := hotFinalizers.next; hotFinalizers.next := NIL;
			Try(ExecFinalizer, 0, 0, 0);
			hotFinalizers := f
		END;
		wouldFinalize := FALSE
	END CallFinalizers;
	
	PROCEDURE Insert (blk: FreeBlock; size: INTEGER);	(* insert block in free list *)
		VAR i: INTEGER;
	BEGIN
		blk.size := size - 4; blk.tag := SYSTEM.VAL(Type, SYSTEM.ADR(blk.size));
		i := MIN(N - 1, (blk.size DIV 16));
		blk.next := free[i]; free[i] := blk
	END Insert;
	
	PROCEDURE Sweep (dealloc: BOOLEAN);
		VAR cluster, last, c: Cluster; blk, next: Block; fblk, b, t: FreeBlock; end, i: INTEGER;
	BEGIN
		cluster := root; last := NIL; allocated := 0;
		i := N;
		REPEAT DEC(i); free[i] := sentinel UNTIL i = 0;
		WHILE cluster # NIL DO
			blk := SYSTEM.VAL(Block, SYSTEM.VAL(INTEGER, cluster) + 12);
			end := SYSTEM.VAL(INTEGER, blk) + (cluster.size - 12) DIV 16 * 16;
			fblk := NIL;
			WHILE SYSTEM.VAL(INTEGER, blk) < end DO
				next := Next(blk);
				IF ODD(SYSTEM.VAL(INTEGER, blk.tag)) THEN
					IF fblk # NIL THEN
						Insert(fblk, SYSTEM.VAL(INTEGER, blk) - SYSTEM.VAL(INTEGER, fblk));
						fblk := NIL
					END;
					DEC(SYSTEM.VAL(INTEGER, blk.tag));	(* unmark *)
					INC(allocated, SYSTEM.VAL(INTEGER, next) - SYSTEM.VAL(INTEGER, blk))
				ELSIF fblk = NIL THEN
					fblk := SYSTEM.VAL(FreeBlock, blk)
				END;
				blk := next
			END;
			IF dealloc & (SYSTEM.VAL(INTEGER, fblk) = SYSTEM.VAL(INTEGER, cluster) + 12) THEN	(* deallocate cluster *)
				c := cluster; cluster := cluster.next;
				IF last = NIL THEN root := cluster ELSE last.next := cluster END;
				FreeHeapMem(c)
			ELSE
				IF fblk # NIL THEN Insert(fblk, end - SYSTEM.VAL(INTEGER, fblk)) END;
				last := cluster; cluster := cluster.next
			END
		END;
		(* reverse free list *)
		i := N;
		REPEAT
			DEC(i);
			b := free[i]; fblk := sentinel;
			WHILE b # sentinel DO t := b; b := t.next; t.next := fblk; fblk := t END;
			free[i] := fblk
		UNTIL i = 0
	END Sweep;
	
	PROCEDURE Collect*;
	BEGIN
		IF root # NIL THEN
			CallFinalizers;	(* trap cleanup *)
IF debug & (watcher # NIL) THEN watcher(1) END;
			MarkGlobals;
			MarkLocals;
			CheckFinalizers;
			Sweep(TRUE);
			CallFinalizers
		END
	END Collect;
	
	PROCEDURE FastCollect*;
	BEGIN
		IF root # NIL THEN
(*			 CallFinalizers;	(* trap cleanup  *)*)
IF debug & (watcher # NIL) THEN watcher(2) END;
			MarkGlobals;
			MarkLocals;
(*			 CheckFinalizers; *)
			MarkFinObj;
			Sweep(FALSE);
(*			CallFinalizers *)
		END
	END FastCollect;

(*
	PROCEDURE GlobalCollect*;
	BEGIN
		IF root # NIL THEN
			MarkGlobals;
			(* MarkLocals; *)
			CheckFinalizers;
			Sweep(FALSE);
		END
	END GlobalCollect;
*)


	PROCEDURE WouldFinalize* (): BOOLEAN;
	BEGIN
		RETURN wouldFinalize
	END WouldFinalize;


	(* --------------------- memory allocation (portable) -------------------- *)
	
	PROCEDURE OldBlock (size: INTEGER): FreeBlock;	(* size MOD 16 = 0 *)
		VAR b, l: FreeBlock; s, i: INTEGER;
	BEGIN
IF debug & (watcher # NIL) THEN watcher(3) END;
		s := size - 4;
		i := MIN(N - 1, s DIV 16);
		WHILE (i # N - 1) & (free[i] = sentinel) DO INC(i) END;
		b := free[i]; l := NIL;
		WHILE b.size < s DO l := b; b := b.next END;
		IF b # sentinel THEN
			IF l = NIL THEN free[i] := b.next ELSE l.next := b.next END
		ELSE b := NIL
		END;
		RETURN b
	END OldBlock;

	PROCEDURE LastBlock (limit: INTEGER): FreeBlock;	(* size MOD 16 = 0 *)
		VAR b, l: FreeBlock; s, i: INTEGER;
	BEGIN
		s := limit - 4;
		i := 0;
		REPEAT
			b := free[i]; l := NIL;
			WHILE (b # sentinel) & (SYSTEM.VAL(INTEGER, b) + b.size # s) DO l := b; b := b.next END;
			IF b # sentinel THEN
				IF l = NIL THEN free[i] := b.next ELSE l.next := b.next END
			ELSE b := NIL
			END;
			INC(i)
		UNTIL (b # NIL) OR (i = N);
		RETURN b
	END LastBlock;

	PROCEDURE NewBlock (size: INTEGER): Block;
		VAR tsize, a, s: INTEGER; b: FreeBlock; new, c: Cluster; r: Reducer;
	BEGIN
		tsize := (size + 19) DIV 16 * 16;
		b := OldBlock(tsize);	(* 1) search for free block *)
		IF b = NIL THEN
			IF dllMem THEN
				FastCollect; b := OldBlock(tsize);	(* 2) collect *)
				IF b = NIL THEN
					AllocHeapMem(tsize + 12, new);	(* 3) allocate new cluster *)
					IF new # NIL THEN
						IF (root = NIL) OR (SYSTEM.VAL(INTEGER, new) < SYSTEM.VAL(INTEGER, root)) THEN
							new.next := root; root := new
						ELSE
							c := root;
							WHILE (c.next # NIL) & (SYSTEM.VAL(INTEGER, new) > SYSTEM.VAL(INTEGER, c.next)) DO 
								c := c.next 
							END;
							new.next := c.next; c.next := new
						END;
						b := SYSTEM.VAL(FreeBlock, SYSTEM.VAL(INTEGER, new) + 12);
						b.size := (new.size - 12) DIV 16 * 16 - 4
					ELSE
						RETURN NIL	(* 4) give up *)
					END
				END
			ELSE
				FastCollect;	(* 2) collect *)
				IF (b = NIL) & (HeapFull(tsize)) & (reducers # NIL) THEN	(* 3) little space => reduce once *)
					r := reducers; reducers := NIL;
					WHILE r # NIL DO r.Reduce(FALSE); r := r.next END;
					Collect
				END;
				s := 3 * (allocated + tsize) DIV 2;
				a := 12 + (root.size - 12) DIV 16 * 16;
				IF s <= total THEN
					b := OldBlock(tsize);
					IF b = NIL THEN s := a + tsize END
				ELSIF s < a + tsize THEN
					s := a + tsize
				END;
				IF total < s THEN	(* 4) enlarge heap *)
					GrowHeapMem(s, root);
					IF root.size >= s THEN
						b := LastBlock(SYSTEM.VAL(INTEGER, root) + a);
						IF b # NIL THEN
							b.size := (root.size - a + b.size + 4) DIV 16 * 16 - 4
						ELSE
							b := SYSTEM.VAL(FreeBlock, SYSTEM.VAL(INTEGER, root) + a);
							b.size := (root.size - a) DIV 16 * 16 - 4
						END
					ELSIF reducers # NIL THEN	(* 5) no space => fully reduce *)
						r := reducers; reducers := NIL;
						WHILE r # NIL DO r.Reduce(TRUE); r := r.next END;
						Collect
					END
				END;
				IF b = NIL THEN
					b := OldBlock(tsize);
					IF b = NIL THEN RETURN NIL END	(* 6) give up *)
				END
			END
		END;
		(* b # NIL *)
		a := b.size + 4 - tsize;
		IF a > 0 THEN Insert(SYSTEM.VAL(FreeBlock, SYSTEM.VAL(INTEGER, b) + tsize), a) END;
		IF size > 0 THEN Erase(SYSTEM.ADR(b.size), (size + 3) DIV 4) END;
		INC(allocated, tsize);
		RETURN SYSTEM.VAL(Block, b)
	END NewBlock;
(*	
	PROCEDURE NewBlock (size: INTEGER): Block;
		VAR tsize, a, s: INTEGER; b: FreeBlock; new, c: Cluster; r: Reducer;
	BEGIN
		tsize := (size + 19) DIV 16 * 16;
		b := OldBlock(tsize);	(* 1) search for free block *)
		IF b = NIL THEN
			(*FastCollect;*) b := OldBlock(tsize);	(* 2) collect *)
			IF b = NIL THEN
				AllocHeapMem(tsize + 12, new);	(* 3) allocate new cluster *)
				IF new # NIL THEN
					IF (root = NIL) OR (SYSTEM.VAL(INTEGER, new) < SYSTEM.VAL(INTEGER, root)) THEN
						new.next := root; root := new
					ELSE
						c := root;
						WHILE (c.next # NIL) & (SYSTEM.VAL(INTEGER, new) > SYSTEM.VAL(INTEGER, c.next)) DO c := c.next END;
						new.next := c.next; c.next := new
					END;
					b := SYSTEM.VAL(FreeBlock, SYSTEM.VAL(INTEGER, new) + 12);
					b.size := (new.size - 12) DIV 16 * 16 - 4
				ELSE
					RETURN NIL	(* 4) give up *)
				END
			END
		END;
		(* b # NIL *)
		a := b.size + 4 - tsize;
		IF a > 0 THEN Insert(SYSTEM.VAL(FreeBlock, SYSTEM.VAL(INTEGER, b) + tsize), a) END;
		IF size > 0 THEN Erase(SYSTEM.ADR(b.size), (size + 3) DIV 4) END;
		INC(allocated, tsize);
		RETURN SYSTEM.VAL(Block, b)
	END NewBlock;*)
	
	PROCEDURE Allocated* (): INTEGER;
	BEGIN
		RETURN allocated
	END Allocated;
	
	PROCEDURE Used* (): INTEGER;
	BEGIN
		RETURN used
	END Used;
	
	PROCEDURE Root* (): INTEGER;
	BEGIN
		RETURN SYSTEM.VAL(INTEGER, root)
	END Root;
	

	(* -------------------- Trap Handling --------------------- *)

	PROCEDURE^ InitFpu;
	
	PROCEDURE Start* (code: Command);
	BEGIN
		restart := code;
		res := LinLibc.sigsetjmp(loopContext, LinLibc.TRUE);
		restart()
	END Start;
	
	PROCEDURE Quit* (exitCode: INTEGER);
		VAR m: Module; term: Command; t: BOOLEAN; res: INTEGER;
	BEGIN
		trapViewer := NIL; trapChecker := NIL; restart := NIL;
		t := terminating; terminating := TRUE; m := modList;
		WHILE m # NIL DO	(* call terminators *)
			IF ~static OR ~t THEN
				term := m.term; m.term := NIL;
				IF term # NIL THEN term() END
			END;
(*			ReleaseIPtrs(m);*)
			m := m.next
		END;
		CallFinalizers;
		hotFinalizers := finalizers; finalizers := NIL;
		CallFinalizers;
(*		WinOle.OleUninitialize();*)
	(*	IF ~inDll THEN
			KERNEL32.RemoveExcp(excpPtr^);
			KERNEL32.ExitProcess(exitCode)	(* never returns *)
		END*)
		res := LinLibc.fflush(0);
		LinLibc.exit(exitCode)
	END Quit;
	
	PROCEDURE FatalError* (id: INTEGER; str: ARRAY OF CHAR);
		VAR res: INTEGER; title: ARRAY 16 OF SHORTCHAR; text: ARRAY 256 OF SHORTCHAR;
	BEGIN
		title := "Error xy";
		title[6] := SHORT(CHR(id DIV 10 + ORD("0")));
		title[7] := SHORT(CHR(id MOD 10 + ORD("0")));
		text := SHORT(str$);
		res := MessageBox(title$, text$, {mbOk});
(*		WinOle.OleUninitialize();*)
(*		IF ~inDll THEN KERNEL32.RemoveExcp(excpPtr^) END;
		KERNEL32.ExitProcess(1)*)
		LinLibc.exit(1);
		(* never returns *)
	END FatalError;

	PROCEDURE DefaultTrapViewer;
		VAR len, ref, end, x, a, b, c: INTEGER; mod: Module;
			name: Name; out: ARRAY 1024 OF SHORTCHAR;
		
		PROCEDURE WriteString (s: ARRAY OF SHORTCHAR);
			VAR i: INTEGER;
		BEGIN
			i := 0;
			WHILE (len < LEN(out) - 1) & (s[i] # 0X) DO out[len] := s[i]; INC(i); INC(len) END
		END WriteString;
		
		PROCEDURE WriteHex (x, n: INTEGER);
			VAR i, y: INTEGER;
		BEGIN
			IF len + n < LEN(out) THEN
				i := len + n - 1;
				WHILE i >= len DO
					y := x MOD 16; x := x DIV 16;
					IF y > 9 THEN y := y + (ORD("A") - ORD("0") - 10) END;
					out[i] := SHORT(CHR(y + ORD("0"))); DEC(i)
				END;
				INC(len, n)
			END
		END WriteHex;

		PROCEDURE WriteLn;
		BEGIN
			IF len < LEN(out) - 1 THEN out[len] := 0AX (* 0DX on Windows *); INC(len) END
		END WriteLn;
		
	BEGIN
		len := 0;
		IF err = 129 THEN WriteString("invalid with")
		ELSIF err = 130 THEN WriteString("invalid case")
		ELSIF err = 131 THEN WriteString("function without return")
		ELSIF err = 132 THEN WriteString("type guard")
		ELSIF err = 133 THEN WriteString("implied type guard")
		ELSIF err = 134 THEN WriteString("value out of range")
		ELSIF err = 135 THEN WriteString("index out of range")
		ELSIF err = 136 THEN WriteString("string too long")
		ELSIF err = 137 THEN WriteString("stack overflow")
		ELSIF err = 138 THEN WriteString("integer overflow")
		ELSIF err = 139 THEN WriteString("division by zero")
		ELSIF err = 140 THEN WriteString("infinite real result")
		ELSIF err = 141 THEN WriteString("real underflow")
		ELSIF err = 142 THEN WriteString("real overflow")
		ELSIF err = 143 THEN WriteString("undefined real result")
		ELSIF err = 200 THEN WriteString("keyboard interrupt")
		ELSIF err = 202 THEN WriteString("illegal instruction:  ");
			WriteHex(val, 4)
		ELSIF err = 203 THEN WriteString("illegal memory read [ad = ");
			WriteHex(val, 8); WriteString("]")
		ELSIF err = 204 THEN WriteString("illegal memory write [ad = ");
			WriteHex(val, 8); WriteString("]")
		ELSIF err = 205 THEN WriteString("illegal execution [ad = ");
			WriteHex(val, 8); WriteString("]")
		ELSIF err < 0 THEN WriteString("exception #"); WriteHex(-err, 2)
		ELSE err := err DIV 100 * 256 + err DIV 10 MOD 10 * 16 + err MOD 10;
			WriteString("trap #"); WriteHex(err, 3)
		END;
		a := pc; b := fp; c := 12;
		REPEAT
			WriteLn; WriteString("- ");
			mod := modList;
			WHILE (mod # NIL) & ((a < mod.code) OR (a >= mod.code + mod.csize)) DO mod := mod.next END;
			IF mod # NIL THEN
				DEC(a, mod.code);
				IF mod.refcnt >= 0 THEN
					WriteString(mod.name); ref := mod.refs;
					REPEAT GetRefProc(ref, end, name) UNTIL (end = 0) OR (a < end);
					IF a < end THEN
						WriteString("."); WriteString(name)
					END
				ELSE
					WriteString("("); WriteString(mod.name); WriteString(")")
				END;
				WriteString("  ")
			END;
			WriteString("(pc="); WriteHex(a, 8);
			WriteString(", fp="); WriteHex(b, 8); WriteString(")");
			IF (b >= sp) & (b < stack) THEN
				SYSTEM.GET(b+4, a);	(* stacked pc *)
				SYSTEM.GET(b, b);	(* dynamic link *)
				DEC(c)
			ELSE c := 0
			END
		UNTIL c = 0;
		out[len] := 0X;
		x := MessageBox("BlackBox", out$, {mbOk})
	END DefaultTrapViewer;
	
	PROCEDURE TrapCleanup;
		VAR t: TrapCleaner;
	BEGIN
		WHILE trapStack # NIL DO
			t := trapStack; trapStack := trapStack.next; t.Cleanup
		END;
		IF (trapChecker # NIL) & (err # 128) THEN trapChecker END
	END TrapCleanup;
(*
	PROCEDURE Unwind(f: KERNEL32.ExcpFrmPtr);	(* COMPILER DEPENDENT *)
		CONST Label = 27;	(* offset of Label: from proc start *)
	BEGIN
		PushFP;
		KERNEL32.RtlUnwind(f, SYSTEM.ADR(Unwind) + Label, NIL, 0);
		(* Label: *)
		PopFP
	END Unwind;*)
	
(*	PROCEDURE TrapHandler (excpRec: KERNEL32.ExcpRecPtr; estFrame: KERNEL32.ExcpFrmPtr;
											context: KERNEL32.ContextPtr; dispCont: INTEGER): INTEGER;
		(* same parameter size as Try *)
	BEGIN
		IF excpRec^.flags * {1, 2} = {} THEN
			IF (excpRec.code MOD 256 = 4) & ~interrupted THEN	(* wrong trace trap *)
				context.debug[5] := 0;	(* disable all debug traps *)
				LdSP8; PopSI; PopDI; PopFP;	(* COMPILER DEPENDENT *)
				Return0(0)	(* return continueExecution without parameter remove *)
			END;
			Unwind(estFrame);
			IF trapped & (excpRec.code MOD 256 # 1) & (excpRec.code MOD 256 # 253) THEN
				DefaultTrapViewer;
				IF ~secondTrap THEN trapped := FALSE; secondTrap := TRUE END
			END;
			err := -(excpRec.code MOD 256);
			pc := context.ip; sp := context.sp; fp := context.bp; stack := baseStack;
			IF err = -4 THEN err := 200	(* keyboard interrupt *)
			ELSIF err = -5 THEN
				val := excpRec.info[1];
				IF val = pc THEN	(* call to undef adr *)
					err := 205; SYSTEM.GET(sp, pc); INC(sp, 4); DEC(pc)
				ELSIF excpRec.info[0] = 0 THEN	(* illegal read *)
					err := 203
				ELSE	(* illegal write *)
					err := 204
				END
			ELSIF (err = -29) OR (err = -30) THEN	(* illegal instruction *)
				err := 202; val := 0;
				IF IsReadable(excpRec.adr, excpRec.adr + 4) THEN
					SYSTEM.GET(excpRec.adr, val);
					IF val MOD 100H = 8DH THEN	(* lea reg,reg *)
						IF val DIV 100H MOD 100H = 0F0H THEN err := val DIV 10000H MOD 100H	(* trap *)
						ELSIF val DIV 1000H MOD 10H = 0EH THEN err := 128 + val DIV 100H MOD 10H	(* run time error *)
						END
					END
				END
			ELSIF err = -142 THEN DEC(pc); err := 140	(* fpu: div by zero *)
			ELSIF (err = -144) OR (err = -146) THEN DEC(pc); err := 143	;	(* fpu: invalid op *)
				val := context.float[0] MOD 4096 * 65536 + context.float[1] MOD 65536
			ELSIF err = -145 THEN DEC(pc); err := 142	(* fpu: overflow *)
			ELSIF err = -147 THEN DEC(pc); err := 141	(* fpu: underflow *)
			ELSIF err = -148 THEN err := 139	(* division by zero *)
			ELSIF err = -149 THEN err := 138	(* integer overflow *)
			ELSIF (err = -1) OR (err = -253) THEN err := 137	(* stack overflow *)
			END;
			INC(trapCount);
			InitFpu;
			IF err # 137 THEN	(* stack overflow handling is delayed *)
				TrapCleanup;
				IF err = 128 THEN	(* do nothing *)
				ELSIF(trapViewer # NIL) & (restart # NIL) & ~trapped & ~guarded THEN
					trapped := TRUE; trapViewer()
				ELSE DefaultTrapViewer
				END
			END;
			trapped := FALSE; secondTrap := FALSE;
			IF dispCont = 0 THEN	(* InterfaceTrapHandler *)	(* COMPILER DEPENDENT *)
				KERNEL32.RemoveExcp(estFrame^);
				SYSTEM.PUTREG(CX, estFrame(ExcpFramePtr).par);
				SYSTEM.PUTREG(SP, SYSTEM.VAL(INTEGER, estFrame) + 12);
				IF err = 137 THEN	(* retrigger stack overflow *)
					TrapCleanup; DefaultTrapViewer;
					res := KERNEL32.VirtualProtect(FPageWord(8), 1024, {2, 8}, old);
					IF res = 0 THEN res := KERNEL32.VirtualProtect(FPageWord(8), 1024, {0}, old) END
				END;
				PopSI; PopDI; PopBX; PopFP;
				ReturnCX(WinApi.E_UNEXPECTED)
			ELSIF estFrame # excpPtr THEN	(* Try failed *)	(* COMPILER DEPENDENT *)
				KERNEL32.RemoveExcp(estFrame^);
				res := SYSTEM.VAL(INTEGER, estFrame);
				SYSTEM.PUTREG(FP, res + (SIZE(KERNEL32.ExcpFrm) + 8));	(* restore fp *)
				SYSTEM.PUTREG(SP, res - 4);	(* restore stack *)
				IF err = 137 THEN	(* retrigger stack overflow *)
					TrapCleanup; DefaultTrapViewer;
					res := KERNEL32.VirtualProtect(FPageWord(8), 1024, {2, 8}, old);
					IF res = 0 THEN res := KERNEL32.VirtualProtect(FPageWord(8), 1024, {0}, old) END
				END;
				PopBX;
				RETURN 0	(* return from Try *)
			ELSIF restart # NIL THEN	(* Start failed *)
				SYSTEM.PUTREG(FP, baseStack);	(* restore fp *)
				SYSTEM.PUTREG(SP, baseStack);	(* restore stack *)
				IF err = 137 THEN	(* retrigger stack overflow *)
					TrapCleanup; DefaultTrapViewer;
					res := KERNEL32.VirtualProtect(FPageWord(8), 1024, {2, 8}, old);
					IF res = 0 THEN res := KERNEL32.VirtualProtect(FPageWord(8), 1024, {0}, old) END
				END;
				restart();
				Quit(1)
			ELSE	(* boot process failed *)
				Quit(1)
			END
			(* never returns *)
		ELSE
			LdSP8; PopSI; PopDI; PopFP;	(* COMPILER DEPENDENT *)
			Return0(1)	(* return continueSearch without parameter remove *)
		END
	END TrapHandler;
	*)
	PROCEDURE SetTrapGuard* (on: BOOLEAN);
	BEGIN
		guarded := on
	END SetTrapGuard;
(*
	PROCEDURE Try* (h: TryHandler; a, b, c: INTEGER);	(* COMPILER DEPENDENT *)
		(* same parameter size as TrapHandler *)
		VAR excp: KERNEL32.ExcpFrm;	(* no other local variables!  *)
	BEGIN
		PushBX;
		excp.handler := TrapHandler;
		KERNEL32.InstallExcp(excp); 
		h(a, b, c);
		KERNEL32.RemoveExcp(excp);
		PopBX
	END Try;
	*)

	PROCEDURE Try* (h: TryHandler; a, b, c: INTEGER);	
		VAR res: INTEGER; context: LinLibc.sigjmp_buf; oldContext: POINTER TO LinLibc.sigjmp_buf;
	BEGIN
		oldContext := currentTryContext;
		res := LinLibc.sigsetjmp(context, LinLibc.TRUE);
		currentTryContext := SYSTEM.ADR(context);
		IF res = 0 THEN (* first time around *)
			h(a, b, c);
		ELSIF res = trapReturn THEN  (* after a trap *)
		ELSE
			HALT(100)
		END;
		currentTryContext := oldContext;
	END Try;
	
	PROCEDURE InterfaceTrapHandler* (excpRec, estFrame, context, dispCont: INTEGER): INTEGER;	(* known to compiler *)
		VAR res: INTEGER;
	BEGIN
(*		res := TrapHandler(SYSTEM.VAL(KERNEL32.ExcpRecPtr, excpRec),
								SYSTEM.VAL(KERNEL32.ExcpFrmPtr, estFrame),
								SYSTEM.VAL(KERNEL32.ContextPtr, context),
								0);
		(* LdSP8 removes parameters of TrapHandler *)
		LdSP8; PopSI; PopDI; PopFP;	(* COMPILER DEPENDENT *)
		Return0(1);	(* return continueSearch without parameter remove *)
		IF FALSE THEN RETURN 0 END*)
		RETURN 0
	END InterfaceTrapHandler;
	
	(* -------------------- keyboard interrupt handling --------------------- *)
	
(*	PROCEDURE KeyboardWatcher (main: INTEGER): INTEGER;	(* runs in a thread *)
		VAR res, id, a, to: INTEGER; msg: USER32.Message; wnd: USER32.Handle;
			context: KERNEL32.Context; mod: Module;
	BEGIN
		wnd := USER32.CreateWindowExA({}, "Edit", "", {}, 0, 0, 0, 0, 0, 0, KERNEL32.GetModuleHandleA(NIL), 0);
		res := USER32.RegisterHotKey(wnd, 13, {1}, 3);	(* ctrl break *)
		IF res = 0 THEN
			res := USER32.RegisterHotKey(wnd, 14, {1, 2}, 3)	(* shift ctrl break *)
		END;
		LOOP
			res := USER32.GetMessageA(msg, 0, 0, 0);
			IF msg.message = USER32.WMHotKey THEN
				wnd := USER32.GetForegroundWindow();
				res := USER32.GetWindowThreadProcessId(wnd, id);
				IF (msg.wParam = 14) OR (id = KERNEL32.GetCurrentProcessId()) THEN
					to := KERNEL32.GetTickCount() + 1000;	(* 1 sec timeout *)
					REPEAT
						res := KERNEL32.SuspendThread(main);
						context.flags := {0, 16};
						res := KERNEL32.GetThreadContext(main, context);
						mod := modList; a := context.ip;
						WHILE (mod # NIL) & ((a < mod.code) OR (a >= mod.code + mod.csize)) DO mod := mod.next END;
						IF (mod # NIL) & (mod.name = "Kernel") THEN mod := NIL END;
						IF mod # NIL THEN
							interrupted := TRUE;
							INCL(SYSTEM.VAL(SET, context.pf), 8);	(* set trap flag *)
							res := KERNEL32.SetThreadContext(main, context)
						END;
						res := KERNEL32.ResumeThread(main);
						KERNEL32.Sleep(0);
						interrupted := FALSE
					UNTIL (mod # NIL) OR (KERNEL32.GetTickCount() > to)
				END
			END
		END;
		RETURN 0
	END KeyboardWatcher;*)
	
(*	PROCEDURE InstallKeyboardInt;
		VAR res, id: INTEGER; t, main: KERNEL32.Handle;
	BEGIN
		res := KERNEL32.DuplicateHandle(KERNEL32.GetCurrentProcess(), KERNEL32.GetCurrentThread(),
					KERNEL32.GetCurrentProcess(), main, {1, 3, 4, 16..19}, 0, {});
		t := KERNEL32.CreateThread(NIL, 4096, KeyboardWatcher, main, {}, id)
	END InstallKeyboardInt;*)
	
	(* -------------------- Initialization --------------------- *)
	
	PROCEDURE InitFpu;	(* COMPILER DEPENDENT *)	(* could be eliminated, delayed for backward compatibility *)
		VAR cw: SET;
	BEGIN
		FINIT;
		FSTCW;
		(* denorm, underflow, precision, zero div, overflow masked *)
		(* invalid trapped *)
		(* round to nearest, temp precision *)
		cw := cw - {0..5, 8..11} + {1, 2, 3, 4, 5, 8, 9};
		FLDCW
	END InitFpu;
	
	PROCEDURE TrapHandler (sig: INTEGER; siginfo: LinLibc.Ptrsiginfo_t; context: LinLibc.Ptrucontext_t);
	BEGIN
	(*
		SYSTEM.GETREG(SP, sp);
		SYSTEM.GETREG(FP, fp);
	*)
		stack := baseStack;
		sp := context.uc_mcontext.gregs[7]; (* TODO: is the stack pointer really stored in register 7? *)
		fp := context.uc_mcontext.gregs[6]; (* TODO: is the frame pointer really stored in register 6? *)
		pc := context.uc_mcontext.gregs[14]; (* TODO: is the pc really stored in register 14? *)
		val := siginfo.si_addr;
	(*
		Int(sig); Int(siginfo.si_signo); Int(siginfo.si_code); Int(siginfo.si_errno);
		Int(siginfo.si_status); Int(siginfo.si_value); Int(siginfo.si_int);
	*)
		err := sig;
		IF trapped THEN DefaultTrapViewer END;
		CASE sig OF
			LinLibc.SIGINT: 
				err := 200 (* Interrupt (ANSI). *)
			| LinLibc.SIGILL: (* Illegal instruction (ANSI). *)
				err := 202; val := 0;
				IF IsReadable(pc, pc + 4) THEN
					SYSTEM.GET(pc, val);
					IF val MOD 100H = 8DH THEN	(* lea reg,reg *)
						IF val DIV 100H MOD 100H = 0F0H THEN err := val DIV 10000H MOD 100H	(* trap *)
						ELSIF val DIV 1000H MOD 10H = 0EH THEN err := 128 + val DIV 100H MOD 10H	(* run time error *)
						END
					END
				END
			| LinLibc.SIGFPE: 
				CASE siginfo.si_code OF
					0: IF siginfo.si_int = 8 THEN err := 139 ELSIF siginfo.si_int = 0 THEN err := 143 END (* TODO: ?????? *)
					| LinLibc.FPE_INTDIV: err := 139 (* Integer divide by zero.  *)
					| LinLibc.FPE_INTOVF: err := 138 (* Integer overflow.  *)
					| LinLibc.FPE_FLTDIV: err := 140 (* Floating point divide by zero.  *)
					| LinLibc.FPE_FLTOVF: err := 142 (* Floating point overflow.  *)
					| LinLibc.FPE_FLTUND: err := 141 (* Floating point underflow.  *)
					| LinLibc.FPE_FLTRES: err := 143 (* Floating point inexact result.  *)
					| LinLibc.FPE_FLTINV: err := 143 (* Floating point invalid operation.  *)
					| LinLibc.FPE_FLTSUB: err := 134 (* Subscript out of range.  *)
				ELSE
				END
			| LinLibc.SIGSEGV: (* Segmentation violation (ANSI). *) 
				err := 203
		ELSE
		END;
		INC(trapCount);
		InitFpu;
		TrapCleanup;
		IF err # 128 THEN
			IF (trapViewer = NIL) OR trapped THEN
				DefaultTrapViewer
			ELSE
				trapped := TRUE;
				trapViewer();
				trapped := FALSE
			END
		END;
		IF currentTryContext # NIL THEN (* Try failed *)
			LinLibc.siglongjmp(currentTryContext, trapReturn)
		ELSE
			IF restart # NIL THEN (* Start failed *)
				LinLibc.siglongjmp(loopContext, trapReturn)
			END;
			Quit(1);
		END;
		trapped := FALSE
	END TrapHandler;
	
	PROCEDURE InstallSignals*;
		VAR sa, old: LinLibc.sigaction_t; res, i: INTEGER;
	BEGIN
		sa.sa_sigaction := TrapHandler;
(*		res := LinLibc.sigemptyset(SYSTEM.ADR(sa.sa_mask));*)
		res := LinLibc.sigfillset(SYSTEM.ADR(sa.sa_mask));
		sa.sa_flags := LinLibc.SA_SIGINFO; (* TrapHandler takes three arguments *)
		(*
		IF LinLibc.sigaction(LinLibc.SIGINT, sa, old) # 0 THEN Msg("failed to install SIGINT") END;
		IF LinLibc.sigaction(LinLibc.SIGILL, sa, old) # 0 THEN Msg("failed to install SIGILL") END;
		IF LinLibc.sigaction(LinLibc.SIGFPE, sa, old) # 0 THEN Msg("failed to install SIGFPE") END;
		IF LinLibc.sigaction(LinLibc.SIGSEGV, sa, old) # 0 THEN Msg("failed to install SIGSEGV") END;
		IF LinLibc.sigaction(LinLibc.SIGPIPE, sa, old) # 0 THEN Msg("failed to install SIGPIPE") END;
		IF LinLibc.sigaction(LinLibc.SIGTERM, sa, old) # 0 THEN Msg("failed to install SIGTERM") END;
		*)
		(* respond to all possible signals *)
		FOR i := 1 TO LinLibc._NSIG - 1 DO 
			IF (i # LinLibc.SIGKILL) & (i # LinLibc.SIGSTOP) THEN
				IF LinLibc.sigaction(i, sa, old) # 0 THEN Msg("failed to install signal"); Int(i) END;
			END
		END
	END InstallSignals;
	
	PROCEDURE SetCmdLine;
		VAR i, l: INTEGER;
	BEGIN
		l := LEN(cmdLine);
		cmdLine := bootInfo.argv[0]$;
		FOR i := 1 TO bootInfo.argc - 1 DO cmdLine := cmdLine + " " + bootInfo.argv[i]END
	END SetCmdLine;
	
	PROCEDURE Init;
		VAR (*excp: KERNEL32.ExcpFrm; *)t: Type; (*res: COM.RESULT;*) i: INTEGER; env: LinLibc.jmp_buf; res: LONGINT;
	BEGIN
		SetCmdLine;
		InstallSignals; (* init exception handling *)
		currentTryContext := NIL;
		t := SYSTEM.VAL(Type, SYSTEM.ADR(Command));	(* type desc of Command *)
		comSig := t.size;	(* size = signature fprint for proc types *)
		allocated := 0; total := 0; used := 0;
		sentinelBlock.size := MAX(INTEGER);
		sentinel := SYSTEM.ADR(sentinelBlock);
		
		SYSTEM.PUTREG(ML, SYSTEM.ADR(modList));

		IF dllMem THEN
			i := N;
			REPEAT DEC(i); free[i] := sentinel UNTIL i = 0;
			root := NIL;
(*			heap := KERNEL32.GetProcessHeap()*)
		ELSE
			i := N;
			REPEAT DEC(i); free[i] := sentinel UNTIL i = 0;
			AllocHeapMem(1, root); ASSERT(root # NIL, 100);
			i := MIN(N - 1, (root.size - 12) DIV 16 - 1);
			free[i] := SYSTEM.VAL(FreeBlock, SYSTEM.VAL(INTEGER, root) + 12);
			free[i].next := sentinel;
			free[i].size := (root.size - 12) DIV 16 * 16 - 4
		END;
		
(*		res := WinOle.OleInitialize(0);
		IF inDll THEN
			baseStack := FPageWord(4)	(* begin of stack segment *)
		ELSE
			InstallKeyboardInt;
			InitFpu
		END;
*)
	
		InitFpu;
		IF ~static THEN
			InitModule(modList);
			IF ~inDll THEN Quit(1) END
		END;
		
		told := 0; shift := 0;
	END Init;
	
BEGIN
	IF modList = NIL THEN	(* only once *)
		modList := bootInfo.modList; (* boot loader initializes the bootInfo struct *)
		SYSTEM.GETREG(SP, baseStack); (* TODO: Check that this is ok. *)
		static := init IN modList.opts;
		inDll := dll IN modList.opts; (*dllMem := inDll;*)
		Init
	END
CLOSE
	IF ~terminating THEN
		terminating := TRUE;
		Quit(0)
	END
END Kernel.

DevDecoder.Decode Kernel
