MODULE DevCPDump;

	IMPORT
		DevCPM, DevCPT, DevCPE, Views, TextModels, TextViews, TextMappers;
		
	
	CONST
		(* nodes classes *)
		Nvar = 0; Nvarpar = 1; Nfield = 2; Nderef = 3; Nindex = 4; Nguard = 5; Neguard = 6;
		Nconst = 7; Ntype = 8; Nproc = 9; Nupto = 10; Nmop = 11; Ndop = 12; Ncall = 13;
		Ninittd = 14; Nif = 15; Ncaselse = 16; Ncasedo = 17; Nenter = 18; Nassign = 19;
		Nifelse = 20; Ncase = 21; Nwhile = 22; Nrepeat = 23; Nloop = 24; Nexit = 25;
		Nreturn = 26; Nwith = 27; Ntrap = 28; Ncomp = 30;
		Ndrop = 50; Nlabel = 51; Ngoto = 52; Njsr = 53; Nret = 54; Ncmp = 55;
		
		(* operations *)
		times = 1; slash = 2; div = 3; mod = 4;
		and = 5; plus = 6; minus = 7; or = 8; eql = 9;
		neq = 10; lss = 11; leq = 12; gtr = 13; geq = 14;
		in = 15; is = 16; ash = 17; msk = 18; len = 19;
		conv = 20; abs = 21; cap = 22; odd = 23; not = 33;
		adr = 24; cc = 25; bit = 26; lsh = 27; rot = 28; val = 29;
		min = 34; max = 35; typfn = 36;
		shl = 50; shr = 51; lshr = 52; xor = 53;

		(*function number*)
		assign = 0;
		haltfn = 0; newfn = 1; absfn = 2; capfn = 3; ordfn = 4;
		entierfn = 5; oddfn = 6; minfn = 7; maxfn = 8; chrfn = 9;
		shortfn = 10; longfn = 11; sizefn = 12; incfn = 13; decfn = 14;
		inclfn = 15; exclfn = 16; lenfn = 17; copyfn = 18; ashfn = 19; assertfn = 32;
		lchrfn = 33; lentierfcn = 34; bitsfn = 37; bytesfn = 38;
		
		(*SYSTEM function number*)
		adrfn = 20; ccfn = 21; lshfn = 22; rotfn = 23;
		getfn = 24; putfn = 25; getrfn = 26; putrfn = 27;
		bitfn = 28; valfn = 29; sysnewfn = 30; movefn = 31;
		thisrecfn = 45; thisarrfn = 46;

		(* object modes *)
		Var = 1; VarPar = 2; Con = 3; Fld = 4; Typ = 5; LProc = 6; XProc = 7;
		SProc = 8; CProc = 9; IProc = 10; Mod = 11; Head = 12; TProc = 13; Attr = 20;

		(* structure forms *)
		Undef = 0; Byte = 1; Bool = 2; Char8 = 3; Int8 = 4; Int16 = 5; Int32 = 6;
		Real32 = 7; Real64 = 8; Set = 9; String8 = 10; NilTyp = 11; NoTyp = 12;
		Pointer = 13; ProcTyp = 14; Comp = 15;
		Char16 = 16; String16 = 17; Int64 = 18;
		
		(* composite structure forms *)
		Basic = 1; Array = 2; DynArr = 3; Record = 4;

		(* sysflags *)
		jint = -11; jarr = -12; jstr = -13;
		

	VAR
		w: TextMappers.Formatter;
		labelNum: INTEGER;
		indent: INTEGER;
		

	PROCEDURE^ DumpExp (exp: DevCPT.Node);
	PROCEDURE^ DumpStat (stat: DevCPT.Node);
	
	PROCEDURE Indent;
		VAR i: INTEGER;
	BEGIN
		i := 0; WHILE i < indent DO w.WriteTab; INC(i) END
	END Indent;
	
	PROCEDURE DumpConst (typ: DevCPT.Struct; con: DevCPT.Const);
		VAR i, hi, low: INTEGER; r: REAL; ch: CHAR;
	BEGIN
		IF typ = DevCPT.guidtyp THEN
			IF con # NIL THEN w.WriteString(con.ext$)
			ELSE w.WriteString(typ.ext$)
			END
		ELSE
			CASE typ.form OF
			| Int8, Int16, Int32: w.WriteInt(con.intval)
			| Char8, Char16: w.WriteIntForm(con.intval, TextMappers.hexadecimal, 2, "0", FALSE); w.WriteChar("X")
			| Bool: IF con.intval = 0 THEN w.WriteString("FALSE") ELSE w.WriteString("TRUE") END
			| Real32, Real64: w.WriteReal(con.realval)
			| Set: w.WriteSet(con.setval)
			| NilTyp: w.WriteString("NIL")
			| String8: w.WriteChar('"'); w.WriteString(con.ext$); w.WriteChar('"')
			| String16:
				w.WriteChar('"'); i := 0; DevCPM.GetUtf8(con.ext^, low, i);
				WHILE low # 0 DO w.WriteChar(CHR(low)); DevCPM.GetUtf8(con.ext^, low, i) END;
				w.WriteChar('"')
			| Int64:
				DevCPE.GetLongWords(con, hi, low);
				w.WriteIntForm(hi, TextMappers.hexadecimal, 8, "0", FALSE); 
				w.WriteIntForm(low, TextMappers.hexadecimal, 8, "0", FALSE); 
				w.WriteChar("L")
			END
		END
	END DumpConst;
	
	PROCEDURE DumpType (typ: DevCPT.Struct; def: DevCPT.Object);
	BEGIN
		IF (typ.strobj # NIL) & (typ.strobj # def) THEN w.WriteString(typ.strobj.name$)
		ELSIF typ.form = Pointer THEN
			w.WriteString("POINTER TO "); DumpType(typ.BaseTyp, def)
		ELSIF typ.comp = Record THEN
			w.WriteString("RECORD");
			IF typ.BaseTyp # NIL THEN
				w.WriteString(" ("); DumpType(typ.BaseTyp, def); w.WriteChar(")")
			END
		ELSIF typ.comp = Array THEN
			w.WriteString("ARRAY "); w.WriteInt(typ.n); w.WriteString(" OF "); DumpType(typ.BaseTyp, def)
		ELSIF typ.comp = DynArr THEN
			w.WriteString("ARRAY OF "); DumpType(typ.BaseTyp, def)
		ELSE w.WriteString("???")
		END
	END DumpType;
	
	PROCEDURE TravObj (obj: DevCPT.Object);
	BEGIN
		IF obj # NIL THEN
			TravObj(obj.left);
			IF (obj.mode = Var) OR (obj.mode = VarPar) THEN
				Indent; w.WriteString("VAR "); w.WriteString(obj.name$);
				w.WriteString(": "); DumpType(obj.typ, NIL); w.WriteLn
			ELSIF obj.mode = Typ THEN
				Indent; w.WriteString("TYPE "); w.WriteString(obj.name$);
				w.WriteString(" = "); DumpType(obj.typ, obj); w.WriteLn
			ELSIF obj.mode = Con THEN
				Indent; w.WriteString("CONST "); w.WriteString(obj.name$);
				w.WriteString(" = "); DumpConst(obj.typ, obj.conval); w.WriteLn
			END;
			TravObj(obj.right)
		END
	END TravObj;
	
	PROCEDURE DumpObjects (header: DevCPT.Object);
	BEGIN
		INC(indent);
		TravObj(header.right);
		DEC(indent)
	END DumpObjects;

	PROCEDURE DumpParList (list: DevCPT.Node);
	BEGIN
		w.WriteChar("(");
		WHILE list # NIL DO
			DumpExp(list); list := list.link;
			IF list # NIL THEN w.WriteString(", ") END
		END;
		w.WriteChar(")")
	END DumpParList;
	
	PROCEDURE DumpExp (exp: DevCPT.Node);
	BEGIN
		ASSERT(exp # NIL);
		CASE exp.class OF
		| Nvar: w.WriteString(exp.obj.name$); ASSERT(exp.obj.name^ # "");
		| Nvarpar: w.WriteChar("'"); w.WriteString(exp.obj.name$)
		| Nfield: DumpExp(exp.left); w.WriteChar("."); w.WriteString(exp.obj.name$);
			IF exp.subcl = 1 THEN w.WriteChar("^") ELSIF exp.subcl = 3 THEN w.WriteChar("@") END
		| Nderef: DumpExp(exp.left); IF exp.subcl = 0 THEN w.WriteChar("^") ELSE w.WriteChar("$") END;
		| Nindex: DumpExp(exp.left); w.WriteChar("["); DumpExp(exp.right); w.WriteChar("]")
		| Nguard: DumpExp(exp.left); w.WriteChar("("); w.WriteString(exp.typ.strobj.name$); w.WriteChar(")")
		| Ntype: w.WriteString(exp.obj.name$)
		| Nproc: w.WriteString(exp.obj.name$)
		| Nconst: DumpConst(exp.typ, exp.conval)
		| Nupto: w.WriteChar("{"); DumpExp(exp.left); w.WriteString(".."); DumpExp(exp.left); w.WriteChar("}")
		| Nmop:
			CASE exp.subcl OF
			| not: w.WriteString("NOT(")
			| minus: w.WriteString("MINUS(")
			| is: w.WriteString("IS("); w.WriteString(exp.obj.name$); w.WriteString(", ")
			| conv: w.WriteString("CONV("); w.WriteString(exp.typ.strobj.name$); w.WriteString(", ")
			| abs: w.WriteString("ABS(")
			| cap: w.WriteString("CAP(")
			| odd: w.WriteString("ODD(")
			| adr: w.WriteString("ADR(")
			| cc: w.WriteString("CC(")
			| val: w.WriteString("VAL(")
			| bit: w.WriteString("BITS(")
			| typfn: w.WriteString("TYP(")
			END;
			DumpExp(exp.left); w.WriteChar(")")
		| Ndop:
			w.WriteChar("("); DumpExp(exp.left);
			CASE exp.subcl OF
			| times: w.WriteString(" * ")
			| slash: w.WriteString(" / ")
			| div: w.WriteString(" DIV ")
			| mod: w.WriteString(" MOD ")
			| and: w.WriteString(" & ")
			| or: w.WriteString(" OR ")
			| plus: w.WriteString(" + ")
			| minus: w.WriteString(" - ")
			| eql: w.WriteString(" = ")
			| neq: w.WriteString(" # ")
			| lss: w.WriteString(" < ")
			| leq: w.WriteString(" <= ")
			| gtr: w.WriteString(" > ")
			| geq: w.WriteString(" >= ")
			| in: w.WriteString(" IN ")
			| ash: w.WriteString(" ASH ")
			| msk: w.WriteString(" MASK ")
			| len: w.WriteString(" LEN ")
			| bit: w.WriteString(" BIT ")
			| lsh: w.WriteString(" LSH ")
			| rot: w.WriteString(" ROT ")
			| min: w.WriteString(" MIN ")
			| max: w.WriteString(" MAX ")
			| thisrecfn: w.WriteString(" THISRECORD ")
			| thisarrfn: w.WriteString(" THISARRAY ")
			| shl: w.WriteString(" SHL ")
			| shr: w.WriteString(" SHR ")
			| lshr: w.WriteString(" LSHR ")
			| xor: w.WriteString(" XOR ")
			END;
			DumpExp(exp.right); w.WriteChar(")")
		| Ncall: DumpExp(exp.left); DumpParList(exp.right)
		| Ncomp: w.WriteString("COMP(");
			DumpStat(exp.left); DumpExp(exp.right);
			w.WriteString(")");
		| Ncmp: w.WriteChar("("); DumpExp(exp.left); w.WriteString(" CMP "); DumpExp(exp.right); w.WriteChar(")")
		END;
	END DumpExp;
	
	PROCEDURE LabelNum (stat: DevCPT.Node);
	BEGIN
		IF stat.conval.realval = 0 THEN
			INC(labelNum); stat.conval.realval := labelNum
		END;
		w.WriteInt(SHORT(ENTIER(stat.conval.realval)))
	END LabelNum;
	
	PROCEDURE DumpIf (s1, s2, s3: ARRAY OF CHAR; stat: DevCPT.Node);
		VAR first: BOOLEAN;
	BEGIN
		first := TRUE;
		WHILE stat # NIL DO
			ASSERT(stat.class = Nif);
			IF first THEN w.WriteString(s1) ELSE Indent; w.WriteString(s2) END;
			DumpExp(stat.left); w.WriteString(s3); w.WriteLn;
			DumpStat(stat.right);
			stat := stat.link; first := FALSE;
		END
	END DumpIf;
	
	PROCEDURE DumpCase (stat: DevCPT.Node);
		VAR n: DevCPT.Node;
	BEGIN
		WHILE stat # NIL DO
			ASSERT(stat.class = Ncasedo);
			Indent; w.WriteString("| "); n := stat.left;
			WHILE n # NIL DO
				ASSERT(n.class = Nconst);
				ASSERT(n.conval # NIL);
				w.WriteInt(n.conval.intval);
				IF n.conval.intval2 # n.conval.intval THEN w.WriteString(".."); w.WriteInt(n.conval.intval2) END;
				n := n.link;
				IF n # NIL THEN w.WriteString(", ") END
			END;
			w.WriteString(":"); w.WriteLn;
			DumpStat(stat.right);
			stat := stat.link
		END
	END DumpCase;
	
	PROCEDURE DumpStat (stat: DevCPT.Node);
	BEGIN
		INC(indent);
		WHILE stat # NIL DO
			Indent;
			CASE stat.class OF
			| Ninittd: w.WriteString("INIT TYPEDESC")
			| Nassign:
				CASE stat.subcl OF
				| assign: DumpExp(stat.left); w.WriteString(" := "); DumpExp(stat.right)
				| newfn: DumpExp(stat.left); w.WriteString(" := NEW "); DumpParList(stat.right)
				| incfn: w.WriteString("INC "); DumpExp(stat.left); w.WriteString(", "); DumpExp(stat.right)
				| decfn: w.WriteString("DEC "); DumpExp(stat.left); w.WriteString(", "); DumpExp(stat.right)
				| inclfn: w.WriteString("INCL "); DumpExp(stat.left); w.WriteString(", "); DumpExp(stat.right)
				| exclfn: w.WriteString("EXCL "); DumpExp(stat.left); w.WriteString(", "); DumpExp(stat.right)
				| copyfn: w.WriteString("COPY "); DumpExp(stat.left); w.WriteString(", "); DumpExp(stat.right)
				| getfn: w.WriteString("GET "); DumpExp(stat.left); w.WriteString(", "); DumpExp(stat.right)
				| putfn: w.WriteString("PUT "); DumpExp(stat.left); w.WriteString(", ");  DumpExp(stat.right)
				| movefn: w.WriteString("MOVE "); DumpExp(stat.left); w.WriteString(", "); DumpExp(stat.right);
					w.WriteString(", "); DumpExp(stat.right.link)
				END
			| Ncall: w.WriteString("CALL "); DumpExp(stat.left); DumpParList(stat.right)
			| Nifelse: DumpIf("IF ", "ELSIF ", " THEN ", stat.left);
				IF stat.right # NIL THEN Indent; w.WriteString("ELSE"); w.WriteLn; DumpStat(stat.right) END;
				Indent; w.WriteString("END")
			| Ncase: w.WriteString("CASE "); DumpExp(stat.left); w.WriteString(" OF"); w.WriteLn;
				DumpCase(stat.right.left);
				IF stat.right.right # NIL THEN Indent; w.WriteString("ELSE"); w.WriteLn; DumpStat(stat.right.right) END;
				Indent; w.WriteString("END")
			| Nwhile: w.WriteString("WHILE "); DumpExp(stat.left); w.WriteString(" DO"); w.WriteLn;
				DumpStat(stat.right); Indent; w.WriteString("END")
			| Nrepeat: w.WriteString("REPEAT"); w.WriteLn; DumpStat(stat.left);
				Indent; w.WriteString("UNTIL "); DumpExp(stat.right)
			| Nloop: w.WriteString("LOOP"); w.WriteLn; DumpStat(stat.right); Indent; w.WriteString("END")
			| Nexit: w.WriteString("EXIT");
			| Nreturn: w.WriteString("RETURN "); DumpParList(stat.left)
			| Nwith: DumpIf("WITH ", "| ", " DO", stat.left);
				IF stat.right # NIL THEN Indent; w.WriteString("ELSE"); w.WriteLn; DumpStat(stat.right) END;
				Indent; w.WriteString("END")
			| Ntrap: w.WriteString("TRAP "); DumpExp(stat.right)
			| Ncomp: w.WriteString("COMPOUND"); w.WriteLn;
				DumpStat(stat.left); DumpStat(stat.right);
				Indent; w.WriteString("END COMPOUND");
			| Ndrop: w.WriteString("DROP "); DumpExp(stat.left);
				IF stat.right # NIL THEN w.WriteChar(" "); DumpExp(stat.right) END
			| Nlabel: w.WriteString("LABEL "); LabelNum(stat)
			| Ngoto: w.WriteString("GOTO "); LabelNum(stat.right);
				IF stat.left # NIL THEN w.WriteString(" if "); DumpExp(stat.left) END
			| Njsr: w.WriteString("JSR "); LabelNum(stat.right)
			| Nret: w.WriteString("RET")
			END;
			w.WriteChar(";"); w.WriteLn;
			stat := stat.link
		END;
		DEC(indent)
	END DumpStat;
	
	PROCEDURE DumpProc (proc: DevCPT.Node);
	BEGIN
		INC(indent);
		WHILE proc # NIL DO
			ASSERT(proc.class = Nenter); ASSERT(proc.obj # NIL);
			Indent; w.WriteString("PROCEDURE "); w.WriteString(proc.obj.name$); w.WriteLn;
			DumpObjects(proc.obj.scope);
			Indent; w.WriteString("BEGIN"); w.WriteLn;
			labelNum := 0;
			DumpStat(proc.right);
			Indent; w.WriteString("END "); w.WriteString(proc.obj.name$); w.WriteLn; w.WriteLn;
			proc := proc.link
		END;
		DEC(indent)
	END DumpProc;

	PROCEDURE DumpTree* (prog: DevCPT.Node);
		VAR t: TextModels.Model;
	BEGIN
		t := TextModels.dir.New(); w.ConnectTo(t);
		ASSERT(prog.class = Nenter); ASSERT(prog.obj = NIL);
		indent := 0;
		w.WriteString("MODULE "); w.WriteString(DevCPT.SelfName$); w.WriteLn; w.WriteLn;
		DumpObjects(DevCPT.GlbMod[0]); w.WriteLn;
		DumpProc(prog.left);
		w.WriteString("BEGIN"); w.WriteLn;
		DumpStat(prog.right);
		w.WriteString("CLOSE"); w.WriteLn;
		DumpStat(prog.link);
		w.WriteString("END "); w.WriteString(DevCPT.SelfName$); w.WriteLn;
		Views.OpenAux(TextViews.dir.New(t), "Dump");
		w.ConnectTo(NIL)
	END DumpTree;
	
	
END DevCPDump.
