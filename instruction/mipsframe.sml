structure MipsFrame : FRAME = struct

	val ZERO = Temp.newtemp()
	val AT = Temp.newtemp()

	val V0 = Temp.newtemp()
	val V1 = Temp.newtemp()

	val A0 = Temp.newtemp()
	val A1 = Temp.newtemp()
	val A2 = Temp.newtemp()
	val A3 = Temp.newtemp()

	val T0 = Temp.newtemp()
	val T1 = Temp.newtemp()
	val T2 = Temp.newtemp()
	val T3 = Temp.newtemp()
	val T4 = Temp.newtemp()
	val T5 = Temp.newtemp()
	val T6 = Temp.newtemp()
	val T7 = Temp.newtemp()
	val T8 = Temp.newtemp()
	val T9 = Temp.newtemp()

	val S0 = Temp.newtemp()
	val S1 = Temp.newtemp()
	val S2 = Temp.newtemp()
	val S3 = Temp.newtemp()
	val S4 = Temp.newtemp()
	val S5 = Temp.newtemp()
	val S6 = Temp.newtemp()
	val S7 = Temp.newtemp()

	val K0 = Temp.newtemp()
	val K1 = Temp.newtemp()

	val GP = Temp.newtemp()
	val SP = Temp.newtemp()
	val FP = Temp.newtemp()
	val RA = Temp.newtemp()

	val wordSize = 4
	val numArgRegs = 4

	val callersaves = [T0, T1, T2, T3, T4, T5, T6, T7, T8, T9]
	val calleesaves = [S0, S1, S2, S3, S4, S5, S6, S7]
	val args = [A0, A1, A2, A3]
	val reserved = [ZERO, AT, V0, V1, K0, K1, GP, SP, FP, RA]

	datatype access = InFrame of int | InReg of Temp.temp

	type frame = {name: Temp.label, formals: access list, numFrameLocals: int ref}


	datatype frag = PROC of {body: Tree.stm, frame: frame}
				   |STRING of Temp.label * string

	fun printFrame {name, formals, numFrameLocals} = (print(Symbol.name(name)); 
												map (fn x => case x of InFrame(num) => print(" InFrame(" ^ Int.toString(num) ^ ") ")
																      |InReg(temp) => print (" InReg(" ^ Int.toString(temp) ^ ") ")) formals;
												print (Int.toString(!numFrameLocals));
												print ("\n")	)

	fun printAccess (InFrame(num)) = print("Alloc InFrame(" ^ Int.toString(num) ^ ") \n")
	   |printAccess (InReg(temp)) = print ("Alloc InReg(" ^ Int.toString(temp) ^ ") \n")


	fun allocLocal ({name, formals, numFrameLocals}:frame) (true) = ( numFrameLocals := !numFrameLocals + 1;
	  																  InFrame( ~1 * (!numFrameLocals-1) * wordSize ) )
	   |allocLocal ({name, formals, numFrameLocals}:frame) (false) =  InReg(Temp.newtemp())

	fun name {name, formals, numFrameLocals} = name

	fun formals {name, formals, numFrameLocals} = formals

	fun newFrame {name, formals} = 
		let val formalsOnStack = ref 0
			val regsUsed = ref 0
			fun processBool true = ( formalsOnStack := !formalsOnStack + 1; InFrame( ~1 * (!formalsOnStack-1) * wordSize ) )
			   |processBool false = if !regsUsed < numArgRegs 
			   						then (regsUsed := !regsUsed +1; InReg(Temp.newtemp())) 
			   						else processBool true

		in 
			{name = name, formals = (map processBool formals), numFrameLocals = formalsOnStack}
		end


	fun exp (InFrame(k)) (fp) = Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST(k)))
	   |exp (InReg(k)) (fp) = Tree.TEMP(k)
(*	   |exp (_)(_) = (print("FP not given in frame exp"); Tree.CONST(0))*)

	fun externalCall(str, expList) = Tree.CALL(Tree.NAME(Temp.namedlabel(str)), expList)



	fun procEntryExit2 (frame, body) = body @ [Assem.OPER{assem="",
														  src=(reserved @ calleesaves),
														  dst=[], jump=SOME([])}]




end


