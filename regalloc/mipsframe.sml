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

	val callersaves = [(T0, "$t0"), (T1, "$t1"), (T2, "$t2"), (T3, "$t3"), (T4, "$t4"), (T5, "$t5"), (T6, "$t6"), (T7, "$t7"), (T8, "$t8"), (T9, "$t9")]
	val calleesaves = [(S0, "$s0"), (S1, "$s1"), (S2, "$s2"), (S3, "$s3"), (S4, "$s4"), (S5, "$s5"), (S6, "$s6"), (S7, "$s7")]
	val args = [(A0, "$a0"), (A1, "$a1"), (A2, "$a2"), (A3, "$a3")]
	val reserved = [(ZERO, "$r0"), (AT, "$at"), (V0, "$v0"), (V1, "$v1"), (K0, "$k0"), (K1, "$k1"), (GP, "$gp"), (SP, "$sp"), (FP, "$fp"), (RA, "$ra")]

	datatype access = InFrame of int | InReg of Temp.temp

	type frame = {name: Temp.label, formals: access list, numFrameLocals: int ref}


	datatype frag = PROC of {body: Tree.stm, frame: frame}
				   |STRING of Temp.label * string

	val tempMap = 
		let
			fun helper((k, v), running) = Temp.Table.enter(running, k, v)
		in
			foldl helper Temp.Table.empty (callersaves @ calleesaves @ args @ reserved)
		end

	fun regToString(reg) = case Temp.Table.look(tempMap, reg) of SOME(s) => s
																|NONE    => Temp.makestring(reg)


	fun properIntToString i = if i < 0 
						  then ("-" ^ Int.toString(i * ~1))
						  else Int.toString(i)


	fun getPrecoloredAlloc() = foldl (fn ((a,b), running) => Temp.Table.enter(running, a, b)) Temp.Table.empty (callersaves @ calleesaves @ args @ reserved)
	fun getAllRegStrs() = map (fn (a,b) => b) (callersaves @ calleesaves @ args @ reserved)

	fun getCallerSaves() = map (fn (a,b) => a) callersaves
	fun getCallerSavesStr() = map (fn (a,b) => b) callersaves
	fun getCalleeSaves() = map (fn (a,b) => a) calleesaves
	fun getArgRegs() = map (fn (a,b) => a) args
	fun getReservedRegs() = map (fn (a,b) => a) reserved
	fun getReturnRegisters() = [V0, V1]
	fun getReturnAddress() = RA

	fun  seq [s] = s
	    |seq [first,second] = Tree.SEQ(first, second)
	    |seq (a::l) = Tree.SEQ(a, seq l)
	    |seq [] = ((*print("Used seq incorrectly");*) Tree.EXP(Tree.CONST(0)))


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
			   |processBool false = (regsUsed := !regsUsed +1; InReg(Temp.newtemp()))

		in 
			{name = name, formals = (map processBool formals), numFrameLocals = formalsOnStack}
		end


	fun exp (InFrame(k)) (fp) = Tree.MEM(Tree.BINOP(Tree.PLUS, fp, Tree.CONST(k)))
	   |exp (InReg(k)) (fp) = Tree.TEMP(k)
(*	   |exp (_)(_) = (print("FP not given in frame exp"); Tree.CONST(0))*)

	fun externalCall(str, expList) = Tree.CALL(Tree.NAME(Temp.namedlabel(str)), expList)


    (* TODO procEntryExit1 does label, moves args to where the callee expects them, more? *)

    fun procEntryExit1 (frame:frame, body:Tree.stm) = 
    	let val frameLabel = name(frame)
    		val num = List.length(formals(frame))
    		val offset = num * ~4


    		fun inner(formals, idx) = 
    			if idx >= num then [] else
	    			
	    			if idx < 4
	    			
	    			then 
	    				[
	    				 case List.nth(formals, idx) of
	    					InFrame i => Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP(FP), Tree.CONST(i))), Tree.TEMP(List.nth(getArgRegs(), idx)))
	    				   |InReg t => Tree.MOVE(Tree.TEMP(t), Tree.TEMP(List.nth(getArgRegs(), idx)))
	    				]
	    				@ 
	    				inner(formals, idx+1)
	    				

	    			else     				
	    				[
	    				 case List.nth(formals, idx) of
	    					InFrame i => Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP(FP), Tree.CONST(i))), Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP(FP), Tree.CONST( ((  (idx+1)-4) * 4) )))  )
	    				   |InReg t => Tree.MOVE(Tree.TEMP(t), Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.TEMP(FP), Tree.CONST(  (( (idx+1)-4 ) * 4)  ) ))  )
	    				]
	    				@ 
	    				inner(formals, idx+1)
					


    	in
    		seq ( [Tree.LABEL(frameLabel)] @  inner(formals(frame), 0)  @ [body] )
    	end


	fun procEntryExit2 (frame:frame, body) = body @ [Assem.OPER{assem="\n",
														  src=((map (fn (a,b) => a) reserved) @ (map (fn (a,b) => a) calleesaves)),
														  dst=[], jump=SOME([])}]

	fun procEntryExit3 ({name, formals, numFrameLocals}, lab::body, spills) = 
		let
			val ret = Assem.OPER{assem="jr `s0\n\n",
							   src= (*(map (fn (a,b) => a) reserved) @ (map (fn (a,b) => a) calleesaves)*) [RA],
							   dst=[], jump=SOME([])}

			
			val saveRA = Assem.OPER{assem="sw `s0, " ^ properIntToString(!numFrameLocals * ~4) ^ "(`s1) \n", 
									src=[RA, FP], dst=[], jump=NONE}

			val loadRA = Assem.OPER{assem="lw `d0, " ^ properIntToString(!numFrameLocals * ~4) ^ "(`s0) \n", 
									src=[FP], dst=[RA], jump=NONE}

			


			val counter = ref 0 (* return addr at 0*)
			val spillStores = map (fn x => (counter:= !counter+1; 
												Assem.OPER{
			   									assem="sw `s0, " ^ properIntToString((!numFrameLocals + !counter+1) * ~4) ^ "(`s1) \n",
												src=[x, FP], dst=[], jump=NONE})  )   spills


			val counter = ref 0 (* return addr at 0*)
			val spillLoads = map (fn x => (counter:= !counter+1; 
												Assem.OPER{
			   									assem="lw `d0, " ^ properIntToString((!numFrameLocals + !counter+1) * ~4) ^ "(`s0) \n",
												src=[FP], dst=[x], jump=NONE})  )   spills
			val stackSize = (!numFrameLocals+List.length(spills)+2)*(~4)
			val fpLoc = (!numFrameLocals+1) * ~4
			val allocStack = [Assem.OPER{
									assem="sw `s0, " ^ properIntToString(fpLoc) ^ "(`s1) \n", 
									src=[FP, SP], dst=[], jump=NONE},
							  Assem.MOVE{
									assem="move `d0, `s0\n",
									src=SP, dst=FP},
							  Assem.OPER{
									assem="addi `d0, `s0, " ^ properIntToString(stackSize) ^ "\n",
									src=[SP], dst=[SP], jump=NONE }]
			val deallocStack = [Assem.OPER{
									assem="addi `d0, `s0, " ^ properIntToString(~1*stackSize) ^ "\n",
									src=[SP], dst=[SP], jump=NONE },
								Assem.OPER{
									assem="lw `d0, " ^ properIntToString(fpLoc) ^ "(`s0)\n",
									src=[FP], dst=[FP], jump=NONE }] 

		in
			{prolog= "PROCEDURE " ^ Symbol.name(name) ^ "\n", 
			 body= [lab] @ 
			 	   allocStack @
			 	   [saveRA] @
			 	   spillStores @ 
			 	   
			 	   body @ 
			 	   
			 	   [loadRA] @
			 	   spillLoads @
			 	   deallocStack @ 
			 	   [ret], 
			 epilog="END " ^ Symbol.name(name) ^ "\n"}
		end
		

end


