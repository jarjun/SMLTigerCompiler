structure MipsFrame : FRAME = struct

	
	val RV = Temp.newtemp()
	val FP = Temp.newtemp()
	val wordSize = 4
	val numArgRegs = 4

	(* TODO view shift instrs?? *)

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










end


