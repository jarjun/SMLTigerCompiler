structure MipsFrame : FRAME = struct

	
	val RV = Temp.newtemp()
	val FP = Temp.newtemp()
	val wordSize = 4

	(* TODO view shift instrs?? *)

	datatype access = InFrame of int | InReg of Temp.temp

	type frame = {name: Temp.label, formals: access list, numFrameLocals: int ref}

	fun allocLocal ({name, formals, numFrameLocals}:frame) (true) = ( numFrameLocals := !numFrameLocals + 1;
																	  print("Num locals after this add: " ^ Int.toString(!numFrameLocals) ^ "\n" );
	  																  InFrame( ~1 * !numFrameLocals * wordSize ) )
	   |allocLocal ({name, formals, numFrameLocals}:frame) (false) = InReg(Temp.newtemp())

	fun name {name, formals, numFrameLocals} = name

	fun formals {name, formals, numFrameLocals} = formals

	fun newFrame {name, formals} = 
		let val formalsOnStack = ref 0
			fun processBool true = ( formalsOnStack := !formalsOnStack + 1; InFrame( ~1 * !formalsOnStack * wordSize ) )
			   |processBool false = InReg(Temp.newtemp())

		in 
			{name = name, formals = (map processBool formals), numFrameLocals = formalsOnStack}
		end


	fun printFrame {name, formals, numFrameLocals} = (print(Symbol.name(name)); 
													map (fn x => case x of InFrame(num) => print(" InFrame(" ^ Int.toString(num) ^ ") ")
																	      |InReg(temp) => print (" InReg(" ^ Int.toString(temp) ^ ") ")) formals;
													print (Int.toString(!numFrameLocals));
													print ("\n")	)
	fun printAccess (InFrame(num)) = print(" InFrame(" ^ Int.toString(num) ^ ") \n")
	   |printAccess (InReg(temp)) = print (" InReg(" ^ Int.toString(temp) ^ ") \n")

end