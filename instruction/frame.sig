signature FRAME = 
sig 
	type frame
	type access

	val FP : Temp.temp
	val V0 : Temp.temp
	val SP : Temp.temp

	val wordSize : int

	val exp : access -> Tree.exp -> Tree.exp 

	val newFrame : {name: Temp.label, formals: bool list} -> frame
	val name : frame -> Temp.label
	val formals : frame -> access list
	val allocLocal : frame -> bool -> access
	
	val externalCall : string * Tree.exp list -> Tree.exp
	(*val procEntryExit1 : frame -> Tree.stm -> Tree.stm*)
	
	val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
(*	val procEntryExit3 : frame * Assem.instr list -> {prolog:string, body: Assem.instr list, epilog: string}*)

	datatype frag = PROC of {body: Tree.stm, frame: frame}
				   |STRING of Temp.label * string

	
	val printFrame : frame -> unit
	val printAccess : access -> unit

	val getCallerSaves: unit -> Temp.temp list
	val getCalleeSaves: unit -> Temp.temp list
	val getArgTemps: unit -> Temp.temp list
	val getReturnRegisters: unit -> Temp.temp list
	val getReturnAddress: unit -> Temp.temp
	
end