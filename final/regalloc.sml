structure RegAlloc:
sig

	type allocation = string Temp.Table.table
	val alloc : Assem.instr list * MipsFrame.frame -> Assem.instr list * allocation

end
=
struct
	type allocation = string Temp.Table.table


	fun spillManager( [], _, alloc, instrs, curSpilled) = (instrs, alloc, curSpilled)
	   |spillManager( _ , [], alloc, instrs, curSpilled) = (ErrorMsg.error ~1 "Cannot register allocate this program"; (instrs, alloc, curSpilled))
	   |spillManager( _ , regToSpill::spillableRegs, alloc, instrs, curSpilled) = 
			
			let (*val _ = print("Spilling " ^ MipsFrame.regToString(regToSpill) ^ "\n")*)

				val instrListNoSink = List.take(instrs, List.length(instrs) - 1)
				val newSink = Assem.OPER{assem="",
										 src=( MipsFrame.getSinkRegs() @ spillableRegs),
										 dst=[], jump=SOME([])}
				val newInstrs = instrListNoSink @ [newSink]

				(*redo everything*)
				val (flowgraph, nodes) = MakeGraph.instrs2graph(newInstrs)
		        val (interference, _) = Liveness.interferenceGraph(flowgraph)
		        val (newAlloc, spills) = Color.color({interference=interference, initial=MipsFrame.getPrecoloredAlloc(), spillCost=(fn x => 1), registers=MipsFrame.getAllRegStrs()})
		        (*val _ = Color.printAlloc(newAlloc, interference)*)


			in
				spillManager(spills, spillableRegs, newAlloc, newInstrs, curSpilled @ [regToSpill])
			end






	fun alloc(instrs, frame) = 
		let	val (flowgraph, nodes) = MakeGraph.instrs2graph(instrs)
	        val (interference, _) = Liveness.interferenceGraph(flowgraph)
	        val (alloc, spills) = Color.color({interference=interference, initial=MipsFrame.getPrecoloredAlloc(), spillCost=(fn x => 1), registers=MipsFrame.getAllRegStrs()})
	        (*val _ = Color.printAlloc(alloc, interference)*)

	        val (finInstrs, finAlloc, spillsOnFrame) = spillManager(spills, MipsFrame.getCalleeSaves(), alloc, instrs, [])

	        val finInstrsForReal = #body(MipsFrame.procEntryExit3(frame, finInstrs, spillsOnFrame))
	    in
(*			print("spills:\n");
			app (fn x => print(Temp.makestring(x) ^ ", ") ) spills;*)

(*			spillManager(spills, MipsFrame.getCalleeSaves(), alloc, instrs, []);*)

			(finInstrsForReal, finAlloc)
		
		end
end