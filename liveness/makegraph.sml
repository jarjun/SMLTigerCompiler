structure FG = FuncGraph(struct
                          type ord_key = string
                          val compare = String.compare
                         end)

structure MakeGraph:
sig
	type info 
	val instrs2graph: Assem.instr list -> info FG.graph * info FG.node list
end
=
struct

	type info = {defs: Temp.temp list,
			    uses: Temp.temp list,
			    ismove: bool}

	val instrIdx = ref 0

	structure labelMapStruct = SplayMapFn(struct
				                            type ord_key = string
				                            val compare = String.compare
				                          end)
	val labelMap:(info FG.node labelMapStruct.map ref) = ref labelMapStruct.empty

	fun getNewID(instr) = 
		let val x = !instrIdx
			val format0 = Assem.format(MipsFrame.regToString)
			in 
				instrIdx := x + 1;
				Int.toString(x) ^ " " ^ (format0 instr)
			end
					
	fun printNode(id, {defs, uses, ismove}) = 
		let fun temps2string (t, s) = s ^ ", " ^ MipsFrame.regToString(t)
		in
			id ^ "defs: " ^ (foldl temps2string "" defs) ^ " uses: " ^ (foldl temps2string "" uses) ^ " ismove: " ^ Bool.toString(ismove)
		end

	fun instrs2graph instrList = 
		let fun makeNodes (instr as Assem.OPER{assem, dst, src, jump}, graph) = 
				let val newID = getNewID(instr)
					val newInfo = {defs=dst,
								   uses=src,
								   ismove=false}
				in
					FG.addNode(graph, newID, newInfo)
				end

			   |makeNodes (instr as Assem.MOVE{assem, dst, src}, graph)= 
				let val newID = getNewID(instr)
					val newInfo = {defs=[dst],
								   uses=[src],
								   ismove=true}
				in
					FG.addNode(graph, newID, newInfo)
				end

			   |makeNodes (instr as Assem.LABEL{assem, lab}, graph)=
			   	let val newID = getNewID(instr)
					val newInfo = {defs=[],
								   uses=[],
								   ismove=false}
					val (newGraph, newNode) = FG.addNode'(graph, newID, newInfo)
				in
					labelMap := labelMapStruct.insert(!labelMap, Symbol.name(lab), newNode);
					newGraph
				end

		in 
			foldl makeNodes FG.empty instrList;
			(FG.empty, []) 
		end

end