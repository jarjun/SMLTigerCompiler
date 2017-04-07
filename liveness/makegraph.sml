structure FG = FuncGraph(struct
                          type ord_key = int
                          val compare = Int.compare
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
	val labelMap:(int labelMapStruct.map ref) = ref labelMapStruct.empty


	structure instrMapStruct = SplayMapFn(struct
				                            type ord_key = int
				                            val compare = Int.compare
				                          end)

	val instrMap:(string instrMapStruct.map ref) = ref instrMapStruct.empty

	fun getNewID(instr) = 
		let val x = !instrIdx
			val format0 = Assem.format(MipsFrame.regToString)
			in 
				instrIdx := x + 1;
				(*Int.toString(x) ^ " " ^ (format0 instr)*)
				instrMap := instrMapStruct.insert(!instrMap, x, (format0 instr));
				x
			end
					
	fun printNode(id, {defs, uses, ismove}) = 
		let fun temps2string (t, s) = s ^ ", " ^ MipsFrame.regToString(t)
		in
			Int.toString(id) ^ " " ^ valOf(instrMapStruct.find(!instrMap, id)) (*^ "defs: " ^ (foldl temps2string "" defs) ^ " uses: " ^ (foldl temps2string "" uses) ^ " ismove: " ^ Bool.toString(ismove)*)
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
					labelMap := labelMapStruct.insert(!labelMap, Symbol.name(lab), newID);
					newGraph
				end

			val initGraph = (instrIdx := 0; foldl makeNodes FG.empty instrList)

			fun  makeEdges (instr as Assem.OPER{assem, dst, src, jump}, graph) = 
					let val curNode = !instrIdx
						val nextIdx = !instrIdx+1
						val edgeList = case jump of NONE          => if nextIdx >= List.length(FG.nodes(graph)) then [] else [{to=nextIdx, from=curNode}]
												   |SOME([])      => if nextIdx >= List.length(FG.nodes(graph)) then [] else [{to=nextIdx, from=curNode}]
												   |SOME(dstList) => foldl (fn (x, r) => r @ [{to=valOf(labelMapStruct.find(!labelMap, Symbol.name(x))), from=curNode}]) [] dstList 
					in
(*						print("from: " ^ Int.toString(curNode) ^ "\n");
						print("to: " ^ Int.toString(nextIdx) ^ "\n");*)
						instrIdx := !instrIdx + 1;
						foldl (fn (c, r) => FG.addEdge(r, c)) graph edgeList 
					end

				|makeEdges (instr as Assem.MOVE{assem, dst, src}, graph) = 
					let val curNode = !instrIdx
						val nextIdx = !instrIdx + 1
						val edge = {to=nextIdx, from=curNode}
					in
(*						print("from: " ^ Int.toString(curNode) ^ "\n");
						print("to: " ^ Int.toString(nextIdx) ^ "\n");*)
						instrIdx := !instrIdx + 1;
						if nextIdx >= List.length(FG.nodes(graph))
						then graph 
						else FG.addEdge(graph, edge)
					end

				|makeEdges (instr as Assem.LABEL{assem, lab}, graph) =
					let val curNode = !instrIdx
						val nextIdx = !instrIdx + 1
						val edge = {to=nextIdx, from=curNode}
					in
(*						print("LABEL\n");
						print("from: " ^ Int.toString(curNode) ^ "\n");
						print("to: " ^ Int.toString(nextIdx) ^ "\n");*)
						instrIdx := !instrIdx + 1;
						if nextIdx >= List.length(FG.nodes(graph))
						then graph 
						else FG.addEdge(graph, edge)
					end

			val edgeGraph = (instrIdx := 0; foldl makeEdges initGraph instrList)

		in 
			(*FG.printGraph printNode edgeGraph;*)

			(edgeGraph, FG.nodes(edgeGraph)) 
		end

end