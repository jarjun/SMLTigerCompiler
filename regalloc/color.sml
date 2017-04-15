structure Color :
sig 
  
  type allocation = string Temp.Table.table

  val color: {interference: Liveness.igraph,
        initial: allocation,
        spillCost: unit FGL.node -> int,
        registers: string list} -> allocation * Temp.temp list

  val printAlloc: allocation * Liveness.igraph-> unit
  
end
=
struct
  type allocation = string Temp.Table.table

  (*val colors = MipsFrame.getCallerSavesStr()
  val k = List.length(colors)*)
  fun stringListContains(l:string list, el:string) = List.length(List.filter (fn x => x = el) l) > 0

  fun color ({interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, registers}) = 
  		let
  			fun removePrecolored(nodes) = List.filter (fn x => case Temp.Table.look(initial, gtemp(x)) of SOME(t) => not (stringListContains(registers, t))
  																								|NONE => true			
  																								) nodes
  		in
  			case removePrecolored(FGL.nodes(graph)) of 
  								[node]   => let val unSpilledNeighbors = List.filter (fn x => case Temp.Table.look(initial, x) of SOME(t) => true
  																																			|NONE => false) (FGL.preds(node))
  												val neighborColors = map (fn x => case Temp.Table.look(initial, x) of SOME(t) => t
  																																|NONE =>   (print("error: uncolored, unspilled neighbor"); "Have you ever heard the tale of Darth Plagueis the Wise? It's not a story the Jedi would tell you..." ) ) unSpilledNeighbors
  												val availableColors = List.filter (fn x => not (stringListContains(neighborColors, x))) registers
  											in
  												if List.length(availableColors) = 0
								  				then (initial, [gtemp(node)] )
								  				else (Temp.Table.enter(initial, gtemp(node), List.nth(availableColors, 0)), [])
  												(*(Temp.Table.enter(initial, gtemp(node), List.nth(registers, 0)), [])*)
  											end
  			                    |[]       => (print("no nodes in igraph"); (initial, []))
  			                    |nodelist =>
  			                    			let (*val _ = app (fn x => print(Int.toString(FGL.outDegree(x)) ^ " ")) nodelist*)
									  			val insigNodes = List.filter (fn x => FGL.outDegree(x) < List.length(registers)) (nodelist)
									  			val toSimplify = if List.length(insigNodes) > 0
									  							 then List.nth(insigNodes, 0)
									  							 else (print("potential spill\n"); List.nth(nodelist, 0))
									  			val newGraph = FGL.remove(graph, toSimplify)
									  			val newInterference = Liveness.IGRAPH{graph=newGraph, tnode=tnode, gtemp=gtemp, moves=moves}
									  		in
									  			(*print ("\n ---------------- \n");*)
									  			let val (newAlloc, spillList) = color({interference=newInterference, initial=initial, spillCost=spillCost, registers=registers})
									  				val unSpilledNeighbors = List.filter (fn x => case Temp.Table.look(newAlloc, x) of SOME(t) => true
  																																			|NONE => false) (FGL.preds(toSimplify)) (*(FGL.preds(  FGL.getNode(graph, FGL.getNodeID(toSimplify))  ))*)

									  				val neighborColors = map (fn x => case Temp.Table.look(newAlloc, x) of SOME(t) => t
  																																|NONE =>   (print("error: uncolored, unspilled neighbor"); "Have you ever heard the tale of Darth Plagueis the Wise? It's not a story the Jedi would tell you..." ) ) unSpilledNeighbors
									  				val availableColors = List.filter (fn x => not (stringListContains(neighborColors, x))) registers
									  				(*val _ = app (fn x => print(MipsFrame.regToString(x) ^ " ")) unSpilledNeighbors*)
									  				(*val _ = app (fn x => print(x ^ " ")) neighborColors*)
									  				(*val _ = print(MipsFrame.regToString(gtemp(toSimplify)) ^ ": ")*)
									  				(*val _ = app (fn x => print(x ^ " ")) availableColors*)
									  				(*val _ = print "\n"*)
							

									  			in 
									  				if List.length(availableColors) = 0
									  				then (newAlloc, spillList @ [gtemp(toSimplify)] )
									  				else (Temp.Table.enter(newAlloc, gtemp(toSimplify), List.nth(availableColors, 0)), spillList)
									  			end
									  		end
		end

	fun printAlloc(alloc, Liveness.IGRAPH{graph, tnode, gtemp, moves}) = app (fn x => print( MipsFrame.regToString(gtemp(x)) ^ " -> " ^ (case Temp.Table.look(alloc, gtemp(x)) of SOME(s) => s 
																																						   |NONE => "") ^ "\n")) (FGL.nodes(graph))
end
