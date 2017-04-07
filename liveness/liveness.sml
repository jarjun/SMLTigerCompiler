structure FGL = FuncGraph(Temp.TempOrd)

structure Liveness:
sig

	type info
	type livenessInfoRec

	datatype igraph = 
		IGRAPH of {graph: unit FGL.graph,
				   tnode: Temp.temp -> unit FGL.node,
				   gtemp: unit FGL.node -> Temp.temp,
				   moves: (unit FGL.node * unit FGL.node) list}

	val interferenceGraph: info FG.graph -> (*igraph * (info FG.node -> Temp.temp list)*) int

	(*val show: outstream * igraph -> unit*)


end
=
struct

	type info = MakeGraph.info

	datatype igraph = 
		IGRAPH of {graph: unit FGL.graph,
				   tnode: Temp.temp -> unit FGL.node,
				   gtemp: unit FGL.node -> Temp.temp,
				   moves: (unit FGL.node * unit FGL.node) list}


	structure SplaySet = SplaySetFn(Temp.TempOrd);
	structure SplayMap = SplayMapFn(type ord_key = int; val compare = Int.compare);


	type livenessInfoRec = {livein: SplaySet.set, liveout: SplaySet.set}


    fun setToStr(s) = 
    	SplaySet.foldl (fn (c,r) => r ^ ", " ^ MipsFrame.regToString(c)) "" s

    fun printLiveInfo(graph, liveness) =
    	foldl (fn (c,r) => 

    		let val {livein, liveout} = valOf(SplayMap.find(liveness, r))
    		in
    			(print( Int.toString(r) ^ "\n\tlivein: " ^ setToStr(livein) ^ "\n\tliveout: " ^ setToStr(liveout) ^ "\n");
    			r+1)
    		end



    		) 0 (FG.nodes(graph))


    fun makeLivenessInfoInit(graph) = 
    	let val nodeList = FG.nodes(graph)
    	in 
    		foldl (fn (cur, running) =>  SplayMap.insert(running, FG.getNodeID(cur), {livein=SplaySet.empty, liveout=SplaySet.empty})) SplayMap.empty nodeList
    	end


	fun mapEqual(a, b, graph) = 		
    	#2(foldl (fn ( c , (idx, bl) ) => 

    		let val {livein=ai, liveout=ao} = valOf(SplayMap.find(a, idx))
    			val {livein=bi, liveout=bo} = valOf(SplayMap.find(b, idx))
    		in
    			
    			(idx+1, bl andalso SplaySet.equal(ai, bi) andalso SplaySet.equal(ao, bo)) 

    		end



    		) (0, true) (FG.nodes(graph)))


	fun interferenceGraphHelper(graph, livenessInfo) = 
		
		let val revNodes = List.rev(FG.nodes(graph))
			val oldLiveness = livenessInfo

			fun helper ( curNode , runningLiveness) = 
				let (*val {livein, liveout} = case SplayMap.find(runningLiveness, id) of SOME({livein,liveout}) => {livein,liveout}
																				   |NONE        => (print("Node " ^ Int.toString(id) ^ " not initialized in map"); {SplayMap.empty, SplayMap.empty})*)

					val id = FG.getNodeID(curNode)
					val {uses, defs, ismove}:info = FG.nodeInfo(curNode)


					val useSet = SplaySet.addList(SplaySet.empty, uses)
					val defSet = SplaySet.addList(SplaySet.empty, defs)

					val newLiveOut = FG.foldSuccs (fn(cur, running) => SplaySet.union( case SplayMap.find(runningLiveness, cur) of SOME({livein, liveout}) => livein
																												    |NONE    => (print("Succ " ^ Int.toString(cur) ^ " not initialized in map\n"); SplaySet.empty)
																				, running)) 
											SplaySet.empty curNode

					val newLiveIn = SplaySet.union(useSet, SplaySet.difference (newLiveOut, defSet) )
					
				
				in
					SplayMap.insert(runningLiveness, id , {livein=newLiveIn, liveout=newLiveOut} )
				end 


		in 
			let val newLiveness = foldl helper livenessInfo revNodes
			in 
				if mapEqual(oldLiveness, newLiveness, graph) then newLiveness else interferenceGraphHelper(graph, newLiveness)
			end 
		end









	

	fun interferenceGraph(graph) = 
			let val newL = interferenceGraphHelper(graph, makeLivenessInfoInit(graph))
			in 
				(printLiveInfo(graph, newL); 0)
			end


end