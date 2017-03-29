structure Frame = MipsFrame

signature TRANSLATE =
sig
	type level
	type access
	type exp

	val outermost : level
	val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
	val formals : level -> access list
	val allocLocal: level -> bool -> access
	val printLevel : level -> unit
	val printAccess : access -> unit


	val binop : Tree.binop * exp * exp -> exp
	val relop : Tree.relop * exp * exp * Types.ty -> exp
	val nilExp : unit -> exp
	val intExp : int -> exp

	val ifElse : exp * exp * exp -> exp
	val ifThen : exp * exp -> exp

	val simpleVar : access * level -> exp
	val varDec : access * exp -> exp
	val letBody : exp list * exp -> exp
	val arrExp : exp * exp -> exp
	val subscriptExp: exp * exp -> exp
	val recordExp : exp list -> exp
	val fieldVar : exp * int -> exp
	val assignExp : exp * exp -> exp
	val whileExp : exp * exp * Temp.label -> exp
	val forExp: exp * exp * exp * Temp.label -> exp
	val breakExp: Temp.label option -> exp
	val stringExp: string -> exp

	val seqExp: exp list -> exp
	val callExp: level * level * Temp.label * exp list -> exp

	val procEntryExit : {level: level, body: exp} -> unit
	val getResult : unit -> Frame.frag list

	val seq : Tree.stm list -> Tree.stm

	val printResult : unit -> unit
end

structure Translate : TRANSLATE = struct

	datatype level = NORMAL of {parent: level, frame: Frame.frame, uniq: unit ref}
					|OUTER of {uniq: unit ref}

	datatype exp = 
		Ex of Tree.exp 
	   |Nx of Tree.stm
	   |Cx of Tree.label * Temp.label -> Tree.stm


	type access = level * Frame.access

	val outermost = OUTER( {uniq=  ref ()} )

	val fragList = (ref []):(Frame.frag list ref)

	fun  seq [s] = s
	    |seq [first,second] = Tree.SEQ(first, second)
	    |seq (a::l) = Tree.SEQ(a, seq l)
	    |seq [] = ((*print("Used seq incorrectly");*) Tree.EXP(Tree.CONST(0)))


	
	fun unEx (Ex e) = e
	   |unEx (Cx genstm) =
	   	  let val r = Temp.newtemp()
	   	  	  val t = Temp.newlabel() and f = Temp.newlabel()
	   	  in 
	   	  	  Tree.ESEQ(seq[Tree.MOVE(Tree.TEMP r, Tree.CONST 1),
	   	  	  				genstm(t,f),
	   	  	  				Tree.LABEL f,
	   	  	  				Tree.MOVE(Tree.TEMP r, Tree.CONST 0),
	   	  	  				Tree.LABEL t], 

	   	  	  			Tree.TEMP r)
	   	  end

	   |unEx (Nx s) = Tree.ESEQ(s, Tree.CONST 0)


	fun unNx (Ex e) = Tree.EXP(e)
	   |unNx (Nx s) = s
	   |unNx (Cx f) =
	   	  let val L1 = Temp.newlabel()
	   	  in
	   	  	  seq[ f(L1, L1), Tree.LABEL L1  ]
	   	  end


	fun unCx (Ex(Tree.CONST(0))) = (fn (t,f) => Tree.JUMP (Tree.NAME f, [f])  )
	   |unCx (Ex(Tree.CONST(1))) = (fn (t,f) => Tree.JUMP (Tree.NAME t, [t])  )
	   |unCx (Ex e) = (fn (t,f) => Tree.CJUMP(Tree.EQ, Tree.CONST(0), e , f, t))
	   |unCx (Nx _) = (ErrorMsg.error ~1 "Trying to cx something with no value???";  (fn (t,f) => Tree.EXP(Tree.CONST 0)) )
	   |unCx (Cx f) = f



	fun printLevel (NORMAL({parent, frame, uniq})) = (Frame.printFrame(frame); printLevel(parent))
	   |printLevel (OUTER({uniq})) = print("OUTER\n")

	fun printAccess (lev, acc) = Frame.printAccess(acc)

	fun newLevel {parent, name, formals} = 
		let val newLev = NORMAL({  parent = parent,  frame=Frame.newFrame{name=name, formals= true::formals}, uniq = ref () })
		in
(*			printLevel(newLev);*)
			newLev
		end

	fun formals (NORMAL({parent, frame, uniq}) : level) = (map 
														  (fn x => (  NORMAL({parent=parent, frame=frame, uniq=uniq}):level   , x:Frame.access ):access )   
														  (Frame.formals(frame))   )
	   |formals (OUTER({uniq}):level) = []


	fun allocLocal (NORMAL({parent, frame, uniq}) : level) (esc) = 
		let	val newAccess = Frame.allocLocal(frame)(esc)
			val ret = ( (NORMAL({parent=parent, frame=frame, uniq=uniq}) : level), newAccess )
		in 
(*			Frame.printAccess(newAccess);*)
(*			printLevel((NORMAL({parent=parent, frame=frame, uniq=uniq})));*)
			ret
		end
	   |allocLocal (OUTER({uniq}):level)                   (esc) = ( (ErrorMsg.error ~1 "Can't alloc variables in outermost level."); (OUTER({uniq=uniq}), Frame.allocLocal(Frame.newFrame({formals=[], name=Temp.newlabel()}))(true) )  )  



	fun binop(oper, e1, e2) = Ex(Tree.BINOP(oper, unEx(e1), unEx(e2)))

	fun  relop(Tree.EQ, e1, e2, Types.STRING) = Ex(Frame.externalCall("stringEqual", [unEx(e1), unEx(e2)]))
		|relop(Tree.NE, e1, e2, Types.STRING) = Ex(Frame.externalCall("stringNE", [unEx(e1), unEx(e2)]))
		|relop(Tree.LT, e1, e2, Types.STRING) = Ex(Frame.externalCall("stringLT", [unEx(e1), unEx(e2)]))
		|relop(Tree.GT, e1, e2, Types.STRING) = Ex(Frame.externalCall("stringGT", [unEx(e1), unEx(e2)]))
		|relop(Tree.LE, e1, e2, Types.STRING) = Ex(Frame.externalCall("stringLE", [unEx(e1), unEx(e2)]))
		|relop(Tree.GE, e1, e2, Types.STRING) = Ex(Frame.externalCall("stringGE", [unEx(e1), unEx(e2)]))
		|relop(oper, e1, e2, _) = Cx( (fn (t,f) => Tree.CJUMP(oper, unEx(e1), unEx(e2), t, f) ) )

	fun ifElse(e1, e2, e3) = 	   	  
		let val r = Temp.newtemp()
	   	  	val t = Temp.newlabel() and f = Temp.newlabel() and join = Temp.newlabel()
	   	  in 
	   	  	  Ex(Tree.ESEQ(seq[ unCx(e1)(t,f),
	   	  	  				 Tree.LABEL t,
	   	  	  				 Tree.MOVE(Tree.TEMP r, unEx(e2)),
	   	  	  				 Tree.JUMP (Tree.NAME join, [join]),

	   	  	  				 Tree.LABEL f,
	   	  	  				 Tree.MOVE(Tree.TEMP r, unEx(e3)),
	   	  	  				 (*Tree.JUMP (Tree.NAME join, [join]),*)
	   	  	  				 Tree.LABEL join], 

	   	  	  			Tree.TEMP r))
	   	  end

	fun ifThen(e1, e2) = 
		let val r = Temp.newtemp()
	   	  	val t = Temp.newlabel() and join = Temp.newlabel()
	   	  in 
	   	  	  Nx(seq[ unCx(e1)(t,join),
	   	  	  				 Tree.LABEL t,
	   	  	  				 Tree.EXP(unEx(e2)),
	   	  	  				 (*Tree.JUMP (Tree.NAME join, [join]),*)
	   	  	  				 Tree.LABEL join])
	   	  end

	fun nilExp() = Ex(Tree.CONST(0))

	fun intExp x = Ex(Tree.CONST(x))

	fun  findFrame(OUTER{...}, _) = (ErrorMsg.error ~1 "Declared in outer level"; Tree.CONST(0))
		|findFrame(_, OUTER{...}) = (ErrorMsg.error ~1 "Can't find level using static links"; Tree.CONST(0))
		|findFrame (varLevel as (NORMAL{parent=_, frame=_, uniq=uniqDec}), 
					useLevel as (NORMAL{parent=parentUsed, frame=_ , uniq=uniqUsed})) = 
		if uniqDec = uniqUsed
		then Tree.TEMP(Frame.FP)
		else Tree.MEM(findFrame(varLevel, parentUsed))

	fun simpleVar ((OUTER{...}, frameAccess), _ ) = (ErrorMsg.error ~1 "Variable declared in outer level"; Ex(Tree.CONST(0)))
	   |simpleVar ((_, frameAccess), OUTER{...}) = (ErrorMsg.error ~1 "Trying to invoke variable in outer level"; Ex(Tree.CONST(0)))
	   |simpleVar( (varLevel, frameAccess) , useLevel) = Ex(Frame.exp(frameAccess)( findFrame(varLevel, useLevel) ))

	fun varDec ((NORMAL{parent=parentDec, frame=frameDec, uniq=uniqDec}, frameAccess):access, e1) = 
																Nx(Tree.MOVE( Frame.exp(frameAccess)(Tree.TEMP(Frame.FP)), unEx(e1)))
	   |varDec ((OUTER{...}, frameAccess):access, e1) = (ErrorMsg.error ~1 "Trying to declare variable in outer frame"; Ex(Tree.CONST(0)))

	fun  letBody([], expr) = Ex(unEx(expr))
		|letBody(initList, expr) = Ex(Tree.ESEQ(seq(map unNx initList), unEx(expr)))

	fun arrExp (size, init) = 
		let val arrPointer = Temp.newtemp()

		in
			Ex(Tree.ESEQ(seq[
						Tree.MOVE(Tree.TEMP(arrPointer), Frame.externalCall("initArray", [Tree.BINOP(Tree.PLUS, unEx(size), Tree.CONST 1), unEx(init)])),
						Tree.MOVE(Tree.MEM(Tree.TEMP(arrPointer)), unEx(size))
						], 

				Tree.TEMP(arrPointer)))
		end

	fun subscriptExp (var, index) = 
		let val idxTemp = Temp.newtemp()
			val varTemp = Temp.newtemp()

			val nextLab = Temp.newlabel()
			val endLab = Temp.newlabel()
			val badLab = Temp.newlabel()
		in
		
		Ex(Tree.ESEQ( seq[
						Tree.MOVE(Tree.TEMP(idxTemp), unEx(index)),
						Tree.MOVE(Tree.TEMP(varTemp), unEx(var)),
						Tree.CJUMP(Tree.GE, Tree.TEMP(idxTemp), Tree.MEM(Tree.TEMP(varTemp)), badLab, nextLab),
						Tree.LABEL(nextLab),
						Tree.CJUMP(Tree.LT, Tree.TEMP(idxTemp), Tree.CONST(0), badLab, endLab),
						Tree.LABEL(badLab),
						(* TODO: print error message, array out of bounds exception *)
						Tree.EXP(Frame.externalCall("exit", [Tree.CONST 1])),
						Tree.LABEL(endLab)
						],
		
		Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.MEM(Tree.TEMP(varTemp)), Tree.BINOP(Tree.MUL, Tree.BINOP(Tree.PLUS, Tree.TEMP(idxTemp), Tree.CONST 1), Tree.CONST Frame.wordSize)))
		))

		end

	fun recordExp (explist) = 
		let 
			val recPointer = Temp.newtemp()
			val numFields = List.length(explist)

			fun helper (curExp, (curMoveList, curIdx)) = (curMoveList @ [Tree.MOVE(
																				Tree.MEM(
																						Tree.BINOP(Tree.PLUS, 
																								   Tree.MEM(Tree.TEMP(recPointer)), 
																								   Tree.CONST(curIdx * Frame.wordSize))
																						),
																				unEx(curExp)
																				  )
																			], curIdx+1)

			val (moveList, fieldIdx) = foldl helper ([], 0) explist

		in
			Ex(Tree.ESEQ(seq ([ Tree.MOVE(Tree.TEMP(recPointer), Frame.externalCall("allocRecord", [Tree.CONST(numFields)])) ] @ moveList),

						Tree.TEMP(recPointer))
			  )
		end

	fun fieldVar (recPointer, num) = Ex(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.MEM(unEx(recPointer)), Tree.CONST(num*Frame.wordSize))))

	fun assignExp (var, value) = Nx(Tree.MOVE(unEx(var), unEx(value)))


	fun whileExp (test, body, endLab) = 
		let val L1 = Temp.newlabel()
			val L2 = Temp.newlabel()
		in
			Nx(seq[ Tree.JUMP(Tree.NAME(L1), [L1]),
				 Tree.LABEL(L2),
				 unNx(body),
				 Tree.LABEL(L1),
				 unCx(test)(L2, endLab),
				 Tree.LABEL(endLab)
			])
		end

	fun forExp (lo, hi, body, endLab) = 
		let val iter = Temp.newtemp()
			val highTemp = Temp.newtemp()

			val L1 = Temp.newlabel()
			val L2 = Temp.newlabel()

		in
			Nx(seq[ Tree.MOVE(Tree.TEMP(iter), unEx(lo)),
					Tree.MOVE(Tree.TEMP(highTemp), unEx(hi)),
					Tree.CJUMP(Tree.LE, Tree.TEMP(iter), Tree.TEMP(highTemp), L2, endLab),

					Tree.LABEL(L1),
					Tree.MOVE(Tree.TEMP(iter), Tree.BINOP(Tree.PLUS, Tree.TEMP(iter), Tree.CONST(1))    ),

					Tree.LABEL(L2),
					unNx(body),
					Tree.CJUMP(Tree.LT, Tree.TEMP(iter), Tree.TEMP(highTemp), L1, endLab),

					Tree.LABEL(endLab)


				])
		end

	fun breakExp(SOME(breakLabel)) = Nx(Tree.JUMP(Tree.NAME(breakLabel), [breakLabel]))
	   |breakExp(NONE) = Nx(Tree.EXP(Tree.CONST 0))


	fun  seqExp([]) = Nx(Tree.EXP(Tree.CONST 0))
		|seqExp([last]) = last
	   	|seqExp(explist) = 
	   		let val last = List.last(explist)
	   			val siz = List.length(explist)
	   			val newList = List.take(explist, siz-1)
	   		in 
	   			Ex(Tree.ESEQ( seq( map unNx newList), unEx(last)))
	   		end

	fun stringExp (lit) = 
		let
			val equalStrings = List.filter (fn Frame.PROC(_) => false
							  				  |Frame.STRING(labTest, litTest) => case String.compare(litTest, lit) of EQUAL => true 
							  													                                      |_    => false
											) (!fragList)
		in
			if (List.length(equalStrings) = 0)
			then let 
				 	val lab = Temp.newlabel()
					val frag = Frame.STRING(lab, lit)
				  in
					fragList := !fragList @ [frag];
					Ex(Tree.NAME(lab))
				  end
			else let 
					val Frame.STRING(lab, _) = List.nth(equalStrings, 0)
				 in 
					Ex(Tree.NAME(lab))
				 end
		end

	fun callExp (callLevel, NORMAL{parent , frame, uniq}, lab, args) = 
		let val sl = findFrame(parent, callLevel)
		in
			Ex(Tree.CALL(Tree.NAME(lab), (sl)::(map (fn x => unEx x) args)))
		end
	   |callExp(_,OUTER{...},lab,args) = Ex(Tree.CALL(Tree.NAME(lab), (map (fn x => unEx x) args))) (*no static link for built in function*)

	fun getResult() = !fragList

	(* TODO call procEntryExit 1 on body *)
	fun  procEntryExit({level=OUTER{...}, body}) = (ErrorMsg.error ~1 "Function declared in outer level"; ())
		|procEntryExit({level=NORMAL{parent, frame, uniq}, body}) = 
			let val body = Tree.MOVE(Tree.TEMP(Frame.V0), unEx(body))
				val toAdd = (Frame.PROC{body= Frame.procEntryExit1(frame, body) , frame=frame})
			in
				fragList := (!fragList @ [toAdd])
			end


		

	fun printResult() = List.app (fn Frame.PROC{frame= {name=n, formals=_, numFrameLocals=_}, body=body} =>(print("----------------\n"); 
																			print(Symbol.name(n) ^ "\n");
																			Printtree.printtree(TextIO.stdOut, body))

									|Frame.STRING(lab, lit) => (print("----------------\n"); print(Symbol.name(lab) ^ "\n"); print lit; print "\n")
								  )   
							(!fragList);
end







