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
	val relop : Tree.relop * exp * exp -> exp
	val nilExp : unit -> exp
	val intExp : int -> exp

	val ifElse : exp * exp * exp -> exp
	val ifThen : exp * exp -> exp

	val simpleVar : access * level -> exp
(*	val varDec : *)

	val unNx : exp -> Tree.stm
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

	fun  seq [s] = s
	    |seq [first,second] = Tree.SEQ(first, second)
	    |seq (a::l) = Tree.SEQ(a, seq l)
	    |seq [] = (print("Used seq incorrectly"); Tree.EXP(Tree.CONST(0)))


	
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
			printLevel(newLev);
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
			Frame.printAccess(newAccess);
			printLevel((NORMAL({parent=parent, frame=frame, uniq=uniq})));
			ret
		end
(*	   |allocLocal (OUTER({uniq}):level)                   (esc) = ( (ErrorMsg.error ~1 "Can't alloc variables in outermost level. How did this happen?"); (OUTER({uniq=uniq}), Frame.InFrame(0))  )  *)



	fun binop(oper, e1, e2) = Ex(Tree.BINOP(oper, unEx(e1), unEx(e2)))

	fun relop(oper, e1, e2) = Cx( (fn (t,f) => Tree.CJUMP(oper, unEx(e1), unEx(e2), t, f) ) )

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

	fun simpleVar ((OUTER{...}, frameAccess), _ ) = (ErrorMsg.error ~1 "How dis"; Ex(Tree.CONST(0)))
	   |simpleVar ((_, frameAccess), OUTER{...}) = (ErrorMsg.error ~1 "How dis"; Ex(Tree.CONST(0)))
	   |simpleVar( (NORMAL{parent=parentDec, frame=frameDec, uniq=uniqDec}, frameAccess):access , 
					NORMAL{parent=parentUsed, frame={name=_, formals=_, numFrameLocals=num}, uniq=uniqUsed}) = 
		if uniqDec = uniqUsed
		then Ex(Frame.exp(frameAccess)(Tree.TEMP(Frame.FP)))
		else Ex(Tree.MEM( Tree.BINOP(Tree.PLUS, Tree.CONST(Frame.wordSize* !num),  
				unEx(simpleVar ( (NORMAL{parent=parentDec, frame=frameDec, uniq=uniqDec}, frameAccess):access, parentUsed)
				)))) 

end







