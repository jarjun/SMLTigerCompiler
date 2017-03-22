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

	val unEx: exp -> Tree.exp
	val unNx: exp -> Tree.stm
	val unCx: exp -> (Temp.label * Temp.label -> Tree.stm)

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



	
	






end



