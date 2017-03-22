structure Frame = MipsFrame

signature TRANSLATE =
sig
	type level
	type access

	val outermost : level
	val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
	val formals : level -> access list
	val allocLocal: level -> bool -> access
	val printLevel : level -> unit
	val printAccess : access -> unit

end

structure Translate : TRANSLATE = struct

	datatype level = NORMAL of {parent: level, frame: Frame.frame, uniq: unit ref}
					|OUTER of {uniq: unit ref}

	type access = level * Frame.access

	val outermost = OUTER( {uniq=  ref ()} )

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