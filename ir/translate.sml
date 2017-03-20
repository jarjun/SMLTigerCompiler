structure Frame = MipsFrame

signature TRANSLATE =
sig
	type level
	type access

	val outermost : level
	val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
	val formals : level -> access list
	val allocLocal: level -> bool -> access

end

structure Translate : TRANSLATE = struct

	datatype level = NORMAL of {parent: level, frame: Frame.frame, uniq: unit ref}
					|OUTER of {uniq: unit ref}

	type access = level * Frame.access

	val outermost = OUTER( {uniq=  ref ()} )

	fun formals (NORMAL({parent, frame, uniq}) : level) = (map 
														  (fn x => (  NORMAL({parent=parent, frame=frame, uniq=uniq}):level   , x:Frame.access ):access )   
														  (Frame.formals(frame))   )
	   |formals (OUTER({uniq}):level) = []


	fun allocLocal (NORMAL({parent, frame, uniq}) : level) (esc) = ( (NORMAL({parent=parent, frame=frame, uniq=uniq}) : level), Frame.allocLocal(frame)(esc)  )
(*	   |allocLocal (OUTER({uniq}):level)                   (esc) = ( (ErrorMsg.error ~1 "Can't alloc variables in outermost level. How did this happen?"); (OUTER({uniq=uniq}), Frame.InFrame(0))  )  *)


	fun newLevel {parent, name, formals} = NORMAL({  parent = parent,  frame=Frame.newFrame{name=name, formals=formals}, uniq = ref () })



end