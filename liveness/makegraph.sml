structure FG = FuncGraph(struct
                                  type ord_key = string
                                  val compare = String.compare
                                end)

type info = {def: Temp.temp list,
		    use: Temp.temp list,
		    ismove: bool}

structure MakeGraph:
sig
	val instrs2graph: Assem.instr list -> info FG.graph * info FG.node list
end
=
struct

	fun instrs2graph a = (FG.empty, [])

end