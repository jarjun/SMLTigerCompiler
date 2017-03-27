structure As = Assem

signature CODEGEN = 
sig
	structure  Frame : FRAME
	val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure MipsGen : CODEGEN =
struct

	structure Frame = MipsFrame

	fun codegen (frame) (stm: Tree.stm) : As.instr list = 
		let 
			val ilist = ref (nil: As.instr list)
			fun emit x = 
				ilist := x::(!ilist)
			fun result(gen) = 
				let
					val t = Temp.newtemp() 
				in 
					gen t; 
					t 
				end

			fun munchStm (Tree.SEQ(a,b)) = (munchStm a; munchStm b)
			   |munchStm (Tree.JUMP(Tree.NAME l, q)) = emit (As.OPER{
			   													assem="j " ^ Symbol.name(l) ^ "\n",
																src=[], dst=[], jump=SOME(q)})
			   |munchStm (Tree.MOVE (Tree.TEMP t, e2)) = emit (As.MOVE{
			   														assem="move `d0 `s0\n",
																	src=munchExp e2, dst=t})
			   |munchStm (Tree.LABEL lab) = emit (As.LABEL{assem=Symbol.name(lab) ^ ":\n",
			   											lab=lab})
			  
			and munchExp (Tree.BINOP(Tree.PLUS, e1, e2)) = result (fn r => emit (As.OPER{
																					assem="add `d0, `s0, `s1\n",
																					src=[munchExp e1, munchExp e2], dst=[r], jump=NONE }))
			   |munchExp (Tree.CONST i) = result (fn r => emit (As.OPER{
			   														assem="li `d0, " ^ Int.toString(i) ^ "\n",
																	src=[], dst=[r], jump=NONE}))
			   |munchExp (Tree.TEMP t) = t

		in
			munchStm stm;
			rev(!ilist)
		end
end