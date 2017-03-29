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

			fun binopToString(Tree.PLUS) = "add"
			   |binopToString(Tree.MINUS) = "sub"
			   |binopToString(Tree.MUL) = "mul"
			   |binopToString(Tree.DIV) = "div"

			fun relopToString(Tree.EQ) = "beq"
			   |relopToString(Tree.NE) = "bne"
			   |relopToString(Tree.LT) = "blt"
			   |relopToString(Tree.LE) = "ble"
			   |relopToString(Tree.GT) = "bgt"
			   |relopToString(Tree.GE) = "bge"

			fun properIntToString i = if i < 0 
									  then ("-" ^ Int.toString(i * ~1))
									  else Int.toString(i)

			fun munchStm (Tree.SEQ(a,b)) = (munchStm a; munchStm b)


			   |munchStm (Tree.CJUMP(relop, e1, e2, l1, l2)) = emit (As.OPER{
			   													assem=relopToString(relop) ^ " `s0, `s1, " ^ Symbol.name(l1) ^ " \n",
																src=[munchExp e1, munchExp e2], dst=[], jump=SOME([l1, l2])})

			   |munchStm (Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, e1, Tree.CONST(i))), e2)) = emit (As.OPER{
			   													assem="sw `s0, " ^ properIntToString(i) ^ "(`s1) \n",
																src=[munchExp e2, munchExp e1], dst=[], jump=NONE})

			   |munchStm (Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST(i), e1)), e2)) = emit (As.OPER{
			   													assem="sw `s0, " ^ properIntToString(i) ^ "(`s1) \n",
																src=[munchExp e2, munchExp e1], dst=[], jump=NONE})

			   |munchStm (Tree.MOVE(Tree.MEM(e1), e2)) = emit (As.OPER{
			   													assem="sw `s0, 0(`s1) \n",
																src=[munchExp e2, munchExp e1], dst=[], jump=NONE})

			   |munchStm (Tree.JUMP(Tree.NAME l, q)) = emit (As.OPER{
			   													assem="j " ^ Symbol.name(l) ^ "\n",
																src=[], dst=[], jump=SOME(q)})

			   |munchStm (Tree.MOVE (Tree.TEMP t, e2)) = emit (As.MOVE{
			   														assem="move `d0, `s0\n",
																	src=munchExp e2, dst=t})

			   |munchStm (Tree.LABEL lab) = emit (As.LABEL{assem=Symbol.name(lab) ^ ":\n",
			   											lab=lab})

			   |munchStm (Tree.EXP(e1)) = (munchExp(e1); ())

(*			   |munchStm(_) = ()*)
			  
			and munchExp(Tree.CALL(Tree.NAME(n), args)) = result(fn r => emit(As.OPER{
			   											  assem="jal " ^ Symbol.name(n) ^ "\n",
			   											  src=munchArgs(0, args),
			   											  dst= Frame.getCallerSaves() @ Frame.getReturnRegisters() @ [Frame.getReturnAddress()],
			   											  jump=NONE}))


			   |munchExp (Tree.MEM(Tree.BINOP(Tree.PLUS, e1, Tree.CONST(i)))) = result (fn r => emit (As.OPER{
																					assem="lw `d0, " ^ properIntToString(i) ^ "(`s0)\n",
																					src=[munchExp e1], dst=[r], jump=NONE }))

			   |munchExp (Tree.MEM(Tree.BINOP(Tree.PLUS, Tree.CONST(i), e1))) = result (fn r => emit (As.OPER{
																					assem="lw `d0, " ^ properIntToString(i) ^ "(`s0)\n",
																					src=[munchExp e1], dst=[r], jump=NONE }))

			   |munchExp (Tree.MEM(e1)) = result (fn r => emit (As.OPER{
																					assem="lw `d0, 0(`s0)\n",
																					src=[munchExp e1], dst=[r], jump=NONE }))

			   |munchExp (Tree.BINOP(Tree.PLUS, e1, Tree.CONST(i))) = result (fn r => emit (As.OPER{
																					assem="addi `d0, `s0, " ^ properIntToString(i) ^ "\n",
																					src=[munchExp e1], dst=[r], jump=NONE }))

			   |munchExp (Tree.BINOP(Tree.PLUS, Tree.CONST(i), e1)) = result (fn r => emit (As.OPER{
																					assem="addi `d0, `s0, " ^ properIntToString(i) ^ "\n",
																					src=[munchExp e1], dst=[r], jump=NONE }))

			   |munchExp (Tree.BINOP(binop, e1, e2)) = result (fn r => emit (As.OPER{
																					assem=binopToString(binop) ^ " `d0, `s0, `s1\n",
																					src=[munchExp e1, munchExp e2], dst=[r], jump=NONE }))

			   |munchExp (Tree.CONST i) = result (fn r => emit (As.OPER{
			   														assem="li `d0, " ^ properIntToString(i) ^ "\n",
																	src=[], dst=[r], jump=NONE}))

			   |munchExp (Tree.TEMP t) = t

(*			   |munchExp(_) = Temp.newtemp()*)
		
		and munchArgs(idx, args) = [] (* just adds first 4 into regs, rest onto stack*)

		in
			munchStm stm;
			rev(!ilist)
		end
end