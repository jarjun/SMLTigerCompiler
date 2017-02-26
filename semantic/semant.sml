structure A = Absyn

signature SEMANT = 
sig
	val transProg: A.exp -> unit
end

structure Semant :> SEMANT = 
struct

	structure Translate = struct type exp = unit end

	type ty = Types.ty
	type venv = Env.enventry Symbol.table
	type tenv = ty Symbol.table
	type expty = {exp: Translate.exp, ty:Types.ty}
	
	fun checkInt ({exp,ty}, pos) = case ty of Types.INT => ()
										      | _       => ErrorMsg.error pos "integer required";

	fun checkComparable ({exp=expL,ty=Types.INT}, {exp=expR,ty=Types.INT}, pos) = ()
	   |checkComparable ({exp=expL,ty=Types.STRING}, {exp=expR,ty=Types.STRING}, pos) = ()
	   |checkComparable ({exp=expL,ty=_}, {exp=expR,ty=_}, pos) = ErrorMsg.error pos "matching int/str required";

	fun checkEqualComparable ({exp=expL,ty=Types.INT}, {exp=expR,ty=Types.INT}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=Types.STRING}, {exp=expR,ty=Types.STRING}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=Types.RECORD _}, {exp=expR,ty=Types.RECORD _}, pos) = ()	   
	   |checkEqualComparable ({exp=expL,ty=Types.ARRAY _}, {exp=expR,ty=Types.ARRAY _}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=Types.RECORD _}, {exp=expR,ty=Types.NIL}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=Types.NIL}, {exp=expR,ty=Types.RECORD _}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=_}, {exp=expR,ty=_}, pos) = ErrorMsg.error pos "matching types required for equality check";


	fun transExp(venv, tenv, exp) = 
		let fun 

				(* Normal arithmetic, two ints needed *)
				trexp (A.OpExp{left, oper=A.PlusOp, right, pos}) = 
					  (checkInt(trexp left, pos); 
					  checkInt(trexp right, pos);
					  {exp=(), ty=Types.INT})

				|trexp (A.OpExp{left, oper=A.MinusOp, right, pos}) = 
					  (checkInt(trexp left, pos); 
					  checkInt(trexp right, pos);
					  {exp=(), ty=Types.INT})

				|trexp (A.OpExp{left, oper=A.TimesOp, right, pos}) = 
					  (checkInt(trexp left, pos); 
					  checkInt(trexp right, pos);
					  {exp=(), ty=Types.INT})

				|trexp (A.OpExp{left, oper=A.DivideOp, right, pos}) = 
					  (checkInt(trexp left, pos); 
					  checkInt(trexp right, pos);
					  {exp=(), ty=Types.INT})


				(* Comparing operations, ints or strings needed *)
				|trexp (A.OpExp{left, oper=A.LtOp, right, pos}) = 
					  (checkComparable(trexp left, trexp right, pos);
					  {exp=(), ty=Types.INT})		

				|trexp (A.OpExp{left, oper=A.GtOp, right, pos}) = 
					  (checkComparable(trexp left, trexp right, pos);
					  {exp=(), ty=Types.INT})							 			 

				|trexp (A.OpExp{left, oper=A.LeOp, right, pos}) = 
					  (checkComparable(trexp left, trexp right, pos);
					  {exp=(), ty=Types.INT})		

				|trexp (A.OpExp{left, oper=A.GeOp, right, pos}) = 
					  (checkComparable(trexp left, trexp right, pos);
					  {exp=(), ty=Types.INT})		


				(* Equality operations, ints, strings, records, arrays, or record and nil required*)
				|trexp (A.OpExp{left, oper=A.EqOp, right, pos}) = 
					  (checkEqualComparable(trexp left, trexp right, pos);
					  {exp=(), ty=Types.INT})	

				|trexp (A.OpExp{left, oper=A.NeqOp, right, pos}) = 
					  (checkEqualComparable(trexp left, trexp right, pos);
					  {exp=(), ty=Types.INT})	



				(* Primitives *)
				|trexp(A.IntExp(num)) = {exp=(), ty=Types.INT}
				|trexp(A.StringExp(str)) = {exp=(), ty=Types.STRING}


			    |trexp (_) = (ErrorMsg.error 0 "unknown expression"; {exp=(), ty=Types.INT}) (* default *)

		in trexp(exp) end

	fun transProg(expr) = let val final = transExp(Env.base_venv, Env.base_tenv, expr)
						  in () end

end
