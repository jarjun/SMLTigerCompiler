structure A = Absyn

signature SEMANT = 
sig
	val transProg: A.exp -> unit
end

structure Semant : SEMANT = 
struct

	structure Translate = struct type exp = unit end

	type ty = Types.ty
	type venv = Env.enventry Symbol.table
	type tenv = ty Symbol.table
	type expty = {exp: Translate.exp, ty:ty}
	
	fun checkInt ({exp,ty}, pos) = case ty of Types.INT => ()
										      | _       => ErrorMsg.error pos "integer required";

	fun resolve_type (tenv,Types.NAME(sym, tyRef), pos) = 
		let fun helper (NONE) = (ErrorMsg.error pos "type not defined"; Types.INT) 
			  | helper (SOME ty)  = resolve_type(tenv, ty, pos)
		in 
			helper (!tyRef)
		end
	  | resolve_type (tenv, ty:ty, pos) = ty

	fun actual_ty (tenv,sym:Symbol.symbol, pos) = 
		let val result = Symbol.look(tenv, sym)
		in
			case result of SOME(res) => resolve_type(tenv, res, pos)
						 | NONE 	 => (ErrorMsg.error pos "type not defined"; Types.INT) 
		end

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

	 fun sameType(type1:ty, type2:ty) = (type1 = type2) (*TODO: actually deal with types here: records?*)


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



				|trexp (A.LetExp{decs, body, pos}) =
				    let val {venv=venv', tenv=tenv'} = 
				  		transDecs(venv, tenv, decs)
				  	in 	transExp(venv', tenv', body)
				  	end

				|trexp (A.SeqExp (explist)) =
					let fun checkSeq [] = {exp=(), ty=Types.UNIT}
						   |checkSeq [(exp, pos)] = trexp(exp)
						   |checkSeq ((exp, pos)::rest) = (trexp(exp); checkSeq(rest))
					in 
						checkSeq(explist)
					end


				|trexp (A.VarExp(var)) = trvar(var)

				(* Primitives *)
				|trexp(A.IntExp(num)) = {exp=(), ty=Types.INT}
				|trexp(A.StringExp(str)) = {exp=(), ty=Types.STRING}
				|trexp(A.NilExp) = {exp=(), ty=Types.NIL} 


			    |trexp (_) = (ErrorMsg.error 0 "unknown expression"; {exp=(), ty=Types.INT}) (* default *)


			 and trvar (A.SimpleVar(id, pos)) = (* nonexhaustive *)
			 	   (case Symbol.look(venv, id) 
			 	   	of SOME(Env.VarEntry{ty}) => {exp=(), ty=resolve_type(tenv,ty,pos)}  (* TODO: deal w/ actual_ty *)
			 	   	 | SOME(Env.FunEntry{...}) => (ErrorMsg.error pos ("undefined variable " ^ Symbol.name id); {exp=(), ty=Types.INT})
			 	   	 | NONE                 => (ErrorMsg.error pos ("undefined variable " ^ Symbol.name id); {exp=(), ty=Types.INT}))


		in trexp(exp) end

	and transDecs(venv, tenv, []) = {venv=venv, tenv=tenv}
		   |transDecs(venv, tenv, decs) = 

		   let fun trdec (A.VarDec{name, typ=NONE, init, escape, pos}, {venv, tenv}) = 
		   			let val {exp, ty=typ} = transExp(venv, tenv, init)
		   		
		   			in {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry {ty=typ})}
		   			end

		   		  |trdec ((A.VarDec{name, typ=SOME((declaredType,tyPos)), init, escape, pos}, {venv, tenv})) =
		   		  	let val {exp, ty=typ} = transExp(venv, tenv, init)
		   		
		   			in 
		   				if sameType(actual_ty(tenv,declaredType,tyPos), resolve_type(tenv,typ,pos))
		   				then {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry {ty=typ})} 
		   				else (ErrorMsg.error pos "variable has incorrect type"; {tenv=tenv, venv=venv}) (* TODO not adding to symbol table? *)
		   			end 

		   		  |trdec(_) = {venv=venv, tenv=tenv}
		   		

		   	in foldl trdec {venv=venv, tenv=tenv} decs
		   	end

	fun transProg(expr) = let val final = transExp(Env.base_venv, Env.base_tenv, expr)
						  in () end

end
