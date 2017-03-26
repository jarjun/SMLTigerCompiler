structure A = Absyn
(* structure Frame = MipsFrame *)
structure T = Translate
structure FE = FindEscape


structure Semant = 
struct

	structure Translate = struct type exp = unit end

	type ty = Types.ty
	type venv = Env.enventry Symbol.table
	type tenv = ty Symbol.table
	type expty = {exp: Translate.exp, ty:ty}

	val loopDepth = ref 0
	


	fun typeToString (Types.INT) = "INT"
	   |typeToString (Types.NAME(s,t)) = "NAME"
	   |typeToString (Types.ARRAY(t,r)) = "ARRAY OF " ^ typeToString(t)
	   |typeToString (Types.RECORD(l, u)) = "RECORD"
	   |typeToString (Types.UNIT) = "UNIT"
	   |typeToString (Types.STRING) = "STRING"
	   |typeToString (Types.NIL) = "NIL"


	fun checkInt ({exp,ty}, pos) = case ty of Types.INT => ()
										      | ty       => ErrorMsg.error pos ("error: integer required, got " ^ typeToString(ty));

	fun resolve_type (tenv,Types.NAME(sym, tyRef), pos) = 
		let fun helper (NONE) = (ErrorMsg.error pos ("error: type " ^ (Symbol.name sym) ^ " not defined"); Types.INT) 
			  | helper (SOME ty)  = resolve_type(tenv, ty, pos)
		in 
			helper (!tyRef)
		end
	  (*| resolve_type (tenv, Types.ARRAY(typ, re), pos) = resolve_type(tenv, typ, pos) *)
	  | resolve_type (tenv, ty:ty, pos) = ty

	fun actual_ty (tenv,sym:Symbol.symbol, pos) = 
		let val result = Symbol.look(tenv, sym)
		in
			case result of SOME(res) => resolve_type(tenv, res, pos)
						 | NONE 	 => (ErrorMsg.error pos ("error: type " ^ (Symbol.name sym) ^ " not defined"); Types.INT) 
		end


	fun checkComparable ({exp=expL,ty=Types.INT}, {exp=expR,ty=Types.INT}, pos) = ()
	   |checkComparable ({exp=expL,ty=Types.STRING}, {exp=expR,ty=Types.STRING}, pos) = ()
	   |checkComparable ({exp=expL,ty=_}, {exp=expR,ty=_}, pos) = ErrorMsg.error pos "error: matching int/str required";

	fun checkEqualComparable ({exp=expL,ty=Types.INT}, {exp=expR,ty=Types.INT}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=Types.STRING}, {exp=expR,ty=Types.STRING}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=Types.RECORD(_, refr1)}, {exp=expR,ty=Types.RECORD(_, refr2)}, pos) = if refr1 = refr2 then () else (ErrorMsg.error pos "error: matching record types required for equality check";())	   
	   |checkEqualComparable ({exp=expL,ty=Types.ARRAY(_, refr1)}, {exp=expR,ty=Types.ARRAY(_,refr2)}, pos) = if refr1 = refr2 then () else (ErrorMsg.error pos "error: matching array types required for equality check";())
	   |checkEqualComparable ({exp=expL,ty=Types.RECORD _}, {exp=expR,ty=Types.NIL}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=Types.NIL}, {exp=expR,ty=Types.RECORD _}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=Types.NIL}, {exp=expR,ty=Types.NIL}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=_}, {exp=expR,ty=_}, pos) = ErrorMsg.error pos "error: matching types required for equality check";

	 (* not checking refs sameType *)
	fun sameType(tenv, pos, Types.ARRAY(typ1, refr1), Types.ARRAY(typ2, refr2)) = 
	 		(refr1 = refr2) andalso (sameType(tenv, pos, resolve_type(tenv, typ1, pos), resolve_type(tenv, typ2, pos))) (*might not the andalso part*)
	 	|sameType(tenv, pos, Types.RECORD(typ1, refr1), Types.RECORD(typ2, refr2)) = refr1 = refr2


	 	|sameType(tenv, pos, Types.RECORD(typ1, refr1), Types.NIL) = true
	 	|sameType(tenv, pos, Types.NIL, Types.RECORD(typ2, refr2)) = true

	    |sameType(tenv, pos, type1:ty, type2:ty) = (resolve_type(tenv, type1, pos) = resolve_type(tenv, type2, pos)) 

	fun getArrayType(Types.ARRAY(t, r), pos) = t
	 	|getArrayType(_, pos) = (ErrorMsg.error pos "error: array not of array type"; Types.INT)

	fun getArrayRef(Types.ARRAY(t, r), pos) = r
	 	|getArrayRef(_, pos) = (ErrorMsg.error pos "error: array not of array type"; ref ())

	fun listContains(l, item:Symbol.symbol) = List.length(List.filter (fn x:Symbol.symbol => x=item) l) <> 0

	fun isCycle(tenv, sym, l) = case Symbol.look(tenv, sym) of SOME(Types.NAME(_, t)) => (case !t of SOME(Types.NAME(s, _)) => if listContains(l,s) 
																					     									 then true 
																						 									 else isCycle(tenv, s, l @ [sym])
																						 		   |_ 					    => false)
										  					  |SOME(_)				  => false
										  					  |NONE 				  => false

	fun transExp(venv, tenv, exp, level, nearestDone) = 
		let fun 

				(* Normal arithmetic, two ints needed *)
				trexp (A.OpExp{left, oper=A.PlusOp, right, pos}) = 
					let val {exp=expLeft, ty=tyLeft} = trexp left
						val {exp=expRight, ty=tyRight} = trexp right
					in
					  (checkInt({exp=expLeft, ty=tyLeft}, pos); 
					  checkInt({exp=expRight, ty=tyRight}, pos);
					  {exp=T.binop(Tree.PLUS, expLeft, expRight), ty=Types.INT})
					end

				|trexp (A.OpExp{left, oper=A.MinusOp, right, pos}) = 
					let val {exp=expLeft, ty=tyLeft} = trexp left
						val {exp=expRight, ty=tyRight} = trexp right
					in
					  (checkInt({exp=expLeft, ty=tyLeft}, pos); 
					  checkInt({exp=expRight, ty=tyRight}, pos);
					  {exp=T.binop(Tree.MINUS, expLeft, expRight), ty=Types.INT})
					end

				|trexp (A.OpExp{left, oper=A.TimesOp, right, pos}) = 
					let val {exp=expLeft, ty=tyLeft} = trexp left
						val {exp=expRight, ty=tyRight} = trexp right
					in
					  (checkInt({exp=expLeft, ty=tyLeft}, pos); 
					  checkInt({exp=expRight, ty=tyRight}, pos);
					  {exp=T.binop(Tree.MUL, expLeft, expRight), ty=Types.INT})
					end

				|trexp (A.OpExp{left, oper=A.DivideOp, right, pos}) = 
					let val {exp=expLeft, ty=tyLeft} = trexp left
						val {exp=expRight, ty=tyRight} = trexp right
					in
					  (checkInt({exp=expLeft, ty=tyLeft}, pos); 
					  checkInt({exp=expRight, ty=tyRight}, pos);
					  {exp=T.binop(Tree.DIV, expLeft, expRight), ty=Types.INT})
					end


				(* Comparing operations, ints or strings needed *)
				|trexp (A.OpExp{left, oper=A.LtOp, right, pos}) = 
						let val {exp=expLeft, ty=tyLeft} = trexp left
							val {exp=expRight, ty=tyRight} = trexp right
						in
						  (checkComparable({exp=expLeft, ty=tyLeft}, {exp=expRight, ty=tyRight}, pos);
						  {exp=T.relop(Tree.LT, expLeft, expRight, tyLeft), ty=Types.INT})
						end		

				|trexp (A.OpExp{left, oper=A.GtOp, right, pos}) = 
					  let val {exp=expLeft, ty=tyLeft} = trexp left
							val {exp=expRight, ty=tyRight} = trexp right
						in
						  (checkComparable({exp=expLeft, ty=tyLeft}, {exp=expRight, ty=tyRight}, pos);
						  {exp=T.relop(Tree.GT, expLeft, expRight, tyLeft), ty=Types.INT})
						end									 			 

				|trexp (A.OpExp{left, oper=A.LeOp, right, pos}) = 
					  let val {exp=expLeft, ty=tyLeft} = trexp left
							val {exp=expRight, ty=tyRight} = trexp right
						in
						  (checkComparable({exp=expLeft, ty=tyLeft}, {exp=expRight, ty=tyRight}, pos);
						  {exp=T.relop(Tree.LE, expLeft, expRight, tyLeft), ty=Types.INT})
						end				

				|trexp (A.OpExp{left, oper=A.GeOp, right, pos}) = 
					  let val {exp=expLeft, ty=tyLeft} = trexp left
							val {exp=expRight, ty=tyRight} = trexp right
						in
						  (checkComparable({exp=expLeft, ty=tyLeft}, {exp=expRight, ty=tyRight}, pos);
						  {exp=T.relop(Tree.GE, expLeft, expRight, tyLeft), ty=Types.INT})
						end				


				(* Equality operations, ints, strings, records, arrays, or record and nil required*)
				|trexp (A.OpExp{left, oper=A.EqOp, right, pos}) = 
					  let val {exp=expLeft, ty=tyLeft} = trexp left
							val {exp=expRight, ty=tyRight} = trexp right
						in
						  (checkEqualComparable({exp=expLeft, ty=tyLeft}, {exp=expRight, ty=tyRight}, pos);
						  {exp=T.relop(Tree.EQ, expLeft, expRight, tyLeft), ty=Types.INT})
						end			

				|trexp (A.OpExp{left, oper=A.NeqOp, right, pos}) = 
					  let val {exp=expLeft, ty=tyLeft} = trexp left
							val {exp=expRight, ty=tyRight} = trexp right
						in
						  (checkEqualComparable({exp=expLeft, ty=tyLeft}, {exp=expRight, ty=tyRight}, pos);
						  {exp=T.relop(Tree.NE, expLeft, expRight, tyLeft), ty=Types.INT})
						end		


				(* others *)
				|trexp (A.LetExp{decs, body, pos}) =
				    let val {venv=venv', tenv=tenv', initList=initList'} = 
				  		transDecs(venv, tenv, decs, level, nearestDone)
				  		val {exp=exp', ty=ty'} = transExp(venv', tenv', body, level, nearestDone)
				  	in 	{exp=T.letBody(initList', exp'), ty=ty'}
				  	end

				|trexp (A.SeqExp (explist)) =
					let (*fun checkSeq [] = {exp=T.nilExp(), ty=Types.UNIT}
						   |checkSeq [(exp, pos)] = trexp(exp)
						   |checkSeq ((exp, pos)::rest) = (trexp(exp); checkSeq(rest))*)
					
						val exptys = map (fn(exp, pos) => trexp(exp)) explist
					in
						{exp=T.seqExp(map (fn x => #exp(x)) exptys), ty = if List.length(exptys) = 0 then Types.UNIT else #ty(List.last(exptys))}
					end


				|trexp(A.ArrayExp{typ=typ, size=siz, init=initial, pos=pos}) = 
					let val {exp=initExp, ty=initTy} = trexp(initial)
						val {exp=sizeExp, ty=sizeTy} = trexp(siz)
					in
						checkInt({exp=sizeExp, ty=sizeTy}, pos);
						(if sameType(tenv, pos, initTy  , 
							 		   resolve_type(tenv, getArrayType(actual_ty(tenv, typ, pos), pos), pos))
						then () 
						else ErrorMsg.error pos "error: array initial value type mismatch");
						{exp=T.arrExp(sizeExp, initExp), ty = actual_ty(tenv, typ, pos)}
					end


				|trexp(A.RecordExp{fields=f, typ=typ, pos=pos}) = 
					(let val symType = if isSome(Symbol.look(tenv, typ)) then  valOf(Symbol.look(tenv, typ)) else (ErrorMsg.error pos "error: undefined record type"; Types.NAME(typ, ref NONE)) (*TODO: clean this up*)
						 

						val Types.NAME(s, r) = case symType of Types.NAME(a,b)  => symType
															   |Types.INT        => Types.NAME(Symbol.symbol(""), ref NONE) (* will throw error on its own later *)
															   |Types.STRING     => Types.NAME(Symbol.symbol(""), ref NONE)
															   |_                => (ErrorMsg.error pos "error: this should never happen in record exp"; Types.NAME(Symbol.symbol(""), ref NONE))

						 val arrRes = if isSome(!r) then resolve_type(tenv, valOf(!r), pos) else (ErrorMsg.error pos "error: not a record type"; Types.INT)
						 val resolveFields = map (fn (s, e, p) => (s, (trexp e), p)) f

						 fun compareFields([], []) = ()
						 	|compareFields([], expectedList) = ErrorMsg.error pos "error: not all record parameters defined"
						 	|compareFields(fieldlist, []) = ErrorMsg.error pos "error: too many record parameters defined"
						 	|compareFields(((sym, {exp=exp', ty=ty'}, pos)::rest), (expectedSym, t)::expectedList) = 
						 		if Symbol.name(sym) = Symbol.name(expectedSym) andalso sameType(tenv, pos, resolve_type(tenv, ty', pos), resolve_type(tenv, t, pos))
						 		then compareFields(rest, expectedList)
						 		else ErrorMsg.error pos ("error: record parameter mismatch " ^ Symbol.name(sym) ^ " and " ^ Symbol.name(expectedSym) )

(*							 	let val item = List.filter (fn (s, t) => Symbol.name(s) = Symbol.name(sym)) expectedList
							 	in 
							 		case item of [(s, t)] => (if sameType(tenv, pos, #ty(trexp(exp)), resolve_type(tenv, t, pos)) then () else ErrorMsg.error pos ("error: record parameter type mismatch " ^ typeToString(#ty(trexp(exp))) ^ " and " ^ typeToString(t) ); compareFields(rest, 
							 			                                                            List.filter (fn (s, t) => Symbol.name(s) <> Symbol.name(sym)) expectedList))
							 			        |[]       => ErrorMsg.error pos ("error: paramter error for " ^ Symbol.name(sym))
							 			        |_        => ErrorMsg.error pos "error: record parameter matches multiple fields"

							 	end *) (*relic of trying to actually make Tiger reasonable*)

					in 
						case arrRes of Types.RECORD(symlist, uniq) => compareFields(resolveFields, symlist)
							       |_                   => ErrorMsg.error pos "error: undefined record type"; 
						{exp=T.recordExp(map (fn (s, e, p) => #exp(e)) resolveFields), ty = actual_ty(tenv, typ, pos)}
					end
					)


				|trexp (A.VarExp(var)) = trvar(var)
				|trexp (A.CallExp{func, args, pos}) = 
					let val Env.FunEntry{level = funLevel, label, formals, result} = case Symbol.look(venv, func) of SOME(Env.FunEntry{level=funLevel, label, formals, result}) => valOf(Symbol.look(venv, func))
														 |SOME(Env.VarEntry{access, ty})			  => ((ErrorMsg.error pos "error: this is a variable, not a function"); Env.FunEntry{level=level, label=Temp.newlabel(), formals = [], result=Types.UNIT})
														 |_ 							  => ((ErrorMsg.error pos "error: undefined function"); Env.FunEntry{level=level, label=Temp.newlabel(), formals = [], result=Types.UNIT})
						val exptyList = map (fn x => trexp x) args
						fun compareParams(formal, idx) = (if sameType(tenv, pos, resolve_type(tenv, formal, pos), resolve_type(tenv, #ty(List.nth(exptyList, idx)), pos )) then () else ErrorMsg.error pos "error: parameter mismatch" ; idx+1)    	


					in 
						if List.length(formals) = List.length(args) then (foldl compareParams 0 formals) else (ErrorMsg.error pos "error: incorrect number of arguments"; 0);
						{exp=T.callExp(level, funLevel, label, map (fn x => #exp(x) ) exptyList), ty=resolve_type(tenv, result, pos)} 
					end

				|trexp (A.ForExp{var, escape, lo, hi, body, pos}) = 
					let val newBreakLabel = (loopDepth := !loopDepth+1; Temp.newlabel())
						val {exp=expLo, ty=tyLo} = trexp lo
						val {exp=expHi, ty=tyHi} = trexp hi
					in


						(checkInt( {exp=expLo, ty=tyLo} , pos);
						checkInt( {exp=expHi, ty=tyHi} , pos);
						let val newVenv = Symbol.enter(venv, var, Env.VarEntry{access=T.allocLocal(level)(!escape), ty=Types.INT})
							val {exp=expBody, ty=tyBody} = transExp(newVenv, tenv, body, level, SOME(newBreakLabel)) (* CHANGE DONE LABEL *)
						in
							if sameType(tenv, pos, tyBody, Types.UNIT)
							then (loopDepth := !loopDepth-1; {exp=T.forExp(expLo, expHi, expBody, newBreakLabel), ty=Types.UNIT})
							else (loopDepth := !loopDepth-1; ErrorMsg.error pos "error: for loop body must return unit"; {exp=T.nilExp(), ty=Types.INT})
						end)
					end

				|trexp (A.WhileExp{test, body, pos}) =
					let val newBreakLabel = (loopDepth := !loopDepth+1; Temp.newlabel())
						val {exp=expBody, ty=tyBody} = transExp(venv, tenv, body, level, SOME(newBreakLabel)) (* CHANGE DONE LABEL *)
						val {exp=expTest, ty=tyTest} = transExp(venv, tenv, test, level, nearestDone)
					in
						(checkInt( {exp=expTest, ty=tyTest} , pos);
						
						if sameType(tenv, pos, tyBody, Types.UNIT)
						then (loopDepth := !loopDepth-1; {exp=T.whileExp(expTest, expBody, newBreakLabel), ty=Types.UNIT})
						else (loopDepth := !loopDepth-1; ErrorMsg.error pos "error: while loop body must return unit"; {exp=T.nilExp(), ty=Types.INT}))
					end

				|trexp (A.IfExp{test, then', else', pos}) = 
					let val {exp=expTest, ty=tyTest} =trexp test
					in
						(checkInt({exp=expTest, ty=tyTest}, pos);
						if isSome(else')
						then 
							let val {exp=expThen, ty=tyThen} = trexp then'
								val {exp=expElse, ty=tyElse} = trexp (valOf(else'))
							in
								(if sameType(tenv, pos, resolve_type(tenv, tyThen, pos), resolve_type(tenv, tyElse, pos))
								then ()
								else ErrorMsg.error pos "error: branches don't have matching return type";
								{exp=T.ifElse(expTest, expThen, expElse), ty=resolve_type(tenv, tyThen, pos)})
							end
						else 
							let val {exp=expThen, ty=tyThen} = trexp then'
							in 
								(if sameType(tenv, pos, resolve_type(tenv, tyThen, pos), Types.UNIT)
								then ()
								else ErrorMsg.error pos "error: then clause must evaluate to UNIT";
								{exp=T.ifThen(expTest, expThen), ty=Types.UNIT})
							end
						)
					end



				|trexp (A.AssignExp{var, exp, pos}) = 
					let val {exp=varExp, ty=varTy} = trvar(var)
						val {exp=expExp, ty=expTy} = trexp(exp)
					in 
						if sameType(tenv, pos, resolve_type(tenv, varTy, pos), resolve_type(tenv, expTy, pos))
						then {exp=T.assignExp(varExp, expExp), ty=Types.UNIT}
						else (ErrorMsg.error pos "error: variable and assigned expression don't have same type"; {exp=T.nilExp(), ty=Types.UNIT})
					end

				|trexp (A.BreakExp(pos)) = 
					(if !loopDepth < 0
					then ErrorMsg.error pos "error: loop depth negative, this should never happen" else ();
					if !loopDepth > 0
					then {exp=T.breakExp(nearestDone), ty=Types.UNIT}
					else (ErrorMsg.error pos "error: break expression outside of a loop";  {exp=T.nilExp(), ty=Types.UNIT}))


				(* Primitives *)
				|trexp(A.IntExp(num)) = {exp=T.intExp(num), ty=Types.INT}
				|trexp(A.StringExp(lit, pos)) = {exp=T.stringExp(lit), ty=Types.STRING}
				|trexp(A.NilExp) = {exp=T.nilExp(), ty=Types.NIL} 


			 and trvar (A.SimpleVar(id, pos)) = 
			 	   (case Symbol.look(venv, id) 
			 	   	of SOME(Env.VarEntry{access, ty}) => {exp=T.simpleVar(access, level), ty=resolve_type(tenv,ty,pos)} 
			 	   	 | SOME(Env.FunEntry{...}) => (ErrorMsg.error pos ("error: undefined variable " ^ Symbol.name id); {exp=T.nilExp(), ty=Types.INT})
			 	   	 | NONE                 => (ErrorMsg.error pos ("error: undefined variable " ^ Symbol.name id); {exp=T.nilExp(), ty=Types.INT}))

			 	|trvar (A.SubscriptVar(var, exp, pos)) = 
			 		let val {exp=varExp, ty=varTy} = trvar(var)
			 			val index = trexp(exp)
			 		in
			 			checkInt(index, pos);

			 			{exp=T.subscriptExp(varExp, #exp(index)), ty= resolve_type (tenv, getArrayType( resolve_type(tenv, varTy, pos), pos), pos)}
			 		end
			 	|trvar (A.FieldVar(var, sym, pos)) = 
			 		let val {exp=varExp, ty=varTy} = trvar(var)
			 			val recTy = resolve_type(tenv, varTy, pos)

			 		in 
			 			case recTy of Types.RECORD(l, u) => 
			 				let val item = List.filter (fn (s, t) => Symbol.name(s) = Symbol.name(sym)) l

			 				val (endIdx, answerIdx) = 

				 				let fun helper ( _, (curIdx, ans)) = 
				 					let val (s,t) = List.nth(l, curIdx)
				 					in
				 						if Symbol.name(s) = Symbol.name(sym) then (curIdx+1, curIdx) else (curIdx+1, ans)
				 					end

				 				in
				 					foldl helper (0,~1) l
				 				end


			 				in
			 					case item of [(s, t)] => {exp=T.fieldVar(varExp, answerIdx), ty=resolve_type(tenv, t, pos)}
							 			        |[]       => (ErrorMsg.error pos ("error: paramter error for " ^ Symbol.name(sym)); {exp=T.nilExp(), ty=Types.INT})
							 			        |_        => (ErrorMsg.error pos "error: record parameter matches multiple fields"; {exp=T.nilExp(), ty=Types.INT}) (*this should never happen*)

			 				end
			 				         |_                  => (ErrorMsg.error pos "error: variable not a record"; {exp=T.nilExp(), ty=Types.INT})
			 		end



		in trexp(exp) end

	and transDecs(venv, tenv, [], level, nearestDone) = {venv=venv, tenv=tenv, initList = []}
	   |transDecs(venv, tenv, decs, level, nearestDone) = 

		   let fun trdec (A.VarDec{name, typ=NONE, init, escape, pos}, {venv, tenv, initList}) = 
		   			let val {exp, ty=typ} = transExp(venv, tenv, init, level, nearestDone)
		   				val acc = T.allocLocal(level)(!escape)
		   			in if typ = Types.NIL 
					   then (ErrorMsg.error pos "error: must specify record type to assign variable to nil"; {tenv=tenv, venv=venv, initList=initList})
					   else {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry {access=acc, ty=typ}), initList=initList @ [T.varDec(acc, exp)]}
		   			end

		   		  |trdec (A.VarDec{name, typ=SOME((declaredType,tyPos)), init, escape, pos}, {venv, tenv, initList}) =
		   		  	let val {exp, ty=typ} = transExp(venv, tenv, init, level, nearestDone)
		   				val acc = T.allocLocal(level)(!escape)
		   			in 
		   				if sameType(tenv, pos, actual_ty(tenv,declaredType,tyPos), resolve_type(tenv,typ,pos))
		   				then {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry {access=acc, ty=typ}), initList=initList @ [T.varDec(acc, exp)]} 
		   				else (ErrorMsg.error pos ("error: variable has incorrect type"); {tenv=tenv, venv=venv, initList=initList}) 
		   			end 

		   		  |trdec (A.TypeDec(declist), {venv, tenv, initList}) = 
		   		  	let fun processTyDec (tenv, venv, {name, ty=A.NameTy(typ, tyPos), pos}) = 
				   		  		let val typName = Symbol.look(tenv, typ)
				   		  			val ourName = valOf(Symbol.look(tenv, name))
				   		  			fun updateRef (Types.NAME(name, otherRef), thingToUpdateTo) = ((otherRef := thingToUpdateTo); ())
				   		  			   |updateRef (_) = ()

				   		  			val toRef = (if isSome(typName) 
				   		  						 then typName
				   		  						 else ((ErrorMsg.error tyPos ("error: type " ^ Symbol.name typ ^ " not declared")); NONE))
				   		  		in
				   		  			updateRef(ourName, toRef);
				   		  			if isCycle(tenv, name, []) 
				   		  			then (ErrorMsg.error pos "error: cyclic types are not valid"; updateRef(ourName, NONE))
				   		  			else ();
				   		  			{tenv=tenv, venv=venv, initList=initList}
				   		  		end

		   		  		   |processTyDec (tenv, venv, {name, ty=A.RecordTy(fieldlist), pos}) = 
		   		  		   		let fun addField ({name, escape, typ, pos}, (running,l)) = 
		   		  		   			let val typAdd = Symbol.look(tenv, typ)
		   		  		   				val typCheck = if isSome(typAdd) then valOf(typAdd) else (ErrorMsg.error pos ("error: type " ^ Symbol.name typ ^ " not declared"); Types.INT)
		   		  		   			in
		   		  		   				if listContains(l, name) 
		   		  		   				then (ErrorMsg.error pos ("error: record field " ^ Symbol.name(name) ^ " already declared"); (running, l)) 
		   		  		   				else (running @ [(name, typCheck)], l @ [name])
		   		  		   			end
		   		  		   			val ourName = valOf(Symbol.look(tenv, name))
		   		  		   			val (fieldlist, namelist) = foldl addField ([],[]) fieldlist
		   		  		   			val recTyp = SOME(Types.RECORD(fieldlist, ref ()))
		   		  		   			fun updateRef (Types.NAME(name, otherRef), thingToUpdateTo) = ((otherRef := thingToUpdateTo); ())
				   		  			   |updateRef (_) = ()
		   		  		   		in 
		   		  		   			updateRef(ourName, recTyp);
		   		  		   			{tenv=tenv, venv=venv, initList=initList}
		   		  		   		end

		   		  		   |processTyDec (tenv, venv, {name, ty=A.ArrayTy(typ, tyPos), pos}) = 
		   		  		   		let val ourName = valOf(Symbol.look(tenv, name))
		   		  		   			val typOption = Symbol.look(tenv, typ)
		   		  		   			val typCheck = if isSome(typOption) then valOf(typOption) else (ErrorMsg.error pos ("error: type " ^ Symbol.name typ ^ " not declared"); Types.INT)
		   		  		   			val arrTyp = SOME(Types.ARRAY(typCheck, ref ()))
		   		  		   			fun updateRef (Types.NAME(name, otherRef), thingToUpdateTo) = ((otherRef := thingToUpdateTo); ())
				   		  			   |updateRef (_) = ()
		   		  		   		in
		   		  		   			updateRef(ourName, arrTyp);
		   		  		   			{tenv=tenv, venv=venv, initList=initList}
		   		  		   		end


		   		  		fun enterDecs (tenv, venv, l, []) = {tenv=tenv, venv=venv, initList=initList}
		   		  		   |enterDecs (tenv, venv, l, [dec]) = if listContains(l, #name(dec))
						   									then (ErrorMsg.error (#pos(dec)) ("error: can't redeclare type " ^ Symbol.name(#name(dec)) ^ " in type declaration block"); {tenv=tenv, venv=venv, initList=initList})
						   									else processTyDec(tenv, venv, dec)

		   		  		   |enterDecs (tenv, venv, l, (dec::rest)) = 
		   		  		   		let val {venv=venv', tenv=tenv', initList=initList'} = if listContains(l, #name(dec)) 
						   										   then (ErrorMsg.error (#pos(dec)) ("error: can't redeclare type " ^ Symbol.name(#name(dec)) ^ " in type declaration block"); {tenv=tenv, venv=venv, initList=initList})
						   										   else processTyDec(tenv, venv, dec) 
		   		  		   		in
		   		  		   			enterDecs(tenv', venv', l @ [#name(dec)], rest)
								end

						fun initDecs (tenv, venv, []) = {tenv=tenv, venv=venv, initList=initList}
						   |initDecs (tenv, venv, [{name, ty, pos}]) = {tenv = Symbol.enter (tenv, name, Types.NAME (name, (ref NONE)) ), venv=venv, initList=initList}
						   |initDecs (tenv, venv, {name,ty,pos}::rest) = 
		   		  		   		let val {venv=venv', tenv=tenv', initList=initList'} = {tenv = Symbol.enter (tenv, name, Types.NAME (name, (ref NONE)) ), venv=venv, initList=initList}
		   		  		   		in
		   		  		   			initDecs(tenv', venv', rest)
								end
					in
						let val {venv=venv', tenv=tenv', initList=initList'} = initDecs(tenv, venv, declist)						
						in 
							enterDecs(tenv', venv', [], declist)
						end
					end
		   		  

				  |trdec(A.FunctionDec(fundeclist), {venv, tenv, initList}) = 
				  	let fun processFunDec( tenv, venv, {name, params, body, pos, result=SOME(rt, posi)} ) = 
				  				
						  		let val symbolTableEntry = Symbol.look(venv, name)
							  		val newLevelFromVenv = case symbolTableEntry of SOME(Env.FunEntry{level, label, formals, result}) => level
							  												   |_ => (ErrorMsg.error ~1 "Function not in venv on second pass?"; T.outermost)
								  	val (stat::forms) = T.formals(newLevelFromVenv)
							  		val combined = ListPair.zip(forms, params)
						  			val newVenv = 
						  			let fun putIntoNewVenv( (acc , {name, escape, typ, pos}), venv ) = ((*T.printAccess(acc);*) Symbol.enter(venv, name, Env.VarEntry{access=acc, ty= actual_ty(tenv, typ, pos)}))
							  			
							  		in
							  			foldl putIntoNewVenv venv combined
							  		end
							  		val {exp=exp', ty=ty'} = transExp(newVenv, tenv, body, newLevelFromVenv, NONE) (* TODO figure out if this NONE break label is right *)
							  	in 
							  		if sameType(tenv, pos, ty', actual_ty(tenv, rt, pos)) (* need new level here *)
							  		then (T.procEntryExit({level=newLevelFromVenv, body=exp'});
							  				{venv = venv, tenv=tenv, initList=initList})
							  		else (ErrorMsg.error pos ("error: function return type and body type different"); 	{venv=venv, tenv=tenv, initList=initList})
							  	end


					  		|processFunDec( tenv, venv, {name, params, body, pos, result=NONE} ) = 

					  			(* copy and pasted, change later? *)
					  			let val symbolTableEntry = Symbol.look(venv, name)
							  		val newLevelFromVenv = case symbolTableEntry of SOME(Env.FunEntry{level, label, formals, result}) => level
							  													   |_ => (ErrorMsg.error ~1 "Function not in venv on second pass?"; T.outermost)
							  		val (stat::forms) = T.formals(newLevelFromVenv)
							  		val combined = ListPair.zip(forms, params)
					  				val newVenv = 
						  			let fun putIntoNewVenv( (acc , {name, escape, typ, pos}), venv ) = ((*T.printAccess(acc);*) Symbol.enter(venv, name, Env.VarEntry{access=acc, ty= actual_ty(tenv, typ, pos)}))
							  			
							  		in
							  			foldl putIntoNewVenv venv combined
							  		end
							  		val {exp=exp', ty=ty'} = transExp(newVenv, tenv, body, newLevelFromVenv, NONE)
							  	in 
							  		if sameType(tenv, pos, ty', Types.UNIT) (* need new level here *)
							  		then (T.procEntryExit({level=newLevelFromVenv, body=exp'});
							  			{venv = venv, tenv=tenv, initList=initList})
							  		else (ErrorMsg.error pos ("error: function return type and body type different"); 	{venv=venv, tenv=tenv, initList=initList})
							  	end


					  	fun addFunctionToVenv(venv, tenv, {name, params, body, pos, result=SOME(rt, posi)}) = 
					  			let val levelName = Temp.newlabel()
					  				val newLevel = T.newLevel({parent=level, name=levelName, formals= map (fn {name, escape, typ, pos} => !escape) params}) 
					  				val paramTys = map (fn {name, escape, typ=sym, pos} => actual_ty(tenv, sym, pos)) params
					  			in
					  				{venv = Symbol.enter(venv, name, Env.FunEntry{level = newLevel, label = levelName, formals = paramTys, result = actual_ty(tenv, rt, pos)}), tenv=tenv, initList=initList}
					  			end

					  	   |addFunctionToVenv(venv, tenv, {name, params, body, pos, result=NONE}) = 
					  			let val levelName = Temp.newlabel()
					  				val newLevel = T.newLevel({parent=level, name=levelName, formals= map (fn {name, escape, typ, pos} => !escape) params}) 
					  				val paramTys = map (fn {name, escape, typ=sym, pos} => actual_ty(tenv, sym, pos)) params
					  			in
					  				{venv = Symbol.enter(venv, name, Env.FunEntry{level = newLevel, label = levelName, formals = paramTys, result = Types.UNIT}), tenv=tenv, initList=initList}
					  			end


				  		fun initDecs (tenv, venv, l, []) = {venv=venv, tenv=tenv, initList=initList}
				  		   |initDecs (tenv, venv, l, [fndec] ) = if listContains(l, #name(fndec))
						   									  then (ErrorMsg.error (#pos(fndec)) ("error: can't redeclare function " ^ Symbol.name(#name(fndec)) ^ " in function declaration block"); {tenv=tenv, venv=venv, initList=initList})
						   									  else addFunctionToVenv(venv, tenv, fndec)
				  		   |initDecs (tenv, venv, l, fndec::rest ) = 
				  		    	let val {venv=venv', tenv=tenv', initList=initList'} = if listContains(l, #name(fndec)) 
						   										   then (ErrorMsg.error (#pos(fndec)) ("error: can't redeclare function " ^ Symbol.name(#name(fndec)) ^ " in function declaration block"); {tenv=tenv, venv=venv, initList=initList})
						   										   else addFunctionToVenv(venv, tenv, fndec)
		   		  		   		in
		   		  		   			initDecs(tenv', venv', l @ [#name(fndec)], rest)
								end



		   		  		fun enterDecs (tenv, venv, []) = {tenv=tenv, venv=venv, initList=initList}
		   		  		   |enterDecs (tenv, venv, [fndec]) = processFunDec(tenv, venv, fndec)
		   		  		   |enterDecs (tenv, venv, fndec::rest) = 
		   		  		   		let val {venv=venv', tenv=tenv', initList=initList'} = processFunDec(tenv, venv, fndec) 
		   		  		   		in
		   		  		   			enterDecs(tenv', venv', rest)
								end

				  	in
				  		let val {venv=venv', tenv=tenv', initList=initList'} = initDecs(tenv, venv, [], fundeclist)						
						in 
							enterDecs(tenv', venv', fundeclist)
						end

				  	end
		   		  
		   		

		   	in foldl trdec {venv=venv, tenv=tenv, initList=[]} decs
		   	end

	fun transProg(expr) = let val curLevel = T.newLevel({parent=T.outermost, name=Temp.newlabel(), formals=[]})
							  val _ = FE.findEscape(expr)
							  val final = #exp(transExp(Env.base_venv, Env.base_tenv, expr, curLevel, NONE))
							  val _ = T.procEntryExit({level=curLevel, body=final})
							  val finalFrags = T.getResult()
						  in 
						  	(*T.printResult();*)
						  	finalFrags
						  end

end
