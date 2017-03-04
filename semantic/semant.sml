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
	


	fun typeToString (Types.INT) = "INT"
	   |typeToString (Types.NAME(s,t)) = "NAME"
	   |typeToString (Types.ARRAY(t,r)) = "ARRAY OF " ^ typeToString(t)
	   |typeToString (Types.RECORD(l, u)) = "RECORD"
	   |typeToString (Types.UNIT) = "UNIT"
	   |typeToString (Types.STRING) = "STRING"
	   |typeToString (Types.NIL) = "NIL"


	fun checkInt ({exp,ty}, pos) = case ty of Types.INT => ()
										      | ty       => ErrorMsg.error pos ("integer required, got " ^ typeToString(ty));

	fun resolve_type (tenv,Types.NAME(sym, tyRef), pos) = 
		let fun helper (NONE) = (ErrorMsg.error pos ("type " ^ (Symbol.name sym) ^ " not defined"); Types.INT) 
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
						 | NONE 	 => (ErrorMsg.error pos ("type " ^ (Symbol.name sym) ^ " not defined"); Types.INT) 
		end


	fun checkComparable ({exp=expL,ty=Types.INT}, {exp=expR,ty=Types.INT}, pos) = ()
	   |checkComparable ({exp=expL,ty=Types.STRING}, {exp=expR,ty=Types.STRING}, pos) = ()
	   |checkComparable ({exp=expL,ty=_}, {exp=expR,ty=_}, pos) = ErrorMsg.error pos "matching int/str required";

	fun checkEqualComparable ({exp=expL,ty=Types.INT}, {exp=expR,ty=Types.INT}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=Types.STRING}, {exp=expR,ty=Types.STRING}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=Types.RECORD(_, refr1)}, {exp=expR,ty=Types.RECORD(_, refr2)}, pos) = if refr1 = refr2 then () else (ErrorMsg.error pos "matching record types required for equality check";())	   
	   |checkEqualComparable ({exp=expL,ty=Types.ARRAY(_, refr1)}, {exp=expR,ty=Types.ARRAY(_,refr2)}, pos) = if refr1 = refr2 then () else (ErrorMsg.error pos "matching array types required for equality check";())
	   |checkEqualComparable ({exp=expL,ty=Types.RECORD _}, {exp=expR,ty=Types.NIL}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=Types.NIL}, {exp=expR,ty=Types.RECORD _}, pos) = ()
	   |checkEqualComparable ({exp=expL,ty=_}, {exp=expR,ty=_}, pos) = ErrorMsg.error pos "matching types required for equality check";

	 (* not checking refs sameType *)
	fun sameType(tenv, pos, Types.ARRAY(typ1, refr1), Types.ARRAY(typ2, refr2)) = 
	 		(refr1 = refr2) andalso (sameType(tenv, pos, resolve_type(tenv, typ1, pos), resolve_type(tenv, typ2, pos))) (*might not the andalso part*)
	 	|sameType(tenv, pos, Types.RECORD(typ1, refr1), Types.RECORD(typ2, refr2)) = refr1 = refr2


	 	|sameType(tenv, pos, Types.RECORD(typ1, refr1), Types.NIL) = true
	 	|sameType(tenv, pos, Types.NIL, Types.RECORD(typ2, refr2)) = true

	    |sameType(tenv, pos, type1:ty, type2:ty) = (resolve_type(tenv, type1, pos) = resolve_type(tenv, type2, pos)) (*TODO: actually deal with types here: records?*)

	fun getArrayType(Types.ARRAY(t, r), pos) = t
	 	|getArrayType(_, pos) = (ErrorMsg.error pos "Array not of array type"; Types.INT)

	fun getArrayRef(Types.ARRAY(t, r), pos) = r
	 	|getArrayRef(_, pos) = (ErrorMsg.error pos "Array not of array type"; ref ())

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


				(* others *)
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


				|trexp(A.ArrayExp{typ=typ, size=siz, init=initial, pos=pos}) = 
					(
					checkInt(trexp siz, pos);
					if sameType(tenv, pos, #ty (trexp(initial))  , 
								 		   resolve_type(tenv, getArrayType(actual_ty(tenv, typ, pos), pos), pos))
							then () else ErrorMsg.error pos "Array initial value type mismatch";

					{exp=(), ty = actual_ty(tenv, typ, pos)})

				|trexp(A.RecordExp{fields=f, typ=typ, pos=pos}) = 
					(let val symType = if isSome(Symbol.look(tenv, typ)) then  valOf(Symbol.look(tenv, typ)) else (ErrorMsg.error pos "Undefined record type"; Types.NAME(typ, ref NONE)) (*TODO: clean this up*)
						 

						val Types.NAME(s, r) = case symType of Types.NAME(a,b)  => symType
															   |Types.INT        => Types.NAME(Symbol.symbol(""), ref NONE) (* will throw error on its own later *)
															   |Types.STRING     => Types.NAME(Symbol.symbol(""), ref NONE)
															   |_                => (ErrorMsg.error pos "This should never happen in record exp"; Types.NAME(Symbol.symbol(""), ref NONE))

						 val arrRes = if isSome(!r) then resolve_type(tenv, valOf(!r), pos) else (ErrorMsg.error pos "Not a record type"; Types.INT)
						 fun compareFields([], []) = ()
						 	|compareFields([], expectedList) = ErrorMsg.error pos "Not all record parameters defined"
						 	|compareFields(fieldlist, []) = ErrorMsg.error pos "Too many record parameters defined"
						 	|compareFields(((sym, exp, pos)::rest), expectedList) = 
							 	let val item = List.filter (fn (s, t) => Symbol.name(s) = Symbol.name(sym)) expectedList
							 	in 
							 		case item of [(s, t)] => (if sameType(tenv, pos, #ty(trexp(exp)), resolve_type(tenv, t, pos)) then () else ErrorMsg.error pos ("Record parameter type mismatch " ^ typeToString(#ty(trexp(exp))) ^ " and " ^ typeToString(t) ); compareFields(rest, 
							 			                                                            List.filter (fn (s, t) => Symbol.name(s) <> Symbol.name(sym)) expectedList))
							 			        |[]       => ErrorMsg.error pos ("Paramter error for " ^ Symbol.name(sym))
							 			        |_        => ErrorMsg.error pos "Record parameter matches multiple fields" (*this should never happen*)

							 	end

					in 
						case arrRes of Types.RECORD(symlist, uniq) => compareFields(f, symlist)
							       |_                   => ErrorMsg.error pos "Undefined record type"; 
						{exp=(), ty = actual_ty(tenv, typ, pos)}
					end
					)


				|trexp (A.VarExp(var)) = trvar(var)
				|trexp (A.CallExp{func, args, pos}) = 
					let val Env.FunEntry{formals, result} = case Symbol.look(venv, func) of SOME(Env.FunEntry{formals, result}) => valOf(Symbol.look(venv, func))
														 |SOME(Env.VarEntry{ty})			  => ((ErrorMsg.error pos "This is a variable, not a function"); Env.FunEntry{formals = [], result=Types.UNIT})
														 |_ 							  => ((ErrorMsg.error pos "Undefined function"); Env.FunEntry{formals = [], result=Types.UNIT})

						fun compareParams(formal, idx) = (if sameType(tenv, pos, resolve_type(tenv, formal, pos), resolve_type(tenv, (#ty(trexp(List.nth(args, idx)))), pos )) then () else ErrorMsg.error pos "parameter mismatch" ; idx+1)    	


					in 
						if List.length(formals) = List.length(args) then (foldl compareParams 0 formals) else (ErrorMsg.error pos "incorrect number of arguments"; 0);
						{exp=(), ty=resolve_type(tenv, result, pos)} 
					end


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

			 	|trvar (A.SubscriptVar(var, exp, pos)) = 
			 		let val {exp=_, ty=varTy} = trvar(var)
			 		in
			 			checkInt(trexp(exp), pos);

			 			{exp=(), ty= resolve_type (tenv, getArrayType( resolve_type(tenv, varTy, pos), pos), pos)}
			 		end
			 	|trvar (A.FieldVar(var, sym, pos)) = 
			 		let val {exp=_, ty=varTy} = trvar(var)
			 			val recTy = resolve_type(tenv, varTy, pos)

			 		in 
			 			case recTy of Types.RECORD(l, u) => 
			 				let val item = List.filter (fn (s, t) => Symbol.name(s) = Symbol.name(sym)) l

			 				in
			 					case item of [(s, t)] => {exp=(), ty=resolve_type(tenv, t, pos)}
							 			        |[]       => (ErrorMsg.error pos ("Paramter error for " ^ Symbol.name(sym)); {exp=(), ty=Types.INT})
							 			        |_        => (ErrorMsg.error pos "Record parameter matches multiple fields"; {exp=(), ty=Types.INT}) (*this should never happen*)

			 				end
			 				         |_                  => (ErrorMsg.error pos "variable not a record"; {exp=(), ty=Types.INT})
			 		end



		in trexp(exp) end

	and transDecs(venv, tenv, []) = {venv=venv, tenv=tenv}
		   |transDecs(venv, tenv, decs) = 

		   let fun trdec (A.VarDec{name, typ=NONE, init, escape, pos}, {venv, tenv}) = 
		   			let val {exp, ty=typ} = transExp(venv, tenv, init)
		   		
		   			in {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry {ty=typ})}
		   			end

		   		  |trdec (A.VarDec{name, typ=SOME((declaredType,tyPos)), init, escape, pos}, {venv, tenv}) =
		   		  	let val {exp, ty=typ} = transExp(venv, tenv, init)
		   		
		   			in 
		   				if sameType(tenv, pos, actual_ty(tenv,declaredType,tyPos), resolve_type(tenv,typ,pos))
		   				then {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry {ty=typ})} 
		   				else (ErrorMsg.error pos ("variable has incorrect type"); {tenv=tenv, venv=venv}) (* TODO not adding to symbol table? *)
		   			end 

		   		  |trdec (A.TypeDec(declist), {venv, tenv}) = 
		   		  	let fun processTyDec (tenv, venv, {name, ty=A.NameTy(typ, tyPos), pos}) = 
				   		  		let val typName = Symbol.look(tenv, typ)
				   		  			val ourName = valOf(Symbol.look(tenv, name))
				   		  			fun updateRef (Types.NAME(name, otherRef), thingToUpdateTo) = ((otherRef := thingToUpdateTo); ())
				   		  			   |updateRef (_) = ()

				   		  			val toRef = (if isSome(typName) 
				   		  						 then typName
				   		  						 else ((ErrorMsg.error tyPos ("type " ^ Symbol.name typ ^ " not declared")); NONE))
				   		  		in
				   		  			updateRef(ourName, toRef);
				   		  			{tenv=tenv, venv=venv}
				   		  		end

		   		  		   |processTyDec (tenv, venv, {name, ty=A.RecordTy(fieldlist), pos}) = 
		   		  		   		let fun addField ({name, escape, typ, pos}, running) = 
		   		  		   			let val typAdd = Symbol.look(tenv, typ)
		   		  		   				val typCheck = if isSome(typAdd) then valOf(typAdd) else (ErrorMsg.error pos ("type " ^ Symbol.name typ ^ " not declared"); Types.INT)
		   		  		   			in
		   		  		   				running @ [(name, typCheck)]
		   		  		   			end
		   		  		   			val ourName = valOf(Symbol.look(tenv, name))
		   		  		   			val recTyp = SOME(Types.RECORD((foldl addField [] fieldlist), ref ()))
		   		  		   			fun updateRef (Types.NAME(name, otherRef), thingToUpdateTo) = ((otherRef := thingToUpdateTo); ())
				   		  			   |updateRef (_) = ()
		   		  		   		in 
		   		  		   			updateRef(ourName, recTyp);
		   		  		   			{tenv=tenv, venv=venv}
		   		  		   		end

		   		  		   |processTyDec (tenv, venv, {name, ty=A.ArrayTy(typ, tyPos), pos}) = 
		   		  		   		let val ourName = valOf(Symbol.look(tenv, name))
		   		  		   			val typOption = Symbol.look(tenv, typ)
		   		  		   			val typCheck = if isSome(typOption) then valOf(typOption) else (ErrorMsg.error pos ("type " ^ Symbol.name typ ^ " not declared"); Types.INT)
		   		  		   			val arrTyp = SOME(Types.ARRAY(typCheck, ref ()))
		   		  		   			fun updateRef (Types.NAME(name, otherRef), thingToUpdateTo) = ((otherRef := thingToUpdateTo); ())
				   		  			   |updateRef (_) = ()
		   		  		   		in
		   		  		   			updateRef(ourName, arrTyp);
		   		  		   			{tenv=tenv, venv=venv}
		   		  		   		end


		   		  		fun enterDecs (tenv, venv, []) = {tenv=tenv, venv=venv}
		   		  		   |enterDecs (tenv, venv, [dec]) = processTyDec(tenv, venv, dec)
		   		  		   |enterDecs (tenv, venv, (dec::rest)) = 
		   		  		   		let val {venv=venv', tenv=tenv'} = processTyDec(tenv, venv, dec) 
		   		  		   		in
		   		  		   			enterDecs(tenv', venv', rest)
								end

						fun initDecs (tenv, venv, []) = {tenv=tenv, venv=venv}
						   |initDecs (tenv, venv, [{name, ty, pos}]) = {tenv = Symbol.enter (tenv, name, Types.NAME (name, (ref NONE)) ), venv=venv}
						   |initDecs (tenv, venv, {name,ty,pos}::rest) = 
		   		  		   		let val {venv=venv', tenv=tenv'} = {tenv = Symbol.enter (tenv, name, Types.NAME (name, (ref NONE)) ), venv=venv}
		   		  		   		in
		   		  		   			initDecs(tenv', venv', rest)
								end
					in
						let val {venv=venv', tenv=tenv'} = initDecs(tenv, venv, declist)						
						in 
							enterDecs(tenv', venv', declist)
						end
					end
		   		  

				  |trdec(A.FunctionDec(fundeclist), {venv, tenv}) = 
				  	let fun processFunDec( tenv, venv, {name, params, body, pos, result=SOME(rt, posi)} ) = 

						  		let val newVenv = 
						  			let fun putIntoNewVenv( {name, escape, typ, pos}, venv ) = Symbol.enter(venv, name, Env.VarEntry{ty= actual_ty(tenv, typ, pos)})
							  			
							  		in
							  			foldl putIntoNewVenv venv params
							  		end
							  	in 
							  		if sameType(tenv, pos, #ty(transExp(newVenv, tenv, body)), actual_ty(tenv, rt, pos))
							  		then {venv = venv, tenv=tenv}
							  		else (ErrorMsg.error pos ("Function return type and body type different"); 	{venv=venv, tenv=tenv})
							  	end


					  		|processFunDec( tenv, venv, {name, params, body, pos, result=NONE} ) = 

					  			(* copy and pasted, change later? *)
					  			let val newVenv = 
						  			let fun putIntoNewVenv( {name, escape, typ, pos}, venv ) = Symbol.enter(venv, name, Env.VarEntry{ty= actual_ty(tenv, typ, pos)})
							  			
							  		in
							  			foldl putIntoNewVenv venv params
							  		end
							  	in 
							  		if sameType(tenv, pos, #ty(transExp(newVenv, tenv, body)), Types.UNIT)
							  		then {venv = venv, tenv=tenv}
							  		else (ErrorMsg.error pos ("Function return type and body type different"); 	{venv=venv, tenv=tenv})
							  	end


					  	fun addFunctionToVenv(venv, tenv, {name, params, body, pos, result=SOME(rt, posi)}) = 
					  			let val paramTys = map (fn {name, escape, typ=sym, pos} => actual_ty(tenv, sym, pos)) params
					  			in
					  				{venv = Symbol.enter(venv, name, Env.FunEntry{formals = paramTys, result = actual_ty(tenv, rt, pos)}), tenv=tenv}
					  			end

					  	   |addFunctionToVenv(venv, tenv, {name, params, body, pos, result=NONE}) = 
					  			let val paramTys = map (fn {name, escape, typ=sym, pos} => actual_ty(tenv, sym, pos)) params
					  			in
					  				{venv = Symbol.enter(venv, name, Env.FunEntry{formals = paramTys, result = Types.UNIT}), tenv=tenv}
					  			end


				  		fun initDecs (tenv, venv, []) = {venv=venv, tenv=tenv}
				  		   |initDecs (tenv, venv, [fndec] ) =  addFunctionToVenv(venv, tenv, fndec)
				  		   |initDecs (tenv, venv, fndec::rest ) = 
				  		    	let val {venv=venv', tenv=tenv'} = addFunctionToVenv(venv, tenv, fndec)
		   		  		   		in
		   		  		   			initDecs(tenv', venv', rest)
								end



		   		  		fun enterDecs (tenv, venv, []) = {tenv=tenv, venv=venv}
		   		  		   |enterDecs (tenv, venv, [fndec]) = processFunDec(tenv, venv, fndec)
		   		  		   |enterDecs (tenv, venv, fndec::rest) = 
		   		  		   		let val {venv=venv', tenv=tenv'} = processFunDec(tenv, venv, fndec) 
		   		  		   		in
		   		  		   			enterDecs(tenv', venv', rest)
								end

				  	in
				  		let val {venv=venv', tenv=tenv'} = initDecs(tenv, venv, fundeclist)						
						in 
							enterDecs(tenv', venv', fundeclist)
						end

				  	end
		   		  
		   		

		   	in foldl trdec {venv=venv, tenv=tenv} decs
		   	end

	fun transProg(expr) = let val final = transExp(Env.base_venv, Env.base_tenv, expr)
						  in () end

end
