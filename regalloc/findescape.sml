structure Ab = Absyn

structure FindEscape: 
sig
	val findEscape: Ab.exp -> unit
end =

struct 
	type depth = int
	type escEnv = (depth * bool ref) Symbol.table

	fun traverseVar (env:escEnv, d:depth, s:Ab.var): unit = 
		let 
			fun  travVar (Ab.SimpleVar(id, pos)) = (case Symbol.look(env, id) of SOME((dep, esc)) => if d > dep
																									  then esc := true
																									  else ()
																				|NONE            => ())
				|travVar (Ab.SubscriptVar(var, exp, pos)) = (travVar(var); traverseExp(env, d, exp))
				|travVar (Ab.FieldVar(var, sym, pos)) = travVar(var)

		in
			travVar(s)
		end

    and traverseExp (env:escEnv, d:depth, s:Ab.exp): unit = 
    	let 
    		fun  travExp (Ab.OpExp{left, oper, right, pos}) = (travExp(left); travExp(right))
    			|travExp (Ab.IntExp(num)) = ()
				|travExp (Ab.StringExp(str)) = ()
				|travExp (Ab.NilExp) = ()
				|travExp (Ab.BreakExp(pos)) = () 
				|travExp (Ab.SeqExp (explist)) = List.app (fn (exp, pos) => travExp(exp)) explist
				|travExp (Ab.AssignExp{var, exp, pos}) = (traverseVar(env, d, var); travExp(exp)) 
				|travExp (Ab.IfExp{test, then', else'=SOME(e), pos}) = (travExp(test); travExp(then'); travExp(e))
				|travExp (Ab.IfExp{test, then', else'=NONE, pos}) = (travExp(test); travExp(then'))
				|travExp (Ab.WhileExp{test, body, pos}) = (travExp(test); travExp(body))
				|travExp (Ab.CallExp{func, args, pos}) = List.app travExp args
				|travExp (Ab.ArrayExp{typ=typ, size=siz, init=initial, pos=pos}) = (travExp(siz); travExp(initial))
				|travExp (Ab.VarExp(var)) = traverseVar(env, d, var)
				|travExp (Ab.RecordExp{fields=f, typ=typ, pos=pos}) = List.app (fn (sym, exp, pos) => travExp(exp)) f

				|travExp (Ab.LetExp{decs, body, pos}) = traverseExp( traverseDecs(env, d, decs) , d , body )
				|travExp (Ab.ForExp{var, escape, lo, hi, body, pos}) = (traverseExp( Symbol.enter(env, var, (d, escape)) , d , body ); 
																	   travExp(lo); 
																	   travExp(hi) )

    	in
    		travExp(s)
    	end		



    and traverseDecs(env, d, s: Ab.dec list): escEnv = 
    	let fun processDec (Ab.TypeDec(declist), env) = env

    		   |processDec (Ab.VarDec{name, typ, init, escape, pos}, env) = Symbol.enter(env, name, (d, escape))

    		   |processDec (Ab.FunctionDec(fundeclist), env) = 
    		   		let fun oneFunction({name, params, result, body, pos}) = 
    		   				traverseExp( foldl (fn ({name, escape, typ, pos}, env) => Symbol.enter(env, name, (d+1, escape))) env params  ,  d+1, body)

    		   		in 
    		   			(List.app oneFunction fundeclist; env)
    		   		end

    	in
    		foldl processDec env s
    	end


    fun findEscape(prog: Ab.exp) : unit = traverseExp (Symbol.empty, 0, prog)

end


