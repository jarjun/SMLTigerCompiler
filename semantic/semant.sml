structure A = Absyn

signature SEMANT = 
sig
	val transProg: A.exp -> unit
end

structure Semant :> SEMANT = 
struct
	type ty = Types.ty
	type venv = Env.enventry Symbol.table
	type tenv = ty Symbol.table

	fun transExp(venv, tenv, exp) = ()

	fun transProg(expr) = transExp(Env.base_venv, Env.base_tenv, expr)

end
