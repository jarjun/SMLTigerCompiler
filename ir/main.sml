structure Main = struct
	fun compile file =
		let val err = ErrorMsg.reset(); 
		in
			Semant.transProg(Parse.parse file)
		end
end
