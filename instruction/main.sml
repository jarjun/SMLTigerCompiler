structure Main = struct

   structure Tr = Translate
   structure F = Frame
(*   structure R = RegAlloc*)

  fun getsome (SOME x) = x

   fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit " ^ Symbol.name(Frame.name frame) ^ "\n")
(*         val _ = Printtree.printtree(out,body); *)
	       val stms = Canon.linearize body

(*         val _ = TextIO.output(out, "BEFORE TRACE\n")
         
         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)

         (*val _ = TextIO.output(out, "AFTER TRACE\n")*)
         val _ = (app (fn s => Printtree.printtree(out,s)) stms'; TextIO.output(out, "\n"))

      	 val instrs =   List.concat(map (MipsGen.codegen frame) stms') 

         val format0 = Assem.format(MipsFrame.regToString)
      in  app (fn i => TextIO.output(out,format0 i)) instrs;
          TextIO.output(out, "---------------------\n")

     end

    | emitproc out (F.STRING(lab,s)) = TextIO.output(out,(Symbol.name(lab) ^ ":" ^ s ^ "\n"))

   fun withOpenFile fname f = 
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out) 
	    handle e => (TextIO.closeOut out; raise e)
       end 

   fun compile filename = 
       let val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
        in 
            withOpenFile (filename ^ ".s") 
	     (fn out => (app (emitproc out) frags))
       end

end



