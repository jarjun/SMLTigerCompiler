structure Main = struct

   structure Tr = Translate
   structure F = Frame
(*   structure R = RegAlloc*)

  fun getsome (SOME x) = x

   fun emitproc out (F.PROC{body,frame}) =
     let (*val _ = print ("emit " ^ Symbol.name(Frame.name frame) ^ "\n")*)
(*         val _ = Printtree.printtree(out,body); *)
         val stms = Canon.linearize body

(*         val _ = TextIO.output(out, "BEFORE TRACE\n")
         
         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)

         (*val _ = TextIO.output(out, "AFTER TRACE\n")*)
         (*val _ = (app (fn s => Printtree.printtree(out,s)) stms'; TextIO.output(out, "\n"))
*)
         val instrs =   List.concat(map (MipsGen.codegen frame) stms') 
         val newInstrs = F.procEntryExit2(frame, instrs)


         val (allocInstrs, alloc) = RegAlloc.alloc(newInstrs, frame)


         (*val newerInstrs = #body(F.procEntryExit3(frame, allocInstrs))*) (* TODO procEntryExit3 before reg alloc or after??? need sink if after *)


         val format1 = Assem.format(MipsFrame.regToString)
         val format0 = Assem.format(  (fn x => case (Temp.Table.look(alloc, x)) of SOME(s) => s
                                                                                  |NONE => (ErrorMsg.error ~1 "cannot allocate"; Temp.makestring(x)))) 
      in  (*app (fn i => TextIO.output(out,format1 i)) allocInstrs;*)
          app (fn i => TextIO.output(out,format0 i)) allocInstrs
          (*TextIO.output(out, "---------------------\n")*)

     end

    | emitproc out (F.STRING(lab,s)) = TextIO.output(out,(".data\n" ^ Symbol.name(lab) ^ 
                                                          ":\n.word " ^ Int.toString(String.size(s)) ^
                                                           "\n.ascii \"" ^ s ^ "\"\n" ^ ".text\n"))

   fun withOpenFile fname f = 
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out) 
      handle e => (TextIO.closeOut out; raise e)
       end 

   fun compile filename = 
       let val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)
           val header = ".text\n.align 2\n\nmain:\nmove $fp, $sp\naddi $sp, $sp, -100\nmove $a0, $fp\njal tig_main\naddi $sp, $sp, 100\nlw $fp, 0($fp)\n\nmove $a0, $v0\nli $v0, 1\nsyscall\n\nli $v0, 10\nsyscall\n\n"
        

          val sysString = TextIO.inputAll (TextIO.openIn "sysspim.s")
          val runtimeString = TextIO.inputAll (TextIO.openIn "runtimele.s")

        in 
            withOpenFile (filename ^ ".s") 
       (fn out =>  ( TextIO.output(out, runtimeString); app (emitproc out) frags; TextIO.output(out, sysString) ))
       end

end





