type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val stringInProgress = ref ""
val stringOpen = ref ~1 (* ?!?! *)
val commentOpen = ref ~1
val commentDepth = ref 0
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let 
				val pos = hd(!linePos)
				fun checkUnclosedStringAtEOF () = 
					if !stringOpen <> ~1
					then ErrorMsg.error (!stringOpen) (" unclosed string")
					else ()

				fun checkUnclosedCommentAtEOF () = 
					if !commentDepth <> 0
					then ErrorMsg.error (!commentOpen) (" unclosed comment")
					else ()

			in 
				checkUnclosedStringAtEOF();
				checkUnclosedCommentAtEOF();
				Tokens.EOF(pos,pos) 
			end


%% 
%s COMMENT STRING FORMATTING;
%%

<INITIAL> " "|\t => (continue());



<INITIAL> "/*" => (YYBEGIN COMMENT; 
				   commentDepth := !commentDepth + 1;
				   commentOpen := yypos;
				   continue());

<COMMENT> "/*" => (commentDepth := !commentDepth + 1;
				   continue());

<COMMENT> "*/" => (if !commentDepth = 1 then YYBEGIN INITIAL else ();
				   commentDepth := !commentDepth - 1;
				   continue());

<COMMENT> . => (continue());




<INITIAL> \" => (YYBEGIN STRING;
				stringInProgress:= "";
				stringOpen := yypos;				
				continue());

<STRING> \" => (YYBEGIN INITIAL;
				let val tempStringOpen = !stringOpen 
				in
					stringOpen := ~1;
					Tokens.STRING(!stringInProgress, tempStringOpen, yypos+1)
				end);


<STRING> \\\\ => (stringInProgress := !stringInProgress ^ "\\"; continue());
<STRING> \\\" => (stringInProgress := !stringInProgress ^ "\""; continue());
<STRING> \\n  => (stringInProgress := !stringInProgress ^ "\n"; continue());
<STRING> \\t  => (stringInProgress := !stringInProgress ^ "\t"; continue());

<STRING> \\[0-1][0-9][0-9]|\\2[0-4][0-9]|\\25[0-5]=> (stringInProgress := !stringInProgress ^ 
								String.str(chr(valOf(Int.fromString(String.substring(yytext,1,3))))); continue());


<STRING> \\\^[@-_] => (stringInProgress := !stringInProgress ^
						String.str(chr(ord(String.sub(yytext,2))-64)); continue());



<STRING> \\[ \t\n\r\f] => (YYBEGIN FORMATTING;
							if(String.substring(yytext,1,1) = "\n" orelse 
							   String.substring(yytext,1,1) = "\r")

								then (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue())
								else continue()


						   );

<FORMATTING> [\n\r] => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<FORMATTING> [ \t\f] => (continue());
<FORMATTING> \\ => (YYBEGIN STRING; continue());
<FORMATTING> . => (ErrorMsg.error yypos (" invalid character " ^ yytext ^ " in formatting block"); continue());

<STRING> \\ => (ErrorMsg.error yypos (" invalid escape sequence"); continue());

<STRING> [ -~] => (stringInProgress := !stringInProgress ^ yytext; continue());

<STRING> . => (ErrorMsg.error yypos (" non printable character in string"); continue());

<STRING> \n|\r  => (stringInProgress := !stringInProgress ^ yytext; lineNum := !lineNum+1; linePos := yypos :: !linePos; 
					ErrorMsg.error yypos (" cannot have newline in string"); continue());


\n|\r  => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());


<INITIAL> while    => (Tokens.WHILE(yypos,yypos+size yytext));
<INITIAL> for      => (Tokens.FOR(yypos,yypos+size yytext));
<INITIAL> to       => (Tokens.TO(yypos,yypos+size yytext));
<INITIAL> break    => (Tokens.BREAK(yypos,yypos+size yytext));
<INITIAL> let      => (Tokens.LET(yypos,yypos+size yytext));
<INITIAL> in       => (Tokens.IN(yypos,yypos+size yytext));
<INITIAL> end      => (Tokens.END(yypos,yypos+size yytext));
<INITIAL> function => (Tokens.FUNCTION(yypos,yypos+size yytext));
<INITIAL> var  	   => (Tokens.VAR(yypos,yypos+size yytext));
<INITIAL> type     => (Tokens.TYPE(yypos,yypos+size yytext));
<INITIAL> array    => (Tokens.ARRAY(yypos,yypos+size yytext));
<INITIAL> if       => (Tokens.IF(yypos,yypos+size yytext));
<INITIAL> then     => (Tokens.THEN(yypos,yypos+size yytext));
<INITIAL> else     => (Tokens.ELSE(yypos,yypos+size yytext));
<INITIAL> do       => (Tokens.DO(yypos,yypos+size yytext));
<INITIAL> of       => (Tokens.OF(yypos,yypos+size yytext));
<INITIAL> nil      => (Tokens.NIL(yypos,yypos+size yytext));


<INITIAL> ","	=> (Tokens.COMMA(yypos,yypos+size yytext));
<INITIAL> ":"	=> (Tokens.COLON(yypos,yypos+size yytext));
<INITIAL> ";"	=> (Tokens.SEMICOLON(yypos,yypos+size yytext));
<INITIAL> "("	=> (Tokens.LPAREN(yypos,yypos+size yytext));
<INITIAL> ")"	=> (Tokens.RPAREN(yypos,yypos+size yytext));
<INITIAL> "["	=> (Tokens.LBRACK(yypos,yypos+size yytext));
<INITIAL> "]"	=> (Tokens.RBRACK(yypos,yypos+size yytext));
<INITIAL> "{"	=> (Tokens.LBRACE(yypos,yypos+size yytext));
<INITIAL> "}"	=> (Tokens.RBRACE(yypos,yypos+size yytext));
<INITIAL> "."	=> (Tokens.DOT(yypos,yypos+size yytext));
<INITIAL> "+"	=> (Tokens.PLUS(yypos,yypos+size yytext));
<INITIAL> "-"	=> (Tokens.MINUS(yypos,yypos+size yytext));
<INITIAL> "*"	=> (Tokens.TIMES(yypos,yypos+size yytext));
<INITIAL> "/"	=> (Tokens.DIVIDE(yypos,yypos+size yytext));
<INITIAL> "="	=> (Tokens.EQ(yypos,yypos+size yytext));
<INITIAL> "<>"	=> (Tokens.NEQ(yypos,yypos+size yytext));
<INITIAL> "<"	=> (Tokens.LT(yypos,yypos+size yytext));
<INITIAL> "<="	=> (Tokens.LE(yypos,yypos+size yytext));
<INITIAL> ">"	=> (Tokens.GT(yypos,yypos+size yytext));
<INITIAL> ">="	=> (Tokens.GE(yypos,yypos+size yytext));
<INITIAL> "&"	=> (Tokens.AND(yypos,yypos+size yytext));
<INITIAL> "|"	=> (Tokens.OR(yypos,yypos+size yytext));
<INITIAL> ":="	=> (Tokens.ASSIGN(yypos,yypos+size yytext));


<INITIAL> [a-zA-Z][a-zA-Z_0-9]* => (Tokens.ID(yytext, yypos, yypos+size yytext));
<INITIAL> [0-9]+ => (Tokens.INT(valOf(Int.fromString yytext), yypos, yypos+size yytext));


<INITIAL> .       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

