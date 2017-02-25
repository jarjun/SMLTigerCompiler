parse:
	echo 'Main.compile "$(file)"; PrintAbsyn.print(TextIO.openOut("result.txt"), it);' | sml sources.cm
loop:
	echo 'Parse.parse "../book-code/testcases/test$(num).tig";' | sml sources.cm

test:
	echo 'Parse.parse "../book-code/testcases/test$(firstword $(filter-out $@,$(MAKECMDGOALS))).tig"; PrintAbsyn.print(TextIO.openOut("result.txt"), it);' | sml sources.cm

custom:
	echo 'Parse.parse "../custom-testcases/test$(firstword $(filter-out $@,$(MAKECMDGOALS))).tig"; PrintAbsyn.print(TextIO.openOut("result.txt"), it);' | sml sources.cm

semant: 
	echo 'Main.compile "$(file)";' | sml sources.cm
