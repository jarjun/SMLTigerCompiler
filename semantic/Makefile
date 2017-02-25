semant: 
	echo 'Main.compile "$(file)";' | sml sources.cm

loop:
	echo 'Main.compile "../book-code/testcases/test$(num).tig";' | sml sources.cm

test:
	echo 'Main.compile "../book-code/testcases/test$(firstword $(filter-out $@,$(MAKECMDGOALS))).tig";' | sml sources.cm

custom:
	echo 'Main.compile "../custom-testcases/test$(firstword $(filter-out $@,$(MAKECMDGOALS))).tig";' | sml sources.cm
