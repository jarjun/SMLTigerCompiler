lex:
	sml sources.cm

test:
	echo 'Parse.parse "../book-code/testcases/test$(firstword $(filter-out $@,$(MAKECMDGOALS))).tig";' | sml sources.cm

custom:
	echo 'Parse.parse "../custom-testcases/test$(firstword $(filter-out $@,$(MAKECMDGOALS))).tig";' | sml sources.cm
