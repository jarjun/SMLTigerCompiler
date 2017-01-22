lex:
	sml sources.cm

test:
	echo 'Parse.parse "../book-code/testcases/$(firstword $(filter-out $@,$(MAKECMDGOALS))).tig";' | sml sources.cm
