How we handled comments:

We handled comments using a comment state that started when the lexer saw a '/*' expression. 
Then the variable commentOpen tracks the line at which the last unclosed comment started so that if a comment is unclosed we can throw the right error message.
The variable commentDepth increments everytime there is a '/*' expression and decrements whenver there is a '*/' expression. Upon reaching zero, we enter back into the INITIAL state.
If the EOF is read and the commentDepth is not 0, we throw an error at the place of commentOpen. We also made sure to keep incrementing the linenum counter.


How we handled strings:

We started a string state when we saw '"' and set stringOpen to that location and started keeping track of characters in the string with a stringInProgress.
We threw errors on illegal escape sequences and nonprintable characters including newlines.
We allowed the different control sequences and ascii values if they were formatted correctly. 
For the the formatting we made a new formatting state that was entered when there was a '\' followed by a valid formatting character.
In this formatting state we kept track of linenum correctly and closed it when there was another '\' seen and went back to string.
If there was a stringOpen (it was not equal to ~1) then we returned an error upon seeing EOF.


Error Handling:

Beyond the comment and string errors, we threw an error on illegal characters.

EOF handling:

The function eof is called at the end of the file once we see an EOF character and as discussed is where we check for unclosed strings and comments.