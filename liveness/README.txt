Chris Bernt (ctb25)
Arjun Jain (aj169)
Wesley Valentine (wsv)

How to run: 
To run either compile the group with 'sml sources.cm' then in the sml terminal run 'Main.compile "$filename"' inserting the test file into $filename
or use the makefile and run 'make instruction file=$filename' where $filename is the test file (without quotes in this case).
The result will be a ".s" file that is put in the same directory as the test file containing the infinite register MIPS.

Of note, we added labels into instructions before a function body, as well as a jr $ra instruction to the end of each function body. We also store the $ra before making a function call, and restore the $ra immediately after returning from a function call. We note these additions only because they weren't explicity required by these chapters in the book. They are required for the 3 procEntryExit functions as described in the book, but we decided to add them now.
