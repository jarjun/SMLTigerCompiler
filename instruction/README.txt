Chris Bernt (ctb25)
Arjun Jain (aj169)
Wesley Valentine (wsv)

How to run: 
To run either compile the group with 'sml sources.cm' then in the sml terminal run 'Main.compile "$filename"' inserting the test file into $filename
or use the makefile and run 'make semant file=$filename' where $filename is the test file (without quotes in this case).
The result will be a ".s" file that is put in the same directory as the test file containing both the generated IR and infinite register MIPS.
