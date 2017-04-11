Chris Bernt (ctb25)
Arjun Jain (aj169)
Wesley Valentine (wsv)

How to run: 
To run either compile the group with 'sml sources.cm' then in the sml terminal run 'Main.compile "$filename"' inserting the test file into $filename
or use the makefile and run 'make liveness file=$filename' where $filename is the test file (without quotes in this case).
There is currently no result output. 

To view the console output of the flowgraph you can uncomment line 126 in makegraph. 

To view console output of the live-in and live-out sets uncomment line 172. 

To view console output for the interference graph, uncomment line 174.

To view console output for the movelist, uncomment line 176.

Notes:
We did not deal with the case of a move and an interference edge between two nodes. It is out understanding that this will be dealt with in the register allocation stage, where the interference edge will override the move edge. 
