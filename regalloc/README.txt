Chris Bernt (ctb25)
Arjun Jain (aj169)
Wesley Valentine (wsv)

How to run: 
To run either compile the group with 'sml sources.cm' then in the sml terminal run 'Main.compile "$filename"' inserting the test file into $filename
or use the makefile and run 'make regalloc file=$filename' where $filename is the test file (without quotes in this case).
There is currently no result output. The allocation results are apparent in the generated .s MIPS file.


Notes:
We did some partial spilling in that if there are registers used across function calls or not enough registers to allocate with just $t's then it spills over into the $s registers one at a time as needed and liveness analysis and register allocation are rerun. These are saved and restored at the beginning and end of the fragment respectively. The $v and $a registers are treated the same way as $t registers and can be used as temporaries, but not across a function call.