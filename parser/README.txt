Chris Bernt (ctb25)
Arjun Jain (aj169)
Wesley Valentine (wsv)

How to run: 

The majority of our parser follows the specifications described in the book. Every program resolves to a single expression. Within a program, anything can be described as a declaration (Type, Variable, Function), or an expression. Expressions consist of arithmetic, let/if/while structures, function calls, creating arrays, etc. We implemented the abstract syntax using the Absyn structure provided.

We have 3 Shift/Reduce conflicts. First is the same conflict described on page 82 of the book. This is the conflict between 'variable[expression]' and 'type-id [expression] of expression'. It is not harmful because there is no case where there is an array subcript access followed by the 'of' keyword. We always want to prioritize the array creation in this conflict, so the default shift resolution works fine.

The second and third conflicts arise from the necessity to group consecutive function or type declarations into a single list, in case of mutual recursion. It is not harmful, because if we see multiple function/type declarations in a row, then we always want to shift them all into a list before reducing. In that case, the default shift behaves as desired. 
