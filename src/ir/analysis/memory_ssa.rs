/*
==== MEMORY SSA ALGORITHM SUMMARY ====

step 0: Perform GVN pass to obtain a ValueMap
A ValueMap simplifies the work/implementation of the escape analysis, and also
all the followering algorithms can use it to better determine conflicts between
memory accesses.

step 1: Escape Analysis
Determines if a reference to a memory store only exists on the stack. Helpful in
adding a bit of precision in detecting if two stores may alias.

step 2: Memory Liveness
Determine for every block in the CFG what memory accesses are live on entry/exit.

step 3: Insert Memory Phi Nodes
For every block inspect the instructions that would "clobber" or define new values
to a memory access. For every possibly clobbered memory access insert a phi node
for that memory access in the each dominance frontier block if the memory access
  is live on entry.

step 4: Apply Version Tags
Recursively apply versions tags on blocks in the cfg; starting with the entry
block. Keep a stack of versions to iteratively apply to each memory access.
Every time an instruction clobbers an access apply a new version to that access.

step 5: Output
The final output of the algorithm is a map of numbered "Versions" that map to 
instructions that perform a memory access, like store/load. This versioning map
can be used by a subsequent GVN pass perform better optimization by equating same
versioned accesses to be accessing the same value.

==== END ====
*/
