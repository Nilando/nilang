/*
def convert CFG<BasicBlock> => CFG<SSABlock>
    perform a liveness analysis on the cfg

    for each block a
        for each block b in the dominance frontier of a
            if a defines var v AND v is live on entry to b
                insert a phi node for v at b

    apply versioning to the variables for each block
*/

