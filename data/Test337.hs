True `nand` True = False
nand _ _         = True
nor False False = True
_ `nor` _       = False
