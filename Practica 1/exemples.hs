-- Per RString

-- Podeu afegir aquestes definicions al vostre codi o fer directament
-- la crida al readRStringSystem després de carregar el vostre
-- programa amb ghci per fer proves. També podeu usar el let per fer
-- les definicions per linia de comandes del ghci.

myStringSignature :: Signature
let myStringSignature = [("b",0),("w",0),("r",0)]

let myStringSystem = readRStringSystem [("wb" , "bw" ),("rb" , "br" ),("rw" , "wr" )]

let myRString = readRString "wrrbbwrrwwbbwrbrbww"



-- apply a (Subtitution [(b,c)])

-- Si heu posat les definicions, podeu fer aquestes crides
-- despres de carregar amb ghci

-- > validRewriteSystem myStringSignature myStringSystem
-- > True

-- > rewrite myStringSystem myRString leftmost
-- > "bbbbbbwwwwwwwrrrrrr"


-------------------------------------------------------------------------
-- Per RTerm

-- Podeu afegir aquestes definicions al vostre codi o fer directament la
-- crida al readRTermSystem després de carregar el vostre programa
-- amb ghci per fer proves. També podeu usar el let per fer
-- les definicions per linia de comandes del ghci.

myTermSignature :: Signature
myTermSignature = [("len",1),("con",2),("emp",0),("list1",0)]

trs1 = readRTermSystem [
  ([("len",1),("emp",0)] , [("0",-2)]),
  ([("len",1),("con",2),("x",-1),("xs",-1)] , [("+",2),("1",-2),("len",1),("xs",-1)]),
  ([("list1",0)] , [("con",2),("1",-2),("emp",0)])
  ]

term1 = readRTree [("+",2),("len",1),("con",2),("a",0),("con",2),("a",0),("emp",0),("len",1),("emp",0)]
term2 = readRTree [("+",2),("len",1),("con",2),("a",0),("con",2),("a",0),("emp",0),("len",1),("list1",0)]


-- Si heu posat les definicions, podeu fer aquestes crides
-- despres de carregar amb ghci

-- > validRewriteSystem myTermSignature trs1
-- > True

-- >rewrite trs1 term1 parallelinnermost
-- >2

-- >rewrite trs1 term2 parallelinnermost
-- >3

-- >oneStepRewrite trs1 term1 parallelinnermost
-- >+( +( 1 , len( con( a , emp ) ) ) , 0 )

-- >oneStepRewrite trs1 term2 parallelinnermost
-- >+( +( 1 , len( con( a , emp ) ) ) , len( con( 1 , emp ) ) )
