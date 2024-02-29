{-# LANGUAGE DeriveDataTypeable #-}

module Let.Shared where 

import Data.Generics.Zipper
import Data.Data

data Root = Root Let
           deriving (Data, Typeable)

instance Show Root where 
       show (Root l) = show l

data Let = Let List Exp
          deriving (Data, Typeable)

instance Show Let where
       show (Let l e) = "let " ++ aaa ++ "in " ++ show e 
              where a = lines (show l)
                    aa = (head a) : (map ("    " ++) $ tail a)
                    aaa = unlines aa

data  List  
      = NestedLet  Name Let List
      | Assign     Name Exp List
      | EmptyList
           deriving (Data, Typeable)

instance Show List where 
       show (NestedLet s l r)    =  s ++ " = " ++ aaa ++ show r
              where a = lines (show l)
                    aa = (head a) : (map ( take (3 + length s) (repeat ' ') ++) $ tail a)
                    aaa = unlines aa
       show (Assign s e l) =  s ++ " = " ++ show e ++ "\n" ++ show l
       show EmptyList          = ""

data Exp = Add   Exp Exp
         | Sub   Exp Exp
         | Neg   Exp
         | Var   Name
         | Const Int
       deriving (Data, Typeable)

instance Show Exp where 
       show (Add a b) = show a ++ " + " ++ show b
       show (Sub a b) = show a ++ " - " ++ show b
       show (Neg a  ) = " -(" ++ show a ++ ")"
       show (Var a  ) = a
       show (Const a) = show a


type Name   = String
type Env    = [(Name, Int, Maybe Exp)]
type Errors = [Name]

-- --------
-- --
-- - Examples
-- --
-- --------


ex1_ = let a = 1 + 0
           b = 0 + 1
      in a + b

ex1 = Root $ 
      Let ( Assign "a" (Add (Const 1) (Const 0))
        $   Assign "b" (Add (Const 0) (Const 1))
        $  EmptyList)
       $ (Add (Var "a") (Var "b"))  


ex2_ = let a = b - 16
           c = 8
           w = let  z = a + b
               in   z + b   
           b = (c + 3) - c
       in  c + a - w
ex2 = Root $
      Let ( Assign "a" (Add (Var "b") (Const (-16)))
           $ Assign "c" (Const 8)
           $ NestedLet "w" ( Let (Assign "z" (Add (Var "a") (Var "b")) EmptyList)
                            $ (Add (Var "z") (Var "b")))
           $ Assign "b" (Sub ((Add (Var "c") (Const 3))) (Var "c"))
             EmptyList)
      $ (Sub (Add (Var "c") (Var "a")) (Var "w"))

ex3_ = let a = b + 0     
           c = 2
           b = c + 3 - c
       in (a + 7) - c  

ex3 = Let ( Assign "a" (Add (Var "b") (Const (0)))
           (Assign "c" (Const 2)
           (Assign "b" (Sub ((Add (Var "c") (Const 3))) (Var "c"))
      EmptyList)))
       (Sub (Add (Var "a") (Const 7)) (Var "c"))

{-
p :: Let
p =  Let  (Assign     "a"  (Add (Var "b") (Const 0))
          (Assign     "c"  (Const 2)
          (NestedLet  "b"  (Let  (Assign "c" (Const 3)
                                 EmptyList)
                                 (Add (Var "b") (Var "c")))
          EmptyList)))
          (Sub (Add (Var "a") (Const 7)) (Var "c"))
-}

--
{-
ex6_ = let a = b + d    
           c = 2
           b = 3
 	   a = 3
        in (a + 7) - c
-}
ex6' = Let (Assign "a" (Add (Var "b") (Var "d"))
           (Assign "c" (Const 2)
           (Assign "b" (Const 3)
           (Assign "a" (Const 3)
            EmptyList))))
           (Sub (Add (Var "a") (Const 7)) (Var "c"))

ex6 = Root ex6'

{-
ex7 = let a = b + d    
          c = 2
          b = 3
	  z = let d = 100
	          c = 400
	      in  w 
 	  a = 3
        in (a + 7) - c

errors:  [ use d , use w , decl a]

-}


z = Let (Assign "d" (Const 100)
        (Assign "c" (Const 400)
     EmptyList))
    (Var "w")


ex7 = Root $ Let (Assign "a" (Add (Var "b") (Var "d"))
                 (Assign "c" (Const 2)
                 (Assign "b" (Const 3)
         (NestedLet "z" z
                 (Assign "a" (Const 3)
                  EmptyList)))))
                 (Sub (Add (Var "a") (Const 7)) (Var "c"))


{-

let { a = 1 ; b = let { c = a ; a =2 } in c } in b


-}

plev :: Let
plev =  Let  (Assign     "a"  (Const 1)
          (NestedLet  "b"  (Let  (Assign "c" (Var "a")
                                 (Assign "a" (Const 2)
                                 EmptyList))
                                 (Var "c"))
          EmptyList))
          (Var "b")