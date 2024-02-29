module Examples.AG.Let.Let_Opt_Strategic where

import Data.Maybe (fromJust)
import Language.Grammars.ZipperAG
import Data.Generics.Zipper
import Data.Generics.Aliases
import Library.Ztrategic
import Examples.AG.Let.Let_Zippers hiding (semantics)

import Library.StrategicData (StrategicData(..), isJust)

instance StrategicData Let where 
  isTerminal z = isJust (getHole z :: Maybe Name     )
              || isJust (getHole z :: Maybe Int      )

instance StrategicData Root where 
  isTerminal z = isJust (getHole z :: Maybe Name     )
              || isJust (getHole z :: Maybe Int      )

{-
p = let a = b + 0
        c = 2
        b = let c = 3 in c + c
    in a + 7 − c
-}

-- defined in page 3
p :: Let
p =  Let  (Assign     "a"  (Add (Var "b") (Const 0))
          (Assign     "c"  (Const 2)
          (NestedLet  "b"  (Let  (Assign "c" (Const 3)
                                 EmptyList)
                                 (Add (Var "c") (Var "c")))
          EmptyList)))
          (Sub (Add (Var "a") (Const 7)) (Var "c"))


-- --------
-- --
-- - Introductory Examples
-- --
-- --------

-- defined in page 3
expr_short :: Exp -> Maybe Exp
expr_short (Add e (Const 0))  = Just e 
expr_short (Add (Const 0) e)  = Just e  
expr_short _                  = Nothing

-- defined in page 4
opt_short :: Zipper Let -> Maybe (Zipper Let)
opt_short  t = applyTP (full_tdTP step) t
     where step = idTP `adhocTP` expr_short


-- we only introduce some of the concepts used in this line of code later
run_example_short = fromZipper $ fromJust $ opt_short $ toZipper p

-- --------
-- --
-- - Zipper Examples
-- --
-- --------

-- defined in page 5
t_1 = toZipper p 

-- We do not use these examples but we keep them for the sake of completeness
{-
incrC :: Maybe (Zipper Let) 
incrC = do  t_2 <- down'  t_1
            t_3 <- down'  t_2
            t_4 <- right  t_3
            t_5 <- right  t_4
            t_6 <- down'  t_5
            t_7 <- right  t_6
            return (trans incConstantG t_7)

incConstantG :: GenericT
incConstantG = mkT incConstant

incConstant :: Exp -> Exp
incConstant (Const n) = Const (n+1)

sumBZero' :: Maybe Exp
sumBZero' =  do  t_2 <- down'  t_1
                 t_3 <- down'  t_2 
                 t_4 <- right  t_3
                 getHole t_4 
-}

-- defined in page 5
sumBZero :: Maybe Exp
sumBZero = (getHole  .  fromJust . right 
                     .  fromJust . down'
                     .  fromJust . down') t_1


-- --------
-- --
-- - Stategy Example TP
-- --
-- --------

-- defined in page 7
expr :: Exp -> Maybe Exp
expr (Add e (Const 0))          = Just e 
expr (Add (Const 0) t)          = Just t  
expr (Add (Const a) (Const b))  = Just (Const (a+b)) 
expr (Sub a b)                  = Just (Add a (Neg b))    
expr (Neg (Neg f))              = Just f           
expr (Neg (Const n))            = Just (Const (-n)) 
expr _                          = Nothing

-- defined in page 8
opt'  :: Zipper Let -> Maybe (Zipper Let)
opt'  t =  applyTP (innermost step) t
      where step = failTP `adhocTP` expr

run_example_TP = fromZipper $ fromJust $ opt' t_1

-- --------
-- --
-- - Stategy Example TU
-- --
-- --------

-- defined in page 8
select :: List -> [String]
select (Assign    s _ _)  = [s]
select (NestedLet s _ _)  = [s]
select _                  = []

-- defined in page 9
names  :: Zipper Let -> [String]
names  r = applyTU (full_tdTU step) r 
       where step = failTU `adhocTU` select

run_example_TU = names t_1

-- We assert that "names t1 ≡ ["a", "c", "b", "c"] (a bottom-up strategy produces the reverse of this list)"
-- We showcase this with these two test_ examples
test_example_TU  = names t_1 == ["a", "c", "b", "c"]
test_example_TU2 = names t_1 == reverse (namesRev t_1)

-- similar to "names" but traversing bottom-up instead
namesRev  :: Zipper Let -> [String]
namesRev  r = applyTU (full_buTU step) r 
       where step = failTU `adhocTU` select

-- --------
-- --
-- - Stategic Attribute Grammars subsection
-- --
-- --------

-- defined in page 13
expC :: Exp -> Zipper Root -> Maybe Exp
expC (Var i) z = expand (i, lev z) (env z)
expC _ z = Nothing 

-- defined in page 13
opt'' :: Zipper Root -> Maybe (Zipper Root)
opt'' r = applyTP (innermost step) r
 where step = failTP `adhocTPZ` expC `adhocTP` expr  

run_example_opt_t_1 = fromZipper $ fromJust $ opt'' $ toZipper $ Root p





-- defined in page 13
decls :: List -> Zipper Root -> [Name]
decls (Assign       s _ _) z = mNBIn (lexeme_Name z, lev z) (dcli z)
decls (NestedLet    s _ _) z = mNBIn (lexeme_Name z, lev z) (dcli z) 
decls _ _                    = []

-- defined in page 13
uses :: Exp -> Zipper Root -> [Name]
uses (Var i) z = mBIn (lexeme_Name z) (env z)
uses _       z = []

-- defined in page 13
errors :: Zipper Root -> [Name]
errors t = applyTU (full_tdTU step) t
 where step = failTU `adhocTUZ` uses `adhocTUZ` decls

semantics :: Root -> Errors
semantics p = errors (mkAG p)

run_example_errors_t_1 = semantics $ Root p






 
-- --------
-- --
-- - Other Examples
-- --
-- --------

opt''_BU :: Zipper Root -> Maybe (Zipper Root)
opt''_BU r = applyTP (full_buTP step) r
 where step = failTP `adhocTPZ` expC `adhocTP` expr  

run_example_opt_t_1_BU    = fromZipper $ fromJust $ opt''_BU $ toZipper $ Root p
run_example_opt_t_1_Inner = run_example_opt_t_1




-- In this example, we simplify several expressions by applying the aforementioned rules
{-
let a = b + 0     
           c = 2
           b = c + 3 - c
       in (a + 7) - c  
-}
letSimplifiable =  Root (Let ( Assign "a" (Add (Var "b") (Const (0)))
                             (Assign "c" (Const 2)
                             (Assign "b" (Sub ((Add (Var "c") (Const 3))) (Var "c"))
                   EmptyList)))
                             (Sub (Add (Var "a") (Const 7)) (Var "c")))

run_example_opt_Inner = fromZipper $ fromJust $ opt'' $ toZipper letSimplifiable
run_example_opt_BU = fromZipper $ fromJust $ opt''_BU $ toZipper letSimplifiable




-- In this example, we declare "c" twice, and we use undeclared values "b" and "z". 
{-
let a = b + 3
    c = 2
    w = let c = a + b
        in c + z
    c = c + 3 + c
 in a + 7 + c + w
-}

letWithErrors = Root (Let ( Assign "a" (Add (Var "b") (Const 3))
                          $ Assign "c" (Const 2)
                          $ NestedLet "w" (Let (Assign "c" (Add (Var "a") (Var "b"))
                                               EmptyList)
                                               (Add (Var "c") (Var "z")))
                          $ Assign "c" (Add (Add (Var "c") (Const 3)) (Var "c"))
                            EmptyList)
                          (Add (Add (Add (Var "a") (Const 7)) (Var "c")) (Var "w")))

run_example_errors = semantics letWithErrors