module Testing where
import Scopes
import ToBlock
import Interface
import qualified Core as C

treeT1 = C.OpenFuncao (C.DefFuncao (C.Name "main") C.NilIts
            (C.ConsIts (C.Decl "d" (C.Const 100))
            (C.ConsIts (C.Decl "x" (C.Add (C.Var "a") (C.Var "x"))) C.NilIts)))

treeT2 = C.OpenFuncao (C.DefFuncao (C.Name "main") C.NilIts
            (C.ConsIts (C.Decl "a" (C.Const 100))
            (C.ConsIts (C.Decl "x" (C.Var "a")) C.NilIts)))

treeT3 = C.OpenFuncao (C.DefFuncao (C.Name "count25") C.NilIts
            (C.ConsIts (C.Decl "counter" (C.Const 0))
            (C.ConsIts (C.NestedWhile (C.While ((C.Less (C.Var "counter1") (C.Const 5)))
                                    (C.ConsIts (C.Increment (C.Var "counter3"))
                                    C.NilIts)))
            C.NilIts)))

treeT4 = C.OpenFuncao (C.DefFuncao (C.Name "count25") C.NilIts
            (C.ConsIts (C.Decl "counter" (C.Const 0))
            (C.ConsIts (C.NestedIf (C.If ((C.Less (C.Var "counter2") (C.Const 5)))
                                    (C.ConsIts (C.NestedReturn (C.Return (C.Var "counter")))
                                    C.NilIts)))
            C.NilIts)))

treeT5 = C.OpenFuncao (C.DefFuncao (C.Name "count25") C.NilIts
            (C.ConsIts (C.NestedLet (C.Let (C.ConsIts (C.Decl "b" (C.Const 100)) C.NilIts)
                                ((C.Inc (C.Var "a")))))
            C.NilIts))

treeT6 = C.OpenFuncao (C.DefFuncao (C.Name "count25") (C.ConsIts (C.Arg (C.Var "counter")) C.NilIts)
            ((C.ConsIts (C.NestedFuncao (C.Funcao (C.Name "count") (C.ConsIts (C.Arg (C.Var "counter2")) C.NilIts)))
            C.NilIts)))

treeT7 = C.OpenFuncao (C.DefFuncao (C.Name "count25") (C.ConsIts (C.Arg (C.Var "counter")) C.NilIts)
            (C.ConsIts (C.Increment (C.Var "counter"))
            (C.ConsIts (C.NestedIf (C.If (C.Less (C.Var "counter2") (C.Const 5))
                                    (C.ConsIts (C.NestedFuncao (C.Funcao (C.Name "count254") (C.ConsIts (C.Arg (C.Var "counter")) C.NilIts)))
                                    C.NilIts)))
            C.NilIts)))

treeT8 = C.OpenFuncao (C.DefFuncao (C.Name "main") C.NilIts
            (C.ConsIts (C.Decl "a" (C.Const 100))
            (C.ConsIts (C.Decl "b" (C.Const 50))
            (C.ConsIts (C.NestedIf (C.If (C.Equals (C.Var "c") (C.Var "d"))
                                    (C.ConsIts (C.NestedReturn (C.Return (C.Var "a"))) C.NilIts)))
            (C.ConsIts (C.NestedWhile (C.While (C.Less (C.Var "e") (C.Const 200)) (C.ConsIts (C.Increment (C.Var "x")) C.NilIts)))
            C.NilIts)))))
                       
-- Test to check if the two aproaches give the same results
trees = [treeT1,treeT2,treeT3,treeT4,treeT5,treeT6,treeT7,treeT8]
test_same [] = []
test_same (h:t) = ((main h == main' h) && (main' h == (main'' h))) : test_same t