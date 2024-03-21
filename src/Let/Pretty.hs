module Let.Pretty where

import Let.Shared
import PP.Memo.Interface


pretty :: Root -> IO ()
pretty (Root l) = renderAll (prettyLet l) 1000

displ :: Root -> String
displ (Root l) = disp (prettyLet l) 1000


prettyLet :: Let -> PP_Doc
prettyLet (Let (Assign s exp l) e) = text "let" >#< text s >#< text "=" >#< (prettyExp exp) >-< (indent 4 (prettyList l)) >-< text "in" >#< prettyExp e
prettyLet (Let (NestedLet s l r) e) = text "let" >#< text s >#< text "=" >#< (prettyLet l) >-< (indent 4 (prettyList r)) >-< text "in" >#< prettyExp e
prettyLet (Let EmptyList _) = empty

prettyList :: List -> PP_Doc
prettyList (Assign s e l) = text s >#< text "=" >#< prettyExp e >-< prettyList l
prettyList (NestedLet s l r) = text s >#< text "=" >#< prettyLet l >-< prettyList r
prettyList EmptyList = empty


prettyExp :: Exp -> PP_Doc
prettyExp (Add a b) = prettyExp a >#< text "+" >#< prettyExp b
prettyExp (Sub a b) = prettyExp a >#< text "-" >#< prettyExp b
prettyExp (Neg a) = text " -(" >|< prettyExp a >|< text ")"
prettyExp (Var a) = text a
prettyExp (Const a) = text (show a)