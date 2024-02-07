{-Module that contains the functions of the parser-}
module Tools where
import Parser
import Data.Char
import Opt
import Prelude hiding ((<*>),(<$>))
import Core
import Reverse
{-------------------------------------------------------------
*********************** Expressions **************************
--------------------------------------------------------------}
pExp :: Parser Exp
pExp = a  <$> pTermo <*> symbol' '+' <*> symbol' '+'
    <|> b  <$> pTermo <*> symbol' '-' <*> symbol' '-'
    <|> f  <$> pTermo <*> symbol' '+' <*> pExp
    <|> id <$> pTermo
    <|> g  <$> pTermo <*> symbol' '-' <*> pExp
    <|> x  <$> pTermo <*> symbol' '>' <*> pExp
    <|> y  <$> pTermo <*> symbol' '<' <*> pExp
    <|> z  <$> pTermo <*> symbol' '=' <*> symbol' '=' <*> pExp
    <|> t  <$> pTermo <*> symbol' '&' <*> symbol' '&' <*> pExp
    <|> v  <$> pTermo <*> symbol' '|' <*> symbol' '|' <*> pExp
    <|> h  <$> symbol' '!' <*> pExp
    <|> l  <$> pTermo <*> symbol' '>' <*> symbol' '=' <*> pExp
    <|> k  <$> pTermo <*> symbol' '<' <*> symbol' '=' <*> pExp
    <|> r  <$> token' "return" <*> pExp
    <|> tr  <$> token' "True" 
    <|> fl  <$> token' "False" 
    where f a _ c = Add a c
          g a _ c = Sub a c
          x a _ c = Greater a c
          y a _ c = Less a c
          z a _ _ c = Equals a c
          t a _ _ c = And a c
          v a _ _ c = Or a c
          h _ c = Not c
          l a _ _ c = GTE a c
          k a _ _ c = LTE a c
          a x _ _ = Inc x
          b x _ _ = Dec x
          r _ a   = Return a
          tr a = Bool True
          fl a = Bool False
    
pTermo :: Parser Exp
pTermo =  f  <$> pFactor <*> symbol' '*' <*> pTermo
      <|> id <$> pFactor
      <|> g  <$> pFactor <*> symbol' '/' <*> pTermo
      <|> h <$> pFactor <*> symbol' '+' <*> symbol' '+'
    where f a _ c = Mul a c
          g a _ c = Div a c
          h a _ _ = Inc a

pFactor :: Parser Exp
pFactor =  f   <$> number
       <|> Var <$> ident
       <|> g   <$> enclosedBy (symbol' '(')
                              pExp
                              (symbol' ')')
       <|> j   <$> enclosedBy (symbol' '(')
                              pTermo
                              (symbol' ')')
       where f a = Const (read a)
             g a = a
             j a = a
             
{-------------------------------------------------------------
************************** Let *******************************
--------------------------------------------------------------}
pLet = f <$> token' "let" <*> symbol' '{' <*> pItems 
                          <*> symbol' '}' <*> token' "in"
                          <*> pExp
       where f l a i f _ e = Let i (optExp e) 

pId =  f <$>  satisfy' (isLower)
   <|> g <$>   satisfy' isLower <*> pId
   where f a = [a]
         g a b = a : b

pInt =  f <$>   satisfy' isDigit
    <|> g <$>   satisfy' isDigit <*> pInt
    where f a = [a]
          g a b = a:b

{-------------------------------------------------------------
************************ While *******************************
--------------------------------------------------------------}
pWhile = f <$> token' "while" <*> symbol' '(' <*> pExp 
                              <*> symbol' ')'
                              <*> symbol' '{' <*> pItems
                              <*> symbol' '}'
       where f _ _ a _ _ g _ = While (optExp a) g 

{-------------------------------------------------------------
*************************** If *******************************
--------------------------------------------------------------}
pIf = f <$> token' "if" <*> symbol' '(' <*> pExp 
                        <*> symbol' ')'
                        <*> symbol' '{'
                        <*> pItems
                        <*> symbol' '}'
      <|> g <$> token' "if" <*> symbol' '(' <*> pExp 
                        <*> symbol' ')'
                        <*> symbol' '{'
                        <*> pItems
                        <*> symbol' '}'
                        <*> token' "else"
                        <*> symbol' '{'
                        <*> pItems
                        <*> symbol' '}'
        where f _ _ a _ _ b _ = If (optExp a) b
              g _ _ a _ _ b _ _ _ c _ = Else a b c      

{-------------------------------------------------------------
************************ Functions ***************************
--------------------------------------------------------------}
pFuncao = f <$> token' "def" <*> pName <*> symbol' '(' 
                             <*> pArgs
                             <*> symbol' ')'
                             <*> symbol' '{'
                             <*> pItems
                             <*> symbol' '}'
         <|> g <$> token' "func" <*> pName <*> symbol' '(' 
                             <*> pArgs
                             <*> symbol' ')'
       where f _ b _ a _ _ c _ = DefFuncao b a c
             g _ b _ a _ = Funcao b a

pName = f <$> ident
        where f a = Name a

pArgs =         succeed []
      <|>  f <$> pArg <*> symbol' ',' <*> pArgs
      <|>  g <$> pArg
    where f a _ c = a:c
          g a = [a]

pArg =  f <$>  pExp
    <|> g <$> pFuncao
     where  f a = Arg (optExp a)
            g a = NestedFuncao a

pReturn = f <$> token' "return" <*> pExp
        where f _ a = Return a

{-------------------------------------------------------------
************************ Items *******************************
--------------------------------------------------------------}
pItems =         succeed []
      <|>  f <$> pItem <*> symbol' ';' <*> pItems
      <|>  g <$> pItem
    where f a _ c = a:c
          g a = [a]

pItem =    f   <$> ident <*> symbol' '=' <*> pExp
     <|>   h <$> pExp <*> symbol' '+' <*> symbol' '+'
     <|>   k <$> pExp <*> symbol' '-' <*> symbol' '-'
     <|>   x <$> pLet
     <|>   g <$> pWhile
     <|>   j <$> pIf
     <|>   p <$> pFuncao
     <|>   r <$> pReturn
     where  f a _ c = Decl a (optExp c)
            h a _ _ = Increment a 
            k a _ _ = Decrement a
            g c     = NestedWhile c
            j c     = NestedIf c
            x a     = NestedLet a
            p a     = NestedFuncao a
            r a     = NestedReturn a

{------------------------------------------------------------------------------------------------
********************************************* Main **********************************************
- Example of how to run: 
pParse "def main(args){a=100;x=0;while(a>b){x++;if(!a>b){a--;}};let{a=a+b;}in c;func coco();}"
*************************************************************************************************
-------------------------------------------------------------------------------------------------}
pParse =   f <$> pIf
      <|> g <$> pWhile
      <|> h <$> pFuncao
      <|> j <$> pLet
    where f a = OpenIf a
          g a = OpenWhile a 
          h a = OpenFuncao a
          j a = OpenLet a

