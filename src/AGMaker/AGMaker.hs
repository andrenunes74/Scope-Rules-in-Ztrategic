{-# LANGUAGE TemplateHaskell #-}
-- :set -XTemplateHaskell
module AGMaker.AGMaker where

import Language.Haskell.TH
import Control.Monad (forM, replicateM, foldM, liftM2)
import Data.List (isPrefixOf, intersperse)
import Data.Data

import Debug.Trace

{-
cenas = do 
    (DataConI name typ parentname) <- reify 'Root 
    runIO $ print name 
    return name
-}

split [] x = []
split l x = a : split d x
 where (a,b) = span (/= x) l
       d = if null b then [] else tail b

stripPrefix :: Name -> String
stripPrefix n = concat $ init $ intersperse "." $ split (show n) '.'

stripName :: Name -> String
stripName n = last $ split (show n) '.'

makeC :: Name -> Name
makeC m = makeCFromStr $ stripName m

makeCFromStr :: String -> Name
makeCFromStr s = mkName $ "C" ++ s


normalizeConstr :: Con -> (Name, [Type])
normalizeConstr (NormalC n lbt) = (n, map snd lbt)
normalizeConstr (RecC n lvbt) = (n, map (\(l,b,t) -> t) lvbt)

-- currently only used to know the name of the next types to recurse into - so we skip higher-order types
-- this means skipping lists, maybes, and possibly functors and monads
typeToName :: Type -> [Name]
typeToName (ConT n)     = [n]
typeToName x@(AppT a b) = typeToName a ++ typeToName b
typeToName ListT        = []
typeToName (TupleT _)   = []
typeToName x            = error $ "Unhandled constructor " ++ show x ++ " in function typeToName"

-- entry point + debug print 
writeAG n = do 
    ags <- makeAG n 
    let s = pprint ags
    runIO $ writeFile "GeneratedAG.hs" $ "{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}\n{-# HLINT ignore \"Redundant lambda\" #-}\n{-# HLINT ignore \"Redundant bracket\" #-}\n{-# HLINT ignore \"Use camelCase\" #-}\n{-# HLINT ignore \"Avoid lambda\" #-}\nmodule GeneratedAG where\n\nimport Data.Data\nimport Data.Generics.Zipper\nimport " ++ init (stripPrefix n) ++ "\n\n" ++ s 
    return ags 


-- entry point
makeAG :: Name -> DecsQ
makeAG n = do
    (pairs, _) <- makePairs n (stripPrefix n) []
    agArg <- newName "ag"
    -- let failureExp = AppE (VarE $ mkName "error") (LitE $ StringL $ "error in constructor")
    failureExp <- defaultFailureExp agArg
    fullExpress <- foldM (\d (c,m) -> makeExpress agArg c m d) failureExp pairs
    let lambd = LamE [VarP agArg] fullExpress
    let constructorDatatype = makeConstructorDatatype pairs
        constructorFunction = FunD (mkName "constructor") [Clause [] (NormalB lambd ) []]
        lexemeFunctions = concat <$> mapM (makeLexeme . fst) pairs
    fmap ([constructorDatatype, constructorFunction] ++) lexemeFunctions

-- since we are using info generated by makePairs, if it works correctly, we should only get DataD and NewtypeD
makeLexeme :: Name -> DecsQ
makeLexeme n = do
    j <- reify n
    let constructorsOfn = case j of
             -- (DataConI name typ parentname) ->  []
             (TyConI dec) -> case dec of
                              DataD cxt name tyvarbdrList mbKind conList derivCList -> conList
                              NewtypeD cxt name tyvarbdrList mbKind con derivCList -> [con]
                              x -> error $ "LexemeError with node:" ++ show x
            --  _ -> []
    fmap concat $ forM constructorsOfn $ \constr -> do -- for each constructor of n
        agArg <- newName "ag"
        let (constName, consTypes) = normalizeConstr constr
            numTypes = length consTypes
        paramArgs <- replicateM numTypes (newName "arg")
        return $ flip map [1..length paramArgs] $ \i -> -- for each parameter i of this constructor
            let lambd = LamE [VarP agArg] express -- \ag -> exp
                express = CaseE caseExp caseMatches -- case caseExp of caseMatches 
                caseExp =  SigE ( AppE (VarE $ mkName "getHole") (VarE agArg) ) (AppT (ConT $ mkName "Maybe") (ConT n))  -- (getHole ag :: Maybe List)
                caseMatches = [correctMatch, errorMatch]
                -- correctMatch = trace (show (constName, paramArgs)) $ Match (ConP (mkName "Just") [] [ConP constName [] (map VarP paramArgs)]) (NormalB (VarE (paramArgs!!(i-1)))) [] -- Just (ConstName a b c) -> a 
                correctMatch = Match (ConP (mkName "Just") [] [ConP constName [] (map VarP paramArgs)]) (NormalB (VarE (paramArgs!!(i-1)))) [] -- Just (ConstName a b c) -> a 
                errorMatch = Match WildP (NormalB $ AppE (VarE $ mkName "error") (LitE $ StringL $ "error in " ++ show (name constName))) []
                name n = mkName $ "lexeme_" ++ stripName n ++ (if i == 1 then "" else "_" ++ show i)
            in FunD (name constName) [Clause [] (NormalB lambd) []]


type WorkDoneAccum = [String]

-- current = name of constructor to be generated now
-- prefix = prefix of module (for example, "Data.Generics."), we only work on one module
-- done :: WorkDoneAccum = list of previous values of "current" that have been handled already
-- returns: Pairs + updated WorkDoneAccum
-- where Pairs is a list of Pair. 
--       Pair maps one data type, to a list of constructor it has in one alternative
-- ex (not entirely valid): [  (Maybe, [Just, a])   ,   (Maybe, [Nothing])
makePairs :: Name -> String -> WorkDoneAccum -> Q ([(Name, [(Name, [Type])])], WorkDoneAccum)-- ([Dec], [String])
makePairs current prefix done = if not (prefix `isPrefixOf`show current)  || (show current `elem` done) then return ([], done) else do
    j <- reify current
    case j of
--      (DataConI name typ parentname) ->  []
        (TyConI dec) -> case dec of
                            DataD cxt name tyvarbdrList mbKind conList derivCList -> makePairsData current prefix done conList
                            TySynD n l t -> makePairsType current prefix done t
                            NewtypeD cxt name tyvarbdrList mbKind con derivCList -> makePairsData current prefix done [con]
--                             x -> trace ("Error: Making pair where " ++ show current ++ " is actually: " ++ show x) $ return ([], done)
                            x -> error ("Error: Making pair where " ++ show current ++ " is actually: " ++ show x)



makePairsData :: Name -> String -> WorkDoneAccum -> [Con] -> Q ([(Name, [(Name, [Type])])], WorkDoneAccum)
makePairsData current prefix done constructorsOfCurr = do
    let nexts = concatMap ((\(cname, ctypes) -> concatMap typeToName ctypes) . normalizeConstr) constructorsOfCurr -- [Name]
        currentConstrs' = map normalizeConstr constructorsOfCurr
    (namessRec, newDone) <- recCalls prefix (show current : done) nexts
    let this = (current, currentConstrs')
    return (this : concat namessRec, newDone)

makePairsType :: Name -> String -> WorkDoneAccum -> Type -> Q ([(Name, [(Name, [Type])])], WorkDoneAccum)
makePairsType current prefix done currType = do
    let nexts = typeToName currType -- [Name]
    (namessRec, newDone) <- recCalls prefix (show current : done) nexts
    return (concat namessRec, newDone)


recCalls :: String -> [String] -> [Name] -> Q ([[(Name, [(Name, [Type])])]], [String])
recCalls _ done [] = return ([], done)
recCalls prefix done (h:t) = do
    (one, newDone) <- makePairs h prefix done
    (nexts, newNewDone) <- recCalls prefix newDone t
    return (one : nexts, newNewDone)

-- makeConstructorDatatype :: smth -> Dec
makeConstructorDatatype pairs =
    let names = concatMap (map fst . snd) pairs
        constructorsOfNames = map (\n -> NormalC (makeC n) []) names
        primitives = map (\n -> NormalC (makeCFromStr n) []) ["PrimitiveInt", "PrimitiveBool", "PrimitiveString", "PrimitiveList", "PrimitiveMaybe"]
        allConstructors = constructorsOfNames ++ primitives
    in DataD [] (mkName "Constructor") [] Nothing allConstructors [] -- last parameter is deriving clauses, consider adding Show

-- builds one part of the constructor function, thus constructor is build by folding makeExpress over the various data types. 
makeExpress :: Name -> Name -> [(Name, [Type])] -> Exp -> Q Exp
makeExpress agArg c m d = do --constr, matches, default exp
    let caseExp = SigE ( AppE (VarE $ mkName "getHole") (VarE agArg) ) (AppT (ConT $ mkName "Maybe") (ConT c))
        correctMatch (m, lm) = do
            paramArgs <- replicateM (length lm) (newName "arg")
            return $ Match (ConP (mkName "Just") [] [ConP m [] (map VarP paramArgs)]) (NormalB ( ConE $ makeC m) ) []
    let defaultMatch = Match WildP (NormalB d) []
    caseMatches <- fmap (++[defaultMatch]) (mapM correctMatch m) -- ++ d
    return $ CaseE caseExp caseMatches


-- we use for primitive types only
{-
buildCase :: Name -> Exp -> (String, String) -> Q Exp
buildCase agArg previousExp (defname, ourname) = return $ CaseE caseExp (caseMatches : [defaultMatch]) 
 where caseExp = SigE ( AppE (VarE $ mkName "getHole") (VarE agArg) ) (AppT (ConT $ mkName "Maybe") (ConT (mkName defname)))
       caseMatches = Match (ConP (mkName "Just") [] [WildP]) (NormalB ( ConE $ makeCFromStr ourname) ) []
       defaultMatch = Match WildP (NormalB previousExp) []

-- list of (Type name, our internal name for it)
-- we put primitive type names here
defaultTypes = [("String", "PrimitiveString"), 
                ("Bool", "PrimitiveBool"), 
                ("Int", "PrimitiveInt") 
                ]
-}
extraTypes = [  ("String", "PrimitiveString"), 
                ("Bool", "PrimitiveBool"), 
                ("Integer", "PrimitiveInt"), 
                ("Maybe", "PrimitiveMaybe"), 
                ("[]", "PrimitiveList")
                ]

defaultFailureExp :: Name -> Q Exp 
defaultFailureExp agArg = do
    failure <- [|error "error in constructor"|]
    {-
    let withTypes = foldM (buildCase agArg) failure defaultTypes
    v <- [|case query (tyconUQname . dataTypeName . dataTypeOf) $(varE agArg) of
                "Maybe" -> CPrimitiveMaybe
                "[]" -> CPrimitiveList
                _ -> $withTypes
        |]
    -}
    let x_name = mkName "x"
        failCase =  [match (varP x_name) (normalB (appE (varE $ mkName "error") (infixE (Just $ litE (stringL "error in constructor ")) (varE $ mkName "++") (Just $ varE x_name)))) []]
    -- failCase =  [match (wildP) (normalB (appE (varE $ mkName "error") (litE (stringL "error in constructor")))) []]
    caseE [|query nameOfData $(varE agArg)|] $ 
            map (\(defname, ourname) -> 
                match (litP (stringL defname)) (normalB (conE $ makeCFromStr ourname)) [])
                  extraTypes ++ failCase
                  

nameOfData :: Data a => a -> String
nameOfData x = tyconUQname (dataTypeName (dataTypeOf x))

    