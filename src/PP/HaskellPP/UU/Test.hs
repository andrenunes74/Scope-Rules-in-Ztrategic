module Examples.AG.PP.HaskellPP.UU.Test where 

import Examples.Strategies.HsTransform.HsIOWrap (parseWrap)
import Language.Haskell.Syntax

import Examples.AG.PP.Interface 
-- import Examples.AG.PP.Memo.Interface 
import Examples.AG.PP.HaskellPP.UU.PrettyP



parseFile :: FilePath -> IO HsModule 
parseFile f = do 
    x <- readFile f
    let m = parseWrap x 
    return m 

parsePrint :: FilePath -> IO ()
parsePrint f = do 
    hsM <- parseFile f
    putStrLn $ prettyPrint hsM

printModule :: HsModule -> IO () 
printModule = putStrLn . prettyPrint

parseStr :: FilePath -> IO String
parseStr f = do 
    hsM <- parseFile f
    return $ prettyPrint hsM

strModule :: HsModule -> String
strModule = prettyPrint

-- seem to be working fine
test01 = parsePrint "Examples/Strategies/Inputs/smelly.hs"
test02 = parsePrint "Examples/Strategies/Inputs/HsTransform.hs"