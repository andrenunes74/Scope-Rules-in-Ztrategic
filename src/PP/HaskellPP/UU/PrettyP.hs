-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Pretty
-- Copyright   :  (c) The GHC Team, Noel Winstanley 1997-2000
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Andreas Abel
-- Stability   :  stable
-- Portability :  portable
--
-- Pretty printer for Haskell.
--
-----------------------------------------------------------------------------

module Examples.AG.PP.HaskellPP.UU.PrettyP
  ( -- * Pretty printing
    Pretty,
    prettyPrintStyleMode,
    prettyPrintWithMode,
    prettyPrint,

    -- * Pretty-printing styles (from "Text.PrettyPrint.HughesPJ")
    -- P.Style(..),
    -- P.style,
    -- P.Mode(..),

    -- * Haskell formatting modes
    Indent,
  ) where

import           Language.Haskell.Syntax

import           Control.Applicative as App (Applicative (..), Alternative (empty))
import           Control.Monad           (ap)

-- import qualified Text.PrettyPrint        as P

import Examples.AG.PP.UU.Basic as P
-- import Examples.AG.PP.Memo.Interface as P
import Examples.AG.PP.Shared

infixl 5 $$$

-----------------------------------------------------------------------------

notext = text ""

type Indent = Int

classIndent = 8
doIndent = 3
caseIndent = 4
letIndent = 4
whereIndent = 6
onsideIndent = 2

-- | The document type produced by these pretty printers uses a 'PPHsMode'
-- environment.
type Doc = PP_Doc

-- | Things that can be pretty-printed, including all the syntactic objects
-- in "Language.Haskell.Syntax".
class Pretty a where
        -- | Pretty-print something in isolation.
        pretty :: a -> Doc
        -- | Pretty-print something in a precedence context.
        prettyPrec :: Int -> a -> Doc
        pretty = prettyPrec 0
        prettyPrec _ = pretty

-- The pretty printing combinators

nest :: Int -> Doc -> Doc
nest i m = indent i m

-- Simple Combining Forms

parens, brackets, braces :: PP_Doc -> PP_Doc
parens d = text "(" >|< d >|< text ")"
brackets d = text "[" >|< d >|< text "]"
braces d = text "{" >|< d >|< text "}"
-- quotes d = d >>= return . P.quotes
-- doubleQuotes d = d >>= return . P.doubleQuotes

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- Constants

semi,comma,space,equals :: Doc
semi = text ";"
comma = text ","
space = text " "
equals = text "="

-- Combinators

-- aM $+$ bM = do{a<-aM;b<-bM;return (a P.$+$ b)}

hcat,hsep,vcat,fsep :: [Doc] -> Doc
hcat dl =  fill dl
hsep [h] = h
hsep (h:t) = foldl (>#<) h t
vcat [h] = h
vcat (h:t) = foldl (>-<) h t
-- sep dl = sequence dl >>= return . P.sep
-- cat dl = sequence dl >>= return . P.cat
fsep [] = notext
fsep [h] = h
fsep (h:t) = foldl (>#<) h t
-- fcat dl = sequence dl >>= return . P.fcat

-- Some More

-- hang :: Doc -> Int -> Doc -> Doc
-- hang dM i rM = do{d<-dM;r<-rM;return $ P.hang d i r}

-- Yuk, had to cut-n-paste this one from Pretty.hs
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate p (d1:ds) = go d1 ds
                   where
                     go d []     = [d]
                     go d (e:es) = (d >|< p) : go e es

-- | render the document with a given style and mode.
renderStyleMode :: Doc -> String
-- renderStyleMode d = disp d 50
renderStyleMode d = disp d 1000 "" -- check LRC version, we assume its the same here

-- --- | render the document with a given mode.
-- renderWithMode :: PPHsMode -> Doc -> String
-- renderWithMode = renderStyleMode P.style

-- -- | render the document with 'defaultMode'.
-- render :: Doc -> String
-- render = renderWithMode defaultMode

-- | pretty-print with a given style and mode.
prettyPrintStyleMode :: Pretty a => a -> String
prettyPrintStyleMode = renderStyleMode . pretty

-- | pretty-print with the default style and a given mode.
prettyPrintWithMode :: Pretty a => a -> String
prettyPrintWithMode = prettyPrintStyleMode

-- | pretty-print with the default style and 'defaultMode'.
prettyPrint :: Pretty a => a -> String
prettyPrint = prettyPrintWithMode

-- fullRenderWithMode :: PPHsMode -> P.Mode -> Int -> Float ->
--                       (P.TextDetails -> a -> a) -> a -> Doc -> a
-- fullRenderWithMode ppMode m i f fn e mD =
--                    P.fullRender m i f fn e $ (unDocM mD) ppMode
--
--
-- fullRender :: P.Mode -> Int -> Float -> (P.TextDetails -> a -> a)
--               -> a -> Doc -> a
-- fullRender = fullRenderWithMode defaultMode

-------------------------  Pretty-Print a Module --------------------
instance Pretty HsModule where
        pretty (HsModule pos m mbExports imp decls) =
                topLevel (ppHsModuleHeader m mbExports)
                         (map pretty imp ++ map pretty decls)

--------------------------  Module Header ------------------------------
ppHsModuleHeader :: Module -> Maybe [HsExportSpec] ->  Doc
ppHsModuleHeader m mbExportList = mySep [
        text "module",
        pretty m,
        maybePP (parenList . map pretty) mbExportList,
        text "where"]

instance Pretty Module where
        pretty (Module modName) = text modName

instance Pretty HsExportSpec where
        pretty (HsEVar name)                = pretty name
        pretty (HsEAbs name)                = pretty name
        pretty (HsEThingAll name)           = pretty name >|< text "(..)"
        pretty (HsEThingWith name nameList) =
                pretty name >|< (parenList . map pretty $ nameList)
        pretty (HsEModuleContents m)       = text "module" >#< pretty m

instance Pretty HsImportDecl where
        pretty (HsImportDecl pos m qual mbName mbSpecs) =

                mySep [text "import",
                       if qual then text "qualified" else notext,
                       pretty m,
                       maybePP (\m' -> text "as" >#< pretty m') mbName,
                       maybePP exports mbSpecs]
            where
                exports (b,specList) =
                        if b then text "hiding" >#< specs else specs
                    where specs = parenList . map pretty $ specList

instance Pretty HsImportSpec where
        pretty (HsIVar name)                = pretty name
        pretty (HsIAbs name)                = pretty name
        pretty (HsIThingAll name)           = pretty name >|< text "(..)"
        pretty (HsIThingWith name nameList) =
                pretty name >|< (parenList . map pretty $ nameList)

-------------------------  Declarations ------------------------------
instance Pretty HsDecl where
        pretty (HsTypeDecl loc name nameList htype) =
                blankline $

                mySep ( [text "type", pretty name]
                        ++ map pretty nameList
                        ++ [equals, pretty htype])

        pretty (HsDataDecl loc context name nameList constrList derives) =
                blankline $

                mySep ( [text "data", ppHsContext context, pretty name]
                        ++ map pretty nameList)
                        >#< (myVcat (zipWith (>#<) (equals : repeat (text "|"))
                                                   (map pretty constrList))
                        $$$ ppHsDeriving derives)

        pretty (HsNewTypeDecl pos context name nameList constr derives) =
                blankline $

                mySep ( [text "newtype", ppHsContext context, pretty name]
                        ++ map pretty nameList)
                        >#< equals >#< (pretty constr $$$ ppHsDeriving derives)

        --m{spacing=False}
        -- special case for empty class declaration
        pretty (HsClassDecl pos context name nameList []) =
                blankline $

                mySep ( [text "class", ppHsContext context, pretty name]
                        ++ map pretty nameList)
        pretty (HsClassDecl pos context name nameList declList) =
                blankline $

                mySep ( [text "class", ppHsContext context, pretty name]
                        ++ map pretty nameList ++ [text "where"])
                $$$ ppBody classIndent (map pretty declList)

        -- m{spacing=False}
        -- special case for empty instance declaration
        pretty (HsInstDecl pos context name args []) =
                blankline $

                mySep ( [text "instance", ppHsContext context, pretty name]
                        ++ map ppHsAType args)
        pretty (HsInstDecl pos context name args declList) =
                blankline $

                mySep ( [text "instance", ppHsContext context, pretty name]
                        ++ map ppHsAType args ++ [text "where"])
                $$$ ppBody classIndent (map pretty declList)

        pretty (HsDefaultDecl pos htypes) =
                blankline $

                text "default" >#< parenList (map pretty htypes)

        pretty (HsTypeSig pos nameList qualType) =
                blankline $

                mySep ((punctuate comma . map pretty $ nameList)
                      ++ [text "::", pretty qualType])

        pretty (HsForeignImport pos conv safety entity name ty) =
                blankline $

                mySep $ [text "foreign", text "import", text conv, pretty safety] ++
                        (if null entity then [] else [text (show entity)]) ++
                        [pretty name, text "::", pretty ty]

        pretty (HsForeignExport pos conv entity name ty) =
                blankline $

                mySep $ [text "foreign", text "export", text conv] ++
                        (if null entity then [] else [text (show entity)]) ++
                        [pretty name, text "::", pretty ty]

        pretty (HsFunBind matches) =
                ppBindings (map pretty matches)

        pretty (HsPatBind pos pat rhs whereDecls) =

                myFsep [pretty pat, pretty rhs] $$$ ppWhere whereDecls

        pretty (HsInfixDecl pos assoc prec opList) =
                blankline $

                mySep ([pretty assoc, text $ show prec]
                       ++ (punctuate comma . map pretty $ opList))

instance Pretty HsAssoc where
        pretty HsAssocNone  = text "infix"
        pretty HsAssocLeft  = text "infixl"
        pretty HsAssocRight = text "infixr"

instance Pretty HsSafety where
        pretty HsSafe   = text "safe"
        pretty HsUnsafe = text "unsafe"

instance Pretty HsMatch where
        pretty (HsMatch pos f ps rhs whereDecls) =

                myFsep (lhs ++ [pretty rhs])
                $$$ ppWhere whereDecls
            where
                lhs = case ps of
                        l:r:ps' | isSymbolName f ->
                                let hd = [pretty l, ppHsName f, pretty r] in
                                if null ps' then hd
                                else parens (myFsep hd) : map (prettyPrec 2) ps'
                        _ -> pretty f : map (prettyPrec 2) ps

ppWhere :: [HsDecl] -> Doc
ppWhere [] = P.empty
ppWhere l  = nest 2 (text "where" $$$ ppBody whereIndent (map pretty l))

------------------------- Data & Newtype Bodies -------------------------
instance Pretty HsConDecl where
        pretty (HsRecDecl _pos name fieldList) =
                pretty name >|< (braceList . map ppField $ fieldList)

        pretty (HsConDecl _pos name@(HsSymbol _) [l, r]) =
                myFsep [prettyPrec prec_btype l, ppHsName name,
                        prettyPrec prec_btype r]
        pretty (HsConDecl _pos name typeList) =
                mySep $ ppHsName name : map (prettyPrec prec_atype) typeList

ppField :: ([HsName],HsBangType) -> Doc
ppField (names, ty) =
        myFsepSimple $ (punctuate comma . map pretty $ names) ++
                       [text "::", pretty ty]

instance Pretty HsBangType where
        prettyPrec _ (HsBangedTy ty)   = text "!" >|< ppHsAType ty
        prettyPrec p (HsUnBangedTy ty) = prettyPrec p ty

ppHsDeriving :: [HsQName] -> Doc
ppHsDeriving []  = notext
ppHsDeriving [d] = text "deriving" >#< ppHsQName d
ppHsDeriving ds  = text "deriving" >#< parenList (map ppHsQName ds)

------------------------- Types -------------------------
instance Pretty HsQualType where
        pretty (HsQualType context htype) =
                myFsep [ppHsContext context, pretty htype]

ppHsBType :: HsType -> Doc
ppHsBType = prettyPrec prec_btype

ppHsAType :: HsType -> Doc
ppHsAType = prettyPrec prec_atype

-- precedences for types
prec_btype, prec_atype :: Int
prec_btype = 1  -- left argument of ->,
                -- or either argument of an infix data constructor
prec_atype = 2  -- argument of type or data constructor, or of a class

instance Pretty HsType where
        prettyPrec p (HsTyFun a b) = parensIf (p > 0) $
                myFsep [ppHsBType a, text "->", pretty b]
        prettyPrec _ (HsTyTuple l) = parenList . map pretty $ l
        prettyPrec p (HsTyApp a b)
                | a == list_tycon = brackets $ pretty b         -- special case
                | otherwise = parensIf (p > prec_btype) $
                        myFsep [pretty a, ppHsAType b]
        prettyPrec _ (HsTyVar name) = pretty name
        prettyPrec _ (HsTyCon name) = pretty name

------------------------- Expressions -------------------------
instance Pretty HsRhs where
        pretty (HsUnGuardedRhs e)        = equals >#< pretty e
        pretty (HsGuardedRhss guardList) = myVcat . map pretty $ guardList

instance Pretty HsGuardedRhs where
        pretty (HsGuardedRhs _pos guard body) =
                myFsep [text "|", pretty guard, equals, pretty body]

instance Pretty HsLiteral where
        pretty (HsInt i)        = text (show i)
        pretty (HsChar c)       = text (show c)
        pretty (HsString s)     = text (show s)
        pretty (HsFrac r)       = text (show $ fromRational r)
        -- GHC unboxed literals:
        pretty (HsCharPrim c)   = text (show c)           >|< text "#"
        pretty (HsStringPrim s) = text (show s)           >|< text "#"
        pretty (HsIntPrim i)    = text (show i)               >|< text "#"
        pretty (HsFloatPrim r)  = text  (show $ fromRational r) >|< text "#"
        pretty (HsDoublePrim r) = text (show $ fromRational r) >|< text "##"

instance Pretty HsExp where
        pretty (HsLit l) = pretty l
        -- lambda stuff
        pretty (HsInfixApp a op b) = myFsep [pretty a, pretty op, pretty b]
        pretty (HsNegApp e) = myFsep [text "-", pretty e]
        pretty (HsApp a b) = myFsep [pretty a, pretty b]
        pretty (HsLambda _loc expList body) = myFsep $
                text "\\" : map pretty expList ++ [text "->", pretty body]
        -- keywords
        pretty (HsLet expList letBody) =
                myFsep [text "let" >#< ppBody letIndent (map pretty expList),
                        text "in", pretty letBody]
        {-
        pretty (HsIf cond thenexp elsexp) =
                myFsep [text "if", pretty cond,
                        text "then", pretty thenexp,
                        text "else", pretty elsexp]
        -}
        {-
        pretty (HsIf cond thenexp elsexp) =
                myFsep [text "if", pretty cond] $$$
                myFsep [text " then", pretty thenexp] $$$
                myFsep [text " else", pretty elsexp]
        -}
        pretty (HsIf cond thenexp elsexp) =
           ((par >>|<< par >>|<< par)
            >>^<< 
             -- (((par >>|<< par >>^<< par >>-<< par)
             --             >>$<< [par, par >>-<< par])))  
                 ((par >>|<< (par >>-<< par)) >>^<< (par >>-<< (par >>-<< par))))
           >>$<     [ myFsep [text "if", pretty cond]
                    , myFsep [text " then", pretty thenexp]
                    , myFsep [text " else", pretty elsexp]
                    ]
                    
        pretty (HsCase cond altList) =
                myFsep [text "case", pretty cond, text "of"]
                $$$ ppBody caseIndent (map pretty altList)
        pretty (HsDo stmtList) =
                text "do" $$$ ppBody doIndent (map pretty stmtList)
        -- Constructors & Vars
        pretty (HsVar name) = pretty name
        pretty (HsCon name) = pretty name
        pretty (HsTuple expList) = parenList . map pretty $ expList
        -- weird stuff
        pretty (HsParen e) = parens . pretty $ e
        pretty (HsLeftSection e op) = parens (pretty e >#< pretty op)
        pretty (HsRightSection op e) = parens (pretty op >#< pretty e)
        pretty (HsRecConstr c fieldList) =
                pretty c >|< (braceList . map pretty $ fieldList)
        pretty (HsRecUpdate e fieldList) =
                pretty e >|< (braceList . map pretty $ fieldList)
        -- patterns
        -- special case that would otherwise be buggy
        pretty (HsAsPat name (HsIrrPat e)) =
                myFsep [pretty name >|< text "@", text "~" >|< pretty e]
        pretty (HsAsPat name e) = hcat [pretty name, text "@", pretty e]
        pretty HsWildCard = text "_"
        pretty (HsIrrPat e) = text "~" >|< pretty e
        -- Lists
        pretty (HsList list) =
                bracketList . punctuate comma . map pretty $ list
        pretty (HsEnumFrom e) =
                bracketList [pretty e, text ".."]
        pretty (HsEnumFromTo from to) =
                bracketList [pretty from, text "..", pretty to]
        pretty (HsEnumFromThen from thenE) =
                bracketList [pretty from >|< comma, pretty thenE, text ".."]
        pretty (HsEnumFromThenTo from thenE to) =
                bracketList [pretty from >|< comma, pretty thenE,
                             text "..", pretty to]
        pretty (HsListComp e stmtList) =
                bracketList ([pretty e, text "|"]
                             ++ (punctuate comma . map pretty $ stmtList))
        pretty (HsExpTypeSig _pos e ty) =
                myFsep [pretty e, text "::", pretty ty]

------------------------- Patterns -----------------------------

instance Pretty HsPat where
        prettyPrec _ (HsPVar name) = pretty name
        prettyPrec _ (HsPLit lit) = pretty lit
        prettyPrec _ (HsPNeg p) = myFsep [text "-", pretty p]
        prettyPrec p (HsPInfixApp a op b) = parensIf (p > 0) $
                myFsep [pretty a, pretty (HsQConOp op), pretty b]
        prettyPrec p (HsPApp n ps) = parensIf (p > 1) $
                myFsep (pretty n : map pretty ps)
        prettyPrec _ (HsPTuple ps) = parenList . map pretty $ ps
        prettyPrec _ (HsPList ps) =
                bracketList . punctuate comma . map pretty $ ps
        prettyPrec _ (HsPParen p) = parens . pretty $ p
        prettyPrec _ (HsPRec c fields) =
                pretty c >|< (braceList . map pretty $ fields)
        -- special case that would otherwise be buggy
        prettyPrec _ (HsPAsPat name (HsPIrrPat pat)) =
                myFsep [pretty name >|< text "@", text "~" >|< pretty pat]
        prettyPrec _ (HsPAsPat name pat) =
                hcat [pretty name, text "@", pretty pat]
        prettyPrec _ HsPWildCard = text "_"
        prettyPrec _ (HsPIrrPat pat) = text "~" >|< pretty pat

instance Pretty HsPatField where
        pretty (HsPFieldPat name pat) =
                myFsep [pretty name, equals, pretty pat]

------------------------- Case bodies  -------------------------
instance Pretty HsAlt where
        pretty (HsAlt _pos e gAlts decls) =
                myFsep [pretty e, pretty gAlts] $$$ ppWhere decls

instance Pretty HsGuardedAlts where
        pretty (HsUnGuardedAlt e)      = text "->" >#< pretty e
        pretty (HsGuardedAlts altList) = myVcat . map pretty $ altList

instance Pretty HsGuardedAlt where
        pretty (HsGuardedAlt _pos e body) =
                myFsep [text "|", pretty e, text "->", pretty body]

------------------------- Statements in monads & list comprehensions -----
instance Pretty HsStmt where
        pretty (HsGenerator _loc e from) =
                pretty e >#< text "<-" >#< pretty from
        pretty (HsQualifier e) = pretty e
        pretty (HsLetStmt declList) =
                text "let" >#< ppBody letIndent (map pretty declList)

------------------------- Record updates
instance Pretty HsFieldUpdate where
        pretty (HsFieldUpdate name e) =
                myFsep [pretty name, equals, pretty e]

------------------------- Names -------------------------
instance Pretty HsQOp where
        pretty (HsQVarOp n) = ppHsQNameInfix n
        pretty (HsQConOp n) = ppHsQNameInfix n

ppHsQNameInfix :: HsQName -> Doc
ppHsQNameInfix name
        | isSymbolName (getName name) = ppHsQName name
        | otherwise = text "`" >|< ppHsQName name >|< text "`"

instance Pretty HsQName where
        pretty name = parensIf (isSymbolName (getName name)) (ppHsQName name)

ppHsQName :: HsQName -> Doc
ppHsQName (UnQual name) = ppHsName name
ppHsQName (Qual m name) = pretty m >|< text "." >|< ppHsName name
ppHsQName (Special sym) = text (specialName sym)

instance Pretty HsOp where
        pretty (HsVarOp n) = ppHsNameInfix n
        pretty (HsConOp n) = ppHsNameInfix n

ppHsNameInfix :: HsName -> Doc
ppHsNameInfix name
        | isSymbolName name = ppHsName name
        | otherwise = text "`" >|< ppHsName name >|< text "`"

instance Pretty HsName where
        pretty name = parensIf (isSymbolName name) (ppHsName name)

ppHsName :: HsName -> Doc
ppHsName (HsIdent s)  = text s
ppHsName (HsSymbol s) = text s

instance Pretty HsCName where
        pretty (HsVarName n) = pretty n
        pretty (HsConName n) = pretty n

isSymbolName :: HsName -> Bool
isSymbolName (HsSymbol _) = True
isSymbolName _            = False

getName :: HsQName -> HsName
getName (UnQual s)         = s
getName (Qual _ s)         = s
getName (Special HsCons)   = HsSymbol ":"
getName (Special HsFunCon) = HsSymbol "->"
getName (Special s)        = HsIdent (specialName s)

specialName :: HsSpecialCon -> String
specialName HsUnitCon      = "()"
specialName HsListCon      = "[]"
specialName HsFunCon       = "->"
specialName (HsTupleCon n) = "(" ++ replicate (n-1) ',' ++ ")"
specialName HsCons         = ":"

ppHsContext :: HsContext -> Doc
ppHsContext []      = notext
ppHsContext context = mySep [parenList (map ppHsAsst context), text "=>"]

-- hacked for multi-parameter type classes

ppHsAsst :: HsAsst -> Doc
ppHsAsst (a,ts) = myFsep (ppHsQName a : map ppHsAType ts)

------------------------- pp utils -------------------------
maybePP :: (a -> Doc) -> Maybe a -> Doc
maybePP _ Nothing   = notext
maybePP pp (Just a) = pp a

parenList :: [Doc] -> Doc
parenList = parens . myFsepSimple . punctuate comma

braceList :: [Doc] -> Doc
braceList = braces . myFsepSimple . punctuate comma

bracketList :: [Doc] -> Doc
bracketList = brackets . myFsepSimple

-- Wrap in braces and semicolons, with an extra space at the start in
-- case the first doc begins with "-", which would be scanned as {-
flatBlock :: [Doc] -> Doc
flatBlock = braces . (space >|<) . hsep . punctuate semi

-- Same, but put each thing on a separate line
prettyBlock :: [Doc] -> Doc
prettyBlock = braces . (space >|<) . vcat . punctuate semi

-- Monadic PP Combinators -- these examine the env

blankline :: Doc -> Doc
blankline dl = space >-< dl
topLevel :: Doc -> [Doc] -> Doc
topLevel header dl = header >-< vcat dl

ppBody :: Int -> [Doc] -> Doc
ppBody i dl = nest i . vcat $ dl

ppBindings :: [Doc] -> Doc
ppBindings dl = vcat dl

($$$) :: Doc -> Doc -> Doc
a $$$ b = hv2i a b -- a >-< b

-- ensure paragraph fills with indentation.
mySep :: [Doc] -> Doc
mySep [x]    = x
mySep (x:xs) = x >#< fsep xs
mySep []     = error "Internal error: mySep"

myVcat :: [Doc] -> Doc
myVcat = vcat

myFsepSimple :: [Doc] -> Doc
myFsepSimple = fsep

-- same, except that continuation lines are indented,
-- which is necessary to avoid triggering the offside rule.
myFsep :: [Doc] -> Doc
myFsep = hsep
-- myFsep [] = notext
-- myFsep (d:ds) = nest onsideIndent (fsep ( d:ds))