-----------------------------------------------------------------------
--
--  The Fun of Programming With Attribute Grammars
--
--  code shared by AG solutions
--
----------------------------------------------------------------------

module Block.SharedAG ( module Block.Shared, Constructor(..), constructor, lexeme
                , AGTree, mkAG, (.$>), (.$<)) where

import Library.ZipperAG hiding (mkAG,(.$>),(.$<))
import Data.Generics.Zipper
import Data.Data
import Block.Shared

data Constructor = CConsIts
                 | CNilIts
                 | CDecl
                 | CUse
                 | CBlock
                 | CRoot
                deriving Show

constructor :: (Typeable a) => Zipper a -> Constructor
constructor a = case ( getHole a :: Maybe Its ) of
                 Just (ConsIts _ _) -> CConsIts
                 Just (NilIts) -> CNilIts
                 otherwise -> case ( getHole a :: Maybe It ) of
                                Just (Decl _ _) -> CDecl
                                Just (Use _ _) -> CUse
                                Just (Block _) -> CBlock
                                otherwise -> case ( getHole a :: Maybe P) of 
                                                Just (Root _) -> CRoot
                                                otherwise -> error "Naha, that production does not exist!"

lexeme z = case (getHole z :: Maybe It) of
            Just (Use x _) -> x
            Just (Decl x _) -> x
            Just (Block a) -> (show a) ++ "!!!! SHOWING BLOCK (TODO REMOVE THIS)"
            _              -> error "unexpected use of Block lexeme!"

type AGTree a  = Zipper P -> a

mkAG :: Data t => t -> Zipper t
mkAG = toZipper

(.$>) :: Zipper a -> Int -> Zipper a
zipper .$> n = let current = aux zipper 1
               in  (parent zipper).$(current+n)

(.$<) :: Zipper a -> Int -> Zipper a
zipper .$< n = let current = aux zipper 1
               in  (parent zipper).$(current-n)

aux :: Zipper a -> Int -> Int
aux m n = case left m of
                     Nothing  -> n                     
                     Just m'  -> aux m' (n+1)
