{-# LANGUAGE TemplateHaskell #-}

module Wrat where

import Control.Category
import Control.Monad.Reader
import Data.Char
import Language.Haskell.Exts

import Control.Applicative      hiding ((<$>), empty)
import Data.Monoid              hiding ((<>))
import Prelude                  hiding ((.))
import Text.PrettyPrint.Leijen  hiding (Pretty)

import qualified Data.Label         as L
import qualified Data.Label.Monadic as M

data Configuration
  = Configuration { _topLevel :: Bool }
  deriving (Eq, Show)

defaultConfiguration :: Configuration
defaultConfiguration
  = Configuration { _topLevel = True }

L.mkLabels [''Configuration]

extendedParseMode :: ParseMode
extendedParseMode
  = ParseMode { parseFilename         = ""
              , baseLanguage          = Haskell2010
              , extensions            = map EnableExtension mostExtensions
              , ignoreLinePragmas     = False
              , ignoreLanguagePragmas = False
              , fixities              = Just baseFixities
              }

mostExtensions :: [KnownExtension]
mostExtensions
  = [ Arrows
    , BangPatterns
    , FlexibleContexts
    , FlexibleInstances
    , ForeignFunctionInterface
    , FunctionalDependencies
    , GADTs
    , GeneralizedNewtypeDeriving
    , KindSignatures
    , LiberalTypeSynonyms
    , MagicHash
    , MultiParamTypeClasses
    , QuasiQuotes
    , RankNTypes
    , ScopedTypeVariables
    , TemplateHaskell
    , TupleSections
    , TypeFamilies
    , TypeOperators
    , TypeSynonymInstances
    , UnboxedTuples
    ]

parseExtendedDecl :: String -> Either String Decl
parseExtendedDecl s
  = case parseWithMode extendedParseMode s of
      ParseOk decl        -> Right decl
      ParseFailed loc err -> Left err

subLevel :: Reader Configuration a -> Reader Configuration a
subLevel
  = local (L.set topLevel False)

forall, arrow, doubleArrow :: Doc

forall      = text "forall"
arrow       = text "->"
doubleArrow = text "=>"

prettyToDoc :: Pretty a => a -> Doc
prettyToDoc
  = text . prettyPrint

prettyTyVarBinds :: Maybe [TyVarBind] -> Doc
prettyTyVarBinds Nothing
  = empty

prettyTyVarBinds (Just vs)
  = forall <+> fillSep (map prettyToDoc vs) <> dot

prettyContext :: Context -> Doc
prettyContext []
  = empty

prettyContext as
  = parens . align . fillSep . punctuate comma . map prettyToDoc $ as

prettyType :: Type -> Reader Configuration Doc
prettyType (TyForall maybe_vs1 cxt1 (TyForall maybe_vs2 cxt2 t))
  = prettyType (TyForall (maybe_vs1 `mappend` maybe_vs2) (cxt1 `mappend` cxt2) t)

prettyType (TyForall maybe_vs cxt t)
  = fmap (\d -> dvs <> dcxt <$> doubleArrow <+> d) (prettyType t)
    where
      dvs   = prettyTyVarBinds maybe_vs
      dcxt  = prettyContext cxt

prettyType (TyFun t1 t2) = do
  d1 <- subLevel $ prettyType t1
  d2 <- prettyType t2

  top_level <- M.asks topLevel
  return $ if top_level
    then d1 <$> arrow <+> d2
    else parens (d1 <+> arrow <+> d2)

prettyType t
  = return $ prettyToDoc t

prettyDecl :: Decl -> Reader Configuration (Maybe Doc)
prettyDecl (TypeSig _ names t)
  = fmap mkSig (prettyType t)
    where
      dnames  = fillSep $ punctuate comma (map prettyToDoc names)
      pad x   = if odd x then empty else text " "
      padded  = width dnames pad
      colons  = colon <> colon
      mkSig d = Just (padded <+> align (colons <+> d))

prettyDecl _
  = return Nothing

isConstructor :: String -> Bool
isConstructor []        = False
isConstructor (c : _)   = isUpper c

toFunction, toConstructor :: String -> String

toFunction []           = []
toFunction (c : cs)     = toLower c : cs

toConstructor []        = []
toConstructor (c : cs)  = toUpper c : cs

format :: String -> String
format input
  | Right decl <- parseExtendedDecl (toFunction input')
  , Just d <- runReader (prettyDecl decl) defaultConfiguration
      = let output        = flip displayS [] $
                              renderPretty 0.9 80 $ indent space_count d

            (_, output')  = span isSpace output

        in  spaces ++ if is_con then toConstructor output' else output'

  | otherwise
      = input

  where
    (spaces, input')  = span isSpace input
    space_count       = length spaces
    is_con            = isConstructor input'

main :: IO ()
main
  = interact format
