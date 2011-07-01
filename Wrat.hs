{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wrat where

import Control.Applicative hiding ((<$>), empty)
import Control.Category
import Control.Monad.Reader
import Data.Char
import Data.Monoid
import Data.Record.Label
import Language.Haskell.Exts
import Text.PrettyPrint.Leijen hiding (Pretty)

import Prelude hiding ((.))

data Configuration
  = Configuration { _topLevel :: Bool }
  deriving (Eq, Show)

defaultConfiguration :: Configuration
defaultConfiguration
  = Configuration { _topLevel = True }

mkLabels [''Configuration]

extendedParseMode :: ParseMode
extendedParseMode
  = ParseMode { parseFilename         = ""
              , extensions            = mostExtensions
              , ignoreLinePragmas     = False
              , ignoreLanguagePragmas = False
              , fixities              = baseFixities
              }

mostExtensions :: [Extension]
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
  = local (setL topLevel False)

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
  = fmap (\d -> dvs <+> dcxt <$> doubleArrow <+> d) (prettyType t)
    where
      dvs   = prettyTyVarBinds maybe_vs
      dcxt  = prettyContext cxt

prettyType (TyFun t1 t2) = do
  d1 <- subLevel $ prettyType t1
  d2 <- prettyType t2

  top_level <- askM topLevel
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
      colons  = colon <> colon
      mkSig d = Just (dnames <+> align (colons <+> d))

prettyDecl _
  = return Nothing

format :: String -> String
format s
  | Right decl  <- parseExtendedDecl s
  , Just d      <- runReader (prettyDecl decl) defaultConfiguration
                = displayS (renderPretty 0.9 80 $ indent spaces d) []

  | otherwise   = s
  where
    spaces = length (takeWhile isSpace s)

main :: IO ()
main
  = interact format
