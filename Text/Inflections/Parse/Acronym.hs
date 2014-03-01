{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Text.Inflections.Parse.Acronym ( acronym ) where

import qualified Text.ParserCombinators.Parsec.Char as C
import qualified Text.Parsec as P
import qualified Text.Parsec.Prim as Prim

import Text.Inflections.Parse.Types

import Control.Applicative ((<$>))

acronym :: P.Stream s m Char => [String] -> P.ParsecT s u m Word
acronym as = Acronym <$> (P.choice $ map (Prim.try . C.string) as)
