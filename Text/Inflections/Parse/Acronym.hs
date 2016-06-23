-- |
-- Module      :  Text.Inflections.Parse.Acronym
-- Copyright   :  © 2016 Justin Leitgeb
-- License     :  MIT
--
-- Maintainer  :  Justin Leitgeb <justin@stackbuilders.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for acronyms.

{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

module Text.Inflections.Parse.Acronym ( acronym ) where

import qualified Text.ParserCombinators.Parsec.Char as C
import qualified Text.Parsec as P
import qualified Text.Parsec.Prim as Prim

import Text.Inflections.Parse.Types

import Control.Applicative ((<$>))

import Prelude (Char, String, (.), map)

-- | Parser that accepts a string from given collection and turns it into
-- an 'Acronym'.
acronym :: P.Stream s m Char => [String] -> P.ParsecT s u m Word
acronym as = Acronym <$> P.choice (map (Prim.try . C.string) as)
