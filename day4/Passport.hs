{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Passport (Passport (..), byr, eyr, iyr, hgt, hcl, ecl, pid, cid, parseport) where

import Control.Applicative (Alternative (empty))
import Control.Applicative.Permutations
  ( intercalateEffect,
    toPermutation,
    toPermutationWithDefault,
  )
import Control.Lens (makeLenses, (^.))
import Data.Attoparsec.Text.Lazy (Parser, char, endOfInput, parseOnly, space, string, takeTill)
import Data.Char (isSpace)
import Data.Text (Text)

data Passport byr eyr iyr hgt hcl ecl pid cid = Passport
  { _byr :: byr,
    _eyr :: eyr,
    _iyr :: iyr,
    _hgt :: hgt,
    _hcl :: hcl,
    _ecl :: ecl,
    _pid :: pid,
    _cid :: cid
  }
  deriving (Show)

makeLenses ''Passport

parseport ::
  Passport
    (Maybe byr, Parser byr)
    (Maybe eyr, Parser eyr)
    (Maybe iyr, Parser iyr)
    (Maybe hgt, Parser hgt)
    (Maybe hcl, Parser hcl)
    (Maybe ecl, Parser ecl)
    (Maybe pid, Parser pid)
    (Maybe cid, Parser cid) ->
  Parser (Passport byr eyr iyr hgt hcl ecl pid cid)
parseport p =
  intercalateEffect space $
    Passport
      <$> keyValue "byr" (p ^. byr)
      <*> keyValue "eyr" (p ^. eyr)
      <*> keyValue "iyr" (p ^. iyr)
      <*> keyValue "hgt" (p ^. hgt)
      <*> keyValue "hcl" (p ^. hcl)
      <*> keyValue "ecl" (p ^. ecl)
      <*> keyValue "pid" (p ^. pid)
      <*> keyValue "cid" (p ^. cid)
  where
    keyValue key (d, v) =
      toPermutationMaybeWithDefault d $
        string key *> char ':' *> (takeTill isSpace >>= parseInner v)
    toPermutationMaybeWithDefault (Just d) = toPermutationWithDefault d
    toPermutationMaybeWithDefault Nothing = toPermutation

parseInner :: Parser a -> Text -> Parser a
parseInner parser = either (const empty) pure . parseOnly (parser <* endOfInput)