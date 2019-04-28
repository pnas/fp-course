{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
anagrams gs df =
  ( \ ls -> compLines $ (toLower <$> ) ls ) <$> readFile df
  where 
    pms = permutations (toLower <$> gs)
    compLines l = (intersectBy (==) l ) <$> pms
    
  -- error "todo: Course.Anagrams#anagrams"
  -- (<$>) (intersectBy equalIgnoringCase (permutations name) . lines) . readFile

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase  =
  on (==) (toLower <$> ) 
  -- error "todo: Course.Anagrams#equalIgnoringCase"
