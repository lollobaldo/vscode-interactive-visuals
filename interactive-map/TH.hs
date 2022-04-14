{-# LANGUAGE TemplateHaskell #-}

import Data.List

import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax
import Language.Haskell.Meta

import Protocol

ppprint :: String -> String
ppprint = go 0
  where
    go n ('(':xs) = "(\n" ++ replicate (n+2) ' ' ++ go (n+2) xs
    go n ('[':xs) = "[\n" ++ replicate (n+2) ' ' ++ go (n+2) xs
    go n ('{':xs) = "{\n" ++ replicate (n+2) ' ' ++ go (n+2) xs
    -- go n ('{':xs) = "{" ++ go (n+2) (dropWhile (/= '}') xs)
    go n (')':xs) = ")\n" ++ replicate (n-2) ' ' ++ go (n-2) xs
    go n (']':xs) = "]\n" ++ replicate (n-2) ' ' ++ go (n-2) xs
    go n ('}':xs) = "}\n" ++ replicate (n-2) ' ' ++ go (n-2) xs
    go n (x:xs)   = x : go n xs
    go n []       = []


parseRes :: ParseResult a -> Either Error a
parseRes (ParseOk a) = Right a
parseRes (ParseFailed _ s) = Left s

parseMod :: Module l -> [Decl l]
parseMod (Module l _ _ _ ds) = ds

-- parseDec :: Decl l -> String
parseDec (PatBind _ (PVar _ (Ident s n)) _ _) = (s,n)
parseDec _ = undefined

useDecl :: Decl l -> Decl l
useDecl = undefined

identifiers :: FilePath -> IO (Either Error [String])
identifiers file = do
  pr <- parseFile file
  let (Module l _ _ _ ds) = parseRes pr
  return $ parseRes pr



processor :: String -> IO ()
processor file = do
  pr <- parseFile file
  let mod = parseRes pr
  let ds = parseMod mod
  let ns = map parseDec ds
  print $ exactPrint (head ds) []
  print ns
  let str = ppprint (show ns)
  writeFile "temp.txt" str
