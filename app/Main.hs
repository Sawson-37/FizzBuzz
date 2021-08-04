 -- module Main where

import System.Directory (listDirectory, doesFileExist)
import Text.Read(readMaybe)
import System.IO
import System.FilePath(takeExtension ,(</>))
import System.Environment(getArgs)
import Data.List(isInfixOf)

main :: IO()
main = do
  (str:_) <- getArgs

  contents <- getContents 
  putStrLn $ grepPure str contents

grepPure :: String -> String -> String
grepPure str contents = unlines $ filter isContainsStr $ lines contents 
 where isContainsStr :: String -> Bool
       isContainsStr line = str `isInfixOf` line 

countLine :: String -> IO Int -- 大本の
countLine filePath = do

  isExsit <- doesFileExist filePath -- 指しているPathがfileかの確認
  if isExsit
      then if ".hs" == takeExtension filePath -- fileであれば“.has”のファイルか`
         then do
            str <- readFile filePath -- Pathのfileをreadfileで読む　
            return $ length $ lines str -- 文字列をくっつけて、長さを返す
         else return 0 -- 異なれば０行として返す
      else do
         paths <- listDirectory filePath
         let absPaths = map toAbsPath paths -- IO以外で返ってくるとき
         ns <- mapM countLine absPaths -- IOで返ってくるとき
         return $ sum ns
 where
   toAbsPath :: String -> String
   toAbsPath file = filePath </> file
{-



import System.Directory (listDirectory, doesFileExist)
import Text.Read(readMaybe)
import System.IO
import System.FilePath(takeExtension ,(</>))

Prerude
-- getLIne
-- putStr
-- putStrLn
-- print
-- readFile
-- writeFile

system.directory
--listDIrectory


system.filepath

ioは基本的に自分で定義できない
isSuffiXOf <- 

fizzbazz、COUNTLINES 復讐すること

1



-}

{-
import System.Directory (listDirectory, doesFileExist)
import Text.Read(readMaybe)
import System.IO
import System.FilePath(takeExtension ,(</>))

Prerude
-- getLIne
-- putStr
-- putStrLn
-- print
-- readFile
-- writeFile

system.directory
--listDIrectory

-}

countLine2 :: String -> IO Int
countLine2 filePath = do
   isExsit <- doesFileExist filePath
   if isExsit 
      then do
         if ".hs" == takeExtension filePath
            then do 
               str <-　readFile filePath 
               return $ length $ lines str
            else return 0 
         else do
            pathList <- listDirectory filePath
            let absPathlist = map toAbs pathList  
            ns <- mapM countLine2 absPathlist
            return $ sum ns
 where
  toAbs :: String -> String 
  toAbs file =  filePath </> file


{-



-}
