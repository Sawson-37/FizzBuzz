import System.IO
import Text.Read(readMaybe)

-- FizzBuzz判定
fzBz :: Int -> IO()
fzBz num 
          | fzFlag && bzFlag = print "FizzBuzz"
          | fzFlag = print "Fizz"
          | bzFlag =  print "Buzz"
          | otherwise =  print num
          where 
            fzFlag = num `mod` 3 == 0
            bzFlag = num `mod` 5 == 0

 
readInt :: String -> Maybe Int -- 標準入力を数値に変換するための苦肉の案
readInt = readMaybe

main :: IO ()
main = do
      r <- readInt <$> getLine
      case r of
          Just r' -> fzBz r'
          otherwise -> print "error"

-- 順番(1～入力という指定があったので)
-- 文字列に対応(これじゃあ答え合わせ...)
-- 勝敗決まってからループ出る

