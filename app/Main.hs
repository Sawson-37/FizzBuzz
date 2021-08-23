

import System.Directory (listDirectory, doesFileExist)
import Text.Read(readMaybe)
import System.IO
import System.FilePath(takeExtension ,(</>))
import System.Environment(getArgs)
import Data.List(isInfixOf)
import Data.Set (Set)
import qualified Data.Set as S (fromList, toList, insert, delete, member, union, intersection, difference, map, empty)
import Data.Map (Map)
import qualified Data.Map as M (fromList, toList, insert, delete, lookup, union, intersection, difference, map, insertWith, empty)
import Data.Array (Array, (!), (//))
import qualified Data.Array as A (array, accumArray)
import Data.Array.IArray (amap, Ix)
import Data.Char (toUpper)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.ByteString (ByteString) -- ファイルの読み書き、サーバに流すとき　Word8の配列　メモリそのものを表現するときにも使用する
import qualified Data.ByteString as B (readFile, writeFile)　--　エンコードでコードのことをシリアライゼーション
import Data.Serialize
import GHC.Generics (Generic)
import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as N 
import qualified Network.Socket.ByteString as N (recv, sendTo, send)

{-
ネットワークの送受信は、初めにソケットを送信してからそれに
daiagrams　→　UDP
-}

data Gender
   = Male
   | Female
   deriving (Show, Generic, Serialize) -- Generic：自動的に型を導出する　Serial：エンコード・デコードが使用できるようになる

data Person = Person -- ※　
   { personName :: String
   , personHender :: Gender
   } 
   deriving (Show, Generic, Serialize)

meibo  :: [Person]
meibo = 
   [Person "Fujinaga" Male
   ,Person "Sawamura" Female
   ,Person "Sakari" Male
   ]

data TrackID
     = T801A
     | T802A
     | T803A
     | T801B
     | T802B
     | T803B
         deriving(Show, Eq, Ord, Ix, Enum, Bounded)
        
{-
   -- B.writeFile "Meibo" $ encode meibo
   bstr <- B.readFile "C:/Users/anura/Desktop/FizzBuzz/syain_meibo"
   case decode bstr :: Either String [Person] of
      Right meibo -> do
         putStrLn "Decode Success!" -- デコードが成功したとき(Eitherの場合)
         putStrLn $ show meibo
      Left err -> do -- errを使用しないときにはワイルドカード(_)を使用する
         putStrLn "Decode Error!"
         putStrLn err
-}
   
   --let meibo:: [Person]
   --    maibo = decode bstr
   --bstr <- B.readFile "C:/Users/anura/ByteString.txt"
main :: IO ()
main = mainTCPReceiver

openSockTCPServer
    :: String -- ^ Port number or name; 514 is default
    -> IO Socket
openSockTCPServer port = do
    addrinfos <- N.getAddrInfo (Just (N.defaultHints {N.addrFlags = [N.AI_PASSIVE]})) Nothing (Just port)
    let serveraddr = head addrinfos
    sock <- N.socket (N.addrFamily serveraddr) N.Stream N.defaultProtocol
    N.bind sock (N.addrAddress serveraddr)
    N.listen sock 2048
    return sock

openSockTCPClient
    :: String -- ^ IP Address of the target server to communicate
    -> String -- ^ Port number of the target server to communicate
    -> IO Socket
openSockTCPClient hostname port = do
    addrinfos <- N.getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos
    sock <- N.socket (N.addrFamily serveraddr) N.Stream N.defaultProtocol
--     setSocketOption sock KeepAlive 1
    N.connect sock (N.addrAddress serveraddr)
    return sock

--TCP受信
mainTCPReceiver = do
    sock <- openSockTCPServer "60001"　-- ポート番号のみ(受ける側)
    let loop = do
            bstr <- N.recv sock 1472 -- イーサネットの最大フレーム長　(UDPの場合、イーサネットのフレーム長を超えるとパソケットがばらける) 
            -- ブロッキング状態　：　何も受信しなければ処理が止まる(タイムアウトさせるか、手動で終わらせる)
            case decode bstr :: Either String [Person] of
                Right maibo -> do
                    putStrLn $ show maibo
                    
                    loop
                Left err -> do
                    putStrLn err
                    putStrLn "Exitting"
                    N.close sock
    loop

--TCP送信
mainTCPSender = do
    sock <- openSockTCPClient  "172.21.102.5" "60002" -- ブロードキャスト　IPアドレス　ポート番号
    -- 並列処理が関わってくる
    N.send sock $ encode meibo
    -- ソケット　バイト列の情報(ByteString)
    N.close sock



--UDP受信
mainUdpReceiver = do
    sock <- openSockUdpReceiver "60001"　-- ポート番号のみ(受ける側)
    let loop = do
            bstr <- N.recv sock 1472 -- イーサネットの最大フレーム長　(UDPの場合、イーサネットのフレーム長を超えるとパソケットがばらける) 
            -- ブロッキング状態　：　何も受信しなければ処理が止まる(タイムアウトさせるか、手動で終わらせる)
            case decode bstr :: Either String [Person] of
                Right meibo -> do
                    putStrLn $　"受信しました"
                    putStrLn $ show meibo                   
                    loop
                Left err -> do
                    putStrLn err
                    putStrLn "Exitting"
                    N.close sock
    loop

--UDP送信
mainUDPSender = do
    (sock,addr) <- openSockUdpSender False "172.21.102.5" "60001" -- ブロードキャスト　IPアドレス　ポート番号
    N.sendTo sock (encode meibo) addr 
    -- ソケット　バイト列の情報(ByteString)
    N.close sock

openSockUdpReceiver
    :: String -- ^ Port number or name
    -> IO Socket
openSockUdpReceiver port = do
    addrinfos <- N.getAddrInfo (Just (N.defaultHints {N.addrFlags = [N.AI_PASSIVE]})) Nothing (Just port)
    let serveraddr = head addrinfos
    sock <- N.socket (N.addrFamily serveraddr) N.Datagram N.defaultProtocol
    N.bind sock (N.addrAddress serveraddr)
    return sock

openSockUdpSender
     :: Bool -- ^ isBroadCast
     -> String -- ^ IP Address
     -> String -- ^ Port Number
     -> IO (Socket, SockAddr)
openSockUdpSender isBroadCast addr port = do
     addrinfos <- N.getAddrInfo Nothing (Just addr) (Just port)
     let receiverAddr = head addrinfos
     sock <- N.socket N.AF_INET N.Datagram 0
     if isBroadCast
         then N.setSocketOption sock N.Broadcast 1
         else return ()
     N.setSocketOption sock N.SendBuffer 1472
     return (sock, N.addrAddress receiverAddr)


safeArray :: (Ix i, Enum i, Bounded i) => e -> [(i,e)] -> Array i e
safeArray def list = A.accumArray (\ e a -> a ) def (minBound, maxBound) list

someArray4 :: Array TrackID String 
someArray4 = safeArray "" $ map indexToPair [minBound .. maxBound]
 where indexToPair :: TrackID -> (TrackID, String)
       indexToPair i = (i, show i)

someArray3 :: Array Int String
someArray3 = amap (map toUpper) someArray2 

-- toUpper は　Charなので　mapが必要

someArray2 :: Array Int String
someArray2 = someArray // [(1,"abc"),(30,"def")] 



someArray :: Array Int String
someArray = A.array (0,100) $ map indexToPair[0..100] 
 where indexToPair :: Int -> (Int, String)
       indexToPair i = (i, show i)

deleteDuplicateAndSort :: [Int]  -> [Int] --toAscList .. 昇順　toDescList .. 降順
deleteDuplicateAndSort = S.toList . S.fromList
-- someSet :: Set Int -- IntSet int用のほうが効率が良い 

-- (1, 'a'), (2, 'c'), (1, 'b'), (3, 'd'), (4, 'e'), (3, 'f')
groupMap :: Ord k => [(k, a)] -> Map k [a]
groupMap [] = M.fromList []
groupMap ((k, x): xs) = 
   M.insertWith (++) k [x] mp
 where mp = groupMap xs



{-

-}

{-
{-
someList :: [(Int, String)]
someList = [ (5630,"藤永さん")
        　　, (6104,"澤村")]
lookup :: k -> [(k, a)] -> Maybe
lookup "イカ" someList
=> Just 10

someMap :: Map String Int
someMap

・データ構造の名前のみ修飾なし　
・関数は就職あり　…就職なしだとコンフリクトエラーを起こす
↑　このインポートが多い

Functor
fmap ... どの構造体にも使用できる。importしてきた構造体
(<$>) = fmap :: FizzBuzzの時に使用した関数

Maybe、IO aに対してもfmapは使用できる
※ functorは何に対しても適応可能

-}

someList :: [Int]
someList  = [1, 2, 3, 4]
someList' = [4, 3, 2, 1]
{-
・定数時間で要素を追加可能
-}

someSet :: Set Int -- IntSet int用のほうが効率が良い
someSet  = S.fromList[1, 2, 3, 4]
someSet' = S.fromList[4, 3, 2, 1]

{-
・木構造
・ほとんど処理負荷ない
・要素を追加するときもlogn掛かる
-}


main :: IO()
main = do
  (str:_) <- getArgs

  contents <- getContents 
  putStrLn $ grepPure str contents

grepPure :: String -> String -> String
grepPure str contents = unlines $ filter isContainsStr $ lines contents 
 where isContainsStr :: String -> Bool
       isContainsStr line = str `isInfixOf` lines

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
-}
