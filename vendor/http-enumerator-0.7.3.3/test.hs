{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Network.HTTP.Enumerator
import Network
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import System.Environment.UTF8 (getArgs)
import Data.CaseInsensitive (original)

main :: IO ()
main = withSocketsDo $ do
    [url] <- getArgs
    _req2 <- parseUrl url
    {-
    let req = urlEncodedBody
                [ ("foo", "bar")
                , ("baz%%38**.8fn", "bin")
                ] _req2
    -}
    Response sc hs b <- withManager $ httpLbsRedirect _req2
#if DEBUG
    return ()
#else
    print sc
    mapM_ (\(x, y) -> do
        S.putStr $ original x
        putStr ": "
        S.putStr y
        putStrLn "") hs
    putStrLn ""
    L.putStr b
#endif
