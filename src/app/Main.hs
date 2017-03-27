{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           System.IO.Unsafe
import           Lib

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "KS Test - call with <host>:<port>/kstest/<samples 1 file-name>/<samples 2 file-name>") <|>
    route [ ("foo", writeBS "bar")
          , ("kstest/:samples1/:samples2", ksTestHandler)
          ] <|>
    dir "static" (serveDirectory ".")

ksTestHandler :: Snap ()
ksTestHandler = do
    samples1Param <- getParam "samples1"
    samples2Param <- getParam "samples2"
    let result = handleKSTest (B.unpack (fromJust samples1Param)) (B.unpack (fromJust samples2Param))
    writeBS $ B.pack result

basePath :: String
basePath = "/Users/ehoning/data/kstest/"

handleKSTest :: String  -> String -> String
handleKSTest s1 s2 = unsafePerformIO $ ksTest s1Path s2Path
  where s1Path = basePath ++ s1
        s2Path = basePath ++ s2
