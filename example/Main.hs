-- ConstraintKinds needed only for 7.8.4
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main (main) where

import           Data.Maybe
                 (fromMaybe)
import           Lucid
import           Lucid.Servant
import           Network.Wai
                 (Application)
import           Servant
import           Servant.HTML.Lucid
import           System.Environment
                 (getArgs, lookupEnv)
import           Text.Read
                 (readMaybe)

import qualified Network.Wai.Handler.Warp as Warp

type API = "string" :> Get '[HTML] String
    :<|> "nested" :> "html" :> Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

apiLink_
    :: (IsElem endpoint API, HasLink endpoint)
    => Proxy endpoint -> MkLink endpoint Attribute
apiLink_ = safeAbsHref_ (Proxy :: Proxy API)

stringLink_ :: Attribute
stringLink_ = apiLink_ (Proxy :: Proxy ("string" :> Get '[HTML] String))

server :: Server API
server = return "example" :<|> return html where
    html :: Html ()
    html = do
        p_ $ b_ "bar"
        p_ $ i_ "iik"
        p_ $ a_ [stringLink_] "string"

app :: Application
app = serve api server

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")
            putStrLn $ "http://localhost:" ++ show port ++ "/"
            Warp.run port app
        _ -> do
            putStrLn "Example application, used as a compilation check"
            putStrLn "To run, pass run argument: --test-arguments run"
