{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE CPP               #-}
module Keter.Rewrite
  ( ReverseProxyConfig (..)
  , RewriteRule (..)
  , RPEntry (..)
  , simpleReverseProxy
  )
  where

import Control.Applicative
import Control.Exception (bracket)
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map ( Map )
import Data.Aeson
import Control.Monad (unless)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BSC
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Blaze.ByteString.Builder (fromByteString)

import Keter.Common

-- Regular expression parsing, replacement, matching
import Text.Regex.TDFA (makeRegex, matchOnceText)
import Text.Regex.TDFA.Common (Regex (..))

-- Reverse proxy apparatus
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as I
import Network.HTTP.Client.Conduit
import qualified Network.HTTP.Client as NHC
import Network.HTTP.Types
import Network.URI (URI (..), URIAuth (..), nullURI)

import Keter.Proxy.Rewrite (RewritePath, rewrite, rewritePathRule)

data RPEntry = RPEntry
    { config :: ReverseProxyConfig
    , httpManager :: Manager
    }

instance Show RPEntry where
  show x = "RPEntry { config = " ++ show (config x) ++ " }"

rewriteHeader :: Map HeaderName RewriteRule -> Header -> Header
rewriteHeader rules header@(name, value) =
  case Map.lookup name rules of
    Nothing -> header
    Just  r -> (name, regexRewrite r value)

rewriteHeaders :: Map HeaderName RewriteRule -> [Header] -> [Header]
rewriteHeaders ruleMap = map (rewriteHeader ruleMap)

regexRewrite :: RewriteRule -> S.ByteString -> S.ByteString
regexRewrite (RewriteRule _ regex' replacement) input =
  case matchOnceText regex strInput of
    Just  match -> encodeUtf8 $ rewrite '\\' match strInput strReplacement
    Nothing     -> input
  where
    strRegex = T.unpack regex'
    regex :: Regex
    regex = makeRegex strRegex
    strInput = T.unpack . decodeUtf8 $ input
    strReplacement = T.unpack replacement

filterHeaders :: [Header] -> [Header]
filterHeaders = filter useHeader
  where
    useHeader ("Transfer-Encoding", _) = False
    useHeader ("Content-Length", _)    = False
    useHeader ("Host", _)              = False
    useHeader _                        = True

mkRuleMap :: Set RewriteRule -> Map HeaderName RewriteRule
mkRuleMap = Map.fromList . map (\k -> (CI.mk . encodeUtf8 $ ruleHeader k, k)) . Set.toList

mkRequest :: ReverseProxyConfig -> Wai.Request -> (Request, Maybe URI)
mkRequest rpConfig request =
   (, mkURI) $
   NHC.defaultRequest
      { NHC.checkResponse = \_ _ -> return ()
      , NHC.responseTimeout = maybe NHC.responseTimeoutNone NHC.responseTimeoutMicro $ reverseTimeout rpConfig
      , method = Wai.requestMethod request
      , secure = reversedUseSSL rpConfig
      , host   = BSC.pack host
      , port   = reversedPort rpConfig
      , path           = BSC.pack $ uriPath  uri
      , queryString    = BSC.pack $ uriQuery uri
      , requestHeaders = filterHeaders $ rewriteHeaders reqRuleMap headers
      , requestBody =
          case Wai.requestBodyLength request of
            Wai.ChunkedBody   -> RequestBodyStreamChunked ($ I.getRequestBodyChunk request)
            Wai.KnownLength n -> RequestBodyStream (fromIntegral n) ($ I.getRequestBodyChunk request)
      , decompress = const False
      , redirectCount = 10 -- FIXMEE: Why is this reduced to 0 from default 10???
      , cookieJar = Nothing
      , requestVersion = Wai.httpVersion request
      }
  where
    headers    = Wai.requestHeaders request
    mkURI      = rewritePathRule (rewritePath rpConfig) rewURI
    uri        = fromMaybe rewURI mkURI
    reqRuleMap = mkRuleMap $ rewriteRequestRules rpConfig
    host       = T.unpack  $ reversedHost        rpConfig
    rewURI     =
      nullURI{uriAuthority = Just $ URIAuth ""
                                            (maybe host BSC.unpack $ lookup "Host" headers)
                                            "",
              uriPath      = BSC.unpack $ Wai.rawPathInfo    request,
              uriQuery     = BSC.unpack $ Wai.rawQueryString request}


simpleReverseProxy :: Manager -> ReverseProxyConfig -> Wai.Application
simpleReverseProxy mgr rpConfig request sendResponse = bracket
    (NHC.responseOpen proxiedRequest mgr)
    responseClose
    $ \res -> sendResponse $ Wai.responseStream
        (responseStatus res)
        (rewriteHeaders respRuleMap $ responseHeaders res)
        (sendBody $ responseBody res)
  where
    (proxiedRequest, mRewrite) = mkRequest rpConfig request
    respRuleMap = mkRuleMap $ rewriteResponseRules rpConfig
    sendBody body send _flush = fix $ \loop -> do
        bs <- body
        unless (S.null bs) $ do
            () <- send $ fromByteString bs
            loop

data ReverseProxyConfig = ReverseProxyConfig
    { reversedHost :: Text
    , reversedPort :: Int
    , reversedUseSSL :: Bool
    , reversingHost :: Text
    , reversingUseSSL :: !SSLConfig
    , reverseTimeout :: Maybe Int
    , rewriteResponseRules :: Set RewriteRule
    , rewriteRequestRules :: Set RewriteRule
    , rewritePath          :: [RewritePath]
    } deriving (Eq, Ord, Show)

instance FromJSON ReverseProxyConfig where
    parseJSON (Object o) = ReverseProxyConfig
        <$> o .: "reversed-host"
        <*> o .: "reversed-port"
        <*> o .: "reversed-ssl" .!= False
        <*> o .: "reversing-host"
        <*> o .:? "ssl" .!= SSLFalse
        <*> o .:? "timeout" .!= Nothing
        <*> o .:? "rewrite-response" .!= Set.empty
        <*> o .:? "rewrite-request" .!= Set.empty
        <*> o .:? "rewrite-path"     .!= []
    parseJSON _ = fail "Wanted an object"

instance ToJSON ReverseProxyConfig where
    toJSON ReverseProxyConfig {..} = object
        [ "reversed-host" .= reversedHost
        , "reversed-port" .= reversedPort
        , "reversed-ssl" .= reversedUseSSL
        , "reversing-host" .= reversingHost
        , "ssl" .= reversingUseSSL
        , "timeout" .= reverseTimeout
        , "rewrite-response" .= rewriteResponseRules
        , "rewrite-request" .= rewriteRequestRules
        , "rewrite-path"     .= rewritePath
        ]

defaultReverseProxyConfig :: ReverseProxyConfig
defaultReverseProxyConfig = ReverseProxyConfig
        { reversedHost = ""
        , reversedPort = 80
        , reversedUseSSL = False
        , reversingHost = ""
        , reversingUseSSL = SSLFalse
        , reverseTimeout = Nothing
        , rewriteResponseRules = Set.empty
        , rewriteRequestRules = Set.empty
        , rewritePath          = []
        }

data RewriteRule = RewriteRule
    { ruleHeader :: Text
    , ruleRegex :: Text
    , ruleReplacement :: Text
    } deriving (Eq, Ord, Show)

instance FromJSON RewriteRule where
    parseJSON (Object o) = RewriteRule
        <$> o .: "header"
        <*> o .: "from"
        <*> o .: "to"
    parseJSON _ = fail "Wanted an object"

instance ToJSON RewriteRule where
    toJSON RewriteRule {..} = object
        [ "header" .= ruleHeader
        , "from" .= ruleRegex
        , "to" .= ruleReplacement
        ]
