{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Types.Status(ok200)
import qualified Network.Wai.Handler.Warp as Warp
import Keter.Config.V10
import           Control.Concurrent        (forkIO)
import Data.Maybe (isJust)
import Keter.LabelMap as LM
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad
import           Control.Exception          (SomeException)
import           Network.HTTP.Conduit              (Manager)
import Data.ByteString(ByteString)
import qualified Network.Wreq as Wreq
import Control.Monad.STM
import Control.Concurrent.STM.TQueue
import qualified Network.Wai                       as Wai
import qualified Network.HTTP.Conduit      as HTTP
import Keter.Proxy

import qualified RewritePathSpec
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.Hspec (TreatPendingAs(..), testSpec)

main :: IO ()
main = defaultMain keterTests

keterTests :: TestTree
keterTests =
  testGroup
    "Tests"
    [ testCase "Subdomain Integrity" caseSubdomainIntegrity
    , testCase "Head then post doesn't crash" headThenPostNoCrash
    , testCase "Wildcard Domains" caseWildcards
    , localOption TreatPendingAsSuccess
    $ unsafePerformIO (testSpec "Rewriting vendor-extension" RewritePathSpec.spec)
    ]

caseSubdomainIntegrity :: IO ()
caseSubdomainIntegrity = do
  let test0 = LM.empty
      test1 = LM.insert "someapp.com" () test0
      test2 = LM.insert "api.someapp.com" () test1
      test3a = LM.delete "someapp.com" test2
      test3b = LM.insert "api.someapp.com" () test0 -- case from the bug report
      msg = "Subdomains inserted and deleted between bundles"
  assertBool msg $ test3a == test3b

caseWildcards :: IO ()
caseWildcards = do
  let test0 = LM.empty
      test1 = LM.insert "*.someapp.com" () test0
      test2 = LM.lookup "a.someapp.com" test1
      msg = "Wildcards domains"
  assertBool msg $ isJust test2

headThenPostNoCrash :: IO ()
headThenPostNoCrash = do
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  exceptions <- newTQueueIO

  void . forkIO $ do
    Warp.run 6781 $ \req resp -> do
      void $ Wai.strictRequestBody req
      resp $ Wai.responseLBS ok200 [] "ok"

  void . forkIO $ do
    reverseProxy (settings exceptions manager) $ LPInsecure "*" 6780

  void $ Wreq.head_ "http://localhost:6780"

  void $ Wreq.post "http://localhost:6780" content

  found <- atomically $ flushTQueue exceptions
  assertBool ("the list is not empty " <> show found) (null found)
  where
    content :: ByteString
    content = "a"

    settings :: TQueue (Wai.Request, SomeException) ->  Manager -> ProxySettings
    settings expections manager = MkProxySettings {
        psHostLookup     = const $ pure $ Just ((PAPort 6781 Nothing [], False), error "unused tls certificate")
      , psManager        = manager
      , psUnkownHost     = const ""
      , psMissingHost    = ""
      , psProxyException = ""
      , psLogException   = \req exception ->
          atomically $ writeTQueue expections (req, exception)
      , psIpFromHeader   = False
      , psConnectionTimeBound = 5 * 60 * 1000
      }
