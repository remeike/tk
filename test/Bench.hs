{-# LANGUAGE OverloadedStrings #-}

import           Blaze.ByteString.Builder
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Criterion.Main
import           Criterion.Types
import qualified Data.ByteString            as BS
import           Data.ByteString.Char8      (unpack)
import           Data.ByteString.Lazy       (toStrict)
import qualified Data.HashMap.Strict        as H
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.IO          as TIO
import           Data.Time.Clock
import           Data.Time.Format
import           Examples
import           Heist
import qualified Heist.Compiled             as HC
import           Heist.Internal.Types
import qualified Heist.Interpreted          as HI
import qualified Text.XmlHtml               as X

import           TK

main = runBench

runBench :: IO ()
runBench = do
  lib <- TK.loadTemplates "test" defaultOverrides
  es <- read . LT.unpack <$> TIO.readFile "test/entries.txt"
  ps <- read . LT.unpack <$> TIO.readFile "test/people.txt"
  let entries = mapSubs (entrySubs ps) es
  defaultMainWith (defaultConfig {reportFile = Just "report.html"}) [
      bgroup "runTemplate" [ bench "no blanks" $ nfIO $ runTpl tpl1
                         , bench "simple blank" $ nfIO $ runTpl tpl2
                         , bench "applyTemplate" $ nfIO $ runTpl tpl3
                         , bench "mapFills" $ nfIO $ runTpl tpl4
                         , bench "funFill" $ nfIO $ runTpl tpl5
                         , bench "lots of html" $ nfIO $ runBigTpl tpl6]
    , bgroup "render" [ bench "housetab example" $ nfIO $ renderTpl lib es ps ]
    , bgroup "interpreted heist" [
         bench "no blanks" $ nfIO (doHeist "tpl1" tpl1)
       , bench "simple blank" $ nfIO (doHeist "tpl2" tpl2)
       , bench "mapFills" $ nfIO (doHeist "tpl4" tpl4)
       ]
      -- still need compiled Heist
    ]

renderTpl :: Library () IO -> [Entry] -> [Person] -> IO (Maybe Text)
renderTpl lib es ps = renderWith lib (subs [("entries", mapSubs (entrySubs ps) es)]) () ["list"]

runTpl :: Text -> IO Text
runTpl x = evalStateT (runTemplate (parse $ LT.fromStrict x) ["default"] subst tplLib) ()

runBigTpl :: Text -> IO Text
runBigTpl x = evalStateT (runTemplate (parse $ LT.fromStrict x) ["default"] subst positionTplLib) ()


data Person = Person { id        :: Int
                     , accountId :: Int
                     , name      :: Text
                     } deriving (Eq, Show, Read, Ord)


data Entry = Entry { entryId        :: Int
                   , entryAccountId :: Int
                   , whoId          :: Int
                   , description    :: Text
                   , date           :: UTCTime
                   , howmuch        :: Double
                   , whopaysIds     :: [Int]
                   } deriving (Eq, Show, Read)

entrySubs :: [Person] -> Entry -> Substitutions () IO
entrySubs ps (Entry i a w desc dt hm wps) =
  subs [("id", textFill (tshow i))
       ,("account-id", textFill (tshow a))
       ,("who", textFill (name (getP w)))
       ,("description", textFill desc)
       ,("date", textFill
                   (T.pack $ formatTime
                               defaultTimeLocale
                               "%Y-%m-%d"
                               dt))
       ,("howmuch", textFill "blah")
       ,("whopays", mapSubs (\(isl, pi) -> subs [("id", textFill (tshow pi))
                                                ,("name", textFill (name (getP pi)))
                                                ,("sep", textFill $ if isl
                                                                       then ""
                                                                       else ",")])
                              (zip (replicate (length wps - 1) False ++ repeat True) wps))]
  where getP i = fromJust $ lookup i (map (\p -> (Main.id p, p)) ps)

tshow = T.pack . show


splicesI :: MonadIO m => Splices (HI.Splice m)
splicesI = do "site-title" ## siteTitleSpliceI
              "name"       ## siteTitleSpliceI
              "skater"     ## skaterSplicesI
              "skaters"    ## skatersSpliceI
    --        "desc"       ## descSplice

siteTitleSpliceI :: MonadIO m => HI.Splice m
siteTitleSpliceI = HI.textSplice "Gotham Girls roster"

skaterSplicesI :: MonadIO m => HI.Splice m
skaterSplicesI = HI.runChildrenWith $ "name" ## HI.textSplice "Amy Roundhouse"

skatersSpliceI :: MonadIO m => HI.Splice m
skatersSpliceI = HI.mapSplices namePositionSpliceI
                 [ ("Bonnie Thunders", "jammer")
                 , ("Donna Matrix", "blocker")
                 , ("V-Diva", "jammer") ]

namePositionSpliceI :: MonadIO m => (Text, Text) -> HI.Splice m
namePositionSpliceI (n, p) =
  HI.runChildrenWith $ do
    "name"     ## HI.textSplice n
    "position" ## HI.textSplice p

{- How to get attributes?
descSplice :: MonadIO m => HI.Splice m
descSplice = undefined
-}

{-
splicesC :: MonadIO m => Splices (HC.Splice m)
splicesC = do "site-title" ## siteTitleSpliceC
              "name"       ## siteTitleSpliceC
    --        "skater"     ## skaterSplicesC
    --        "skaters"    ## skatersSpliceC
    --         "desc"       ## descSplice

siteTitleSpliceC :: MonadIO m => HC.Splice m
siteTitleSpliceC = HC.textSplice "Gotham Girls roster"
-}

{- How to do `runChildrenWith` in HC?
skaterSplicesC :: MonadIO m => HC.Splice m
skaterSplicesC = HC.runChildrenWith $ "name" ## HC.textSplice "Amy Roundhouse"
-}

{-
skatersSpliceC :: MonadIO m => HC.Splice m
skatersSpliceC = HC.deferMany namePositionSpliceC
                [("Bonnie Thunders", "jammer")
                ,("Donna Matrix", "blocker")
                ,("V-Diva", "jammer")]

namePositionSpliceC :: MonadIO m => (Text, Text) -> HC.Splice m
namePositionSpliceC (n, p) =
  HC.runChildrenWith $ do
    "name" ## HC.textSplice n
    "position" ## HC.textSplice p
-}

doHeist :: BS.ByteString -> Text -> IO Text
doHeist tplName htpl = do
  eitherHs <- runEitherT $ initHeist (heistConf tplName htpl)
  let hs = case eitherHs of
            Left e -> error $ concat e
            Right x -> x
  mTextMIME <- HI.renderTemplate hs tplName
  case mTextMIME of
   Nothing -> error "blah"
   Just (html,_) -> return $ decodeUtf8 $ toStrict $ toLazyByteString html

heistConf :: BS.ByteString -> Text -> HeistConfig IO
heistConf tplName htpl = HeistConfig sc "" False
  where
    sc = mempty &
         scInterpretedSplices .~ splicesI &
         scTemplateLocations .~ [EitherT $ return (hTplRepo tplName htpl)]

hTplRepo :: BS.ByteString -> Text -> Either [String] TemplateRepo
hTplRepo tplName htpl =
  case parsed of
    Left e -> Left [e]
    Right d -> Right (H.fromList [([tplName], d)])
  where parsed = fmap (\x -> DocumentFile x mBSTplName) docFile
        mBSTplName = Just (unpack tplName)
        docFile = X.parseHTML (unpack tplName) (encodeUtf8 htpl)
