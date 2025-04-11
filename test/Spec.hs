{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE TypeSynonymInstances #-}


--------------------------------------------------------------------------------
import           Control.Concurrent.MVar  ( newEmptyMVar, putMVar, takeMVar )
import           Control.Exception        ( Exception, throw, try )
import           Lens.Micro
import           Control.Monad.State      ( StateT(..)
                                          , evalStateT
                                          , get
                                          , modify
                                          , runStateT
                                          )
import qualified Control.Monad.State     as S
import           Control.Monad.Trans      ( liftIO )
import qualified Data.ByteString.Lazy    as LBytes
import qualified Data.Aeson              as Aeson
import qualified Data.Map                as M
import           Data.Maybe               ( fromMaybe )
import           Data.Text                ( Text )
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import           Data.Typeable
import           Test.Hspec
import qualified Test.Hspec.Core.Spec    as H
--------------------------------------------------------------------------------
import           Examples
import           TK
--------------------------------------------------------------------------------


infix  4 .=
(.=) :: S.MonadState s m => ASetter s s a b -> b -> m ()
l .= b = modify (l .~ b)
{-# INLINE (.=) #-}

data LarcenyState =
  LarcenyState
    { _lPath      :: [Text]
    , _lSubs      :: Substitutions () IO
    , _lLib       :: Library () IO
    , _lSettings  :: Settings IO
    }

lPath :: Lens' LarcenyState [Text]
lPath = lens _lPath (\ls p -> ls { _lPath = p })
lSubs :: Lens' LarcenyState (Substitutions () IO)
lSubs = lens _lSubs (\ls s -> ls { _lSubs = s })
lLib :: Lens' LarcenyState (Library () IO)
lLib = lens _lLib (\ls l -> ls { _lLib = l })
lSettings :: Lens' LarcenyState (Settings IO)
lSettings = lens _lSettings (\ls s -> ls { _lSettings = s })


lOverrides :: Overrides -> LarcenyHspecM ()
lOverrides o = do
  (LarcenyHspecState _ (LarcenyState _ _ _ settings)) <- S.get
  hLarcenyState.lSettings .= settings { setOverrides = o }


type LarcenyHspecM =
  StateT LarcenyHspecState IO


data LarcenyHspecState =
  LarcenyHspecState
    { _hResult       :: H.Result
    , _hLarcenyState :: LarcenyState
    }


hResult :: Lens' LarcenyHspecState H.Result
hResult = lens _hResult (\hs r -> hs { _hResult = r })
hLarcenyState :: Lens' LarcenyHspecState LarcenyState
hLarcenyState = lens _hLarcenyState (\hs ls -> hs { _hLarcenyState = ls })

instance H.Example (LarcenyHspecM ()) where
  type Arg (LarcenyHspecM ()) = LarcenyHspecState
  evaluateExample s _params actionWithToIO _progCallback =
    do mv <- newEmptyMVar
       actionWithToIO $ \st ->
         do r <- do ((), larcenyHspecState) <- runStateT s st
                    return (larcenyHspecState ^. hResult)
            putMVar mv r
       takeMVar mv

withLarceny :: SpecWith LarcenyHspecState
            -> Spec
withLarceny spec' =
  let larcenyHspecState =
        LarcenyHspecState (H.Result "" H.Success) (LarcenyState ["default"] mempty mempty defaultSettings) in
  afterAll return $
    before (return larcenyHspecState) spec'

setResult :: H.ResultStatus -> LarcenyHspecM ()
setResult r = case r of
                H.Success -> hResult .= H.Result "" r
                _ -> throw r

tpl4Output :: Text
tpl4Output = "\
\        <body>                         \
\          <h1>                         \
\            Gotham Girls Roller Derby  \
\          </h1>                        \
\          <ul>                         \
\            <li>                       \
\              <h2>Bonnie Thunders</h2> \
\              <p>jammer</p>            \
\              <p>A really long...</p>  \
\            </li>                      \
\            <li>                       \
\              <h2>Donna Matrix</h2>    \
\              <p>blocker</p>           \
\              <p>A really long...</p>  \
\            </li>                      \
\            <li>                       \
\              <h2>V-Diva</h2>          \
\              <p>jammer</p>            \
\              <p>A really long...</p>  \
\            </li>                      \
\          </ul>                        \
\        </body>"

newtype SomeError = SomeError Text deriving (Eq, Show)

instance Exception SomeError

removeSpaces :: Text -> Text
removeSpaces = T.replace " " ""

renderM :: Text -> LarcenyHspecM Text
renderM templateText = do
  (LarcenyHspecState _ (LarcenyState p s l settings)) <- S.get
  let tpl = parseWithSettings settings (LT.fromStrict templateText)
  fmap toHtml $ liftIO $ fmap fst $ evalStateT (runTemplate tpl p s l) ()

renderJson :: Text -> LarcenyHspecM Text
renderJson templateText = do
  (LarcenyHspecState _ (LarcenyState p s l settings)) <- S.get
  let tpl = parseWithSettings settings (LT.fromStrict templateText)
  fmap (T.decodeUtf8 . LBytes.toStrict . Aeson.encode . toJson)
    $ liftIO
    $ fmap fst
    $ evalStateT (runTemplate tpl p s l) ()


shouldRenderJson :: Text -> Text -> LarcenyHspecM ()
shouldRenderJson template output = do
  rendered <- renderJson template
  liftIO $ shouldBe rendered output


shouldRenderM :: Text -> Text -> LarcenyHspecM ()
shouldRenderM templateText output = do
  rendered <- renderM templateText
  if removeSpaces rendered == removeSpaces output
    then setResult H.Success
    else let msg = T.unpack $ rendered <> " doesn't match " <> output in
         setResult (H.Failure Nothing (H.Reason msg))

shouldRenderContainingM :: Text -> Text -> LarcenyHspecM ()
shouldRenderContainingM templateText excerpt = do
  rendered <- renderM templateText
  if excerpt `T.isInfixOf` rendered
  then setResult H.Success
  else let msg = T.unpack $ excerpt <> " not found in " <> templateText in
       setResult (H.Failure Nothing (H.Reason msg))

shouldErrorM :: (Exception a, Eq a) => Text -> Selector a -> LarcenyHspecM ()
shouldErrorM templateText p =
   do hspecState <- S.get
      let renderAttempt = evalStateT (renderM templateText) hspecState
      result <- liftIO $ do
        let forceRenderAttempt = do !result <- renderAttempt
                                    return result
        r <- try forceRenderAttempt
        case r of
          Right _ ->
               return $ H.Failure Nothing $
                 H.Reason ("rendered successfully instead of throwing expected exception: " <>
                 exceptionType)
          Left e ->
            if p e then return H.Success
                   else return $ H.Failure Nothing $ H.Reason ("did not get expected exception: " <>
                        exceptionType <> ", got this exception instead: " <> show e)
      setResult result
  where exceptionType = (show . typeOf . instanceOf) p
        instanceOf :: Selector a -> a
        instanceOf _ = error "Test.Hspec.Expectations.shouldThrow: broken Typeable instance"

main :: IO ()
main = spec

spec :: IO ()
spec = hspec $ do
  withLarceny $ do
    describe "shorthand" $ do
      it "should use templating shorthand to render inner nodes" $ do
        hLarcenyState.lSubs .=
          subs
            [ ("skater", textFill "Beyonslay")
            , ("parent", fillChildrenWith $ subs [("child", textFill "toy")])
            , ("grand-parent",
                fillChildrenWith $ subs
                  [ ("parent"
                    , fillChildrenWith $
                        subs
                          [ ("child", textFill "onesie")
                          , ("cousin", useAttrs (a "name") $ \name -> textFill name)
                          ]
                    )
                  ]
              )
            ]

        "<p><parent.child/></p>" `shouldRenderM` "<p>toy</p>"
        "<p><grand-parent.parent.child/></p>" `shouldRenderM` "<p>onesie</p>"
        "<p><grand-parent.parent.cousin name=\"Slay\"/></p>" `shouldRenderM` "<p>Slay</p>"
        "<p><grand-parent.parent>My: <child/></grand-parent.parent></p>"
          `shouldRenderM` "<p>My: onesie</p>"

    describe "short circuiting rendering" $ do
      it "should implement a sort of pattern matching fill" $ do
        hLarcenyState.lSubs .=
          subs
            [ ( "case"
              , useAttrs (a "match") $
                  \(match :: Text) ->
                    fillChildrenWith $
                      subs
                        [ ( "when"
                          , useAttrs (a "on") $
                              \on ->
                                if match == on then
                                  shortFill fillChildren
                                else
                                  textFill ""
                          )
                        , ( "else"
                          , fillChildren
                          )
                        ]
              )
            ]

        "<case match='dog'>\
          \<when on='dog'><p>Woof</p></when>\
          \<when on='cat'><p>Meow</p></when>\
          \<else><p>Hi there</p></else>\
        \</case>!"
          `shouldRenderM` "<p>Woof</p>!"

        "<case match='cat'>\
          \<when on='dog'><p>Woof</p></when>\
          \<when on='cat'><p>Meow</p></when>\
          \<else><p>Hi there</p></else>\
        \</case>!"
          `shouldRenderM` "<p>Meow</p>!"

        "<case match='human'>\
          \<when on='dog'><p>Woof</p></when>\
          \<when on='cat'><p>Meow</p></when>\
          \<else><p>Hi there</p></else>\
        \</case>!"
          `shouldRenderM` "<p>Hi there</p>!"

    describe "fragments" $ do
      it "should bubble fragment to top" $ do
        hLarcenyState.lSubs .=
          subs
            [ ( "bubble", bubbleFill fillChildren )
            , ( "to1", textFill "Dolly" )
            , ( "to2", textFill "World" )
            , ( "to3", textFill "there" )
            , ( "person1", fillChildrenWith $ subs [("name", textFill "Jane Doe")])
            , ( "person2", fillChildrenWith $ subs [("name", textFill "John Doe")])
            ]

        "<main><bubble><p>Hello <span><to1/></span></p></bubble>\
        \<p>Hey <span><to2/></span></p>\
        \<p>Hi <span><to3/></span></p></main>"
          `shouldRenderM` "<p>Hello <span>Dolly</span></p>"

        "<main><p>Hello <span><to1/></span></p>\
        \<bubble><p>Hey <span><to2/></span></p></bubble>\
        \<p>Hi <span><to3/></span></p></main>"
          `shouldRenderM` "<p>Hey <span>World</span></p>"

        "<main><p>Hello <span><to1/></span></p>\
        \<p>Hey <span><to2/></span></p>\
        \<bubble><p>Hi <span><to3/></span></p></bubble></main>"
          `shouldRenderM` "<p>Hi <span>there</span></p>"

        "<main><p>Hello <bubble><span><to1/></span></bubble></p>\
        \<p>Hey <span><to2/></span></p>\
        \<p>Hi <span><to3/></span></p></main>"
          `shouldRenderM` "<span>Dolly</span>"

        "<main><p>Hello <span><to1/></span></p>\
        \<p>Hey <bubble><span><to2/></span></bubble></p>\
        \<p>Hi <span><to3/></span></p></main>"
          `shouldRenderM` "<span>World</span>"

        "<main><p>Hello <span><to1/></span></p>\
        \<p>Hey <span><to2/></span></p>\
        \<p>Hi <bubble><span><to3/></span></bubble></p></main>"
          `shouldRenderM` "<span>there</span>"

        "<main><div><p>Ok... </p><person1><bubble><p>Hi, <name/></p></bubble></person1></div>\
        \<div><p>Ok... </p><person2><p>Hi, <name/></p></person2></div></main>"
          `shouldRenderM` "<p>Hi, Jane Doe</p>"

        "<main><div><p>Ok... </p><person1><p>Hi, <name/></p></person1></div>\
        \<div><p>Ok... </p><person2><bubble><p>Hi, <name/></p></bubble></person2></div></main>"
          `shouldRenderM` "<p>Hi, John Doe</p>"

      it "should use match to set fragment" $ do
        let
          fragSubs =
            subs
              [ ( "fragment"
                , useAttrs (a "key" % a "match") $
                    \x y ->
                      if x == (y :: Text) then
                        bubbleFill fillChildren
                      else
                        fillChildren
                )
              ]

          tpl =
            "<main><fragment key='dolly' match='${frag}'><p>Hello <span>Dolly</span></p></fragment>\
            \<fragment key='world' match='${frag}'><p>Hey <span>World</span></p></fragment>\
            \<fragment key='there' match='${frag}'><p>Hi <span>there</span></p></fragment></main>"

        hLarcenyState.lSubs .= subs [("frag", textFill "dolly")] <> fragSubs
        tpl `shouldRenderM` "<p>Hello <span>Dolly</span></p>"

        hLarcenyState.lSubs .= subs [("frag", textFill "world")] <> fragSubs
        tpl `shouldRenderM` "<p>Hey <span>World</span></p>"

        hLarcenyState.lSubs .= subs [("frag", textFill "there")] <> fragSubs
        tpl `shouldRenderM` "<p>Hi <span>there</span></p>"

      it "should not run actions for nodes that are not rendered" $ do
        let
          tplSubs =
            subs
              [ ( "bubble", bubbleFill fillChildren )
              , ( "who"
                , Fill $
                    \_ _ _ -> do
                      modify (+1)
                      return $ TextOutput "world"
                )
              , ( "count"
                , Fill $
                    \_ _ _ -> do
                      n <- get
                      return $ TextOutput (T.pack $ show n)
                )
              ]

          tpl =
            "<main><p>Hi <who/> (<count/>)</p>\
            \<bubble><p>Hey <who/> (<count/>)</p></bubble>\
            \<p>Hello <who/> (<count/>)</p></main>"

          runTpl =
            fmap fst $ runTemplate (parse tpl) ["default"] tplSubs mempty :: StateT Int IO Output

        (output, n) <- liftIO $ runStateT runTpl 0
        let txt = toHtml output
        liftIO $ txt `shouldBe` "<p>Hey world (2)</p>"
        liftIO $ n `shouldBe` 2
        setResult H.Success

    describe "json" $ do
      it "should render values" $ do
        "<j:value bool='True'/>" `shouldRenderJson` "true"
        "<j:value bool='4' j:def='False'/>" `shouldRenderJson` "false"
        "<j:value bool='4'/>" `shouldRenderJson` "null"

        "<j:value number='21'/>" `shouldRenderJson` "21"
        "<j:value number='Hello' j:def='0'/>" `shouldRenderJson` "0"
        "<j:value number='Hello'/>" `shouldRenderJson` "null"

        "<j:value string='Hello'/>" `shouldRenderJson` "\"Hello\""
        "<j:value string=''/>" `shouldRenderJson` "\"\""

        "<j:value field='True'/>" `shouldRenderJson` "true"
        "<j:value field='21'/>" `shouldRenderJson` "21"
        "<j:value field='Hello'/>" `shouldRenderJson` "\"Hello\""
        "<j:value field=''/>" `shouldRenderJson` "\"\""
        "<j:value field='null'/>" `shouldRenderJson` "null"

      it "should render values with fills" $ do
        hLarcenyState.lSubs .=
          subs
            [ ( "nums"
              , fillChildrenWith $ subs [("good", textFill "1"), ("bad", textFill "NaN")]
              )
            , ( "bools"
              , fillChildrenWith $ subs [("good", textFill "True"), ("bad", textFill "No")]
              )
            , ( "names"
              , fillChildrenWith $ subs [("hello", textFill "Dolly"), ("hi", textFill "World")]
              )
            ]

        "<bools><j:value bool='${good}'/></bools>" `shouldRenderJson` "true"
        "<bools><j:value bool='${bad}' j:def='False'/></bools>" `shouldRenderJson` "false"
        "<bools><j:value bool='${bad}'/></bools>" `shouldRenderJson` "null"

        "<nums><j:value number='${good}'/></nums>" `shouldRenderJson` "1"
        "<nums><j:value number='${bad}' j:def='0'/></nums>" `shouldRenderJson` "0"
        "<nums><j:value number='${bad}'/></nums>" `shouldRenderJson` "null"

        "<names><j:value string='${hello}'/></names>" `shouldRenderJson` "\"Dolly\""
        "<names><j:value string='${hi}'/></names>" `shouldRenderJson` "\"World\""

      it "should render objects" $ do
        hLarcenyState.lSubs .=
          subs
            [ ( "person-a"
              , fillChildrenWith $
                  subs
                    [ ("name", textFill "Jane Doe")
                    , ("age", textFill "28")
                    , ("employed", textFill "True")
                    , ("cat", fillChildrenWith $ subs [("name", textFill "Fluffer"), ("age", textFill "7")])
                    ]
              )
            , ( "person-b"
              , fillChildrenWith $
                  subs
                    [ ("name", textFill "John Doe")
                    , ("age", textFill "26")
                    , ("employed", textFill "False")
                    ]
              )
            ]

        "<j:object><person-a><j:string name='${name}'/><j:number age='${age}'/><j:bool employed='${employed}'/></person-a></j:object>"
          `shouldRenderJson` "{\"age\":28,\"employed\":true,\"name\":\"Jane Doe\"}"

        "<person-b><j:object><j:string name='${name}'/><j:number age='${age}'/><j:bool employed='${employed}'/></j:object></person-b>"
          `shouldRenderJson` "{\"age\":26,\"employed\":false,\"name\":\"John Doe\"}"

        "<person-a><j:object><j:string name='${name}'/><j:object pet><cat><j:string name='${name}'/><j:string age='${age}'/></cat></j:object></j:object></person-a>"
          `shouldRenderJson` "{\"name\":\"Jane Doe\",\"pet\":{\"age\":\"7\",\"name\":\"Fluffer\"}}"

      it "should render object with embeded json" $ do
        "<j:object><j:string name='Jane Doe'/><j:object pet='{\"age\":\"7\",\"name\":\"Fluffer\"}'/></j:object>"
          `shouldRenderJson` "{\"name\":\"Jane Doe\",\"pet\":{\"age\":\"7\",\"name\":\"Fluffer\"}}"

      it "should render arrays" $ do
        hLarcenyState.lSubs .=
          subs
            [ ( "persons"
              , mapSubs
                  ( \(name, age, pets) ->
                      subs
                        [ ("name", textFill name)
                        , ("age", textFill age)
                        , ("pets", mapSubs (\(name', age') -> subs [("name", textFill name'), ("age", textFill age')]) pets)
                        ]
                  )
                  [ ("Jane Doe", "28", [("Fluffer", "7"), ("Barks", "5")])
                  , ("John Doe", "35", [("Chuck", "9")])
                  ]
              )
            ]

        "<j:value number='1'/><j:value number='3'/><j:value number='5'/>"
          `shouldRenderJson` "[1,3,5]"

        "<j:object><j:field val='1'/></j:object><j:object><j:field val='2'/></j:object>"
          `shouldRenderJson` "[{\"val\":1},{\"val\":2}]"

        "<persons><j:object><j:string name='${name}'/><j:number age='${age}'/><j:array pets><pets><j:object><j:string name='${name}'/><j:number age='${age}'/></j:object></pets></j:array></j:object></persons>"
          `shouldRenderJson` "[{\"age\":28,\"name\":\"Jane Doe\",\"pets\":[{\"age\":7,\"name\":\"Fluffer\"},{\"age\":5,\"name\":\"Barks\"}]},{\"age\":35,\"name\":\"John Doe\",\"pets\":[{\"age\":9,\"name\":\"Chuck\"}]}]"

    describe "white space" $ do
      it "should remove spaces next to line breaks" $ do
        txt1 <-
          renderM
            "  <p>              \
            \\n    Hello,       \
            \\n    Dolly.       \
            \\n    Hello, World \
            \\n</p>             "

        txt2 <-
          renderM
            "  <p>Hello,        \
            \\n    Dolly.       \
            \\n    Hello, World \
            \\n</p>             "

        liftIO $ txt1 `shouldBe` "<p>Hello, Dolly. Hello, World</p>"
        liftIO $ txt1 `shouldBe` txt2

      it "should reduce white space on lines to just one space" $ do
        txt <-
          renderM
            "   <p>   Hello,  Dolly.    Hi!!!   </p> \
            \\n <p>                                  \
            \\n    Hello,        World.    Hey!      \
            \\n</p>                                  "

        liftIO $ txt `shouldBe` "<p> Hello, Dolly. Hi!!! </p><p>Hello, World. Hey!</p>"

      it "should not remove white space from <pre> tags" $ do
        txt <-
          renderM
            "   <p>   Hello,  Dolly.    Hi!!!   </p> \
            \\n <pre>                                \
            \\n    Here be    Javacript...           \
            \\n    function() { console.log('Yay')}; \
            \\n                                      \
            \\n</pre>                                \
            \\n<div>Back to normal</div>"

        liftIO $
          txt `shouldBe`
            "<p> Hello, Dolly. Hi!!! </p>\
            \<pre>                                \
            \\n    Here be    Javacript...           \
            \\n    function() { console.log('Yay')}; \
            \\n                                      \
            \\n</pre>\
            \<div>Back to normal</div>"

    describe "parse" $ do
      it "should parse HTML into a Template" $ do
        hLarcenyState.lSubs .= subst
        hLarcenyState.lLib .= mempty
        tpl4 `shouldRenderM` tpl4Output

      it "should allow self-closing tags" $ do
        "<br />" `shouldRenderM` "<br />"

      it "should not remove opening tag brackets in strings" $ do
        -- This was a bug in html-conduit: https://github.com/snoyberg/xml/issues/139
        let customComponentScript =
              "<script>console.log(\"<custom-component />\")console.log(\"<custom-component />\")</script>"
        customComponentScript `shouldRenderM` customComponentScript

      it "should return final state" $ do
        let
          mySubs =
            subs
              [ ( "who"
                , Fill $
                    \_ _ _ -> do
                      modify (+1)
                      return $ TextOutput "world"
                )
              , ( "count"
                , Fill $
                    \_ _ _ -> do
                      n <- get
                      return $ TextOutput (T.pack $ show n)
                )
              ]

          myTpl =
            fmap fst $ runTemplate (parse "<p>hello <who/> (<count/>)</p>") ["default"] mySubs mempty :: StateT Int IO Output

        (output, n) <- liftIO $ runStateT myTpl 1
        let txt = toHtml output

        liftIO $ txt `shouldBe` "<p>hello world (2)</p>"
        liftIO $ n `shouldBe` 2
        setResult H.Success

    describe "delayed renders" $ do
      it "should render in specified order" $ do
        let
          splices =
            subs
              [ ( "add"
                , useAttrs (a "w") $
                    \txt ->
                      Fill $
                        \_ _ _ -> do
                          modify (<> [txt])
                          return VoidOutput
                )
              , ( "delayed"
                , delayedFill
                )
              ]

          t1 =
            "The <add w='a'/>quick <add w='b'/>brown <add w='c'/>fox <add w='d'/>"

          t2 =
            "The <delayed n='3'><add w='a'/>quick </delayed>\
            \<delayed n='1'><add w='b'/>brown </delayed>\
            \<delayed n='2'><add w='c'/>fox </delayed>\
            \<add w='d'/>"

          t3 =
            "The <delayed n='1'><add w='a'/>quick </delayed>\
            \<delayed n='1'><add w='b'/>brown </delayed>\
            \<delayed n='3'><add w='c'/>fox </delayed>\
            \<add w='d'/>"

          t4 =
            "The <delayed n='3'><add w='a'/>quick </delayed>\
            \<delayed n='2'><add w='b'/>brown </delayed>\
            \<delayed n='1'><add w='c'/>fox </delayed>\
            \<add w='d'/>"

          runTpl tpl =
            liftIO $ runStateT (fmap fst $ runTemplate (parse tpl) ["default"] splices mempty :: StateT [Text] IO Output) []

        (output1, letters1) <- runTpl t1
        liftIO $ toHtml output1 `shouldBe` " The quick brown fox "
        liftIO $ letters1 `shouldBe` ["a","b","c","d"]

        (output2, letters2) <- runTpl t2
        liftIO $ toHtml output2 `shouldBe` " The quick brown fox "
        liftIO $ letters2 `shouldBe` ["d","b","c","a"]

        (output3, letters3) <- runTpl t3
        liftIO $ toHtml output3 `shouldBe` " The quick brown fox "
        liftIO $ letters3 `shouldBe` ["d","a","b","c"]

        (output4, letters4) <- runTpl t4
        liftIO $ toHtml output4 `shouldBe` " The quick brown fox "
        liftIO $ letters4 `shouldBe` ["d","c","b","a"]
        setResult H.Success

      it "should render with wrapping substitutions" $ do
        let
          splices =
            subs
              [ ( "add"
                , useAttrs (a "w") $
                    \txt ->
                      Fill $
                        \_ _ _ -> do
                          modify (<> [txt])
                          return VoidOutput
                )
              , ( "delayed"
                , delayedFill
                )
              , ( "subject1"
                , fillChildrenWith $ subs [("color", textFill "purple"), ("animal", textFill "cat")]
                )
              , ( "subject2"
                , fillChildrenWith $ subs [("color", textFill "black"), ("animal", textFill "sheep")]
                )
              ]

          t1 =
            "The <delayed n='3'><add w='a'/>quick </delayed>\
            \<subject1><delayed n='1'><add w='b'/><color/> </delayed></subject1>\
            \<subject2><delayed n='2'><add w='c'/><animal/> </delayed></subject2>\
            \<add w='d'/>"

          t2 =
            "The <delayed n='3'><add w='a'/>quick </delayed>\
            \<subject1><delayed n='1'><add w='b'/><color/> </delayed>\
            \<delayed n='2'><add w='c'/><animal/> </delayed></subject1>\
            \<add w='d'/>"

          runTpl tpl =
            liftIO $ runStateT (fmap fst $ runTemplate (parse tpl) ["default"] splices mempty :: StateT [Text] IO Output) []

        (output1, letters1) <- runTpl t1
        liftIO $ toHtml output1 `shouldBe` " The quick purple sheep "
        liftIO $ letters1 `shouldBe` ["b","c","d","a"]

        (output2, letters2) <- runTpl t2
        liftIO $ print letters2
        liftIO $ print $ toHtml output2
        liftIO $ toHtml output2 `shouldBe` " The quick purple cat "
        liftIO $ letters2 `shouldBe` ["b","c","d","a"]

        setResult H.Success

    describe "xml" $ do
      it "should render xml" $ do
        "<x:hello><x:world>Hi</x:world></x:hello>" `shouldRenderM`
          "<hello><world>Hi</world></hello>"

    describe "add" $ do
      it "should allow overriden tags" $ do
        hLarcenyState.lSubs .= subst
        "<name /><skater><name /></skater>" `shouldRenderM` "Gotham Girls Amy Roundhouse"

    describe "apply" $ do
      it "should allow templates to be included in other templates" $ do
        hLarcenyState.lLib .= M.fromList [(["hello"], parse "hello")]
        "<apply template=\"hello\" />" `shouldRenderM` "hello"

      it "should allow templates with unfilled holes to be included in other templates" $ do
        hLarcenyState.lSubs .= subs [("alias", textFill "Fifi Nomenom")]
        hLarcenyState.lLib .= M.fromList [(["skater"], parse "<alias />")]
        "<apply template=\"skater\" />" `shouldRenderM` "Fifi Nomenom"

      it "should allow templates to be included in other templates" $ do
        hLarcenyState.lLib .= M.fromList [(["skater"], parse "<apply-content />")]
        "<apply template=\"skater\">V-Diva</apply>" `shouldRenderM` "V-Diva"

      it "should allow compicated templates to be included in other templates" $ do
        let lib = M.fromList [(["_base"], parse "<h1><siteTitle /></h1>\
                                              \<apply-content />")]
        hLarcenyState.lSubs .= subs [("siteTitle", textFill "Ohio Roller Girls")]
        hLarcenyState.lLib .= lib
        "<apply template=\"_base\"><p>The Smacktivist</p></apply>" `shouldRenderM`
          "<h1>Ohio Roller Girls</h1>\
          \<p>The Smacktivist</p>"

      it "should look higher in tree for matching template" $ do
        hLarcenyState.lPath .= ["foo","bar"]
        hLarcenyState.lLib .= M.fromList [(["base"], parse "hello")]
        "<apply template=\"base\" />" `shouldRenderM` "hello"

      it "should look first look in same directory for matching template" $ do
         let lib = M.fromList [(["base"], parse "hello")
                              ,(["foo", "base"], parse "goodbye")]
         hLarcenyState.lLib .= lib
         hLarcenyState.lPath .= ["foo","bar"]
         "<apply template=\"base\" />" `shouldRenderM` "goodbye"

      it "should traverse down via slashes" $ do
         let lib = M.fromList [(["base"], parse "hello")
                              ,(["foo", "base"], parse "goodbye")]
         hLarcenyState.lLib .= lib
         "<apply template=\"foo/base\" />" `shouldRenderM` "goodbye"

      it "should only truncate parts from current path, not specified template path" $ do
         hLarcenyState.lLib .= M.fromList [(["baz"], parse "hello")]
         hLarcenyState.lPath .= ["foo"]
         "<apply template=\"bar/baz\" />" `shouldErrorM` (== (ApplyError ["bar","baz"] ["foo"]))

      it "should use the path to the applied template when looking" $ do
         let lib = M.fromList [(["base"], parse "hello")
                              ,(["foo", "bar", "base"], parse "goodbye")
                              ,(["foo", "bar", "baz"], parse "<apply template=\"base\"/>")]
         hLarcenyState.lLib .= lib
         "<apply template=\"foo/bar/baz\" />" `shouldRenderM` "goodbye"

      it "should use the path to the applied template when looking" $ do
         let lib = M.fromList [(["default", "x"], parse "hello")
                             ,(["foo", "bar", "baz"], parse "<apply-content/>")]
         hLarcenyState.lLib .= lib
         hLarcenyState.lPath .= ["default", "hello"]
         "<apply template=\"foo/bar/baz\"><apply template=\"x\"/></apply>" `shouldRenderM` "hello"

      it "should allow blanks in the the template name" $ do
        let lib = M.fromList [(["zone1-currentIssue"], parse "Current Issue")]
        hLarcenyState.lLib .= lib
        hLarcenyState.lSubs .= subs [("zone1", textFill "zone1-currentIssue")]
        "<apply template=\"${zone1}\" />" `shouldRenderM` "Current Issue"

      it "should apply splices from outer template to apply-content" $ do
        hLarcenyState.lSubs .=
          subs
            [ ( "recipient", fillChildrenWith $ subs [("name", textFill "Jane Doe")] )
            , ( "message", textFill "How are you doing?" )
            , ( "signer", textFill "John Doe")
            ]
        let lib = M.fromList [(["base"], parse "Dear <recipient><apply-content/></recipient> Yours truly,")]
        hLarcenyState.lLib .= lib
        "<apply template=\"base\"><name/>, <message/></apply> <signer/>"
           `shouldRenderM` "Dear Jane Doe, How are you doing? Yours truly, John Doe"

    describe "overriding HTML tags" $ do
      it "should allow overriden Html tags" $ do
        hLarcenyState.lSubs .= subs [("div", textFill "notadivatall")]
        lOverrides $ Overrides mempty ["div"] mempty
        "<html><div></div></html>" `shouldRenderM` "<html>not a div at all</html>"

      it "should allow (nested) overriden Html tags" $ do
        hLarcenyState.lSubs .= subs [("div", textFill "notadivatall")
                                    ,("custom", fillChildrenWith mempty)]
        lOverrides $ Overrides mempty ["div"] mempty
        "<html><custom><div></div></custom></html>"
          `shouldRenderM` "<html>not a div at all</html>"

      it "should not need fills for manually added plain nodes" $ do
        lOverrides $ Overrides ["blink"] mempty mempty
        "<html><blink>retro!!</blink></html>"
          `shouldRenderM` "<html><blink>retro!!</blink></html>"

      it "should allow custom self-closing tags" $ do
        lOverrides $ Overrides ["blink"] mempty ["blink"]
        "<blink />" `shouldRenderM` "<blink />"

    describe "bind" $ do
      it "should let you bind tags to fills within templates" $
        "<bind tag=\"sport\">Roller derby</bind><sport />" `shouldRenderM` "Roller derby"

      it "should let you use binds within binds" $ do
         "<bind tag=\"sport\"> \
         \  <bind tag=\"adjective\">awesome</bind> \
         \  Roller derby is <adjective /> \
         \</bind> \
         \<sport />" `shouldRenderM` "Roller derby is awesome"

      it "should let you bind with nested blanks" $ do
        hLarcenyState.lSubs .= subs [("adjective", textFill "awesome")]
        "<bind tag=\"sport\">Roller derby is <adjective /></bind><sport />"
          `shouldRenderM` "Roller derby is awesome"

      it "should apply binds to applied templates" $ do
        let lib = M.fromList [(["blah"], parse "<apply-content /><foo />")]
        hLarcenyState.lLib .=  lib
        "<bind tag=\"foo\">         \
         \  Fill this in             \
         \</bind>                    \
         \<apply template=\"blah\">  \
         \  <foo />                  \
         \</apply>" `shouldRenderM` "Fill this in Fill this in"

      it "should not let binds escape the apply-content tag" $ do
        hLarcenyState.lSubs .= fallbackSub (Fill $ \_ _ _ -> throw $ SomeError "not found!")
        let lib = M.fromList [(["blah"], parse "<apply-content /><foo />")]
        hLarcenyState.lLib .= lib
        "<apply template=\"blah\"> \
         \  <bind tag=\"foo\">      \
         \    Fill this in          \
         \  </bind>                 \
         \  <foo />                 \
         \</apply>"
           `shouldErrorM` (== SomeError "not found!")

      it "should use bind tags from applied template" $ do
        let lib = M.fromList [(["blah"], parse "<bind tag='foo'>fill this in<arg:target/></bind><bar/>")]
        hLarcenyState.lLib .= lib
        "<bind tag='bar'>Please!</bind>\
         \<apply template=\"blah\"/>  \
         \ Please <foo target=', dude'/>. <bar/>"
           `shouldRenderM` "Please! Please fill this in, dude. Please!"

      it "shouldn't matter if there's no `tag` attribute" $ do
        "<bind>This won't ever be rendered!!</bind>\
        \<p>Since the non-existent tag is never referenced \
        \or rendered, it won't blow up.</p>"
          `shouldRenderM`
          "<p>Since the non-existent tag is never referenced \
          \or rendered, it won't blow up.</p>"

      it "should pass arguments to bind tag" $ do
        hLarcenyState.lSubs .=
          subs
            [ ( "person1", fillChildrenWith $ subs [("name", textFill "Jane Doe")])
            , ( "person2", fillChildrenWith $ subs [("name", textFill "John Doe")])
            ]

        "<bind tag='intro' name='Mario'><p>Hi there, <arg:name/></p></bind>\
        \<person1><intro name='${name}'/></person1>\
        \<person2><intro name='${name}'/></person2>\
        \<intro name='Mario'/>\
        \<intro name='<strong>Mario!</strong>'/>"
          `shouldRenderM`
          "<p>Hi there, Jane Doe</p>\
          \<p>Hi there, John Doe</p>\
          \<p>Hi there, Mario</p>\
          \<p>Hi there, <strong>Mario!</strong></p>"

      it "should turn bind tag into an alias for another fill" $ do
        hLarcenyState.lSubs .=
          subs
            [ ( "person1", fillChildrenWith $ subs [("name", textFill "Jane Doe")])
            , ( "person2", fillChildrenWith $ subs [("name", textFill "John Doe")])
            ]

        "<bind tag='intro' assign='person1'/><intro><p>Hi <name/></p></intro>"
          `shouldRenderM` "<p>Hi Jane Doe</p>"

      it "should use outer bind tag within the apply-content tag" $ do
        let lib = M.fromList [(["base"], parse "<bind tag='foo'>Bar</bind>Hey<apply-content/>")]
        hLarcenyState.lLib .= lib
        "<apply template=\"base\"> \
         \  Yo                 \
         \  <foo/>\
         \</apply>"
           `shouldRenderM` "Hey Yo Bar"

    describe "mapSubs" $ do
      it "should map the subs over a list" $ do
        hLarcenyState.lSubs .= subst
        tpl4 `shouldRenderM` tpl4Output

    describe "writing functions" $ do
      it "should allow you to write functions for fills" $ do
        let subs' =
              subs [("desc",
                     Fill $ \m _t _l -> return $ TextOutput $ T.take (read $ T.unpack (m M.! "length"))
                                        "A really long description"
                                        <> "...")]
        hLarcenyState.lSubs .= subs'
        "<l:desc length=\"10\" />" `shouldRenderM` "A really l..."

      it "should allow you to use IO in fills" $ do
        let subs' =
              subs [("desc", Fill $
                          \m _t _l -> do liftIO $ putStrLn "***********\nHello World\n***********"
                                         return $ TextOutput $ T.take (read $ T.unpack (m M.! "length"))
                                           "A really long description"
                                           <> "...")]
        hLarcenyState.lSubs .= subs'
        "<l:desc length=\"10\" />" `shouldRenderM` "A really l..."

    describe "attributes" $ do
      it "should apply substitutions to attributes as well" $ do
        hLarcenyState.lSubs .= subs [("skater", textFill "Beyonslay")]
        "<p id=\"${skater}\"><skater /></p>"
          `shouldRenderM` "<p id=\"Beyonslay\">Beyonslay</p>"

      it "should apply substitutions with attributes to attributes" $ do
        hLarcenyState.lSubs .=
          subs
            [ ("skater", textFill "Beyonslay")
            , ("color", useAttrs (a"set")(textFill . fromMaybe "brown"))
            ]
        "<p id=\"${color}\"><skater /></p>"
          `shouldRenderM` "<p id=\"brown\">Beyonslay</p>"
        "<p id=\"${color?set=yellow}\"><skater /></p>"
          `shouldRenderM` "<p id=\"yellow\">Beyonslay</p>"


      it "should apply nested substitutions with attributes to attributes" $ do
        hLarcenyState.lSubs .=
          subs
            [ ("skater", textFill "Beyonslay")
            , ("parent", fillChildrenWith $ subs [("child", textFill "toy")])
            , ("grand-parent",
                fillChildrenWith $ subs
                  [ ("parent"
                    , fillChildrenWith $ subs [("child", textFill "onesie")]
                    )
                  ]
              )
            ]
        "<p id=\"${parent.child}\"><skater /></p>"
          `shouldRenderM` "<p id=\"toy\">Beyonslay</p>"
        "<p id=\"${grand-parent.parent.child}\"><skater /></p>"
          `shouldRenderM` "<p id=\"onesie\">Beyonslay</p>"

      it "should apply substitutions to attributes inside of blanks" $ do
        hLarcenyState.lSubs .= subs [("skater", useAttrs (a"name")
                                       (\name -> textFill $ "Skater: " <> name))
                                    ,("name", textFill "Beyonslay")]
        "<skater name=\"${name}\"><skater />" `shouldRenderM` "Skater: Beyonslay"

      it "should substitute blanks that are only part of attributes" $ do
        hLarcenyState.lSubs .= subs [("skater", useAttrs (a"name")
                                    (\name -> textFill $ "Skater: " <> name))
                                    ,("name", textFill "Beyonslay")]
        "<skater name=\"the great ${name}\"><skater />"
          `shouldRenderM` "Skater: the great Beyonslay"

      it "should substitute multiple blanks in an attribute" $ do
        hLarcenyState.lSubs .=
          subs [("skater", useAttrs (a"name")
                           (\name -> textFill $ "Skater: " <> name))
               ,("name", textFill "Beyonslay")
               ,("adj", textFill "great")]
        "<skater name=\"the ${adj} ${name}\"><skater />"
          `shouldRenderM` "Skater: the great Beyonslay"

      it "should keep special characters in attribute" $ do
        hLarcenyState.lSubs .= subs [("token", textFill "123")
                                    ,("magazine", textFill "BloodAndThunder")
                                    ,("number", textFill "5")]
        "<a href=\"/s/${token}/${magazine}-${number}.pdf\">Issue 5</a>"
          `shouldRenderM` "<a href=\"/s/123/BloodAndThunder-5.pdf\">Issue 5</a>"

      it "should strip whitespace from beginning and end" $
         "<bind tag=\"someAttr\">\n\
         \        lots of space  \n\
         \</bind> <p class=\"${someAttr}\"></p>"
           `shouldRenderM` "<p class=\"lots of space\"></p>"

      it "should know what the template path is" $ do
        let fill = Fill $ \_ (p, _) _ -> return $ TextOutput (head p)
        hLarcenyState.lSubs .= subs [("template", fill)]
        "<p class=\"${template}\"></p>"
          `shouldRenderM` "<p class=\"default\"></p>"

    describe "ternaries and case expressions in attributes" $ do
      it "should render based on whether value is true" $ do
        hLarcenyState.lSubs .=
          subs
            [ ("yes", textFill "True")
            , ("no", textFill "False")
            , ("hello", textFill "Dolly")
            , ("goodbye", textFill "World")
            ]

        "<input value=\"${yes|hello??goodbye}\"/>"
          `shouldRenderM` "<input value=\"Dolly\"/>"
        "<input value=\"${no|hello??goodbye}\"/>"
          `shouldRenderM` "<input value=\"World\"/>"

        "<input value=\"${yes|hello??}\"/>"
          `shouldRenderM` "<input value=\"Dolly\"/>"
        "<input value=\"${no|hello??}\"/>"
          `shouldRenderM` "<input value/>"
        "<input value=\"${yes|??goodbye}\"/>"
          `shouldRenderM` "<input value/>"
        "<input value=\"${no|??goodbye}\"/>"
          `shouldRenderM` "<input value=\"World\"/>"

        "<input value=\"${yes|'Hell yeah'??'Oh nooo!'}\"/>"
          `shouldRenderM` "<input value=\"Hell yeah\"/>"
        "<input value=\"${no|'Hell yeah'??'Oh nooo!'}\"/>"
          `shouldRenderM` "<input value=\"Oh nooo!\"/>"

        "<input value=\"${'True' | 'Hell yeah' ?? 'Oh nooo!'}\"/>"
          `shouldRenderM` "<input value=\"Hell yeah\"/>"
        "<input value=\"${'False' | 'Hell yeah' ?? 'Oh nooo!'}\"/>"
          `shouldRenderM` "<input value=\"Oh nooo!\"/>"

      it "should render based on whether value exists" $ do
        hLarcenyState.lSubs .=
          subs
            [ ("nothing", textFill "")
            , ("something", textFill "some stuff")
            , ("hello", textFill "Dolly")
            , ("goodbye", textFill "World")
            ]

        "<input value=\"${something|hello!!goodbye}\"/>"
          `shouldRenderM` "<input value=\"Dolly\"/>"
        "<input value=\"${nothing|hello!!goodbye}\"/>"
          `shouldRenderM` "<input value=\"World\"/>"

        "<input value=\"${something|hello!!}\"/>"
          `shouldRenderM` "<input value=\"Dolly\"/>"
        "<input value=\"${nothing|hello!!}\"/>"
          `shouldRenderM` "<input value/>"
        "<input value=\"${something|!!goodbye}\"/>"
          `shouldRenderM` "<input value/>"
        "<input value=\"${nothing|!!goodbye}\"/>"
          `shouldRenderM` "<input value=\"World\"/>"

        "<input value=\"${something|'Hell yeah'!!'Oh nooo!'}\"/>"
          `shouldRenderM` "<input value=\"Hell yeah\"/>"
        "<input value=\"${no|'Hell yeah'!!'Oh nooo!'}\"/>"
          `shouldRenderM` "<input value=\"Oh nooo!\"/>"

        "<input value=\"${'Yerrrr' | 'Hell yeah' !! 'Oh nooo!'}\"/>"
          `shouldRenderM` "<input value=\"Hell yeah\"/>"
        "<input value=\"${'' | 'Hell yeah' !! 'Oh nooo!'}\"/>"
          `shouldRenderM` "<input value=\"Oh nooo!\"/>"

      it "should render based on match" $ do
        hLarcenyState.lSubs .=
          subs
            [ ("cat", textFill "Meow!")
            , ("dog", textFill "Woof!")
            , ("duck", textFill "Quack!")
            , ("tree", textFill "stillness")
            ]

        "<bind tag=\"animal\">cat</bind>\
        \<input value=\"${animal|'cat'->cat;'dog'->dog;'duck'->duck;tree}\"/>"
          `shouldRenderM` "<input value=\"Meow!\"/>"
        "<bind tag=\"animal\">dog</bind>\
        \<input value=\"${animal|'cat'->cat;'dog'->dog;'duck'->duck;tree}\"/>"
          `shouldRenderM` "<input value=\"Woof!\"/>"
        "<bind tag=\"animal\">duck</bind>\
        \<input value=\"${animal|'cat'->cat;'dog'->dog;'duck'->duck;tree}\"/>"
          `shouldRenderM` "<input value=\"Quack!\"/>"
        "<bind tag=\"animal\">whatever</bind>\
        \<input value=\"${animal|'cat'->cat;'dog'->dog;'duck'->duck;tree}\"/>"
          `shouldRenderM` "<input value=\"stillness\"/>"

        "<input value=\"${'cat'|'cat'->'purr';'dog'->'howl';'duck'->'quakery';'silence'}\"/>"
          `shouldRenderM` "<input value=\"purr\"/>"
        "<input value=\"${'dog'|'cat'->'purr';'dog'->'howl';'duck'->'quakery';'silence'}\"/>"
          `shouldRenderM` "<input value=\"howl\"/>"
        "<input value=\"${'duck'|'cat'->'purr';'dog'->'howl';'duck'->'quakery';'silence'}\"/>"
          `shouldRenderM` "<input value=\"quakery\"/>"
        "<input value=\"${'nada'|'cat'->'purr';'dog'->'howl';'duck'->'quakery';'silence'}\"/>"
          `shouldRenderM` "<input value=\"silence\"/>"

    describe "a large template" $ do
      it "should render large HTML files" $ do
        hLarcenyState.lSubs .= subst
        hLarcenyState.lLib .= positionTplLib
        tpl6 `shouldRenderContainingM` "Verso Books"

    describe "escaping" $ do
      it "should escape html in textFill" $ do
        hLarcenyState.lSubs .=
         subs [("someHtml", textFill "<strong>Some HTML</strong>")]
        "  <p><someHtml /></p>" `shouldRenderM`
           "<p>&lt;strong&gt;Some HTML&lt;/strong&gt;</p>"

      it "should not escape html with rawTextFill" $ do
        hLarcenyState.lSubs .=
         subs [("someHtml", rawTextFill "<strong>Some HTML</strong>")]
        "<p><someHtml /></p>" `shouldRenderM`
         "<p><strong>Some HTML</strong></p>"

    describe "br" $ do
      it "should allow self-closing tags" $ do
        "<br />" `shouldRenderM` "<br />"

    describe "selected" $ do
      it "should allow attributes that aren't k-v pairs" $ do
        "<option selected>Hello</option>" `shouldRenderM` "<option selected>Hello</option>"

      it "should allow blanks in attributes that aren't k-v pairs" $ do
        hLarcenyState.lSubs .=
          subs [("selectedA", textFill ""), ("selectedB", textFill "selected")]
        "<option ${selectedA}>Option A</option> \
        \ <option ${selectedB}>Option B</option>" `shouldRenderM`
          "<option >Option A</option><option selected>Option B</option>"

    fallbackTests
    attrTests
    doctypeTests
    conditionTests
    namespaceTests
  statefulTests

namespaceTests :: SpecWith LarcenyHspecState
namespaceTests =
  describe "namespaces" $ do
    it "should assume that tags with namespaces are blanks" $ do
      "<address><l:address /></address>"
        `shouldRenderM` "<address></address>"
      hLarcenyState.lSubs .=
        subs [("address", textFill "5 Jones St")]
      "<address><l:address /></address>"
        `shouldRenderM` "<address>5 Jones St</address>"
    it "doesn't parse namespaces in attributes" $ do
      hLarcenyState.lSubs .=
        subs [("l:class", textFill "some-class")]
      "<p class=\"${l:class}\">Hello</p>"
        `shouldRenderM` "<p class=\"some-class\">Hello</p>"
    it "should not parse all namespaces as blanks" $ do
      "<svg:svg><path></path></svg>"
        `shouldRenderM` "<svg:svg><path></path></svg:svg>"

statefulTests :: SpecWith ()
statefulTests =
  describe "statefulness" $ do
      it "a fill should be able to affect subsequent fills" $ do
         renderWith (M.fromList [(["default"], parse "<x/><x/>")])
                    (subs [("x", Fill $ \_ _ _ ->
                                   do modify ((+1) :: Int -> Int)
                                      s <- get
                                      return $ TextOutput (T.pack (show s)))])
                    0
                    ["default"]
         `shouldReturn` Just "12"
       {- The following test was prompted by a bug where I refuktored the
          bind tag handling to be inside the case statement in `process`.
          The bind tag processor itself calls bind but doesn't return any
          text. This resulted in portions of the template being evaluated
          over and over again.
       -}
      it "should not be affected by binds" $ do
       let tpl = "<bind tag=\"test1\">test1</bind>\
                 \<bind tag=\"test2\">test2</bind>\
                 \<x/><x/>"
       renderWith (M.fromList [(["default"], parse tpl)])
                    (subs [("x", Fill $ \_ _ _ ->
                                   do modify ((+1) :: Int -> Int)
                                      s <- get
                                      return $ TextOutput (T.pack (show s)))])
                    0
                    ["default"]
         `shouldReturn` Just "12"

doctypeTests :: SpecWith LarcenyHspecState
doctypeTests = do
  describe "doctypes" $ do
    it "should render blank doctypes" $ do
      "<doctype />" `shouldRenderM` "<!DOCTYPE html>"
    it "should render regular doctype" $ do
      "<!DOCTYPE html>" `shouldRenderM` "<!DOCTYPE html>"
    it "should render doctype in the correct place" $ do
      "<!DOCTYPE html><html><p>Hello world</p></html>"
      `shouldRenderM` "<!DOCTYPE html><html><p>Hello world</p></html>"

conditionTests :: SpecWith LarcenyHspecState
conditionTests = do
  describe "conditions" $ do
    let template cond =
          "<if condition=\"" <> cond <> "\">\
          \  <then>It's true!</then>\
          \  <else>It's false!</else>\
          \</if>"
    describe "true condition" $ do
      it "should display the stuff within the `then` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)]
        template "True" `shouldRenderM` "It's true!"
      it "should work with a blank in the attribute" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("userIsLoggedIn", textFill "True")]
        template "${userIsLoggedIn}" `shouldRenderM` "It's true!"
    describe "false condition" $ do
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)]
        template "False" `shouldRenderM` "It's false!"
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("userIsLoggedIn", textFill "False")]
        template "${userIsLoggedIn}" `shouldRenderM` "It's false!"

  describe "exists" $ do
    let template =
          "<if exists=\"${existing}\">\
          \  <then>It <existing />!</then>\
          \  <else>It doesn't exist!</else>\
          \</if>"
    describe "the fill exists" $ do
      it "should display the stuff within the `then` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("existing", textFill "exists")]
        template `shouldRenderM` "It exists!"
    describe "the fill is the string \"False\"" $
      it "should display the stuff within the `then` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("existing", textFill "False")]
        template `shouldRenderM` "It False!"
    describe "the fill is an empty string" $
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("existing", textFill "")]
        template `shouldRenderM` "It doesn't exist!"
    describe "the fill doesn't exist" $ do
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)]
        template `shouldRenderM` "It doesn't exist!"

  describe "using condition and exists at the same time" $ do
    let template cond =
          "<if condition=\"" <> cond <> "\" exists=\"${existing}\">\
          \  <then>It <existing />!</then>\
          \  <else>It doesn't exist and/or it's false!</else>\
          \</if>"
    describe "condition is true and tag exists" $ do
      it "should display the stuff within the `then` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("existing", textFill "exists")]
        template "True" `shouldRenderM` "It exists!"
    describe "any other combination" $ do
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)]
        template "True" `shouldRenderM` "It doesn't exist and/or it's false!"
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("existing", textFill "exists")]
        template "False" `shouldRenderM` "It doesn't exist and/or it's false!"
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)]
        template "False" `shouldRenderM` "It doesn't exist and/or it's false!"

  describe "using `exists` for lists and nested tags" $ do
    let template =
          "<bind tag=\"rendered\"><list><l:i /></list></bind>\
          \<if exists=\"${rendered}\">\
          \  <then>It is not empty! <list><l:i /></list></then>\
          \  <else>It is empty!</else>\
          \</if>"

    describe "list is not empty" $ do
      it "should display the stuff within the `then` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("list", mapSubs (\i -> subs [("i", textFill i)]) ["a", "b", "c"])]
        template `shouldRenderM` "It is not empty! abc"

    describe "list is empty" $ do
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)
                ,("list", mapSubs (\i -> subs [("i", textFill i)]) [])]
        template `shouldRenderM` "It is empty!"

    describe "list is doesn't exist" $ do
      it "should display the stuff within the `else` tag" $ do
        hLarcenyState.lSubs .=
           subs [("if", ifFill)]
        template `shouldRenderM` "It is empty!"


fallbackTests ::SpecWith LarcenyHspecState
fallbackTests = do
  describe "templates with missing blanks" $ do
    it "should render empty text by default" $ do
      "<p>missing: <missing /></p>" `shouldRenderM` "<p>missing: </p>"
    it "should work if the missing blank has children" $ do
      "<p>missing: <missing>some stuff</missing></p>" `shouldRenderM` "<p>missing: </p>"
  describe "setting custom fallbacks" $ do
    it "should custom fallbacks" $ do
      hLarcenyState.lSubs .= fallbackSub (rawTextFill "I'm a fallback.")
      "<p>missing: <missing /></p>" `shouldRenderM` "<p>missing: I'm a fallback.</p>"
    it "should use fallback set on tag" $ do
      "<p>missing: <missing def=\"I'm a fallback\"/></p>"
        `shouldRenderM` "<p>missing: I'm a fallback</p>"
      "<p>missing: <a href=\"${missing?def=/fallback}\">link</a></p>"
        `shouldRenderM` "<p>missing: <a href=\"/fallback\">link</a></p>"
    it "should allow errors to be thrown, e.g., in dev mode" $ do
        hLarcenyState.lSubs .= fallbackSub (Fill $ \_ _ _ -> throw $ SomeError "missing blank!")
        "<p>missing: <missing /></p>" `shouldErrorM` (== (SomeError "missing blank!"))

attrTests :: SpecWith LarcenyHspecState
attrTests =
  describe "useAttrs" $ do
      it "should allow you to *easily* write functions for fills" $ do
        hLarcenyState.lSubs .=
          subs [("desc", useAttrs (a"length")
                         (\n -> textFill $ T.take n
                                "A really long description"
                                <> "..."))]
        "<l:desc length=\"10\" />" `shouldRenderM` "A really l..."

      it "should allow you use multiple args" $ do
         let subs' = subs [("desc", useAttrs (a"length" % a"text")
                                    (\n d -> textFill $ T.take n d <> "..."))]
         hLarcenyState.lSubs .= subs'
         "<l:desc length=\"10\" text=\"A really long description\" />"
           `shouldRenderM` "A really l..."

      it "should allow you use child elements" $ do
        let descTplFill =
              useAttrs (a"length")
                       (\n -> Fill $ \_attrs (_pth, tpl) _l -> liftIO $ do
                           t' <- evalStateT (runTemplate tpl ["default"] mempty mempty) ()
                           return $ TextOutput $ T.take n (toText $ fst t') <> "...")
        hLarcenyState.lSubs .= subs [ ("adverb", textFill "really")
                                    , ("desc", descTplFill)]
        "<l:desc length=\"10\">A <adverb /> long description</desc>"
           `shouldRenderM` "A really l..."

      it "should allow optional attributes by giving a Maybe type (not using the optional)" $ do
        hLarcenyState.lSubs .= subs [("desc", descFill)]
        "<l:desc length=\"10\">A really long description</desc>"
          `shouldRenderM` "A really l..."

      it "should allow optional attributes by giving a Maybe type (using optional)" $ do
        hLarcenyState.lSubs .= subs [("desc", descFill)]
        "<l:desc length=\"10\" ending=\" and such \">A really long description</desc>"
          `shouldRenderM` "A really l and such"

      it "should give a nice error message if attribute is missing" $ do
        hLarcenyState.lSubs .=
          subs [("desc", useAttrs (a"length")
                  (\n -> textFill $ T.take n
                         "A really long description"
                         <> "..."))]
        "<l:desc />" `shouldErrorM` (== AttrMissing "length")

      it "should give a nice error message if attribute is unparsable" $ do
        hLarcenyState.lSubs .=
         subs [("desc", useAttrs (a"length")
                                 (\n -> textFill $ T.take n
                                        "A really long description"
                                        <> "..."))]
        "<l:desc length=\"infinite\" />" `shouldErrorM` (== AttrUnparsable "Int" "length")
  where descFill :: Fill () IO
        descFill =
          useAttrs (a"length" % a"ending") descFunc

        descFunc :: Int -> Maybe Text -> Fill () IO
        descFunc n e = Fill $
          do let ending = fromMaybe "..."  e
             \_attrs (_pth, tpl) _l -> liftIO $ do
               renderedText <- evalStateT (runTemplate tpl ["default"] mempty mempty) ()
               return $ TextOutput $ T.take n (toText $ fst renderedText) <> ending

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
