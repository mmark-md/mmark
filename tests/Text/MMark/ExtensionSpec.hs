{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.ExtensionSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Text.MMark
import Text.MMark.Extension
import Text.Megaparsec
import qualified Data.Text.Lazy as TL
import qualified Lucid          as L

spec :: Spec
spec = do
  describe "blockTrans" $
    it "works" $ do
      doc <- mkDoc "# My heading"
      renderToText (useExtension h1_to_h2 doc)
        `shouldBe` "<h2>My heading</h2>\n"
  describe "blockRender" $
    it "works" $ do
      doc <- mkDoc "# My heading"
      renderToText (useExtension (add_h1_id "foo") doc)
        `shouldBe` "<h1 id=\"foo\">My heading</h1>\n"
  describe "inlineTrans" $
    it "works" $ do
      doc <- mkDoc "# My *heading*"
      renderToText (useExtension em_to_strong doc)
        `shouldBe` "<h1>My <strong>heading</strong></h1>\n"
  describe "inlineRender" $
    it "works" $ do
      doc <- mkDoc "# My *heading*"
      renderToText (useExtension (add_em_class "foo") doc)
        `shouldBe` "<h1>My <em class=\"foo\">heading</em></h1>\n"

----------------------------------------------------------------------------
-- Testing extensions

-- | Convert H1 headings into H2 headings.

h1_to_h2 :: Extension
h1_to_h2 = blockTrans $ \case
  Heading1 inner -> Heading2 inner
  other          -> other

-- | Add given id to all headings with on level 1.

add_h1_id :: Text -> Extension
add_h1_id given = blockRender $ \old block ->
  case block of
    Heading1 inner -> L.with (old (Heading1 inner)) [L.id_ given]
    other          -> old other

-- | Covert all 'Emphasis' to 'Strong'.

em_to_strong :: Extension
em_to_strong = inlineTrans $ \case
  Emphasis inner -> Strong inner
  other          -> other

-- | Add given class to all 'Emphasis' things.

add_em_class :: Text -> Extension
add_em_class given = inlineRender $ \old inline ->
  case inline of
    Emphasis inner -> L.with (old (Emphasis inner)) [L.class_ given]
    other          -> old other

----------------------------------------------------------------------------
-- Helpers

-- | Create an 'MMark' document from given input reporting an expectation
-- failure if it cannot be parsed.

mkDoc :: Text -> IO MMark
mkDoc input =
  case parseMMark "" input of
    Left errs -> do
      expectationFailure (concatMap (parseErrorPretty' input) errs)
      undefined
    Right x -> return x

-- | Render an 'MMark' document to 'Text'.

renderToText :: MMark -> Text
renderToText = TL.toStrict . L.renderText . renderMMark
