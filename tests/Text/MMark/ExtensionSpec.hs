{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.MMark.ExtensionSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Text.MMark.Extension (Block (..), Inline (..))
import Text.MMark.TestUtils
import qualified Lucid as L
import qualified Text.MMark           as MMark
import qualified Text.MMark.Extension as Ext

spec :: Spec
spec = parallel $ do
  describe "blockTrans" $
    it "works" $ do
      doc <- mkDoc "# My heading"
      toText (MMark.useExtension h1_to_h2 doc)
        `shouldBe` "<h2 id=\"my-heading\">My heading</h2>\n"
  describe "blockRender" $
    it "works" $ do
      doc <- mkDoc "# My heading"
      toText (MMark.useExtension (add_h1_class "foo") doc)
        `shouldBe` "<h1 id=\"my-heading\" class=\"foo\">My heading</h1>\n"
  describe "inlineTrans" $
    it "works" $ do
      doc <- mkDoc "# My *heading*"
      toText (MMark.useExtension em_to_strong doc)
        `shouldBe` "<h1 id=\"my-heading\">My <strong>heading</strong></h1>\n"
  describe "inlineRender" $
    it "works" $ do
      doc <- mkDoc "# My *heading*"
      toText (MMark.useExtension (add_em_class "foo") doc)
        `shouldBe` "<h1 id=\"my-heading\">My <em class=\"foo\">heading</em></h1>\n"

----------------------------------------------------------------------------
-- Testing extensions

-- | Convert H1 headings into H2 headings.

h1_to_h2 :: MMark.Extension
h1_to_h2 = Ext.blockTrans $ \case
  Heading1 inner -> Heading2 inner
  other          -> other

-- | Add given class to all headings of level 1.

add_h1_class :: Text -> MMark.Extension
add_h1_class given = Ext.blockRender $ \old block ->
  case block of
    Heading1 inner -> L.with (old (Heading1 inner)) [L.class_ given]
    other          -> old other

-- | Covert all 'Emphasis' to 'Strong'.

em_to_strong :: MMark.Extension
em_to_strong = Ext.inlineTrans $ \case
  Emphasis inner -> Strong inner
  other          -> other

-- | Add given class to all 'Emphasis' things.

add_em_class :: Text -> MMark.Extension
add_em_class given = Ext.inlineRender $ \old inline ->
  case inline of
    Emphasis inner -> L.with (old (Emphasis inner)) [L.class_ given]
    other          -> old other
