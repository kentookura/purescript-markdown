module Text.Markdown.SlamDown.Render
  ( renderBlock
  , renderInline
  , renderListBlock
  , renderMd
  , renderMdFromString
  ) where

import Prelude

import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Syntax (SlamDown, SlamDownP(..))
import Text.Markdown.SlamDown.Syntax.Block (Block(..), ListType(..))
import Text.Markdown.SlamDown.Syntax.Inline (Inline(..), LinkTarget(..))
import Data.Foldable (foldl)
import Data.Array as A
import Data.Either (Either(..))
import Data.List (List)
import Deku.Attributes (klass_, href)
import Deku.Control (blank, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Katex as Katex

renderInline :: forall a. Inline a -> Nut
renderInline =
  case _ of
    Str s -> text_ s
    Space -> text_ " "
    LineBreak -> D.br_ []
    SoftBreak -> D.br_ []
    Emph is -> D.i_ $ A.fromFoldable (map renderInline is)
    Strong is -> D.strong_ $ A.fromFoldable (map renderInline is)
    Code bool str -> D.code_ [ text_ str ]
    Link is (InlineLink tgt) -> D.a [ href (pure tgt) ] $ A.fromFoldable (map renderInline is)
    Link is (ReferenceLink tgt _) -> D.a_ $ A.fromFoldable (map renderInline is)
    Inline s -> Katex.inline s
    (Entity _) -> blank
    (Image _ _) -> blank
    --(FormField _ _ _) -> blank

renderBlock :: forall a. Block a -> Nut
renderBlock =
  case _ of
    Paragraph is -> D.div_ $ A.fromFoldable (map renderInline is)
    Header lvl is ->
      let
        h
          | lvl == 1 = D.h1_
          | lvl == 2 = D.h2_
          | lvl == 3 = D.h3_
          | lvl == 4 = D.h4_
          | lvl == 5 = D.h5_
          | otherwise = D.h5_
      in
        h $ A.fromFoldable (map renderInline is)

    Blockquote bs -> D.blockquote_ $ A.fromFoldable (map renderBlock bs)
    Lst listType listElems -> case listType of
      Bullet s ->
        D.ul_ $ A.fromFoldable (map renderListBlock listElems)
      Ordered s ->
        D.ol_ $ A.fromFoldable (map renderListBlock listElems)

    CodeBlock blockType lines -> D.code_ $ A.fromFoldable (map text_ lines)
    LinkReference s1 s2 -> blank
    Rule -> D.br_ []

renderListBlock :: forall a. List (Block a) -> Nut
renderListBlock bs = D.li_ [ foldl ((<>)) blank (map renderBlock bs) ]

renderMd :: SlamDown -> Nut
renderMd (SlamDown bs) = D.article [ klass_ "prose" ] (A.fromFoldable (map renderBlock bs))

renderMdFromString :: String -> Nut
renderMdFromString s =
  case parseMd s of
    Right md -> renderMd md
    Left err -> text_ $ "Parse error : " <> err
