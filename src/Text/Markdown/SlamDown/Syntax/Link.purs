module Text.Markdown.SlamDown.Syntax.Link where

import Prelude
import Data.List.NonEmpty (NonEmptyList)
import Data.Argonaut.Encode (class EncodeJson)


data HashQualified 
  = NameOnly FQN
  | HashOnly Hash
  | HashQualified FQN Hash

newtype Hash = Hash String

instance Show Hash where
  show (Hash hash) = hash
derive instance Ord Hash 
derive instance Eq Hash 


newtype FQN = FQN (NonEmptyList String)

derive instance Ord FQN
derive instance Eq FQN


data Reference 
  = TermReference HashQualified

derive newtype instance encodeJsonHash :: EncodeJson Hash
derive newtype instance encodeJsonFQN :: EncodeJson FQN