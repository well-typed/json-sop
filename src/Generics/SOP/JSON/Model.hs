{-# LANGUAGE OverloadedStrings #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
module Generics.SOP.JSON.Model (
    JsonModel(..)
  , gjsonModel
    -- * Re-exports
  , Tagged(..)
  , untag
  ) where

import Data.Aeson
import Data.Tagged
import qualified Data.Text      as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector    as Vector

import Generics.SOP
import Generics.SOP.JSON

-- For instances only
import Data.Time (UTCTime)
import Data.Text (Text)

class JsonModel (a :: *) where
  jsonModel :: Tagged a Value

{-------------------------------------------------------------------------------
  Some standard instances
-------------------------------------------------------------------------------}

instance JsonModel UTCTime where
  jsonModel = Tagged $ String "UTCTime"

instance JsonModel Text where
  jsonModel = Tagged $ String "String"

instance JsonModel Text.Lazy.Text where
  jsonModel = Tagged $ String "String"

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPING #-}
#endif
  JsonModel String where
  jsonModel = Tagged $ String "String"

instance JsonModel Int where
  jsonModel = Tagged $ String "Int"

instance JsonModel Double where
  jsonModel = Tagged $ String "Double"

instance JsonModel Rational where
  jsonModel = Tagged $ String "Rational"

instance JsonModel Bool where
  jsonModel = Tagged $ String "Bool"

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPABLE #-}
#endif
  JsonModel a => JsonModel [a] where
  jsonModel = let model :: Tagged a Value
                  model = jsonModel
              in Tagged $ object [ "List" .= untag model ]

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPABLE #-}
#endif
  JsonModel a => JsonModel (Maybe a) where
  jsonModel = let model :: Tagged a Value
                  model = jsonModel
              in Tagged $ Array $ Vector.fromList [ untag model, Null ]

{-------------------------------------------------------------------------------
  Generic instance
-------------------------------------------------------------------------------}

-- | Generic computation of the JSON model
--
-- Do NOT use for recursive types, you will get an infinite model.
gjsonModel :: forall a. (HasDatatypeInfo a, All2 JsonModel (Code a))
           => JsonOptions -> Tagged a Value
gjsonModel opts = unproxy $ \pa -> gjsonModel' (jsonInfo pa opts)

gjsonModel' :: All2 JsonModel xss => NP JsonInfo xss -> Value
gjsonModel' = mkValue . hcollapse . hcliftA allp (K . constructorModel)
  where
    -- In the case of a single-argument datatype, just return the type of
    -- the constructor, rather than a singleton list of types
    mkValue :: [Value] -> Value
    mkValue [v] = v
    mkValue vs  = Array $ Vector.fromList vs

constructorModel :: forall xs. All JsonModel xs => JsonInfo xs -> Value
constructorModel (JsonZero n) =
    object [ "Literal" .= toJSON n ]
constructorModel info@(JsonOne t) = tagModel t $
    constructorModelOne info
constructorModel (JsonMultiple t) = tagModel t $
    object [ "Tuple" .= (tupleModel . hcollapse $ aux) ]
  where
    aux :: All JsonModel xs => NP (K Value) xs
    aux = hcpure p jsonModelK
constructorModel (JsonRecord t fs) = tagModel t $
    object [ "Object" .= (objectModel . hcollapse . hcliftA p aux $ fs) ]
  where
    aux :: forall a. JsonModel a => K String a -> K (Text, Value) a
    aux (K f) = K (Text.pack f, untag (jsonModel :: Tagged a Value))

tupleModel :: [Value] -> Value
tupleModel = Array . Vector.fromList

objectModel :: [(Text, Value)] -> Value
objectModel = Array . Vector.fromList . map aux
  where
    aux :: (Text, Value) -> Value
    aux (name, typ) = object [ "name" .= name, "type" .= typ ]

constructorModelOne :: forall a. JsonModel a => JsonInfo '[a] -> Value
constructorModelOne _ = untag (jsonModel :: Tagged a Value)

jsonModelK :: forall a. JsonModel a => K Value a
jsonModelK = K $ untag (jsonModel :: Tagged a Value)

tagModel :: Tag -> Value -> Value
tagModel NoTag   v = v
tagModel (Tag n) v = object [ "Object" .= object [ Text.pack n .= v ] ]

p :: Proxy JsonModel
p = Proxy

allp :: Proxy (All JsonModel)
allp = Proxy
