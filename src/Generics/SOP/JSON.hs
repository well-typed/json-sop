{-# LANGUAGE PolyKinds #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
module Generics.SOP.JSON (
    -- * Configuration
    JsonFieldName
  , JsonTagName
  , JsonOptions(..)
  , defaultJsonOptions
    -- * JSON view of a datatype
  , Tag(..)
  , JsonInfo(..)
  , jsonInfo
    -- * Generic functions
  , gtoJSON
  , gparseJSON
    -- * UpdateFromJSON and co
  , UpdateFromJSON(..)
  , gupdateFromJSON
  , replaceWithJSON
  , parseWith
    -- * Re-exports
  , ToJSON(..)
  , FromJSON(..)
  , Proxy(..)
  ) where

import Control.Arrow (first)
import Control.Monad
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Aeson.Types (Parser, modifyFailure)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import qualified Data.Vector         as Vector

import Generics.SOP
import Generics.SOP.Lens
import Generics.SOP.Util.PartialResult

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

type JsonFieldName = String
type JsonTagName   = String

-- | JSON encoder/decoder configuration
data JsonOptions = JsonOptions {
    -- | Construct the name for JSON object fields (not for the tags that are
    -- used for sum-types, however)
    --
    -- The default just uses the name of the corresponding Haskell constructor
    jsonFieldName :: DatatypeName -> FieldName -> JsonFieldName

    -- | Construct the name for a tag for sum-types.
    --
    -- The default just uses the name of the Haskell constructor.
  , jsonTagName :: ConstructorName -> JsonTagName
  }

defaultJsonOptions :: JsonOptions
defaultJsonOptions = JsonOptions {
    jsonFieldName = const id
  , jsonTagName   = id
  }

{-------------------------------------------------------------------------------
  The JSON view of the world

  We translate the metadata independent of the encoding/decoding. This has two
  advantages: it makes the encoder and decoder clearer, as they (and their
  types!) are driven by this metadata; and two, we can give a readable
  description of this metadata to give the user a static description of what
  the JSON encoding of their datatype will look like.
-------------------------------------------------------------------------------}

-- | Constructor tag
--
-- For a datatype with a single constructor we do not need to tag values with
-- their constructor; but for a datatype with multiple constructors we do.
data Tag = NoTag | Tag JsonTagName

data JsonInfo :: [*] -> * where
  -- Constructor without arguments
  --
  -- In this we _just_ output the name of the constructor (as a string);
  -- we do this even if the datatype has only a single argument.
  JsonZero :: ConstructorName -> JsonInfo '[]

  -- Single argument constructor
  -- This includes newtypes (record or not), but not other record constructors
  --
  -- We just output the argument, discarding the wrapping datatype
  JsonOne :: Tag -> JsonInfo '[a]

  -- Multiple argument constructor, but not a record
  --
  -- We output the arguments as a JSON array
  JsonMultiple :: SListI xs => Tag -> JsonInfo xs

  -- Record constructor
  --
  -- We output the arguments as a JSON object (even if there is only one field)
  JsonRecord :: SListI xs => Tag -> NP (K String) xs -> JsonInfo xs

jsonInfoFor :: forall xs. JsonOptions -> DatatypeName -> (ConstructorName -> Tag) -> ConstructorInfo xs -> JsonInfo xs
jsonInfoFor _    _ tag (Infix n _ _)   = JsonMultiple (tag n)
jsonInfoFor _    _ tag (Constructor n) =
  case shape :: Shape xs of
    ShapeNil           -> JsonZero     n
    ShapeCons ShapeNil -> JsonOne      (tag n)
    _                  -> JsonMultiple (tag n)
jsonInfoFor opts d tag (Record n fields) =
    JsonRecord (tag n) (hliftA fieldName fields)
  where
    fieldName :: FieldInfo a -> K String a
    fieldName (FieldInfo name) = K (jsonFieldName opts d name)

jsonInfo :: forall a. (HasDatatypeInfo a, SListI (Code a))
         => Proxy a -> JsonOptions -> NP JsonInfo (Code a)
jsonInfo pa opts =
  case datatypeInfo pa of
    Newtype _ _ _  -> JsonOne NoTag :* Nil
    ADT     _ n cs -> hliftA (jsonInfoFor opts n (tag cs)) cs
  where
    tag :: NP ConstructorInfo (Code a) -> ConstructorName -> Tag
    tag cs | _ :* Nil <- cs = const NoTag
           | otherwise      = Tag . jsonTagName opts

{-------------------------------------------------------------------------------
  Encoder
-------------------------------------------------------------------------------}

gtoJSON :: forall a. (Generic a, HasDatatypeInfo a, All2 ToJSON (Code a))
        => JsonOptions -> a -> Value
gtoJSON opts a =
  hcollapse $ hcliftA2 allpt gtoJSON' (jsonInfo (Proxy :: Proxy a) opts)
                                      (unSOP $ from a)

gtoJSON' :: All ToJSON xs => JsonInfo xs -> NP I xs -> K Value xs
gtoJSON' (JsonZero n) Nil =
    K $ String (Text.pack n)
gtoJSON' (JsonOne tag) (I a :* Nil) =
    tagValue tag (toJSON a)
gtoJSON' (JsonMultiple tag) cs =
    tagValue tag
  . Array
  . Vector.fromList
  . hcollapse
  . hcliftA pt (K . toJSON . unI)
  $ cs
gtoJSON' (JsonRecord tag fields) cs =
    tagValue tag
  . Object
  . HashMap.fromList
  . hcollapse
  $ hcliftA2 pt (\(K field) (I a) -> K (Text.pack field, toJSON a)) fields cs
gtoJSON' _ _ = error "unreachable"

{-------------------------------------------------------------------------------
  Decoder

  NOTE: We use 'mzero' in various places, rather than failing with a more
  informative error message. The reason for this is that we constructor parsers
  for each of the constructors of a datatype, and then msum them together.
  If they all fail, we will get the error message from the last parser; if that
  says something like "missing field X" that might be very confusing if in fact
  we were trying to parse a different constructor altogether which may not
  even have a field X. If we want to fix this we have to restructure this
  so that we first find the right constructor, and then attempt to parse it.

  TODO: Maybe return a Parser of a Parser in parseValues?
-------------------------------------------------------------------------------}

gparseJSON :: forall a. (Generic a, HasDatatypeInfo a, All2 FromJSON (Code a))
           => JsonOptions -> Value -> Parser a
gparseJSON opts v = to `liftM` gparseJSON' v (jsonInfo (Proxy :: Proxy a) opts)

gparseJSON' :: forall (xss :: [[*]]). All2 FromJSON xss
   => Value -> NP JsonInfo xss -> Parser (SOP I xss)
gparseJSON' v info = runPartial failWith
                   . msum
                   . hcollapse
                   $ hcliftA2 allpf (parseConstructor v) info injs
  where
    failWith :: [String] -> Parser (SOP I xss)
    failWith []   = fail $ "Unknown error"
    failWith errs = fail $ intercalate " or " errs

    -- Necessary type annotation. Don't know why.
    injs :: NP (Injection (NP I) xss) xss
    injs = injections

parseConstructor :: forall (xss :: [[*]]) (xs :: [*]). All FromJSON xs
                 => Value -> JsonInfo xs -> Injection (NP I) xss xs -> K (Partial Parser (SOP I xss)) xs
parseConstructor v info (Fn inj) = K $ do
    vals <- parseValues info v
    prod <- lift . hsequence $ hcliftA pf aux vals
    return $ SOP $ unK (inj prod)
  where
    aux :: FromJSON a => K (Maybe String, Value) a -> Parser a
    aux (K (Just fieldName, val)) = modifyFailure (\str -> fieldName ++ ": " ++ str) $ parseJSON val
    aux (K (Nothing,        val)) = parseJSON val

-- | Given information about a constructor, check if the given value has the
-- right shape, and if so, return a product of (still encoded) values for
-- each of the arguments of the constructor
parseValues :: forall (xs :: [*]). SListI xs
            => JsonInfo xs -> Value -> Partial Parser (NP (K (Maybe String, Value)) xs)
parseValues (JsonZero n) =
  withText ("Expected literal " ++ show n) $ \txt -> do
    guard $ Text.unpack txt == n
    return Nil
parseValues (JsonOne tag) =
  untag tag $ \v ->
    return (K (Nothing, v) :* Nil)
parseValues (JsonMultiple tag) =
  untag tag $ withArray "Array" $ \arr -> do
    case fromList (map (\v -> (Nothing, v)) arr) of
      Just values -> return values
      Nothing     -> fail $ "Got " ++ show (length arr) ++ "values, "
                         ++ "expected " ++ show (lengthSList (Proxy :: Proxy xs))
parseValues (JsonRecord tag fields) =
  untag tag $ withObject "Object" $ \obj -> do
    values <- hsequenceK =<< lineup fields obj
    return $ hliftA2 pairFieldName fields values
  where
    pairFieldName (K x) (K y) = K (Just x, y)

untag :: (Monad m, Functor m) => Tag -> (Value -> Partial m a) -> Value -> Partial m a
untag NoTag   f = f
untag (Tag n) f = withObject "Object" $ \obj ->
  case obj of
    [(n', v)] | n' == n -> partialResult $ f v
    _                   -> fail $ "Expected tag " ++ show n

{-------------------------------------------------------------------------------
  Updating values
-------------------------------------------------------------------------------}

-- | For some values we can support "updating" the value with a "partial"
-- JSON value; record types are the prime example (and the only one supported
-- by the generic function). For non-record types we typically can only
-- replace the value with a "complete" JSON value; in this case, we simply
-- ignore the old value (see 'replaceWithJSON'). Typical class instances will
-- look like
--
-- > instance UpdateFromJSON SomeRecordType where
-- >    updateFromJSON = gupdateFromJSON <jsonOptions>
--
-- or
--
-- > instance UpdateFromJSON SomeNonRecordType where
-- >    updateFromJSON = replaceWithJSON
--
-- NOTE: The generic function uses one-level lenses for the object fields.
-- We could generalize this to arbitrary paths, but then the type would change
-- to
--
-- > updateFromJSON :: Value -> Parser (a -> UpdateM a)
--
-- I.e., updating a value from JSON would, in general, involve a database
-- write.
class UpdateFromJSON a where
  updateFromJSON :: Value -> Parser (a -> a)

-- | For types that we can only replace "whole", rather than update field by field
replaceWithJSON :: FromJSON a => Value -> Parser (a -> a)
replaceWithJSON v = parseJSON v >>= \new -> return $ \_old -> new

-- | Conversely, for types that we can only parse if we have a starting point
parseWith :: UpdateFromJSON a => a -> Value -> Parser a
parseWith a = liftM ($ a) . updateFromJSON

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPABLE #-}
#endif
  FromJSON a => UpdateFromJSON [a]       where updateFromJSON = replaceWithJSON
instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPABLE #-}
#endif
  FromJSON a => UpdateFromJSON (Maybe a) where updateFromJSON = replaceWithJSON

-- Primitive types we can only replace whole
instance UpdateFromJSON Int      where updateFromJSON = replaceWithJSON
instance UpdateFromJSON Double   where updateFromJSON = replaceWithJSON
instance UpdateFromJSON Rational where updateFromJSON = replaceWithJSON
instance UpdateFromJSON Bool     where updateFromJSON = replaceWithJSON
instance UpdateFromJSON Text     where updateFromJSON = replaceWithJSON
instance
#if __GLASGOW_HASKELL__ >= 710
  {-# OVERLAPPING #-}
#endif
  UpdateFromJSON String   where updateFromJSON = replaceWithJSON

{-------------------------------------------------------------------------------
  Generic instance for UpdateFromJSON
-------------------------------------------------------------------------------}

-- | Construct a function that updates a value of some record type, given
-- a JSON object with new values for some (or none, or all) of the fields
gupdateFromJSON :: forall a xs. (Generic a, HasDatatypeInfo a, All UpdateFromJSON xs, Code a ~ '[xs])
                => JsonOptions -> Value -> Parser (a -> a)
gupdateFromJSON opts v = do
  case jsonInfo (Proxy :: Proxy a) opts of
    JsonRecord _ fields :* Nil -> gupdateRecord fields glenses v
    _ :* Nil -> error "cannot update non-record type"
    _        -> error "inaccessible"

gupdateRecord :: forall (xs :: [*]) (a :: *). All UpdateFromJSON xs
              => NP (K String) xs -> NP (GLens (->) (->) a) xs -> Value -> Parser (a -> a)
gupdateRecord fields lenses = withObject "Object" $ \obj -> do
    values :: NP (K (Maybe Value)) xs <- lineup fields obj
    updates <- hcollapse `liftM` hsequenceK (hcliftA2 pu update values lenses)
    return $ foldr (.) id updates
  where
    update :: forall b. UpdateFromJSON b
           => K (Maybe Value) b -> GLens (->) (->) a b -> K (Parser (a -> a)) b
    update (K Nothing)  _ = K $ return id
    update (K (Just v)) l = K $ do f <- updateFromJSON v
                                   return $ \a -> modify l (f, a)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Given a product of keys in a particular order, and a list of values indexed
-- by keys, reorder the second list in the order specified by the first list.
-- Unexpected keys make the whole thing fail (outer monad @m@); missing keys
-- make the inner monad fail @m'@.
--
-- The following are instances of this type
--
-- > NP (K String) xs -> [(String, Value)] -> Parser (NP (K (Parser Value)) xs)
-- > NP (K String) xs -> [(String, Value)] -> Parser (NP (K (Maybe Value)) xs)
--
-- The first form is useful when all fields of a record need to be present;
-- the second when they are optional.
lineup :: (Monad m, MonadPlus m', Eq a, Show a)
       => NP (K a) xs -> [(a, b)] -> m (NP (K (m' b)) xs)
lineup Nil []   = return Nil
lineup Nil vals = fail $ "Unexpected key(s): " ++ show (map fst vals)
lineup (K k :* ks) [] = do bs <- lineup ks [] ; return $ K (missingKey k) :* bs
lineup (K k :* ks) vs =
  case remove ((== k) . fst) vs of
    Nothing            -> do bs <- lineup ks vs  ; return $ K (missingKey k) :* bs
    Just ((_, b), vs') -> do bs <- lineup ks vs' ; return $ K (return b)     :* bs

-- | Error message for a missing key (used in lineup)
missingKey :: (Monad m, Show a) => a -> m b
missingKey k = fail $ "missing key " ++ show k

-- | Remove the first element that satisfies the predicate
remove :: (a -> Bool) -> [a] -> Maybe (a, [a])
remove _ [] = Nothing
remove f (x:xs) | f x       = Just (x, xs)
                | otherwise = do (y, ys) <- remove f xs ; return (y, x:ys)

tagValue :: Tag -> Value -> K Value a
tagValue NoTag   v = K v
tagValue (Tag t) v = K $ Object $ HashMap.fromList [(Text.pack t, v)]

{-------------------------------------------------------------------------------
  Constraint proxies
-------------------------------------------------------------------------------}

pt :: Proxy ToJSON
pt = Proxy

allpt :: Proxy (All ToJSON)
allpt = Proxy

pf :: Proxy FromJSON
pf = Proxy

allpf :: Proxy (All FromJSON)
allpf = Proxy

pu :: Proxy UpdateFromJSON
pu = Proxy

{-------------------------------------------------------------------------------
  Adaptation of some of Aeson's combinators
-------------------------------------------------------------------------------}

withObject :: Monad m => String -> ([(String, Value)] -> m a) -> Value -> m a
withObject _        f (Object obj) = f $ map (first Text.unpack) (HashMap.toList obj)
withObject expected _ v            = typeMismatch expected v

withText :: Monad m => String -> (Text -> m a) -> Value -> m a
withText _        f (String txt) = f txt
withText expected _ v            = typeMismatch expected v

withArray :: Monad m => String -> ([Value] -> m a) -> Value -> m a
withArray _         f (Array arr) = f $ Vector.toList arr
withArray expected  _ v           = typeMismatch expected v

typeMismatch :: Monad m
             => String -- ^ The name of the type you are trying to parse.
             -> Value  -- ^ The actual value encountered.
             -> m a
typeMismatch expected actual =
    fail $ "when expecting a " ++ expected ++ ", encountered " ++ name ++
           " instead"
  where
    name = case actual of
             Object _ -> "Object"
             Array _  -> "Array"
             String _ -> "String"
             Number _ -> "Number"
             Bool _   -> "Boolean"
             Null     -> "Null"
