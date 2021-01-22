-- | Monad for partial results
module Generics.SOP.Util.PartialResult (
    Partial(..)
  , runPartial
  , partialResult
    -- * Re-exports
  , lift
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

-- | Repeat f zero or more times
--
-- Note that this is a free monad construction, but the difference is in the
-- MonadPlus instead. We regard successive PSucc applications to indicate
-- "more success". This is very useful in parser construction: if all parsers
-- for all constructors fail immediately, we want to show an error message
-- for all the top-level parsers ("expected T1 or T2 .."). But if the parser
-- for T1, say, success in parsing the tag for T1, then we don't want to
-- try any more parsers for other constructors even if the parser for T1
-- now fails in parsing the arguments of T1. Instead, we want to give the
-- error message about attempting to parse T1.
data Partial (f :: * -> *) (a :: *) =
    Fail [String]
  | PZero a
  | PSucc (f (Partial f a))

partialResult :: Monad f => Partial f a -> Partial f a
partialResult = PSucc . return

instance Functor f => Functor (Partial f) where
  fmap _ (Fail e)   = Fail e
  fmap f (PZero a)  = PZero (f a)
  fmap f (PSucc pa) = PSucc (fmap (fmap f) pa)

instance Functor f => Monad (Partial f) where
  return = PZero
#if !MIN_VERSION_base(4,13,0)
  fail   = Fail . return
#endif

  Fail e   >>= _ = Fail e
  PZero a  >>= f = f a
  PSucc fa >>= f = PSucc (fmap (>>= f) fa)

#if MIN_VERSION_base(4,13,0)
instance Functor f => MonadFail (Partial f) where
  fail = Fail . return
#endif

instance (MonadPlus f, Functor f) => MonadPlus (Partial f) where
  mzero = Fail []

  Fail  a `mplus` Fail  b = Fail (a ++ b)
  Fail  _ `mplus` b       = b
  a       `mplus` Fail  _ = a

  PZero a `mplus` PZero _ = PZero a
  PZero _ `mplus` PSucc b = PSucc b
  PSucc a `mplus` PZero _ = PSucc a
  PSucc a `mplus` PSucc b = PSucc (a `mplus` b)

instance MonadTrans Partial where
  lift ma = PSucc (PZero `liftM` ma)

instance Functor f => Applicative (Partial f) where
  pure = return
  f <*> a = do f' <- f ; a' <- a ; return (f' a')

instance (MonadPlus f, Functor f) => Alternative (Partial f) where
  empty = mzero
  (<|>) = mplus

runPartial :: Monad m => ([String] -> m a) -> Partial m a -> m a
runPartial failWith = go
  where
    go (PZero a)  = return a
    go (PSucc fa) = fa >>= go
    go (Fail  es) = failWith es
