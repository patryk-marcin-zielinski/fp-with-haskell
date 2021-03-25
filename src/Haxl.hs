{-# LANGUAGE ExistentialQuantification, GADTs, StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Haxl where

import Data.Time
import Data.IORef
import Data.Sequence
import Control.Applicative
import Data.List
import Data.Function
import Data.Ord
import qualified Data.Map as Map
import Data.Map (Map)
import Prelude

type Id = Int
type Date = UTCTime

newtype PostId = PostId Id
  deriving (Eq, Ord, Num)

type PostContent = String

data FetchStatus a = NotFetched | FetchSuccess a


data Request a where
  FetchPosts       :: Request [PostId]
  FetchPostInfo    :: PostId -> Request PostInfo
  FetchPostContent :: PostId -> Request PostContent
  FetchPostViews   :: PostId -> Request Int
-- >>

deriving instance Show PostId
deriving instance Show PostInfo
deriving instance Show (Request a)

deriving instance Eq (Request a)

data BlockedRequest =
  forall a . BlockedRequest (Request a)
                       (IORef (FetchStatus a))




-- <<Monad
data Result a
  = Done a
  | Blocked (Seq BlockedRequest) (Fetch a)

newtype Fetch a = Fetch { unFetch :: IO (Result a) }

instance Monad Fetch where
  return a = Fetch $ return (Done a)

  Fetch m >>= k = Fetch $ do
    r <- m
    case r of
      Done a       -> unFetch (k a)
      Blocked br c -> return (Blocked br (c >>= k))
-- >>

instance Functor Fetch where
  fmap f fio = fio >>= (return . f)

-- <<Applicative
instance Applicative Fetch where
  pure = return

  Fetch f <*> Fetch x = Fetch $ do
    f' <- f
    x' <- x
    case (f',x') of
      (Done g,        Done y       ) -> return (Done (g y))
      (Done g,        Blocked br c ) -> return (Blocked br (g <$> c))
      (Blocked br c,  Done y       ) -> return (Blocked br (c <*> return y))
      (Blocked br1 c, Blocked br2 d) -> return (Blocked (br1 <> br2) (c <*> d))
-- >>

data PostInfo = PostInfo
    { postId :: PostId
    , postDate :: Date
    , postTopic :: String
    }

getPostIds :: Fetch [PostId]
getPostIds = undefined

getPostInfo :: PostId -> Fetch PostInfo
getPostInfo = undefined

getPostContent :: PostId -> Fetch PostContent
getPostContent = undefined

getPostViews :: PostId -> Fetch Int
getPostViews = undefined

getAllPostsInfo :: Fetch [PostInfo]
getAllPostsInfo = mapM getPostInfo =<< getPostIds


