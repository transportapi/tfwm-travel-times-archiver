{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Query where

import           Control.Monad.IO.Class (liftIO, MonadIO)

import qualified Data.ByteString as BS
import           Data.Monoid ((<>))
import qualified Data.Serialize as S

import qualified Database.LevelDB as LDB
import qualified Database.LevelDB.Streaming as LDB

type family ToList (xs :: [*]) where
  ToList '[a] = a
  ToList (a:as) = (a, ToList as)
  ToList '[] = ()

type family IsSubkey (xs :: [*]) (ys :: [*]) where
  IsSubkey '[] '[] = 'True
  IsSubkey '[a] '[a] = 'True
  IsSubkey '[a] (a:as) = 'True
  IsSubkey (a:as) (a:as') = IsSubkey as as'
  IsSubkey a b = 'False

data Key (ks :: [*]) = Key (ToList ks)

class ToByteString a where
  toByteString :: a -> BS.ByteString

class FromByteString a where
  parseByteString :: S.Get a

class ToKey a where
  toKey :: Key a -> BS.ByteString

instance {-# OVERLAPPABLE #-} (ToByteString a, ToKey rs, ToList (a:rs) ~ (a, ToList rs)) => ToKey (a:rs) where
  toKey (Key (a, rs)) = toByteString a <> toKey (Key rs :: Key rs)

instance ToByteString a => ToKey '[a] where
  toKey (Key a) = toByteString a

instance ToKey '[] where
  toKey (Key _) = ""

class FromKey a where
  parseKey :: BS.ByteString -> Maybe (Key a)

instance {-# OVERLAPPABLE #-} (FromByteString a, FromKey rs, ToList (a:rs) ~ (a, ToList rs)) => FromKey (a:rs) where
  parseKey k
    | Right (a, rest) <- S.runGetState (parseByteString :: S.Get a) k 0
    , Just (Key rs :: Key rs) <- parseKey rest = Just $ Key (a, rs)
    | otherwise = Nothing

instance FromByteString a => FromKey '[a] where
  parseKey k
    | Right x <- S.runGet (parseByteString :: S.Get a) k = Just $ Key x
    | otherwise = Nothing

instance FromKey '[] where
  parseKey _ = Just $ Key ()

--------------------------------------------------------------------------------

onlyKey :: a -> Key '[a]
onlyKey a = Key a

class FromTuple a b | a -> b where
  fromTuple :: a -> Key b

instance FromTuple () '[] where
  fromTuple () = Key ()

instance FromTuple (a, b) '[a, b] where
  fromTuple (a, b) = Key (a, b)

instance FromTuple (a, b, c) '[a, b, c] where
  fromTuple (a, b, c) = Key (a, (b, c))

instance FromTuple (a, b, c, d) '[a, b, c, d] where
  fromTuple (a, b, c, d) = Key (a, (b, (c, d)))

instance FromTuple (a, b, c, d, e) '[a, b, c, d, e] where
  fromTuple (a, b, c, d, e) = Key (a, (b, (c, (d, e))))

instance FromTuple (a, b, c, d, e, f) '[a, b, c, d, e, f] where
  fromTuple (a, b, c, d, e, f) = Key (a, (b, (c, (d, (e, f)))))

class ToTuple a b | a -> b where
  toTuple :: Key a -> b

instance ToTuple '[] () where
  toTuple (Key ()) = ()

instance ToTuple '[a, b] (a, b) where
  toTuple (Key (a, b)) = (a, b)

instance ToTuple '[a, b, c] (a, b, c) where
  toTuple (Key (a, (b, c))) = (a, b, c)

instance ToTuple '[a, b, c, d] (a, b, c, d) where
  toTuple (Key (a, (b, (c, d)))) = (a, b, c, d)

instance ToTuple '[a, b, c, d, e] (a, b, c, d, e) where
  toTuple (Key (a, (b, (c, (d, e))))) = (a, b, c, d, e)

instance ToTuple '[a, b, c, d, e, f] (a, b, c, d, e, f) where
  toTuple (Key (a, (b, (c, (d, (e, f)))))) = (a, b, c, d, e, f)

--------------------------------------------------------------------------------

data Table key value = Table BS.ByteString

query
  :: forall k key v m
   . MonadIO m
  => IsSubkey key k ~ 'True
  => ToKey key
  => FromKey k
  => ToKey k
  => S.Serialize v
  => LDB.DB
  -> Table (Key k) v
  -> Key key
  -> m [(Key k, v)]
query ldb (Table prefix) k = do
  fmap parseEntries
    $ liftIO
    $ LDB.runResourceT
    $ LDB.withIterator ldb readOptions
    $ \i -> LDB.toList $ LDB.entrySlice i keyRange LDB.Asc
  where
    parseEntries :: [LDB.Entry] -> [(Key k, v)]
    parseEntries entries =
      [ (key', value)
      | (k', v) <- entries
      , Just key' <- [ parseKey $ BS.drop (BS.length prefix) k' ]
      , Right value <- [ S.decode v ]
      ]

    readOptions = LDB.defaultReadOptions
      { LDB.fillCache = False
      }

    key = prefix <> toKey k

    keyRange = LDB.KeyRange
      { LDB.start = key
      , LDB.end   = \cur -> compare (BS.take (BS.length key) cur) key
      }

between
  :: forall k key1 key2 v m
   . MonadIO m
  => IsSubkey key1 k ~ 'True
  => ToKey key1
  => IsSubkey key2 k ~ 'True
  => ToKey key2
  => FromKey k
  => ToKey k
  => S.Serialize v
  => LDB.DB
  -> Table (Key k) v
  -> Key key1
  -> Key key2
  -> m [(Key k, v)]
between ldb (Table prefix) from to = do
  fmap parseEntries
    $ liftIO
    $ LDB.runResourceT
    $ LDB.withIterator ldb readOptions
    $ \i -> LDB.toList $ LDB.entrySlice i keyRange LDB.Asc
  where
    parseEntries :: [LDB.Entry] -> [(Key k, v)]
    parseEntries entries =
      [ (key, value)
      | (k, v) <- entries
      , Just key <- [ parseKey $ BS.drop (BS.length prefix) k ]
      , Right value <- [ S.decode v ]
      ]

    readOptions = LDB.defaultReadOptions
      { LDB.fillCache = False
      }

    keyFrom = prefix <> toKey from
    keyTo = prefix <> toKey to

    keyRange = LDB.KeyRange
      { LDB.start = keyFrom
      , LDB.end   = \cur -> compare (BS.take (BS.length keyTo) cur) keyTo
      }

keysForTable
  :: forall k v m
   . MonadIO m
  => FromKey k
  => LDB.DB
  -> Table (Key k) v
  -> m [Key k]
keysForTable ldb (Table prefix)
  = fmap parseKeys
  $ liftIO
  $ LDB.runResourceT
  $ LDB.withIterator ldb readOptions
  $ \i -> LDB.toList $ LDB.keySlice i keyRange LDB.Asc
  where
    parseKeys :: [LDB.Key] -> [Key k]
    parseKeys entries =
      [ key
      | k <- entries
      , Just key <- [ parseKey $ BS.drop (BS.length prefix) k ]
      ]

    readOptions = LDB.defaultReadOptions
      { LDB.fillCache = False
      }

    keyRange = LDB.KeyRange
      { LDB.start = prefix
      , LDB.end   = \cur -> compare (BS.take (BS.length prefix) cur) prefix
      }

putOp
  :: ToKey k
  => S.Serialize v
  => Table (Key k) v
  -> Key k
  -> v
  -> LDB.BatchOp
putOp (Table prefix) k v = LDB.Put (prefix <> toKey k) (S.encode v)
