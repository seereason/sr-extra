{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-orphans #-}

module Extra.Orphans where

import Data.Graph.Inductive as G
import Data.List (intercalate)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (base, contain, deriveSafeCopy, safeGet, safePut,
                      SafeCopy(errorTypeName, getCopy, kind, putCopy, version))
import Data.Serialize (label, Serialize(..))
import Data.Text as T hiding (concat, intercalate)
import Data.Text.Lazy as LT hiding (concat, intercalate)
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime(..), Day(ModifiedJulianDay), TimeOfDay(..), timeOfDayToTime, toModifiedJulianDay, DiffTime)
import Data.UserId (UserId(..))
import Data.UUID.Orphans ()
import Data.UUID (UUID)
import Data.UUID.V4 as UUID (nextRandom)
import Data.UUID.Orphans ()
import Extra.Orphans2 ()
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH (Loc(..), Ppr(ppr))
import Language.Haskell.TH.Lift -- (deriveLift, deriveLiftMany)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.PprLib (ptext)
import Network.URI (URI(..), URIAuth(..), uriToString)
import System.IO.Unsafe (unsafePerformIO)
#if !__GHCJS__
import Test.QuickCheck (Arbitrary(arbitrary), choose, elements, Gen, listOf, listOf1, resize)
#endif

instance SafeCopy (Proxy t) where
      putCopy Proxy = contain (do { return () })
      getCopy = contain (label "Data.Proxy.Proxy:" (pure Proxy))
      version = 0
      kind = base
      errorTypeName _ = "Data.Proxy.Proxy"

instance Serialize UTCTime where
    get = uncurry UTCTime <$> get
    put (UTCTime day time) = put (day, time)

instance Serialize Day where
    get = ModifiedJulianDay <$> get
    put = put . toModifiedJulianDay

instance Serialize DiffTime where
    get = fromRational <$> get
    put = put . toRational

instance Serialize T.Text where
    put = put . TE.encodeUtf8
    get = TE.decodeUtf8 <$> get

instance Serialize LT.Text where
    put = put . TLE.encodeUtf8
    get = TLE.decodeUtf8 <$> get

deriving instance Serialize Loc

-- Use the SafeCopy methods to implement Serialize.  This is a pretty
-- neat trick, it automatically does SafeCopy migration on any
-- deserialize of a type with this implementation.
instance Serialize UUID where
    get = safeGet
    put = safePut

deriving instance Serialize URI
deriving instance Generic URIAuth
deriving instance Serialize URIAuth

-- $(deriveLift ''UserId)
instance Lift UserId where lift (UserId x0) = [|UserId $(lift x0)|]

$(deriveLift ''G.Gr)
$(deriveLift ''G.NodeMap)

instance Ppr UserId where ppr = ptext . show

#if !__GHCJS__
instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary LT.Text where
    arbitrary = LT.pack <$> arbitrary

instance Arbitrary UUID where
    arbitrary = pure (unsafePerformIO UUID.nextRandom)

instance Arbitrary UserId where
    arbitrary = UserId <$> choose (0, 20)

instance Arbitrary UTCTime where arbitrary = UTCTime <$> arbitrary <*> arbitrary
instance Arbitrary Day where arbitrary = ModifiedJulianDay <$> arbitrary
instance Arbitrary DiffTime where arbitrary = timeOfDayToTime <$> arbitrary
instance Arbitrary TimeOfDay where arbitrary = TimeOfDay <$> choose (0,23) <*> choose (0,59) <*> (fromInteger <$> choose (0,60999999999999))

-- from https://gist.github.com/roman

newtype URIPair
  = URIPair { fromPair :: (String, String) }
  deriving (Show)

genWord :: Gen String
genWord = listOf1 (choose ('a', 'z'))

genCanonicalURI :: Gen URI
genCanonicalURI =
    URI <$> elements ["http:", "https:"]
        <*> (Just <$> genURIAuthority)
        <*> (('/':) <$> genPaths)
        <*> pure ""
        <*> pure ""
  where
    genURIAuthority =
      URIAuth <$> pure ""
              <*> genRegName
              <*> pure ""
    genRegName = do
      domainName <- elements ["noomii", "google", "yahoo"]
      return $ mconcat ["www.", domainName, ".com"]
    genPaths = resize 10 (intercalate "/" <$> listOf genWord)

genNormalURI :: URI -> Gen URI
genNormalURI uri = do
    qs  <- genQueryString
    fragment <-  genFragment
    return $ uri { uriQuery = qs, uriFragment = fragment }
  where
    genParam = do
      name  <- genWord
      value <- genWord
      return $ name ++ "=" ++ value
    genQueryString = resize 10 $
      ('?':) <$> (intercalate "&" <$> listOf genParam)
    genFragment = ('#':) <$> genWord

instance Arbitrary URIPair where
    arbitrary = do
      canonical <- genCanonicalURI
      normal    <- genNormalURI canonical
      return (URIPair (uriToString id canonical "", uriToString id normal ""))

instance Arbitrary URI where
    arbitrary = genCanonicalURI >>= genNormalURI
#endif

$(concat <$>
  sequence
  [ deriveSafeCopy 0 'base ''URI
  , deriveSafeCopy 0 'base ''URIAuth
  , deriveLiftMany [''URI, ''URIAuth] ])

