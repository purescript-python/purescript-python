module Test.TestQuickCheck where

import Prelude
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Exception (try)
import Control.Monad.Gen.Class as MGen
import Data.Array.Partial (head)
import Data.Either (isLeft)
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Tuple (fst)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)
import Test.QuickCheck (class Testable, quickCheck, (/=?), (<=?), (<?), (==?), (>=?), (>?))
import Test.QuickCheck.Arbitrary (arbitrary, genericArbitrary, class Arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf, randomSample', resize, Size, runGen, sized)
import Random.LCG (mkSeed)

data Foo a
  = F0 a
  | F1 a a
  | F2 { foo :: a, bar :: Array a }

derive instance genericFoo :: Generic (Foo a) _

instance showFoo :: Show a => Show (Foo a) where
  show = genericShow

instance arbitraryFoo :: Arbitrary a => Arbitrary (Foo a) where
  arbitrary = genericArbitrary

quickCheckFail :: forall t. Testable t => t -> Effect Unit
quickCheckFail = assert <=< map isLeft <<< try <<< quickCheck

testResize :: (forall a. Size -> Gen a -> Gen a) -> Boolean
testResize resize' =
  let
    initialSize = 2

    gen = do
      s1 <- sized pure
      s2 <- resize' 1 (sized pure)
      s3 <- sized pure
      pure $ [ s1, s2, s3 ] == [ initialSize, 1, initialSize ]
  in
    fst $ runGen gen { newSeed: mkSeed 0, size: initialSize }

testQuickCheck :: Effect Unit
testQuickCheck = do
  log "MonadGen.resize"
  assert (testResize (MGen.resize <<< const))
  log "Gen.resize"
  assert (testResize (resize))
  log "Try with some little Gens first"
  logShow =<< go 10
  logShow =<< go 100
  logShow =<< go 1000
  logShow =<< go 10000
  log "Testing stack safety of Gen"
  logShow =<< go 20000
  logShow =<< go 100000
  log "Generating via Generic"
  logShow =<< randomSample' 10 (arbitrary :: Gen (Foo Int))
  log "Arbitrary instance for records"
  listOfRecords ← randomSample' 10 (arbitrary :: Gen { foo :: Int, nested :: { bar :: Boolean } })
  let
    toString rec = "{ foo: " <> show rec.foo <> "; nested.bar: " <> show rec.nested.bar <> " }"
  logShow (toString <$> listOfRecords)
  quickCheck \(x :: Int) -> x <? x + 1
  quickCheck \(x :: Int) -> x <=? x + 1
  quickCheck \(x :: Int) -> x >=? x - 1
  quickCheck \(x :: Int) -> x >? x - 1
  quickCheck \(x :: Int) -> x + x ==? x * 2
  quickCheck \(x :: Int) -> x + x /=? x * 3
  quickCheck $ 1 ==? 1
  quickCheckFail $ 1 /=? 1
  quickCheck $ 1 <? 2
  quickCheckFail $ 1 >=? 2
  quickCheck $ 3 <=? 3
  quickCheckFail $ 3 >? 3
  quickCheck $ 3 >=? 3
  quickCheckFail $ 3 <? 3
  quickCheck $ 4 /=? 3
  quickCheckFail $ 4 ==? 3
  quickCheck $ 4 >? 3
  quickCheckFail $ 4 <=? 3
  where
  go n = map (sum <<< unsafeHead) $ randomSample' 1 (vectorOf n (arbitrary :: Gen Int))

  unsafeHead :: forall x. Array x -> x
  unsafeHead xs = unsafePartial (head xs)
