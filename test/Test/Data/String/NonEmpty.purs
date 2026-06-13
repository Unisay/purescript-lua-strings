module Test.Data.String.NonEmpty (testNonEmptyString) where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromJust)
import Data.String.NonEmpty (Pattern(..), nes)
import Data.String.NonEmpty as NES
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert, assertEqual)
import Type.Proxy (Proxy(..))

testNonEmptyString :: Effect Unit
testNonEmptyString = do
  testFromString
  testToString
  testAppendString
  testPrependString
  testContains
  testLocaleCompare
  testReplace
  testReplaceAll
  testStripPrefix
  testStripSuffix
  testToLower
  testToUpper
  testTrim
  testJoinWith
  testJoin1With
  testJoinWith1

testFromString :: Effect Unit
testFromString = do
  log "fromString"
  assertEqual
    { actual: NES.fromString ""
    , expected: Nothing
    }
  assertEqual
    { actual: NES.fromString "hello"
    , expected: Just (nes (Proxy :: Proxy "hello"))
    }

testToString :: Effect Unit
testToString = do
  log "toString"
  assertEqual
    { actual: (NES.toString <$> NES.fromString "hello")
    , expected: Just "hello"
    }

testAppendString :: Effect Unit
testAppendString = do
  log "appendString"
  assertEqual
    { actual: NES.appendString (nes (Proxy :: Proxy "Hello")) " world"
    , expected: nes (Proxy :: Proxy "Hello world")
    }
  assertEqual
    { actual: NES.appendString (nes (Proxy :: Proxy "Hello")) ""
    , expected: nes (Proxy :: Proxy "Hello")
    }

testPrependString :: Effect Unit
testPrependString = do
  log "prependString"
  assertEqual
    { actual: NES.prependString "be" (nes (Proxy :: Proxy "fore"))
    , expected: nes (Proxy :: Proxy "before")
    }
  assertEqual
    { actual: NES.prependString "" (nes (Proxy :: Proxy "fore"))
    , expected: nes (Proxy :: Proxy "fore")
    }

testContains :: Effect Unit
testContains = do
  log "contains"
  assert $ NES.contains (Pattern "") (nes (Proxy :: Proxy "abcd"))
  assert $ NES.contains (Pattern "bc") (nes (Proxy :: Proxy "abcd"))
  assert $ not NES.contains (Pattern "cb") (nes (Proxy :: Proxy "abcd"))
  assert $ NES.contains (Pattern "needle") (nes (Proxy :: Proxy "haystack with needle"))
  assert $ not NES.contains (Pattern "needle") (nes (Proxy :: Proxy "haystack"))

testLocaleCompare :: Effect Unit
testLocaleCompare = do
  log "localeCompare"
  assertEqual
    { actual: NES.localeCompare (nes (Proxy :: Proxy "a")) (nes (Proxy :: Proxy "a"))
    , expected: EQ
    }
  assertEqual
    { actual: NES.localeCompare (nes (Proxy :: Proxy "a")) (nes (Proxy :: Proxy "b"))
    , expected: LT
    }
  assertEqual
    { actual: NES.localeCompare (nes (Proxy :: Proxy "b")) (nes (Proxy :: Proxy "a"))
    , expected: GT
    }

testReplace :: Effect Unit
testReplace = do
  log "replace"
  assertEqual
    { actual: NES.replace (Pattern "b") (NES.NonEmptyReplacement (nes (Proxy :: Proxy "!"))) (nes (Proxy :: Proxy "abc"))
    , expected: nes (Proxy :: Proxy "a!c")
    }
  assertEqual
    { actual: NES.replace (Pattern "b") (NES.NonEmptyReplacement (nes (Proxy :: Proxy "!"))) (nes (Proxy :: Proxy "abbc"))
    , expected: nes (Proxy :: Proxy "a!bc")
    }
  assertEqual
    { actual: NES.replace (Pattern "d") (NES.NonEmptyReplacement (nes (Proxy :: Proxy "!"))) (nes (Proxy :: Proxy "abc"))
    , expected: nes (Proxy :: Proxy "abc")
    }

testReplaceAll :: Effect Unit
testReplaceAll = do
  log "replaceAll"
  assertEqual
    { actual: NES.replaceAll (Pattern "[b]") (NES.NonEmptyReplacement (nes (Proxy :: Proxy "!"))) (nes (Proxy :: Proxy "a[b]c"))
    , expected: nes (Proxy :: Proxy "a!c")
    }
  assertEqual
    { actual: NES.replaceAll (Pattern "[b]") (NES.NonEmptyReplacement (nes (Proxy :: Proxy "!"))) (nes (Proxy :: Proxy "a[b]c[b]"))
    , expected: nes (Proxy :: Proxy "a!c!")
    }
  assertEqual
    { actual: NES.replaceAll (Pattern "x") (NES.NonEmptyReplacement (nes (Proxy :: Proxy "!"))) (nes (Proxy :: Proxy "abc"))
    , expected: nes (Proxy :: Proxy "abc")
    }

testStripPrefix :: Effect Unit
testStripPrefix = do
  log "stripPrefix"
  assertEqual
    { actual: NES.stripPrefix (Pattern "") (nes (Proxy :: Proxy "abc"))
    , expected: Just (nes (Proxy :: Proxy "abc"))
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "a") (nes (Proxy :: Proxy "abc"))
    , expected: Just (nes (Proxy :: Proxy "bc"))
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "abc") (nes (Proxy :: Proxy "abc"))
    , expected: Nothing
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "!") (nes (Proxy :: Proxy "abc"))
    , expected: Nothing
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "http:") (nes (Proxy :: Proxy "http://purescript.org"))
    , expected: Just (nes (Proxy :: Proxy "//purescript.org"))
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "http:") (nes (Proxy :: Proxy "https://purescript.org"))
    , expected: Nothing
    }
  assertEqual
    { actual: NES.stripPrefix (Pattern "Hello!") (nes (Proxy :: Proxy "Hello!"))
    , expected: Nothing
    }

testStripSuffix :: Effect Unit
testStripSuffix = do
  log "stripSuffix"
  assertEqual
    { actual: NES.stripSuffix (Pattern ".exe") (nes (Proxy :: Proxy "purs.exe"))
    , expected: Just (nes (Proxy :: Proxy "purs"))
    }
  assertEqual
    { actual: NES.stripSuffix (Pattern ".exe") (nes (Proxy :: Proxy "purs"))
    , expected: Nothing
    }
  assertEqual
    { actual: NES.stripSuffix (Pattern "Hello!") (nes (Proxy :: Proxy "Hello!"))
    , expected: Nothing
    }

testToLower :: Effect Unit
testToLower = do
  log "toLower"
  assertEqual
    { actual: NES.toLower (nes (Proxy :: Proxy "bAtMaN"))
    , expected: nes (Proxy :: Proxy "batman")
    }

testToUpper :: Effect Unit
testToUpper = do
  log "toUpper"
  assertEqual
    { actual: NES.toUpper (nes (Proxy :: Proxy "bAtMaN"))
    , expected: nes (Proxy :: Proxy "BATMAN")
    }

testTrim :: Effect Unit
testTrim = do
  log "trim"
  assertEqual
    { actual: NES.trim (nes (Proxy :: Proxy "  abc  "))
    , expected: Just (nes (Proxy :: Proxy "abc"))
    }
  assertEqual
    { actual: NES.trim (nes (Proxy :: Proxy "   \n"))
    , expected: Nothing
    }

testJoinWith :: Effect Unit
testJoinWith = do
  log "joinWith"
  assertEqual
    { actual: NES.joinWith "" []
    , expected: ""
    }
  assertEqual
    { actual: NES.joinWith "" [nes (Proxy :: Proxy "a"), nes (Proxy :: Proxy "b")]
    , expected: "ab"
    }
  assertEqual
    { actual: NES.joinWith "--" [nes (Proxy :: Proxy "a"), nes (Proxy :: Proxy "b"), nes (Proxy :: Proxy "c")]
    , expected: "a--b--c"
    }

testJoin1With :: Effect Unit
testJoin1With = do
  log "join1With"
  assertEqual
    { actual: NES.join1With "" (nea [nes (Proxy :: Proxy "a"), nes (Proxy :: Proxy "b")])
    , expected: nes (Proxy :: Proxy "ab")
    }
  assertEqual
    { actual: NES.join1With "--" (nea [nes (Proxy :: Proxy "a"), nes (Proxy :: Proxy "b"), nes (Proxy :: Proxy "c")])
    , expected: nes (Proxy :: Proxy "a--b--c")
    }
  assertEqual
    { actual: NES.join1With ", " (nea [nes (Proxy :: Proxy "apple"), nes (Proxy :: Proxy "banana")])
    , expected: nes (Proxy :: Proxy "apple, banana")
    }
  assertEqual
    { actual: NES.join1With "" (nea [nes (Proxy :: Proxy "apple"), nes (Proxy :: Proxy "banana")])
    , expected: nes (Proxy :: Proxy "applebanana")
    }

testJoinWith1 :: Effect Unit
testJoinWith1 = do
  log "joinWith1"
  assertEqual
    { actual: NES.joinWith1 (nes (Proxy :: Proxy " ")) (nea ["a", "b"])
    , expected: nes (Proxy :: Proxy "a b")
    }
  assertEqual
    { actual: NES.joinWith1 (nes (Proxy :: Proxy "--")) (nea ["a", "b", "c"])
    , expected: nes (Proxy :: Proxy "a--b--c")
    }
  assertEqual
    { actual: NES.joinWith1 (nes (Proxy :: Proxy ", ")) (nea ["apple", "banana"])
    , expected: nes (Proxy :: Proxy "apple, banana")
    }
  assertEqual
    { actual: NES.joinWith1 (nes (Proxy :: Proxy "/")) (nea ["a", "b", "", "c", ""])
    , expected: nes (Proxy :: Proxy "a/b//c/")
    }

nea :: Array ~> NEA.NonEmptyArray
nea = unsafePartial fromJust <<< NEA.fromArray
