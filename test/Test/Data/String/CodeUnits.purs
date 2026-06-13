module Test.Data.String.CodeUnits (testStringCodeUnits) where

import Prelude

import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..), isNothing)
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert, assertEqual)

testStringCodeUnits :: Effect Unit
testStringCodeUnits = do
  testStripPrefix
  testStripSuffix
  testCharAt
  testSingleton
  testCharCodeAt
  testToChar
  testUncons
  testTakeWhile
  testDropWhile
  testFromCharArray
  testIndexOf
  testIndexOf'
  testLastIndexOf
  testLastIndexOf'
  testLength
  testTake
  testTakeRight
  testDrop
  testDropRight
  testCountPrefix
  testSplitAt
  testToCharArray
  testSlice

testStripPrefix :: Effect Unit
testStripPrefix = do
  log "stripPrefix"
  assertEqual
    { actual: SCU.stripPrefix (Pattern "abc") "abcde"
    , expected: Just "de"
    }
  assertEqual
    { actual: SCU.stripPrefix (Pattern "xyz") "abcde"
    , expected: Nothing
    }
  assertEqual
    { actual: SCU.stripPrefix (Pattern "abcd") "ab"
    , expected: Nothing
    }
  assertEqual
    { actual: SCU.stripPrefix (Pattern "abc") "abc"
    , expected: Just ""
    }
  assertEqual
    { actual: SCU.stripPrefix (Pattern "") "abc"
    , expected: Just "abc"
    }
  assertEqual
    { actual: SCU.stripPrefix (Pattern "") ""
    , expected: Just ""
    }

testStripSuffix :: Effect Unit
testStripSuffix = do
  log "stripSuffix"
  assertEqual
    { actual: SCU.stripSuffix (Pattern "cde") "abcde"
    , expected: Just "ab"
    }
  assertEqual
    { actual: SCU.stripSuffix (Pattern "xyz") "abcde"
    , expected: Nothing
    }
  assertEqual
    { actual: SCU.stripSuffix (Pattern "abcd") "cd"
    , expected: Nothing
    }
  assertEqual
    { actual: SCU.stripSuffix (Pattern "abc") "abc"
    , expected: Just ""
    }
  assertEqual
    { actual: SCU.stripSuffix (Pattern "") "abc"
    , expected: Just "abc"
    }
  assertEqual
    { actual: SCU.stripSuffix (Pattern "") ""
    , expected: Just ""
    }

testCharAt :: Effect Unit
testCharAt = do
  log "charAt"
  assertEqual
    { actual: SCU.charAt 0 ""
    , expected: Nothing
    }
  assertEqual
    { actual: SCU.charAt 0 "a"
    , expected: Just 'a'
    }
  assertEqual
    { actual: SCU.charAt 1 "a"
    , expected: Nothing
    }
  assertEqual
    { actual: SCU.charAt 0 "ab"
    , expected: Just 'a'
    }
  assertEqual
    { actual: SCU.charAt 1 "ab"
    , expected: Just 'b'
    }
  assertEqual
    { actual: SCU.charAt 2 "ab"
    , expected: Nothing
    }

testSingleton :: Effect Unit
testSingleton = do
  log "singleton"
  assertEqual
    { actual: SCU.singleton 'a'
    , expected: "a"
    }

testCharCodeAt :: Effect Unit
testCharCodeAt = do
  log "charCodeAt"
  assertEqual
    { actual: (fromEnum <$> SCU.charAt 0 "")
    , expected: Nothing
    }
  assertEqual
    { actual: (fromEnum <$> SCU.charAt 0 "a")
    , expected: Just 97
    }
  assertEqual
    { actual: (fromEnum <$> SCU.charAt 1 "a")
    , expected: Nothing
    }
  assertEqual
    { actual: (fromEnum <$> SCU.charAt 0 "ab")
    , expected: Just 97
    }
  assertEqual
    { actual: (fromEnum <$> SCU.charAt 1 "ab")
    , expected: Just 98
    }
  assertEqual
    { actual: (fromEnum <$> SCU.charAt 2 "ab")
    , expected: Nothing
    }

testToChar :: Effect Unit
testToChar = do
  log "toChar"
  assertEqual
    { actual: SCU.toChar ""
    , expected: Nothing
    }
  assertEqual
    { actual: SCU.toChar "a"
    , expected: Just 'a'
    }
  assertEqual
    { actual: SCU.toChar "ab"
    , expected: Nothing
    }

testUncons :: Effect Unit
testUncons = do
  log "uncons"
  assert $ isNothing (SCU.uncons "")
  assertEqual
    { actual: SCU.uncons "a"
    , expected: Just { head: 'a', tail: "" }
    }
  assertEqual
    { actual: SCU.uncons "ab"
    , expected: Just { head: 'a', tail: "b" }
    }

testTakeWhile :: Effect Unit
testTakeWhile = do
  log "takeWhile"
  assertEqual
    { actual: SCU.takeWhile (\c -> true) "abc"
    , expected: "abc"
    }
  assertEqual
    { actual: SCU.takeWhile (\c -> false) "abc"
    , expected: ""
    }
  assertEqual
    { actual: SCU.takeWhile (\c -> c /= 'b') "aabbcc"
    , expected: "aa"
    }

testDropWhile :: Effect Unit
testDropWhile = do
  log "dropWhile"
  assertEqual
    { actual: SCU.dropWhile (\c -> true) "abc"
    , expected: ""
    }
  assertEqual
    { actual: SCU.dropWhile (\c -> false) "abc"
    , expected: "abc"
    }
  assertEqual
    { actual: SCU.dropWhile (\c -> c /= 'b') "aabbcc"
    , expected: "bbcc"
    }

testFromCharArray :: Effect Unit
testFromCharArray = do
  log "fromCharArray"
  assertEqual
    { actual: SCU.fromCharArray []
      , expected: ""
      }
  assertEqual
    { actual: SCU.fromCharArray ['a', 'b']
    , expected: "ab"
    }

testIndexOf :: Effect Unit
testIndexOf = do
  log "indexOf"
  assertEqual
    { actual: SCU.indexOf (Pattern "") ""
    , expected: Just 0
    }
  assertEqual
    { actual: SCU.indexOf (Pattern "") "abcd"
    , expected: Just 0
    }
  assertEqual
    { actual: SCU.indexOf (Pattern "bc") "abcd"
    , expected: Just 1
    }
  assertEqual
    { actual: SCU.indexOf (Pattern "cb") "abcd"
    , expected: Nothing
    }

testIndexOf' :: Effect Unit
testIndexOf' = do
  log "indexOf'"
  assertEqual
    { actual: SCU.indexOf' (Pattern "") 0 ""
    , expected: Just 0
    }
  assertEqual
    { actual: SCU.indexOf' (Pattern "") (-1) "ab"
    , expected: Nothing
    }
  assertEqual
    { actual: SCU.indexOf' (Pattern "") 0 "ab"
    , expected: Just 0
    }
  assertEqual
    { actual: SCU.indexOf' (Pattern "") 1 "ab"
    , expected: Just 1
    }
  assertEqual
    { actual: SCU.indexOf' (Pattern "") 2 "ab"
    , expected: Just 2
    }
  assertEqual
    { actual: SCU.indexOf' (Pattern "") 3 "ab"
    , expected: Nothing
    }
  assertEqual
    { actual: SCU.indexOf' (Pattern "bc") 0 "abcd"
    , expected: Just 1
    }
  assertEqual
    { actual: SCU.indexOf' (Pattern "bc") 1 "abcd"
    , expected: Just 1
    }
  assertEqual
    { actual: SCU.indexOf' (Pattern "bc") 2 "abcd"
    , expected: Nothing
    }
  assertEqual
    { actual: SCU.indexOf' (Pattern "cb") 0 "abcd"
    , expected: Nothing
    }

testLastIndexOf :: Effect Unit
testLastIndexOf = do
  log "lastIndexOf"
  assertEqual
    { actual: SCU.lastIndexOf (Pattern "") ""
    , expected: Just 0
    }
  assertEqual
    { actual: SCU.lastIndexOf (Pattern "") "abcd"
    , expected: Just 4
    }
  assertEqual
    { actual: SCU.lastIndexOf (Pattern "bc") "abcd"
    , expected: Just 1
    }
  assertEqual
    { actual: SCU.lastIndexOf (Pattern "cb") "abcd"
    , expected: Nothing
    }

testLastIndexOf' :: Effect Unit
testLastIndexOf' = do
  log "lastIndexOf'"
  assertEqual
    { actual: SCU.lastIndexOf' (Pattern "") 0 ""
    , expected: Just 0
    }
  assertEqual
    { actual: SCU.lastIndexOf' (Pattern "") (-1) "ab"
    , expected: Just 0
    }
  assertEqual
    { actual: SCU.lastIndexOf' (Pattern "") 0 "ab"
    , expected: Just 0
    }
  assertEqual
    { actual: SCU.lastIndexOf' (Pattern "") 1 "ab"
    , expected: Just 1
    }
  assertEqual
    { actual: SCU.lastIndexOf' (Pattern "") 2 "ab"
    , expected: Just 2
    }
  assertEqual
    { actual: SCU.lastIndexOf' (Pattern "") 3 "ab"
    , expected: Just 2
    }
  assertEqual
    { actual: SCU.lastIndexOf' (Pattern "bc") 0 "abcd"
    , expected: Nothing
    }
  assertEqual
    { actual: SCU.lastIndexOf' (Pattern "bc") 1 "abcd"
    , expected: Just 1
    }
  assertEqual
    { actual: SCU.lastIndexOf' (Pattern "bc") 2 "abcd"
    , expected: Just 1
    }
  assertEqual
    { actual: SCU.lastIndexOf' (Pattern "cb") 0 "abcd"
    , expected: Nothing
    }

testLength :: Effect Unit
testLength = do
  log "length"
  assertEqual
    { actual: SCU.length ""
    , expected: 0
    }
  assertEqual
    { actual: SCU.length "a"
    , expected: 1
    }
  assertEqual
    { actual: SCU.length "ab"
    , expected: 2
    }

testTake :: Effect Unit
testTake = do
  log "take"
  assertEqual
    { actual: SCU.take 0 "ab"
    , expected: ""
    }
  assertEqual
    { actual: SCU.take 1 "ab"
    , expected: "a"
    }
  assertEqual
    { actual: SCU.take 2 "ab"
    , expected: "ab"
    }
  assertEqual
    { actual: SCU.take 3 "ab"
    , expected: "ab"
    }
  assertEqual
    { actual: SCU.take (-1) "ab"
    , expected: ""
    }

testTakeRight :: Effect Unit
testTakeRight = do
  log "takeRight"
  assertEqual
    { actual: SCU.takeRight 0 "ab"
    , expected: ""
    }
  assertEqual
    { actual: SCU.takeRight 1 "ab"
    , expected: "b"
    }
  assertEqual
    { actual: SCU.takeRight 2 "ab"
    , expected: "ab"
    }
  assertEqual
    { actual: SCU.takeRight 3 "ab"
    , expected: "ab"
    }
  assertEqual
    { actual: SCU.takeRight (-1) "ab"
    , expected: ""
    }

testDrop :: Effect Unit
testDrop = do
  log "drop"
  assertEqual
    { actual: SCU.drop 0 "ab"
    , expected: "ab"
    }
  assertEqual
    { actual: SCU.drop 1 "ab"
    , expected: "b"
    }
  assertEqual
    { actual: SCU.drop 2 "ab"
    , expected: ""
    }
  assertEqual
    { actual: SCU.drop 3 "ab"
    , expected: ""
    }
  assertEqual
    { actual: SCU.drop (-1) "ab"
    , expected: "ab"
    }

testDropRight :: Effect Unit
testDropRight = do
  log "dropRight"
  assertEqual
    { actual: SCU.dropRight 0 "ab"
    , expected: "ab"
    }
  assertEqual
    { actual: SCU.dropRight 1 "ab"
    , expected: "a"
    }
  assertEqual
    { actual: SCU.dropRight 2 "ab"
    , expected: ""
    }
  assertEqual
    { actual: SCU.dropRight 3 "ab"
    , expected: ""
    }
  assertEqual
    { actual: SCU.dropRight (-1) "ab"
    , expected: "ab"
    }

testCountPrefix :: Effect Unit
testCountPrefix = do
  log "countPrefix"
  assertEqual
    { actual: SCU.countPrefix (_ == 'a') ""
    , expected: 0
    }
  assertEqual
    { actual: SCU.countPrefix (_ == 'a') "ab"
    , expected: 1
    }
  assertEqual
    { actual: SCU.countPrefix (_ == 'a') "aaab"
    , expected: 3
    }
  assertEqual
    { actual: SCU.countPrefix (_ == 'a') "abaa"
    , expected: 1
    }

testSplitAt :: Effect Unit
testSplitAt = do
  log "splitAt"
  assertEqual
    { actual: SCU.splitAt 1 ""
    , expected: {before: "", after: ""}
    }
  assertEqual
    { actual: SCU.splitAt 0 "a"
    , expected: {before: "", after: "a"}
    }
  assertEqual
    { actual: SCU.splitAt 1 "a"
    , expected: {before: "a", after: ""}
    }
  assertEqual
    { actual: SCU.splitAt 1 "ab"
    , expected: {before: "a", after: "b"}
    }
  assertEqual
    { actual: SCU.splitAt 3 "aabcc"
    , expected: {before: "aab", after: "cc"}
    }
  assertEqual
    { actual: SCU.splitAt (-1) "abc"
    , expected: {before: "", after: "abc"}
    }
  assertEqual
    { actual: SCU.splitAt 10 "Hi"
    , expected: {before: "Hi", after: ""}
    }

testToCharArray :: Effect Unit
testToCharArray = do
  log "toCharArray"
  assertEqual
    { actual: SCU.toCharArray ""
    , expected: []
    }
  assertEqual
    { actual: SCU.toCharArray "a"
    , expected: ['a']
    }
  assertEqual
    { actual: SCU.toCharArray "ab"
    , expected: ['a', 'b']
    }

testSlice :: Effect Unit
testSlice = do
  log "slice"
  assertEqual
    { actual: SCU.slice 0 0   "purescript"
    , expected: ""
    }
  assertEqual
    { actual: SCU.slice 0 1   "purescript"
    , expected: "p"
    }
  assertEqual
    { actual: SCU.slice 3 6   "purescript"
    , expected: "esc"
    }
  assertEqual
    { actual: SCU.slice 3 10  "purescript"
    , expected: "escript"
    }
  assertEqual
    { actual: SCU.slice 10 10 "purescript"
    , expected: ""
    }
  assertEqual
    { actual: SCU.slice (-4) (-1) "purescript"
    , expected: "rip"
    }
  assertEqual
    { actual: SCU.slice (-4) 3  "purescript"
    , expected: ""
    }
  assertEqual
    { actual: SCU.slice 1000 3  "purescript"
    , expected: ""
    }
  assertEqual
    { actual: SCU.slice 2 (-15) "purescript"
    , expected: ""
    }
  assertEqual
    { actual: SCU.slice (-15) 9 "purescript"
    , expected: "purescrip"
    }
  assertEqual
    { actual: SCU.slice 3 1000 "purescript"
    , expected: "escript"
    }
