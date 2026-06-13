-- Deviation from upstream purescript-strings: in pslua a String is a UTF-8
-- byte string, not a sequence of UTF-16 code units. The upstream test string
-- was built around lone surrogates ("a\xDC00\xD800\xD800\x16805\x16A06z"),
-- which are not representable in UTF-8. This port keeps the structure of the
-- upstream suite (7 code points, a repeated one, astral characters, ASCII
-- anchors at both ends) but uses well-formed Unicode of varying UTF-8 widths:
-- 'a' (1 byte), 'é' (2), 'П' (2, repeated), U+16805 (4), U+16A06 (4),
-- 'z' (1). Cases probing lone-surrogate behaviour (e.g. searching for
-- "\xD81A", the lead surrogate of U+16805) have no UTF-8 counterpart and
-- were dropped. The Char type is a single byte in pslua, so
-- codePointFromChar is only exercised on ASCII.
--
-- The single upstream do-block is also split into per-section functions:
-- one huge do-block generates Lua nested beyond the parser limit of stock
-- Lua 5.1 interpreters (Unisay/purescript-lua#46).
module Test.Data.String.CodePoints (testStringCodePoints) where

import Prelude

import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.CodePoints as SCP
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assertEqual)

str :: String
str = "aéПП\x16805\x16A06z"

testStringCodePoints :: Effect Unit
testStringCodePoints = do
  testShow
  testCodePointFromChar
  testSingleton
  testToFromCodePointArray
  testBoundaries
  testMalformed
  testCodePointAt
  testUncons
  testLength
  testCountPrefix
  testIndexOf
  testIndexOfStartingAt
  testLastIndexOf
  testLastIndexOfStartingAt1
  testLastIndexOfStartingAt2
  testTake
  testTakeWhile
  testDrop
  testDropWhile
  testSplitAt1
  testSplitAt2

testShow :: Effect Unit
testShow = do
  log "show"
  assertEqual
    { actual: map show (SCP.codePointAt 0 str)
    , expected: Just "(CodePoint 0x61)"
    }
  assertEqual
    { actual: map show (SCP.codePointAt 1 str)
    , expected: Just "(CodePoint 0xE9)"
    }
  assertEqual
    { actual: map show (SCP.codePointAt 2 str)
    , expected: Just "(CodePoint 0x41F)"
    }
  assertEqual
    { actual: map show (SCP.codePointAt 3 str)
    , expected: Just "(CodePoint 0x41F)"
    }
  assertEqual
    { actual: map show (SCP.codePointAt 4 str)
    , expected: Just "(CodePoint 0x16805)"
    }
  assertEqual
    { actual: map show (SCP.codePointAt 5 str)
    , expected: Just "(CodePoint 0x16A06)"
    }
  assertEqual
    { actual: map show (SCP.codePointAt 6 str)
    , expected: Just "(CodePoint 0x7A)"
    }

testCodePointFromChar :: Effect Unit
testCodePointFromChar = do
  log "codePointFromChar"
  assertEqual
    { actual: Just (SCP.codePointFromChar 'A')
    , expected: (toEnum 65)
    }
  assertEqual
    { actual: (SCP.codePointFromChar <$> toEnum 0)
    , expected: toEnum 0
    }
  assertEqual
    { actual: (SCP.codePointFromChar <$> toEnum 0x7A)
    , expected: toEnum 0x7A
    }

testSingleton :: Effect Unit
testSingleton = do
  log "singleton"
  assertEqual
    { actual: (SCP.singleton <$> toEnum 0x30)
    , expected: Just "0"
    }
  assertEqual
    { actual: (SCP.singleton <$> toEnum 0xE9)
    , expected: Just "é"
    }
  assertEqual
    { actual: (SCP.singleton <$> toEnum 0x20AC)
    , expected: Just "€"
    }
  assertEqual
    { actual: (SCP.singleton <$> toEnum 0x16805)
    , expected: Just "\x16805"
    }

testToFromCodePointArray :: Effect Unit
testToFromCodePointArray = do
  log "toCodePointArray"
  assertEqual
    { actual: SCP.toCodePointArray ""
    , expected: []
    }
  assertEqual
    { actual: map fromEnum (SCP.toCodePointArray str)
    , expected: [ 0x61, 0xE9, 0x41F, 0x41F, 0x16805, 0x16A06, 0x7A ]
    }
  log "fromCodePointArray"
  assertEqual
    { actual: SCP.fromCodePointArray []
    , expected: ""
    }
  assertEqual
    { actual: SCP.fromCodePointArray (SCP.toCodePointArray str)
    , expected: str
    }

-- Exercises the UTF-8 encoder/decoder at every width boundary. Each code
-- point round-trips through fromCodePointArray (encode) and back through
-- toCodePointArray (decode), and singleton agrees with a one-element array.
testBoundaries :: Effect Unit
testBoundaries = do
  log "encode/decode boundaries"
  let
    -- 1-byte edges, 2-byte edges, 3-byte edges, 4-byte edges
    codes = [ 0x0, 0x7F, 0x80, 0x7FF, 0x800, 0xFFFF, 0x10000, 0x10FFFF ]
    cps = map cp codes
    encoded = SCP.fromCodePointArray cps
  assertEqual
    { actual: map fromEnum (SCP.toCodePointArray encoded)
    , expected: codes
    }
  assertEqual
    { actual: SCP.length encoded
    , expected: 8
    }
  -- byte widths: 1 + 1 + 2 + 2 + 3 + 3 + 4 + 4
  assertEqual
    { actual: SCU.length encoded
    , expected: 20
    }
  assertEqual
    { actual: map (\c -> SCU.length (SCP.singleton c)) cps
    , expected: [ 1, 1, 2, 2, 3, 3, 4, 4 ]
    }

-- Malformed UTF-8 (a lone continuation byte, a truncated multi-byte lead)
-- must decode one byte at a time without crashing or desynchronising the
-- bytes that follow. The raw bytes are built with CodeUnits.singleton
-- (a Char is one byte in pslua); a non-ASCII string literal would be
-- re-encoded as valid UTF-8 by the compiler and could not express them.
testMalformed :: Effect Unit
testMalformed = do
  log "malformed input"
  let
    loneCont = "a" <> byte 0x80 <> "b"
    truncated2 = byte 0xC2
    truncated3 = byte 0xE2 <> byte 0x82
  -- lone continuation byte between ASCII anchors: 'b' must survive
  assertEqual
    { actual: map fromEnum (SCP.toCodePointArray loneCont)
    , expected: [ 0x61, 0x80, 0x62 ]
    }
  assertEqual
    { actual: SCP.length loneCont
    , expected: 3
    }
  -- uncons of the lone byte must consume exactly one byte, leaving "b"
  assertEqual
    { actual: (_.tail) <$> SCP.uncons (byte 0x80 <> "b")
    , expected: Just "b"
    }
  assertEqual
    { actual: (fromEnum <<< _.head) <$> SCP.uncons (byte 0x80 <> "b")
    , expected: Just 0x80
    }
  -- truncated two-byte lead (0xC2 with no continuation)
  assertEqual
    { actual: map fromEnum (SCP.toCodePointArray truncated2)
    , expected: [ 0xC2 ]
    }
  -- truncated three-byte lead (first two bytes of the euro sign)
  assertEqual
    { actual: map fromEnum (SCP.toCodePointArray truncated3)
    , expected: [ 0xE2, 0x82 ]
    }

testCodePointAt :: Effect Unit
testCodePointAt = do
  log "codePointAt"
  assertEqual
    { actual: SCP.codePointAt (-1) str
    , expected: Nothing
    }
  assertEqual
    { actual: SCP.codePointAt 0 str
    , expected: (toEnum 0x61)
    }
  assertEqual
    { actual: SCP.codePointAt 1 str
    , expected: (toEnum 0xE9)
    }
  assertEqual
    { actual: SCP.codePointAt 2 str
    , expected: (toEnum 0x41F)
    }
  assertEqual
    { actual: SCP.codePointAt 3 str
    , expected: (toEnum 0x41F)
    }
  assertEqual
    { actual: SCP.codePointAt 4 str
    , expected: (toEnum 0x16805)
    }
  assertEqual
    { actual: SCP.codePointAt 5 str
    , expected: (toEnum 0x16A06)
    }
  assertEqual
    { actual: SCP.codePointAt 6 str
    , expected: (toEnum 0x7A)
    }
  assertEqual
    { actual: SCP.codePointAt 7 str
    , expected: Nothing
    }

testUncons :: Effect Unit
testUncons = do
  log "uncons"
  assertEqual
    { actual: SCP.uncons str
    , expected: Just { head: cp 0x61, tail: "éПП\x16805\x16A06z" }
    }
  assertEqual
    { actual: SCP.uncons (SCP.drop 1 str)
    , expected: Just { head: cp 0xE9, tail: "ПП\x16805\x16A06z" }
    }
  assertEqual
    { actual: SCP.uncons (SCP.drop 2 str)
    , expected: Just { head: cp 0x41F, tail: "П\x16805\x16A06z" }
    }
  assertEqual
    { actual: SCP.uncons (SCP.drop 3 str)
    , expected: Just { head: cp 0x41F, tail: "\x16805\x16A06z" }
    }
  assertEqual
    { actual: SCP.uncons (SCP.drop 4 str)
    , expected: Just { head: cp 0x16805, tail: "\x16A06z" }
    }
  assertEqual
    { actual: SCP.uncons (SCP.drop 5 str)
    , expected: Just { head: cp 0x16A06, tail: "z" }
    }
  assertEqual
    { actual: SCP.uncons (SCP.drop 6 str)
    , expected: Just { head: cp 0x7A, tail: "" }
    }
  assertEqual
    { actual: SCP.uncons ""
    , expected: Nothing
    }

testLength :: Effect Unit
testLength = do
  log "length"
  assertEqual
    { actual: SCP.length ""
    , expected: 0
    }
  assertEqual
    { actual: SCP.length "a"
    , expected: 1
    }
  assertEqual
    { actual: SCP.length "ab"
    , expected: 2
    }
  assertEqual
    { actual: SCP.length "é"
    , expected: 1
    }
  assertEqual
    { actual: SCP.length "€"
    , expected: 1
    }
  assertEqual
    { actual: SCP.length "\x16805"
    , expected: 1
    }
  assertEqual
    { actual: SCP.length str
    , expected: 7
    }

testCountPrefix :: Effect Unit
testCountPrefix = do
  log "countPrefix"
  assertEqual
    { actual: SCP.countPrefix (\_ -> true) ""
    , expected: 0
    }
  assertEqual
    { actual: SCP.countPrefix (\_ -> false) str
    , expected: 0
    }
  assertEqual
    { actual: SCP.countPrefix (\_ -> true) str
    , expected: 7
    }
  assertEqual
    { actual: SCP.countPrefix (\x -> fromEnum x < 0xFFFF) str
    , expected: 4
    }
  assertEqual
    { actual: SCP.countPrefix (\x -> fromEnum x < 0xE9) str
    , expected: 1
    }

testIndexOf :: Effect Unit
testIndexOf = do
  log "indexOf"
  assertEqual
    { actual: SCP.indexOf (Pattern "") ""
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.indexOf (Pattern "") str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.indexOf (Pattern str) str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.indexOf (Pattern "a") str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.indexOf (Pattern "éПП") str
    , expected: Just 1
    }
  assertEqual
    { actual: SCP.indexOf (Pattern "П") str
    , expected: Just 2
    }
  assertEqual
    { actual: SCP.indexOf (Pattern "ПП") str
    , expected: Just 2
    }
  assertEqual
    { actual: SCP.indexOf (Pattern "П\x16805") str
    , expected: Just 3
    }
  assertEqual
    { actual: SCP.indexOf (Pattern "\x16805") str
    , expected: Just 4
    }
  assertEqual
    { actual: SCP.indexOf (Pattern "\x16A06") str
    , expected: Just 5
    }
  assertEqual
    { actual: SCP.indexOf (Pattern "z") str
    , expected: Just 6
    }
  assertEqual
    { actual: SCP.indexOf (Pattern "\n") str
    , expected: Nothing
    }

testIndexOfStartingAt :: Effect Unit
testIndexOfStartingAt = do
  log "indexOf'"
  assertEqual
    { actual: SCP.indexOf' (Pattern "") 0 ""
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.indexOf' (Pattern str) 0 str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.indexOf' (Pattern str) 1 str
    , expected: Nothing
    }
  assertEqual
    { actual: SCP.indexOf' (Pattern "a") 0 str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.indexOf' (Pattern "a") 1 str
    , expected: Nothing
    }
  assertEqual
    { actual: SCP.indexOf' (Pattern "z") 0 str
    , expected: Just 6
    }
  assertEqual
    { actual: SCP.indexOf' (Pattern "z") 1 str
    , expected: Just 6
    }
  assertEqual
    { actual: SCP.indexOf' (Pattern "z") 2 str
    , expected: Just 6
    }
  assertEqual
    { actual: SCP.indexOf' (Pattern "z") 3 str
    , expected: Just 6
    }
  assertEqual
    { actual: SCP.indexOf' (Pattern "z") 4 str
    , expected: Just 6
    }
  assertEqual
    { actual: SCP.indexOf' (Pattern "z") 5 str
    , expected: Just 6
    }
  assertEqual
    { actual: SCP.indexOf' (Pattern "z") 6 str
    , expected: Just 6
    }
  assertEqual
    { actual: SCP.indexOf' (Pattern "z") 7 str
    , expected: Nothing
    }

testLastIndexOf :: Effect Unit
testLastIndexOf = do
  log "lastIndexOf"
  assertEqual
    { actual: SCP.lastIndexOf (Pattern "") ""
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.lastIndexOf (Pattern "") str
    , expected: Just 7
    }
  assertEqual
    { actual: SCP.lastIndexOf (Pattern str) str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.lastIndexOf (Pattern "a") str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.lastIndexOf (Pattern "éПП") str
    , expected: Just 1
    }
  assertEqual
    { actual: SCP.lastIndexOf (Pattern "П") str
    , expected: Just 3
    }
  assertEqual
    { actual: SCP.lastIndexOf (Pattern "ПП") str
    , expected: Just 2
    }
  assertEqual
    { actual: SCP.lastIndexOf (Pattern "П\x16805") str
    , expected: Just 3
    }
  assertEqual
    { actual: SCP.lastIndexOf (Pattern "\x16805") str
    , expected: Just 4
    }
  assertEqual
    { actual: SCP.lastIndexOf (Pattern "\x16A06") str
    , expected: Just 5
    }
  assertEqual
    { actual: SCP.lastIndexOf (Pattern "z") str
    , expected: Just 6
    }
  assertEqual
    { actual: SCP.lastIndexOf (Pattern "\n") str
    , expected: Nothing
    }

testLastIndexOfStartingAt1 :: Effect Unit
testLastIndexOfStartingAt1 = do
  log "lastIndexOf'"
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "") 0 ""
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern str) 0 str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern str) 1 str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "a") (-1) str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "a") 0 str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "a") 7 str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "a") (SCP.length str) str
    , expected: Just 0
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "z") 0 str
    , expected: Nothing
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "z") 1 str
    , expected: Nothing
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "z") 2 str
    , expected: Nothing
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "z") 3 str
    , expected: Nothing
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "z") 4 str
    , expected: Nothing
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "z") 5 str
    , expected: Nothing
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "z") 6 str
    , expected: Just 6
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "z") 7 str
    , expected: Just 6
    }

testLastIndexOfStartingAt2 :: Effect Unit
testLastIndexOfStartingAt2 = do
  log "lastIndexOf' (multibyte)"
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "П") 7 str
    , expected: Just 3
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "П") 6 str
    , expected: Just 3
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "П") 5 str
    , expected: Just 3
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "П") 4 str
    , expected: Just 3
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "П") 3 str
    , expected: Just 3
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "П") 2 str
    , expected: Just 2
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "П") 1 str
    , expected: Nothing
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "П") 0 str
    , expected: Nothing
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "\x16A06") 7 str
    , expected: Just 5
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "\x16A06") 6 str
    , expected: Just 5
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "\x16A06") 5 str
    , expected: Just 5
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "\x16A06") 4 str
    , expected: Nothing
    }
  assertEqual
    { actual: SCP.lastIndexOf' (Pattern "\x16A06") 3 str
    , expected: Nothing
    }

testTake :: Effect Unit
testTake = do
  log "take"
  assertEqual
    { actual: SCP.take (-1) str
    , expected: ""
    }
  assertEqual
    { actual: SCP.take 0 str
    , expected: ""
    }
  assertEqual
    { actual: SCP.take 1 str
    , expected: "a"
    }
  assertEqual
    { actual: SCP.take 2 str
    , expected: "aé"
    }
  assertEqual
    { actual: SCP.take 3 str
    , expected: "aéП"
    }
  assertEqual
    { actual: SCP.take 4 str
    , expected: "aéПП"
    }
  assertEqual
    { actual: SCP.take 5 str
    , expected: "aéПП\x16805"
    }
  assertEqual
    { actual: SCP.take 6 str
    , expected: "aéПП\x16805\x16A06"
    }
  assertEqual
    { actual: SCP.take 7 str
    , expected: str
    }
  assertEqual
    { actual: SCP.take 8 str
    , expected: str
    }

testTakeWhile :: Effect Unit
testTakeWhile = do
  log "takeWhile"
  assertEqual
    { actual: SCP.takeWhile (\_ -> true) str
    , expected: str
    }
  assertEqual
    { actual: SCP.takeWhile (\_ -> false) str
    , expected: ""
    }
  assertEqual
    { actual: SCP.takeWhile (\c -> fromEnum c < 0xFFFF) str
    , expected: "aéПП"
    }
  assertEqual
    { actual: SCP.takeWhile (\c -> fromEnum c < 0xE9) str
    , expected: "a"
    }

testDrop :: Effect Unit
testDrop = do
  log "drop"
  assertEqual
    { actual: SCP.drop (-1) str
    , expected: str
    }
  assertEqual
    { actual: SCP.drop 0 str
    , expected: str
    }
  assertEqual
    { actual: SCP.drop 1 str
    , expected: "éПП\x16805\x16A06z"
    }
  assertEqual
    { actual: SCP.drop 2 str
    , expected: "ПП\x16805\x16A06z"
    }
  assertEqual
    { actual: SCP.drop 3 str
    , expected: "П\x16805\x16A06z"
    }
  assertEqual
    { actual: SCP.drop 4 str
    , expected: "\x16805\x16A06z"
    }
  assertEqual
    { actual: SCP.drop 5 str
    , expected: "\x16A06z"
    }
  assertEqual
    { actual: SCP.drop 6 str
    , expected: "z"
    }
  assertEqual
    { actual: SCP.drop 7 str
    , expected: ""
    }
  assertEqual
    { actual: SCP.drop 8 str
    , expected: ""
    }

testDropWhile :: Effect Unit
testDropWhile = do
  log "dropWhile"
  assertEqual
    { actual: SCP.dropWhile (\_ -> true) str
    , expected: ""
    }
  assertEqual
    { actual: SCP.dropWhile (\_ -> false) str
    , expected: str
    }
  assertEqual
    { actual: SCP.dropWhile (\c -> fromEnum c < 0xFFFF) str
    , expected: "\x16805\x16A06z"
    }
  assertEqual
    { actual: SCP.dropWhile (\c -> fromEnum c < 0xE9) str
    , expected: "éПП\x16805\x16A06z"
    }

testSplitAt1 :: Effect Unit
testSplitAt1 = do
  log "splitAt"
  assertEqual
    { actual: SCP.splitAt 0 ""
    , expected: { before: "", after: "" }
    }
  assertEqual
    { actual: SCP.splitAt 1 ""
    , expected: { before: "", after: "" }
    }
  assertEqual
    { actual: SCP.splitAt 0 "a"
    , expected: { before: "", after: "a" }
    }
  assertEqual
    { actual: SCP.splitAt 1 "ab"
    , expected: { before: "a", after: "b" }
    }
  assertEqual
    { actual: SCP.splitAt 3 "aabcc"
    , expected: { before: "aab", after: "cc" }
    }
  assertEqual
    { actual: SCP.splitAt (-1) "abc"
    , expected: { before: "", after: "abc" }
    }

testSplitAt2 :: Effect Unit
testSplitAt2 = do
  log "splitAt (multibyte)"
  assertEqual
    { actual: SCP.splitAt 0 str
    , expected: { before: "", after: str }
    }
  assertEqual
    { actual: SCP.splitAt 1 str
    , expected: { before: "a", after: "éПП\x16805\x16A06z" }
    }
  assertEqual
    { actual: SCP.splitAt 2 str
    , expected: { before: "aé", after: "ПП\x16805\x16A06z" }
    }
  assertEqual
    { actual: SCP.splitAt 3 str
    , expected: { before: "aéП", after: "П\x16805\x16A06z" }
    }
  assertEqual
    { actual: SCP.splitAt 4 str
    , expected: { before: "aéПП", after: "\x16805\x16A06z" }
    }
  assertEqual
    { actual: SCP.splitAt 5 str
    , expected: { before: "aéПП\x16805", after: "\x16A06z" }
    }
  assertEqual
    { actual: SCP.splitAt 6 str
    , expected: { before: "aéПП\x16805\x16A06", after: "z" }
    }
  assertEqual
    { actual: SCP.splitAt 7 str
    , expected: { before: str, after: "" }
    }
  assertEqual
    { actual: SCP.splitAt 8 str
    , expected: { before: str, after: "" }
    }

cp :: Int -> SCP.CodePoint
cp = unsafePartial fromJust <<< toEnum

-- A single raw byte as a String. `toEnum n :: Maybe Char` is the byte with
-- code n (a Char is one byte in pslua), so CodeUnits.singleton wraps it
-- without any UTF-8 re-encoding.
byte :: Int -> String
byte n = SCU.singleton (unsafePartial fromJust (toEnum n))
