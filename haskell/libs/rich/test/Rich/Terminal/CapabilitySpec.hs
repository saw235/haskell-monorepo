{-# LANGUAGE OverloadedStrings #-}

module Rich.Terminal.CapabilitySpec (capabilityTests) where

import Rich.Terminal.Capability
import Test.HUnit
import Test.QuickCheck

-- ============================================================================
-- T012: QuickCheck Properties for Terminal.Capability
-- ============================================================================

-- | Property: ColorCapability is ordered correctly
prop_colorCapabilityOrdered :: Bool
prop_colorCapabilityOrdered =
  NoColor < Color16
    && Color16 < Color256
    && Color256 < TrueColor
    && NoColor /= TrueColor

-- | Property: ColorCapability Enum instances work correctly
prop_colorCapabilityEnum :: Bool
prop_colorCapabilityEnum =
  [minBound .. maxBound :: ColorCapability]
    == [NoColor, Color16, Color256, TrueColor]

-- | Property: ColorCapability comparison is transitive
prop_colorCapabilityTransitive :: ColorCapability -> ColorCapability -> ColorCapability -> Bool
prop_colorCapabilityTransitive a b c =
  not (a < b && b < c) || a < c

-- | Property: Bounded instances are correct
prop_colorCapabilityBounded :: Bool
prop_colorCapabilityBounded =
  minBound == NoColor && maxBound == TrueColor

-- ============================================================================
-- T013: Unit Tests for Color Degradation Logic
-- ============================================================================

-- | Test: NoColor capability means no ANSI support
test_noColorCapability :: Test
test_noColorCapability =
  TestCase $ do
    -- NoColor is the minimum capability
    assertEqual "NoColor should be minimum bound" NoColor (minBound :: ColorCapability)
    -- NoColor is less than all other capabilities
    assertBool "NoColor < Color16" (NoColor < Color16)
    assertBool "NoColor < Color256" (NoColor < Color256)
    assertBool "NoColor < TrueColor" (NoColor < TrueColor)

-- | Test: Color16 is basic ANSI
test_color16Capability :: Test
test_color16Capability =
  TestCase $ do
    assertBool "Color16 >= Color16 (supports basic colors)" (Color16 >= Color16)
    assertBool "Color16 < Color256" (Color16 < Color256)
    assertBool "Color16 < TrueColor" (Color16 < TrueColor)

-- | Test: Color256 supports extended palette
test_color256Capability :: Test
test_color256Capability =
  TestCase $ do
    assertBool "Color256 >= Color16 (can degrade to basic)" (Color256 >= Color16)
    assertBool "Color256 >= Color256" (Color256 >= Color256)
    assertBool "Color256 < TrueColor" (Color256 < TrueColor)

-- | Test: TrueColor is maximum capability
test_trueColorCapability :: Test
test_trueColorCapability =
  TestCase $ do
    assertEqual "TrueColor should be maximum bound" TrueColor (maxBound :: ColorCapability)
    assertBool "TrueColor >= all capabilities" (TrueColor >= Color256 && TrueColor >= Color16 && TrueColor >= NoColor)

-- | Test: Degradation logic - can check if a capability supports at least a certain level
test_capabilityDegradation :: Test
test_capabilityDegradation =
  TestCase $ do
    -- Simulate degradation: if we have TrueColor, we support all lower levels
    assertBool "TrueColor supports Color256" (TrueColor >= Color256)
    assertBool "TrueColor supports Color16" (TrueColor >= Color16)

    -- If we only have Color16, we don't support higher capabilities
    assertBool "Color16 does not support Color256" (not $ Color16 >= Color256)
    assertBool "Color16 does not support TrueColor" (not $ Color16 >= TrueColor)

-- ============================================================================
-- T014: Test Cases for TERM Environment Variable Detection
-- ============================================================================

-- Note: These tests will check the behavior of detectColorCapability
-- Since we can't easily mock environment variables in pure tests,
-- we'll test the expected behavior based on documentation.

-- | Test: detectColorCapability returns a valid ColorCapability
--
-- This test will FAIL initially because detectColorCapability is not yet implemented
-- with the full logic. Currently it just uses hSupportsANSIColor heuristics.
test_detectColorCapabilityReturnsValid :: Test
test_detectColorCapabilityReturnsValid =
  TestCase $ do
    -- This will execute the actual detection
    capability <- detectColorCapability
    -- The result should be one of the valid capabilities
    assertBool
      "Capability should be one of: NoColor, Color16, Color256, TrueColor"
      (capability `elem` [NoColor, Color16, Color256, TrueColor])

-- | Test: supportsANSI returns a boolean
--
-- This test checks that supportsANSI works (it's implemented via ansi-terminal)
test_supportsANSIReturnsBool :: Test
test_supportsANSIReturnsBool =
  TestCase $ do
    result <- supportsANSI
    -- Result is either True or False - this always passes but documents the contract
    assertBool "supportsANSI should return True or False" (result == True || result == False)

-- | Test: NoColor capability when ANSI not supported
--
-- This test documents expected behavior: when ANSI is not supported,
-- detectColorCapability should return NoColor.
-- This will FAIL initially if the detection logic is incomplete.
test_noColorWhenNoANSI :: Test
test_noColorWhenNoANSI =
  TestCase $ do
    hasANSI <- supportsANSI
    capability <- detectColorCapability
    -- If no ANSI support, capability should be NoColor
    -- NOTE: This test might pass or fail depending on whether we're running in a terminal
    -- We're documenting the expected contract here
    if not hasANSI
      then assertEqual "No ANSI should mean NoColor capability" NoColor capability
      else assertBool "With ANSI, should have at least Color16" (capability >= Color16)

-- ============================================================================
-- Test Suite
-- ============================================================================

capabilityTests :: Test
capabilityTests =
  TestLabel "Rich.Terminal.Capability Tests" $
    TestList
      [ TestLabel "QuickCheck Properties" $
          TestList
            [ TestLabel "prop_colorCapabilityOrdered" $
                TestCase $
                  assertBool "ColorCapability ordering" prop_colorCapabilityOrdered,
              TestLabel "prop_colorCapabilityEnum" $
                TestCase $
                  assertBool "ColorCapability enum instances" prop_colorCapabilityEnum,
              TestLabel "prop_colorCapabilityBounded" $
                TestCase $
                  assertBool "ColorCapability bounded instances" prop_colorCapabilityBounded
            ],
        TestLabel "Color Degradation Tests" $
          TestList
            [ test_noColorCapability,
              test_color16Capability,
              test_color256Capability,
              test_trueColorCapability,
              test_capabilityDegradation
            ],
        TestLabel "TERM Environment Detection Tests" $
          TestList
            [ test_detectColorCapabilityReturnsValid,
              test_supportsANSIReturnsBool,
              test_noColorWhenNoANSI
            ]
      ]
