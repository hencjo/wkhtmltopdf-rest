{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Functor

import qualified Validation as V
import Validation ((<#>))

instance Arbitrary a => Arbitrary (V.Validation a) where
    arbitrary = oneof [ V.Valid <$> arbitrary, return $ V.Invalid "invalid" ]

prop_failure_applied_with_anything :: V.Validation Int -> Bool
prop_failure_applied_with_anything v = case ((V.Failure [""]) <#> v) of
                                            (V.Failure _) -> True
                                            _             -> False

main :: IO ()
main = defaultMain [
        testProperty "Failure <#> _ -> Failure" prop_failure_applied_with_anything
    ]
