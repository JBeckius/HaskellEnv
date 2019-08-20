module RomanNumeralEncoder
    (
        solution
    ) where

-- Create a function taking a positive integer as its parameter and returning a string containing the Roman Numeral representation of that integer.

-- Modern Roman numerals are written by expressing each digit separately 
-- starting with the left most digit and skipping any digit with a value of zero.
-- In Roman numerals 1990 is rendered: 1000=M, 900=CM, 90=XC; resulting in MCMXC.
-- 2008 is written as 2000=MM, 8=VIII; or MMVIII. 
-- 1666 uses each Roman symbol in descending order: MDCLXVI.

-- Example:

-- solution 1000 -- should return "M"
-- Help:

-- Symbol    Value
-- I          1
-- V          5
-- X          10
-- L          50
-- C          100
-- D          500
-- M          1,000
-- Remember that there can't be more than 3 identical symbols in a row.

-- More about roman numerals - http://en.wikipedia.org/wiki/Roman_numerals

-- describe "Some tests" $ do
--     it "should translate 1 to I" $ solution 1 `shouldBe` "I"
--     it "should translate 4 to IV" $ solution 4 `shouldBe` "IV"
--     it "should translate 6 to VI" $ solution 6 `shouldBe` "VI"
--     it "should translate 14 to XIV" $ solution 14 `shouldBe` "XIV"
--     it "should translate 21 to XXI" $ solution 21 `shouldBe` "XXI"
--     it "should translate 89 to LXXXIX" $ solution 89 `shouldBe` "LXXXIX"
--     it "should translate 91 to XCI" $ solution 91 `shouldBe` "XCI"
--     it "should translate 984 to CMLXXXIV" $ solution 984 `shouldBe` "CMLXXXIV"
--     it "should translate 1000 to M" $ solution 1889 `shouldBe` "MDCCCLXXXIX"
--     it "should translate 1889 to MDCCCLXXXIX" $ solution 1889 `shouldBe` "MDCCCLXXXIX"
--     it "should translate 1989 to MCMLXXXIX" $ solution 1989 `shouldBe` "MCMLXXXIX"


    import Data.List
    
    solution :: String -> Int
    solution "" = 0
    solution str = num + solution xs
        where (num, xs):_ = [ (num, drop (length n) str) | (n,num) <- table, n `isPrefixOf` str]
              table = zip (words "M CM D CD C XC L XL X IX V IV I")
                          [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]