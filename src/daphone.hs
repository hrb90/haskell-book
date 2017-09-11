import Data.Char


-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

data DaPhone = DaPhone

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps = undefined
-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined

-- 3. How many times do digits need to be pressed for each message?
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

-- 4. What was the most popular letter for each message? What was
-- its cost? You’ll want to combine reverseTaps and fingerTaps to
-- figure out what it cost in taps. reverseTaps is a list because you
-- need to press a different button in order to get capitals.

mostPopularLetter :: String -> Char
mostPopularLetter = undefined

-- 5. What was the most popular letter overall? What was the most
-- popular word?

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined
