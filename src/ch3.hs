count :: IO ()
count = do
  putStrLn "Count to four for me:"
  putStr "one, two"
  putStr ", three, and"
  putStrLn " four!"


hbc :: String
hbc = "Curry is awesome"

exclaim :: String -> String
exclaim x = x ++ "!"

fifthChar :: String -> Char
fifthChar x = x !! 4

drop9 :: String -> String
drop9 x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> String -> Char
letterIndex idx str = str !! idx

rvrs :: String -> String
rvrs x = (take 7 $ drop 9 x) ++ " " ++ (take 2 $ drop 6 x) ++ " " ++ (take 5 x)
