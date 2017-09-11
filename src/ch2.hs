sayHello :: String -> IO ()
sayHello name = putStrLn ("Hello, " ++ name ++ "!")

triple :: Num a => a -> a
triple = (*) 3

circleArea :: Floating a => a -> a
circleArea r =  pi * r * r

waxOn     = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7
