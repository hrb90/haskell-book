type Message = String

type BinaryTest = () -> Maybe Message

testBinary :: BinaryTest -> Maybe Message
testBinary thunk = thunk ()

appendMsg :: Message -> Message -> Maybe Message
appendMsg msg = Just . ((flip (++)) msg)

prependMsg :: Message -> Message -> Maybe Message
prependMsg msg = Just . ((++) msg)

describe :: Message -> [BinaryTest] -> BinaryTest
describe blockMsg tests =
  \() -> (foldr combineResults Nothing tests) >>= (prependMsg blockMsg)
  where combineResults test currentMsg =
    case currentMsg of
      Nothing -> outcome
      Just msg -> msg 
    where outcome = testBinary test

or :: Message -> [BinaryTest] -> BinaryTest
or newMsg tests =
  \() -> (foldr combineResults Nothing tests) >> (Just newMsg)
  where combineResults = undefined
