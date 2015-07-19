import ImageLoader

--------------------------------------------------------------------------------

import Control.Exception

--------------------------------------------------------------------------------

main = do
  dat <- loadNonExamples
  case dat
    of Left (SomeException e) -> putStrLn $ show e
       Right _                -> putStrLn "Got them!"
