import Data.Char
import Data.List

headHunter ((x:_):_:_) = x
headHunter (_:(x:_):_) = x
headHunter (_:_:(x:_):_) = x
headHunter _ = error "Care"

firstColumn m = [ x | (x:_) <- m ]

shoutOutLoud :: String -> String
shoutOutLoud s = unwords [ first:first:word | word@(first:_) <- words s ]


pad :: String -> String -> (String, String)
pad s1 s2 = (s1 ++ replicate (length s1 - longer) ' ', s2 ++ replicate (length s2 - longer) ' ' )
  where longer = max (length s1) (length s2)
        fun s  = 