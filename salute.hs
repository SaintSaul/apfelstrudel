import List
import Char

eval x = let
  big_ws = words (map toUpper x)
  ws = words x
  ranks = ["MAJOR", "GENERAL", "PRIVATE", "CORPORAL", "CHIEF"]
  in
  case intersect ranks big_ws of
    [] -> ""
    m:ms -> case elemIndex m big_ws of
      Just n -> if (n+1 < length ws) then (ws!!n) ++ " " ++ takeWhile isAlphaNum (ws!!(n+1)) else ""
      Nothing -> "Oh NOES!" --probably not reachable...

