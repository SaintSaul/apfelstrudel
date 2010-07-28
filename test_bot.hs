import Data.List
import Network
import System.IO
import System.Exit
import System.Random
import Control.Arrow
import Control.Monad.Reader
import Control.Exception -- *** for base-3
-- import Control.OldException -- *** for base-4
import Text.Printf
import Prelude hiding (catch)
import Char
 
server = "irc.freenode.org"
port   = 6667
chan   = "#apfelstrudel"
nick   = "ApfelStrudel"
 
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, rangen :: StdGen}
 
-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
--    loop st    = catch (runReaderT run st) (const $ return ())
    loop st    = catch (runReaderT run st) (\(SomeException _) -> return ()) -- *** Control.Exception with base-4
 
-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    g <- getStdGen
    return (Bot h g)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a
 
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :Apfel Strudel")
    write "JOIN" chan
    asks socket >>= listen
 
-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s) (getName s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    getName x = if ("PRIVMSG" `isInfixOf` x) then (takeWhile (/= '!') . drop 1) x else ""
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)
 
-- Dispatch a command, given a string and the name of the commenter
eval :: String -> String -> Net ()
eval     "!quit" _                  = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x n | "!id " `isPrefixOf` x    = privmsg (n ++ ": " ++ drop 4 x)
eval x n | "!roll " `isPrefixOf` x  = do gen <- asks rangen
                                         privmsg (handleRoll x n gen)
eval x n | canSalute x              = emote (salute x)
eval     _ _                        = return () -- ignore everything else

ranks = ["MAJOR", "GENERAL", "PRIVATE", "CORPORAL", "CHIEF", "KERNEL"]

canSalute :: String -> Bool
canSalute x = case (intersect ranks (words ( map toUpper x ))) of
  [] -> False
  m:ms -> case elemIndex m big_ws of
    Just n -> n+1 < length big_ws
    Nothing -> False
  where
    big_ws = words (map toUpper x)

salute :: String -> String
salute x =
  case intersect ranks big_ws of
    m:ms -> case elemIndex m big_ws of
      Just n -> let
        w1 = capWord $ if big_ws!!n == "KERNEL" then "colonel" else ws!!n
        w2 = capWord . takeWhile isAlphaNum $ ws!!(n+1)
        in
        if (n+1 < length ws) then " salutes " ++ w1 ++ " " ++ w2 else ""
      Nothing -> ""
    [] -> ""
  where
    big_ws = words (map toUpper x)
    ws = words x

capWord :: String -> String
capWord [] = []
capWord (x:xs) = toUpper x : xs

handleRoll :: String -> String -> StdGen -> String
handleRoll x n gen = let
  (num, max, opr, delta) = parseDice x
  valid = (num<=30 && num>0 && max>0)
  rolls = if valid then doRolls num max opr delta gen else []
  rollsString = (\str -> take ((length str) - 2) str) (foldl (\str x -> str ++ (show x) ++ ", ") "" rolls)
  diceString  = (show num) ++ "d" ++ (show max) ++ if (delta>0) then (opr ++ (show delta)) else ""
  in
  if valid then
    n ++ ": " ++ diceString ++ ": " ++ rollsString ++ ". Total: " ++ (show $ sum rolls)
  else
    n ++ ": " ++ "No."

-- Parse Ints and operations from a !roll request
parseDice :: String -> (Int, Int, [Char], Int)
parseDice str = let
  x = dropWhile (not . isDigit) $ str
  (num, x1)   = readFirstInt (span (isDigit) x)
  (max, x2)   = readFirstInt (span (isDigit) . (dropWhile (not . isDigit)) $ x1)
  (opr, x3)   = if (length (dropWhile (isSpace) x2) > 0) then splitAt 1 (dropWhile (isSpace) x2) else ("", x2)
  (delta, x4) = if (length (dropWhile (isSpace) x3) > 0) then readFirstInt (span (isDigit) (dropWhile (isSpace) x3)) else (0, x3)
  in
  (num, max, opr, delta)

-- Perform the actual dice rolls
doRolls :: Int -> Int -> [Char] -> Int -> StdGen -> [Int]
doRolls num max opr delta gen = let
  op = case opr of
    "-" -> (-)
    "+" -> (+)
    "*" -> (*)
    _ -> (\x y -> x)
  (randNum, gen') = randomR (1 `op` delta, max `op` delta) gen :: (Int, StdGen)
  in
  if num == 0 then [] else randNum : (doRolls (num-1) max opr delta gen')

-- Parse the first of two strings as an Int, then return the (Int, [Char])
readFirstInt::([Char],[Char])->(Int,[Char])
readFirstInt (x,y) = let 
  rs = reads x :: [(Int, String)]
  in
  case rs of
    [(r,_)] -> (r,y)
    [] -> (-1,y)
    _ -> (-1,y)
 
-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a privmsg to the current chan + server
emote :: String -> Net ()
emote s = write "PRIVMSG" (chan ++ " :/me" ++ s)
 
-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t
 
-- Convenience.
io :: IO a -> Net a
io = liftIO
