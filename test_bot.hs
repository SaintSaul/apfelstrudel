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
nick   = "StrudelTesting2"
 
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
    write "USER" (nick++" 0 * :testing bot")
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
                                         privmsg (handleRoll (drop 6 x) n gen)
  where
    handleRoll :: String -> String -> StdGen -> String
    handleRoll x n gen = let
      num = read (takeWhile (/= 'd') x) :: Int
      max = read ((takeWhile (isDigit) . drop 1 . (dropWhile (/= 'd'))) x) :: Int
      in
        if (num<=30 && num>0) then let
          rolls = getRolls num max gen
          rollsString = (\str -> take ((length str) - 2) str) (foldl (\str x -> str ++ (show x) ++ ", ") "" rolls)
          in
          n ++ ": I will roll " ++ (show num) ++ " dice with " ++ (show max) ++ " sides each. Results:" ++ rollsString ++ ". Total: " ++ (show $ sum rolls)
        else
          "No."
    getRolls :: Int -> Int -> StdGen -> [Int]
    getRolls num max gen = let
      (randNum, gen') = randomR (1,max) gen :: (Int, StdGen)
      in
      if num == 0 then [] else randNum : (getRolls (num-1) max gen')
eval     _ _                        = return () -- ignore everything else
 
-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)
 
-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t
 
-- Convenience.
io :: IO a -> Net a
io = liftIO
