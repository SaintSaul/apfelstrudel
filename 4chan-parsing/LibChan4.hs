module LibChan4 where

import Text.XML.HXT.Arrow
import qualified Text.XML.HXT.DOM.XmlTreeFilter as X
import System.Environment
import System.Exit
import Control.Applicative

main = do 
  args <- getArgs
  if length args /= 1 then do
                        progName <- getProgName
                        putStrLn $ "Usage: " ++ progName ++ " <4chan-page-url>"
                        exitWith $ ExitFailure (0-1)
                      else return ()
  let url = args !! 0
  program url

mash :: [(String, String)] -> [String]
mash = map (\(a, b) -> a ++ ": " ++ b)

program url = do
  comments <- runX (
                    readDocument [(a_issue_warnings, v_0), (a_validate, v_0), (a_parse_html, v_1)] url
                    >>> deep (hasName "blockquote")
                    >>> getChildren >>> isText >>> getText
--                    >>> writeDocument [(a_validate, v_0), (a_show_tree, v_1), (a_indent, v_1)] "-"
  
                 )
  mapM_ putStrLn comments
--  mapM_ putStrLn $ map (\str -> str ++ "\n") comments
  return ()


