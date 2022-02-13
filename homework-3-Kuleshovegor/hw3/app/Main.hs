{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Exception (bracketOnError)
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import Prettyprinter (hardline)
import Prettyprinter.Render.Terminal (putDoc)
import System.Console.Haskeline (defaultSettings, getInputLine)
import System.Console.Haskeline.IO (InputState, cancelInput, closeInput, initializeInput,
                                    queryInput)

main :: IO ()
main = bracketOnError (initializeInput defaultSettings)
            cancelInput -- This will only be called if an exception such
                            -- as a SigINT is received.
            (\hd -> loop hd >> closeInput hd)
    where
        loop :: InputState -> IO ()
        loop hd = do
            minput <- queryInput hd (getInputLine "hi> ")
            case minput of
                Nothing -> return ()
                Just ":exit" -> return ()
                Just input -> do
                  let eithTree = parse input
                  case eithTree of
                    (Left e) -> putStrLn ("Parse error: " ++ show e)
                    (Right treeExpr) -> do
                      eith <- eval treeExpr
                      case eith of
                        (Left e)       -> putStrLn ("Eval error: " ++ show e)
                        (Right resVal) -> putDoc $ ((prettyValue resVal) <> (hardline))
                  loop hd
