module Utils.System.Print
    (
        printFailure,
        maybePrint
    )
where

import System.Exit (ExitCode (ExitFailure), exitWith)

printFailure :: String -> IO ()
printFailure s = do
    putStrLn s
    exitWith $ ExitFailure 84

maybePrint :: Show a => Maybe a -> IO ()
maybePrint Nothing = return ()
maybePrint (Just x) = print x