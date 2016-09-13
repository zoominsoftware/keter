-- {-# LANGUAGE FlexibleContexts #-}
module Debug where

import Debug.Trace
import Data.Set

-- | Shortcut for printing list elements each on new line
printList :: Show a => String -> [a] -> IO ()
printList s xs = putStrLn (s ++ ":") >> mapM_ print xs

-- | Shortcut for printing set elements each on new line
printSet :: Show a => String -> Set a -> IO ()
printSet s xs = printList s $ toList xs

-- | Prints mesage with value of x, and returns x
--   dbg "X" 123 === (X:123)
dbg :: (Show a) => String -> a -> a
dbg s x = trace ("(" ++ s ++ ": " ++ show x ++ ")") x

-- | Prints mesage returned from function that receives x, and returns x
--   dbgF s (show . length) 123 === (X:123)
dbgF :: (Show a) => String -> (a -> String) -> a -> a
dbgF s f x = trace ("(" ++ s ++ ": " ++ f x ++ ")") x

-- | Prints message and returns unit in any monad
dbgM ::  Monad m => String -> m ()
dbgM = traceM

-- | Prints shown value and returns that value
dbgShow :: Show a => a -> b -> b
dbgShow = traceShow

-- | Prints shown value and returns unit in any monad
dbgShowM :: (Show a, Monad m) => a -> m ()
dbgShowM = traceShowM


-- Operator debug, so it can be set to the end of line
-- main = (1 + 2) `debug` "adding"
dbgMsg:: c -> String -> c
dbgMsg = flip trace
