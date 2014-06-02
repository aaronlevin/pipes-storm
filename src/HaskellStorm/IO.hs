{-# LANGUAGE OverloadedString #-}

module HaskellStorm.IO
    (
    ) where

import Pipes
import qualified Pipes.Prelude (stdinLn, takeWhile)

check :: Monad m => (a -> Boolean) -> (a -> b) -> (a -> Left c) -> Pipe a (Either b c) m r
check f t e = do
    x <- await
    if (f x) then yield (Right t x) else yield (e x) 
handshake = stdinLn 
            >-> takeWhile (/= "end")
            >-> 
