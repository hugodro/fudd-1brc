module Commands.Help where

import qualified Options.Runtime as Rto

helpHu :: Rto.RunOptions -> IO ()
helpHu rtOpts =
  putStrLn "@[helpHu] starting."
