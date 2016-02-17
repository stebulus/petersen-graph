module LogErrors where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (catchException, EXCEPTION(), message)
import Data.Maybe (Maybe(Just,Nothing))
import Prelude

logErrors :: forall e a.  Eff (err :: EXCEPTION, console :: CONSOLE | e) a
          -> Eff (console :: CONSOLE | e) (Maybe a)
logErrors m = catchException handle (Just <$> m)
    where handle err = do log $ message err
                          return Nothing
