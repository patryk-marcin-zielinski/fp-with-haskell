{-# LANGUAGE DeriveFunctor #-} 
{-# LANGUAGE ExistentialQuantification #-}

module Infrastructure where

import Control.Monad.Free
import DeviceModel

-- Dummy types, should be designed later.
data Action a = StoreReading String a
              | SendTo String Value a
              | GetCurrentTime (String -> a)
              deriving (Functor)


type DatabaseDsl r = Free Action r

data Script b = DatabaseService (DatabaseDsl b)

data Control a = forall b. EvalScript(Script b) (b -> a)

type ControlProgram a = Free Control a