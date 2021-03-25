module Producer( 
    interpret
  , Interpreter(..)
  , ControllerScript
  , get
  , set
  , read
  , run
  ) where

import Control.Monad.Free    

import Prelude hiding (read)
import DeviceModel

-- Parametrized type for a Free eDSL
data Procedure a
  = Set Controller Property Value a              
  | Get Controller Property (Value -> a)
  | Run Controller Command (CommandResult -> a)
  | Read Controller SensorIndex Parameter (Measurement -> a)


instance Functor Procedure where 
    fmap g (Set c p v next) = Set c p v (g next) 
    fmap g (Get c p nextF) = Get c p (g . nextF) 
    fmap g (Read c si p nextF) = Read c si p (g . nextF) 
    fmap g (Run c cmd nextF) = Run c cmd (g . nextF)


type ControllerScript a = Free Procedure a

-- Smart constructors:
set :: Controller -> Property -> Value -> ControllerScript ()
set c p v = Free (Set c p v (Pure ()))

get :: Controller -> Property -> ControllerScript Value
get c p = Free (Get c p Pure)

read :: Controller -> SensorIndex -> Parameter -> ControllerScript Measurement
read c si p = Free (Read c si p Pure)

run :: Controller -> Command -> ControllerScript CommandResult
run c cmd = Free (Run c cmd Pure)


class Monad m => Interpreter m where
  onSet  :: Controller -> Property -> Value -> m ()
  onGet  :: Controller -> Property -> m Value
  onRead :: Controller -> SensorIndex
         -> Parameter -> m Measurement
  onRun  :: Controller -> Command -> m CommandResult


interpret :: (Monad m, Interpreter m) => ControllerScript a -> m a 
interpret (Pure a) = return a
interpret (Free (Get c p next)) = do
  v <- onGet c p
  interpret (next v)
interpret (Free (Set c p v next)) = do
  onSet c p v
  interpret next
interpret (Free (Read c si p next)) = do
  v <- onRead c si p
  interpret (next v)
interpret (Free (Run c cmd next)) = do
  v <- onRun c cmd
  interpret (next v)  