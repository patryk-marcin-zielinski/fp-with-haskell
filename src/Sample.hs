module Sample where

import Producer as P
import DeviceModel
import Control.Monad.Free    



controller = Controller "test"
sensor = "thermometer 00:01"
version = Version
temperature = Temperature

-- Subroutine:
getTemperature :: Value -> P.ControllerScript String
getTemperature (StringValue "1.0") = do
  temp <- P.read controller sensor temperature
  return (show temp)
getTemperature (StringValue v) = return ("Not supported: " ++ v)
getTemperature _ = error "Value type mismatch."

-- Sample script:
script :: P.ControllerScript String
script = do
  v <- get controller version
  getTemperature v

instance Interpreter IO where
  onSet c prop v = print ("Set", c, v, prop)
  onGet c prop = do
    print ("Get", c, prop)
    return (StringValue "1.0")
  onRead c si par = do
    print ("Read", c, si, par)
    return (toKelvin 1.1)
  onRun c cmd = do
    print ("Run", c, cmd)
    return (Right "OK.")
   