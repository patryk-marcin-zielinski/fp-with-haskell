module HdlRuntime where

import HDL 
import Device( Device, blankDevice, addSensor, addController ) 
import Control.Monad.Free


makeDevice :: Hdl () -> Device 
makeDevice = interpretHdl blankDevice

interpretHdl :: Device -> Hdl () -> Device
interpretHdl device (Pure _) = device
interpretHdl device (Free comp) = interpretComponent device comp

interpretComponent :: Device -> Component (Hdl ()) -> Device 
interpretComponent device (SensorDef c idx par next) =
    let device' = addSensor idx par c device
    in interpretHdl device' next
interpretComponent device (ControllerDef c idx next) =
    let device' = addController idx c device 
    in interpretHdl device' next
