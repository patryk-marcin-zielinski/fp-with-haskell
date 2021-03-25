{-# LANGUAGE DeriveFunctor #-}
module HNDL where

import HDL
import Control.Monad.Free
  

type PhysicalAddress = String

data DeviceInterface = DeviceInterface PhysicalAddress

data TerminalUnitInterface = TerminalUnitInterface PhysicalAddress
data LogicControlInterface = LogicControlInterface PhysicalAddress
-- | Convenient language for defining devices in the network.
data NetworkComponent a
  = DeviceDef PhysicalAddress
              (Hdl ())
              (DeviceInterface -> a)
  | TerminalUnitDef PhysicalAddress (TerminalUnitInterface -> a)
  | LogicControlDef PhysicalAddress (LogicControlInterface -> a)
  | LinkedDeviceDef DeviceInterface TerminalUnitInterface a
  | LinkDef LogicControlInterface [TerminalUnitInterface] a
  deriving (Functor)
-- | Free monad Hardware Network Description Language.
type Hndl a = Free NetworkComponent a
-- | Smart constructors.
remoteDevice :: PhysicalAddress -> Hdl () -> Hndl DeviceInterface
remoteDevice = undefined 

terminalUnit :: PhysicalAddress -> Hndl TerminalUnitInterface
terminalUnit = undefined 

logicControl :: PhysicalAddress -> Hndl LogicControlInterface
logicControl = undefined 

linkedDevice :: DeviceInterface -> TerminalUnitInterface -> Hndl ()
linkedDevice = undefined 

link :: LogicControlInterface -> [TerminalUnitInterface] -> Hndl ()
link = undefined 

networkDef :: Hndl ()
networkDef = do
  iBoosters <- remoteDevice "01" boostersDef
  iBoostersTU <- terminalUnit "03"
  linkedDevice iBoosters iBoostersTU
  iLogicControl <- logicControl "09"
  link iLogicControl [iBoostersTU]