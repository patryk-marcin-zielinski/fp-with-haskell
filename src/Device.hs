module Device (
    Device,
    DeviceComponent,
    blankDevice,
    addSensor,
    addController,
    getComponent,
    updateComponent,
    setMeasurement,
    readMeasurement
) where

import HDL    
import Data.Map

type Measurement = () -- A dummy type. Design it later!

data DeviceComponent = Measurement Guid | Controller Guid deriving (Read, Show, Eq)

newtype Device = DeviceImpl (Map ComponentIndex DeviceComponent) deriving (Read, Show, Eq)

blankDevice :: Device
blankDevice = undefined

addSensor :: ComponentIndex -> Parameter
            -> ComponentDef -> Device -> Device 
addSensor cIndex param cDef device = undefined            

addController :: ComponentIndex -> ComponentDef-> Device -> Device 
addController cIndex cDef device = undefined

getComponent :: ComponentIndex -> Device -> Maybe DeviceComponent 
getComponent cIndex device = undefined

updateComponent :: ComponentIndex -> DeviceComponent-> Device -> Maybe Device 
updateComponent cIndex deviceComponent device = undefined

setMeasurement :: ComponentIndex -> Measurement-> Device -> Maybe Device 
setMeasurement cIndex mesurment device = undefined

readMeasurement :: ComponentIndex-> Device -> Maybe Measurement
readMeasurement cIndex device = undefined


