module HDL where

import Control.Monad.Free

type Guid = String
data Parameter = Temperature | Pressure  deriving (Read, Show)
data ComponentClass = Sensors | Controllers deriving (Read, Show) 
type ComponentIndex = String

temperature = Temperature 
pressure = Pressure

data ComponentDef = ComponentDef
    { componentClass :: ComponentClass 
    , componentGuid :: Guid
    , componentManufacturer :: String 
    , componentName :: String
    }
    deriving (Read, Show)

aaa_p_02 = ComponentDef Sensors "guid1" "AAA Inc." "AAA-P-02"
aaa_t_25 = ComponentDef Sensors "guid2" "AAA Inc." "AAA-T-25"
aaa_c_86 = ComponentDef Controllers "guid3" "AAA Inc." "AAA-C-86"

data Component a 
    = SensorDef ComponentDef ComponentIndex Parameter a
    | ControllerDef ComponentDef ComponentIndex a 
    deriving (Read, Show)


instance Functor Component where 
    fmap f (SensorDef cd idx p a) = SensorDef cd idx p (f a) 
    fmap f (ControllerDef cd idx a) = ControllerDef cd idx (f a)

type Hdl a = Free Component a

sensor :: ComponentDef
        -> ComponentIndex 
        -> Parameter
        -> Hdl ()
sensor c idx p = Free (SensorDef c idx p (Pure ()))

controller :: ComponentDef -> ComponentIndex -> Hdl () 
controller c idx = Free (ControllerDef c idx (Pure ()))

boostersDef :: Hdl ()
boostersDef = do
    sensor aaa_t_25 "nozzle1-t" temperature 
    sensor aaa_p_02 "nozzle1-p" pressure 
    sensor aaa_t_25 "nozzle2-t" temperature 
    sensor aaa_p_02 "nozzle2-P" pressure 
    controller aaa_c_86 "controller"

val1 :: Free Component Float 
val1 = Pure 10.0

val2 :: Free Component Float
val2 = Free (SensorDef aaa_t_25 "nozzle1-t" temperature val1)

val3 :: Hdl Float
val3 = Free (ControllerDef aaa_t_25 "nozzle1-t" val2)

val4 :: Free Component ()
val4 = Free (ControllerDef aaa_t_25 "nozzle1-t" (Pure ()))
