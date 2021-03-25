module DeviceModel where

data Value = FloatValue Float
           | IntValue Int
           | StringValue String
           | BoolValue Bool
           | ListValue [Value]
           deriving (Show)

data Measurement = Measurement Value deriving (Show)

data Parameter = Temperature | Pressure deriving (Show)

-- Dummy types, should be designed later
data Property = Version | Status | SensorsList deriving (Show)
data Controller = Controller String deriving (Show)
data Command = Command String deriving (Show)
type CommandResult = Either String String
type SensorIndex = String

toKelvin v = Measurement (FloatValue v)