module HdlService where

import MVar
import Device
import HDL
import HdlRuntime

data Handle = Handle
    { createDevice :: Hdl () -> Device 
    , getBlankDevice :: Device
    }

type Command = (String, String)
type Result = String
type ServicePipe = Pipe Command Result

-- evaluateCommand :: Command -> String 
-- evaluateCommand ("createDevice", args) = let
--     hdl = (read args) :: Hdl ()
--     dev = makeDevice hdl
--     in (show dev)
-- evaluateCommand ("blankDevice", _) = show blankDevice 
-- evaluateCommand _ = ""


-- createDevice' :: ServicePipe -> Hdl () -> IO Device 
-- createDevice' pipe hdl = do
--     print "Request createDevice."
--     result <- sendRequest pipe ("createDevice", show hdl) 
--     print "Response received."
--     return (read result :: Device)

blankDevice' :: ServicePipe -> IO Device 
blankDevice' pipe = do
    print "Request blankDevice."
    result <- sendRequest pipe ("blankDevice", "") 
    print "Response received."
    return (read result :: Device)