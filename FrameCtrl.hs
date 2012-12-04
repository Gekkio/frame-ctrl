import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.USB
import qualified System.USB.Exceptions as USBEx

vendorId :: Word16
vendorId = 0x04e8

data Model = Model { modelName :: String
                   , storageProductId :: Word16
                   , displayProductId :: Word16
                   } deriving (Show)

type CtxIO t = Ctx -> IO t

models :: [Model]
models = [ Model "SPF-87H"  0x2033 0x2034
         , Model "SPF-107H" 0x2027 0x2028
         ]

usbTimeout :: Timeout
usbTimeout = 10000

modeSwitchPollDelay :: Int
modeSwitchPollDelay = 200 * 1000

modeSwitchTimeout :: Int
modeSwitchTimeout = 10000 * 10000

imageEndpoint :: EndpointAddress
imageEndpoint = EndpointAddress 2 Out

imageChunkSize :: Int
imageChunkSize = 0x4000

imageBufferSize :: Int
imageBufferSize = 0x8000

usbActivateDisplayMode :: DeviceHandle -> IO (Maybe String)
usbActivateDisplayMode devHndl = do
  result <- try $ readControl devHndl Standard ToDevice 0x06 0xfe 0xfe 254 usbTimeout
  case result of
    Left (USBEx.IOException "") -> return Nothing
    _ -> return $ Just "Uneexcepted result from display mode activation"

usbHoldDisplayMode :: DeviceHandle -> IO (Maybe String)
usbHoldDisplayMode devHndl = do
  result <- readControlExact devHndl Vendor ToDevice 0x04 0x00 0x00 1 usbTimeout
  case B.unpack result of
    [0x03] -> return Nothing
    _ -> return $ Just "Unexcepted result from display mode commands"

usbCommunication :: Device -> (DeviceHandle -> IO t) -> IO t
usbCommunication dev action = withDeviceHandle dev $ \devHndl -> withDetachedKernelDriver devHndl 0 $ withClaimedInterface devHndl 0 $ action devHndl

logAndExit :: Maybe String -> IO ()
logAndExit (Just errorMsg) = hPutStrLn stderr errorMsg >> exitFailure
logAndExit _ = return ()

main :: IO ()
main = do
  ctx <- newCtx
  setDebug ctx PrintInfo

  (doWithDevice storageProductId storageMode $
    doWithDevice displayProductId displayMode $
      \_ -> logAndExit $ Just "No supported devices found") ctx

doWithDevice :: (Model -> Word16) -> ((Device, Model) -> CtxIO ()) -> CtxIO () -> CtxIO ()
doWithDevice productId success failure ctx = do
  dev <- lookupDevice productId ctx
  case dev of
    Just devModel -> success devModel ctx
    _ -> failure ctx

lookupDevice :: (Model -> Word16) -> CtxIO (Maybe (Device, Model))
lookupDevice productId ctx = do
  devs <- getDevices ctx
  let matching = findDevices productId devs
  takeOne matching
  where
    takeOne [] = return Nothing
    takeOne (x:[]) = return $ Just x
    takeOne (x:_) = hPutStrLn stderr "Warning! Multiple devices found, using the first one" >> (return $ Just x)

storageMode :: (Device, Model) -> CtxIO ()
storageMode (dev, model) ctx = do
  putStrLn $ "Detected " ++ (modelName model) ++ " in storage mode"
  activateDisplayMode dev >>= logAndExit
  waitForDisplayMode modeSwitchTimeout ctx

activateDisplayMode :: Device -> IO (Maybe String)
activateDisplayMode dev = do
  result <- try $ putStrLn "Activating display mode" >> usbCommunication dev usbActivateDisplayMode
  case result of
    Left (USBEx.NoDeviceException) -> return Nothing
    Left e -> throwIO e
    Right x -> return x

waitForDisplayMode :: Int -> CtxIO ()
waitForDisplayMode timeout
  | timeout <= 0 = const $ logAndExit $ Just "Failed to switch to display mode"
  | otherwise = \ctx -> do
    _ <- threadDelay modeSwitchPollDelay
    doWithDevice displayProductId displayMode continuePolling ctx
  where
    continuePolling = waitForDisplayMode (timeout - modeSwitchPollDelay)

holdDisplayMode :: Device -> IO (Maybe String)
holdDisplayMode dev = putStrLn "Sending display mode hold commands" >> usbCommunication dev usbHoldDisplayMode

displayMode :: (Device, Model) -> CtxIO ()
displayMode (dev, model) _ = do
  putStrLn $ "Detected " ++ (modelName model) ++ " in display mode"  
  holdDisplayMode dev >>= logAndExit
  imageMode dev

imageMode :: (Device) -> IO()
imageMode dev = do
  image <- readImage
  putStrLn "Streaming image to device"
  let header = putByteString (B.pack [0xa5, 0x5a, 0x09, 0x04]) >> putWord32le (fromIntegral $ B.length image) >> putByteString (B.pack [0x46, 0x00, 0x00, 0x00])
  let bytes = runPut (header >> putByteString image)
  usbCommunication dev $ streamImageBytes $ ((B.concat . BL.toChunks) bytes)

streamImageBytes :: B.ByteString -> DeviceHandle -> IO()
streamImageBytes bytes devHndl
  | B.null bytes = return ()
  | otherwise = do
    let size = fromIntegral imageBufferSize
    let (buffer, remainder) = B.splitAt size bytes
    let padding = B.replicate (size - B.length buffer) 0x00
    let paddedBuffer = buffer `B.append` padding

    streamInChunks paddedBuffer devHndl
    streamImageBytes remainder devHndl

streamInChunks :: B.ByteString -> DeviceHandle -> IO()
streamInChunks bytes devHndl
  | B.null bytes = return ()
  | otherwise = do
    let size = fromIntegral imageChunkSize
    let (chunk, remainder) = B.splitAt size bytes
    (transferred, _) <- writeBulk devHndl imageEndpoint chunk usbTimeout
    when (transferred /= B.length chunk) $ throwIO USBEx.incompleteWriteException  
    streamInChunks remainder devHndl

readImage :: IO (B.ByteString)
readImage = do
  args <- getArgs
  case args of
    ("-":_) -> B.getContents
    (path:_) -> B.readFile path
    _ -> B.getContents

findDevices :: (Model -> Word16) -> [Device] -> [(Device, Model)]
findDevices productId devs = do
  model <- models
  dev <- (filter $ isSupported $ productId model) devs
  return (dev, model)

isSupported :: Word16 -> Device -> Bool
isSupported productId dev = (deviceVendorId devDesc == vendorId)
                          && (deviceProductId devDesc == productId)
  where
    devDesc = deviceDesc dev
