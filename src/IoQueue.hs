{-# LANGUAGE TypeApplications #-}

module IoQueue 
    (
        newIoQueue,
        pushIoQueue,
        popIoQueue,
        closeIoQueue,
    )
where

import qualified Data.ByteString.Char8 as BL
import qualified Data.Binary.Put as BP
import GHC.IO.Handle.FD
import GHC.IO.Handle (Handle, hClose, hSeek, SeekMode (AbsoluteSeek))
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import Data.ByteString.Lazy (hPut, hGet)
import Data.Binary.Get 
import GHC.Int 
import Data.IORef (IORef, newIORef, writeIORef, readIORef)

data IoQueue = IoQueue { fd :: Handle, r :: IORef Int, w :: IORef Int }

newIoQueue :: FilePath -> IO (Maybe IoQueue)
newIoQueue p = do
    handle <- openBinaryFile p ReadWriteMode
    readRef <- newIORef 0
    writeRef <- newIORef 0
    return $ Just IoQueue { fd = handle, r = readRef, w = writeRef }

pushIoQueue :: IoQueue -> BL.ByteString -> IO ()
pushIoQueue IoQueue { fd, w } xs = do
    let len = BL.length xs
        computation = BP.putInt64be $ fromIntegral len
    offset <- readIORef w
    hSeek fd AbsoluteSeek $ fromIntegral offset
    writeIORef w (offset + len + 8)
    hPut fd $ BP.runPut computation
    BL.hPut fd xs

popIoQueue :: IoQueue -> IO BL.ByteString
popIoQueue IoQueue { fd, r } = do
    offset <- readIORef r
    hSeek fd AbsoluteSeek $ fromIntegral offset
    len <- hGet fd 8
    let newOffset = fromIntegral @Int64 @Int $ runGet getInt64be len
    writeIORef r (offset + newOffset + 8)
    BL.hGet fd newOffset

closeIoQueue :: IoQueue -> IO ()
closeIoQueue IoQueue { fd } = hClose fd