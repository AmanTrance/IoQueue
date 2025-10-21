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
import GHC.IO.Handle (Handle, hClose, hSeek, SeekMode (AbsoluteSeek), hLock, LockMode (ExclusiveLock))
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import Data.ByteString.Lazy (hPut, hGet)
import Data.Binary.Get 
import GHC.Int 
import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef)
import GHC.IO.Handle.Lock (hUnlock)

data IoQueue = IoQueue { fd :: Handle, r :: IORef Int, w :: IORef Int }

newIoQueue :: FilePath -> IO (Maybe IoQueue)
newIoQueue p = do
    handle <- openBinaryFile p ReadWriteMode
    readRef <- newIORef 0
    writeRef <- newIORef 0
    return $ Just IoQueue { fd = handle, r = readRef, w = writeRef }

pushIoQueue :: IoQueue -> BL.ByteString -> IO ()
pushIoQueue IoQueue { fd, w } xs = do
    hLock fd ExclusiveLock
    let len = BL.length xs
        computation = BP.putInt64be $ fromIntegral len
    offset <- readIORef w
    hSeek fd AbsoluteSeek $ fromIntegral offset
    hPut fd $ BP.runPut computation
    BL.hPut fd xs
    writeIORef w (offset + len + 8)
    hUnlock fd

popIoQueue :: IoQueue -> IO (Maybe BL.ByteString)
popIoQueue IoQueue { fd, r, w } = do
    hLock fd ExclusiveLock
    readOffset <- readIORef r
    writeOffset <- readIORef w
    if readOffset == writeOffset then do 
        return Nothing 
    else do
        hSeek fd AbsoluteSeek $ fromIntegral readOffset
        len <- hGet fd 8
        let newOffset = fromIntegral @Int64 @Int $ runGet getInt64be len
        writeIORef r (readOffset + newOffset + 8)
        content <- BL.hGet fd newOffset
        hUnlock fd
        return $ Just content

closeIoQueue :: IoQueue -> IO ()
closeIoQueue IoQueue { fd } = hClose fd