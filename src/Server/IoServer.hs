module Server.IoServer 
    (

    )
where

import Network.Socket
import Control.Concurrent (forkOS, forkIO)
import Control.Monad (forever)
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import GHC.IO.Handle (Handle)

handleHandle :: Handle -> IO ()
handleHandle h = do
    return ()

createSocket :: PortNumber -> HostAddress -> IO ()
createSocket p h = do 
    seqSocket <- socket AF_INET SeqPacket defaultProtocol
    () <- bind seqSocket (SockAddrInet p h)
    () <- listen seqSocket 1024
    forever $ do
        (clientSocket, clientAddress) <- accept seqSocket
        clientHandle <- socketToHandle clientSocket ReadWriteMode
        forkIO (handleHandle clientHandle)
        return ()