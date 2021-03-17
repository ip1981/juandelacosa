module Server
  ( Listen(..)
  , server
  ) where

import Control.Exception.Base (bracket, catch, throwIO)
import Data.Bits ((.|.))
import Data.Pool (createPool, destroyAllResources)
import Database.MySQL.Base (ConnectInfo)
import qualified Database.MySQL.Simple as MySQL
import Network.Socket
  ( Family(AF_INET, AF_UNIX)
  , SockAddr(SockAddrInet, SockAddrUnix)
  , Socket
  , SocketOption(ReuseAddr)
  , SocketType(Stream)
  , bind
  , close
  , getSocketName
  , inet_addr
  , listen
  , maxListenQueue
  , setSocketOption
  , socket
  )
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettingsSocket)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files
  ( groupReadMode
  , groupWriteMode
  , ownerReadMode
  , ownerWriteMode
  , removeLink
  , setFileMode
  , socketMode
  )

import Application (app)

data Listen
  = Socket FilePath
  | Port Int

server :: Listen -> ConnectInfo -> FilePath -> IO ()
server socketSpec mysqlConnInfo dataDir =
  bracket
    (do sock <- createSocket socketSpec
        mysql <-
          createPool
            (MySQL.connect mysqlConnInfo)
            MySQL.close
            1 -- stripes
            60 -- keep alive (seconds)
            10 -- max connections
        return (sock, mysql))
    (\(sock, mysql) -> do
       closeSocket sock
       destroyAllResources mysql)
    (\(sock, mysql) -> do
       listen sock maxListenQueue
       hPutStrLn stderr $ "Static files from `" ++ dataDir ++ "'"
       runSettingsSocket defaultSettings sock =<< app mysql dataDir)

createSocket :: Listen -> IO Socket
createSocket (Socket path) = do
  removeIfExists path
  sock <- socket AF_UNIX Stream 0
  bind sock $ SockAddrUnix path
  setFileMode path $
    socketMode .|. ownerWriteMode .|. ownerReadMode .|. groupWriteMode .|.
    groupReadMode
  hPutStrLn stderr $ "Listening on UNIX socket `" ++ path ++ "'"
  return sock
createSocket (Port port) = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  addr <- inet_addr "127.0.0.1"
  bind sock $ SockAddrInet (fromIntegral port) addr
  hPutStrLn stderr $ "Listening on localhost:" ++ show port
  return sock

closeSocket :: Socket -> IO ()
closeSocket sock = do
  name <- getSocketName sock
  close sock
  case name of
    SockAddrUnix path -> removeIfExists path
    _ -> return ()

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeLink fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e
