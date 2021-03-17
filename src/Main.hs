module Main
  ( main
  ) where

import Data.ByteString.Char8 (pack)
import Data.Version (showVersion)
import Database.MySQL.Base (ConnectInfo(..))
import Database.MySQL.Base.Types (Option(ReadDefaultFile, ReadDefaultGroup))
import Paths_juandelacosa (getDataDir, version) -- from cabal
import System.IO.Unsafe (unsafePerformIO)

import Options.Applicative
  ( Parser
  , (<**>)
  , (<|>)
  , auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , optional
  , short
  , showDefault
  , strOption
  , value
  )

import Server (Listen(Port, Socket), server)

data Config =
  Config
    { file :: Maybe FilePath
    , group :: String
    , datadir :: FilePath
    , listen :: Listen
    }

parseListen :: Parser Listen
parseListen = port <|> socket
  where
    port =
      Port <$>
      option
        auto
        (long "port" <>
         short 'p' <>
         metavar "INT" <> help "listen on this TCP port (localhost only)")
    socket =
      Socket <$>
      option
        auto
        (long "socket" <>
         short 's' <>
         metavar "PATH" <>
         value "/tmp/juandelacosa.sock" <>
         showDefault <> help "Listen on this UNIX-socket")

{-# NOINLINE dataDir #-}
dataDir :: FilePath
dataDir = unsafePerformIO getDataDir

parseConfig :: Parser Config
parseConfig =
  Config <$>
  optional
    (strOption
       (long "file" <>
        short 'f' <> metavar "FILE" <> help "Read this MySQL client config file")) <*>
  strOption
    (long "group" <>
     short 'g' <>
     metavar "STRING" <>
     value "client" <>
     showDefault <> help "Read this options group in the above file") <*>
  strOption
    (long "datadir" <>
     short 'd' <>
     metavar "DIR" <>
     value dataDir <>
     showDefault <> help "Data directory including static files") <*>
  parseListen

run :: Config -> IO ()
run cfg = do
  let myInfo =
        ConnectInfo
          { connectDatabase = ""
          , connectHost = ""
          , connectOptions =
              case file cfg of
                Nothing -> []
                Just f ->
                  [ReadDefaultFile f, ReadDefaultGroup (pack $ group cfg)]
          , connectPassword = ""
          , connectPath = ""
          , connectPort = 0
          , connectSSL = Nothing
          , connectUser = ""
          }
  server (listen cfg) myInfo (datadir cfg)

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (parseConfig <**> helper) (fullDesc <> header desc)
    desc =
      "juandelacosa " ++
      showVersion version ++ " - manage MariaDB user and roles"
