module Main where

import Control.Exception (finally)
import Control.Monad (forM_, forever, when, liftM)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import System.Random
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Text as T

import qualified Network.WebSockets as WS

type Token = String

type Client = (Either Token Token, WS.Connection)

--(Clientlist, Filtertoken)
type ServerState = ([Client], Maybe Token)

newServerState :: ServerState
newServerState = ([], Nothing)

removeClient :: Client -> ServerState -> ServerState
removeClient (tkn, _) (cs, filtr) = (filter ((/= tkn) . fst) cs, filtr)

sendToReceivers :: T.Text -> ServerState -> IO ()
sendToReceivers message (cs, filtr) = forM_ cs (\(tkn, conn) ->when (isRight tkn) (WS.sendTextData conn message))

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    if msg /= T.pack "OUTPUTINIT" && msg /= T.pack "INPUTINIT"
    then WS.sendTextData conn (T.pack "Wrong opening statement" :: T.Text)
    else do
      newToken <- getNewToken state (msg == T.pack "OUTPUTINIT")
      let client = ((if msg == T.pack "OUTPUTINIT" then Right else Left) newToken,conn)
      flip finally (disconnect client) $ do
          modifyMVar_ state (\(cs, filtr) -> return (client:cs, filtr) )
          WS.sendTextData conn (T.pack ("TOKN: " ++ newToken))
          putStrLn ((show.fst) client ++ " connected")
          (_, filtr) <- readMVar state
          when ((isRight.fst) client) (WS.sendTextData conn (T.pack ("FLUP: " ++ fromMaybe "" filtr)))
          relay conn state client
           where
             disconnect client = modifyMVar state (\s -> let s' = removeClient client s in return (s', s')) >> putStrLn ((show.fst) client ++ " disconnected")
             getNewToken state right = do
               (cs, filtr) <- readMVar state
               newToken <- liftM (randomRs ('A','Z')) getStdGen
               let short = take 4 newToken
               case findIndex (\(t,c) -> (if right then Right else Left) short == t) cs of
                 (Just _) -> getNewToken state right
                 (Nothing) -> return short

relay :: WS.Connection -> MVar ServerState -> Client -> IO ()
relay conn state (tkn, _) = forever $ do
    msg <- WS.receiveData conn
    (putStrLn. T.unpack) msg
    case tkn of
      (Left tkn) -> do
        (cls, filtr) <- readMVar state
        when (maybe True (tkn ==) filtr) (sendToReceivers msg (cls, filtr))
      (Right tkn) -> when (T.pack "FLUP: " `T.isPrefixOf` msg) ( do
          let newFltr = T.drop 6 msg
          modifyMVar_ state (\(cs, filtr) -> return (cs, if T.length newFltr > 0 then (Just. T.unpack) newFltr else Nothing))
          (cls, filtr) <- readMVar state
          sendToReceivers msg (filter ((/=)(Right tkn).fst) cls, filtr)
        )
