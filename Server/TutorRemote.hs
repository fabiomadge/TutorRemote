module Main where

import Control.Exception (finally)
import Control.Monad (forM_, forever, when, liftM)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import System.Random
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C

import qualified Network.WebSockets as WS

type Token = String

data InOut = Input | Output deriving (Read, Show, Eq)

type Client = (Token, InOut, WS.Connection)
tokenOf :: Client -> Token
tokenOf (t,_,_) = t

--(Clientlist, Filtertoken)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

tokenLength = 6

removeClient :: Client -> ServerState -> ServerState
removeClient (tkn, _, _) cs = filter ((/= tkn) . tokenOf) cs

sendToReceivers :: Token -> Token -> InOut -> T.Text -> ServerState -> IO ()
sendToReceivers to from inout message cs = forM_ cs (\(tkn, io, conn) -> when (inout /= io && tkn == to) (WS.sendTextData conn (T.pack $ from ++ ": " ++ T.unpack message)))

consList = ['F','H','K','L','M','P','R','S','T','W','X','Z']
vocList = ['A','E','I','O','U']

makeToken l = makeToken' True l
	where makeToken' c (x:xs) = if c then consList !! x : makeToken' False xs
		else vocList !! (x `mod` length vocList) : makeToken' True xs

getNewToken state = do
    cs <- readMVar state
    tokGen <- liftM (randomRs (0, length consList - 1)) getStdGen
    let newToken = makeToken tokGen
    let short = take tokenLength newToken
    case findIndex (\(t,_,_) -> short == t) cs of
      (Just _) -> getNewToken state
      (Nothing) -> return short

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequestWith pending (WS.AcceptRequest (Just (C.pack "tutorremote")))
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    if msg /= T.pack "OUTPUTINIT" && msg /= T.pack "INPUTINIT"
    then WS.sendTextData conn (T.pack "Wrong opening statement" :: T.Text)
    else do
      newToken <- getNewToken state
      let client = (newToken, (if msg == T.pack "OUTPUTINIT" then Output else Input), conn)
      flip finally (disconnect client) $
        do
          modifyMVar_ state (\cs -> return (client:cs) )
          WS.sendTextData conn (T.pack ((take tokenLength $ repeat '0') ++ ": TOKN: " ++ newToken))
          putStrLn ((show.tokenOf) client ++ " connected")
          relay conn state client
           where
             disconnect client = modifyMVar state (\s -> let s' = removeClient client s in return (s', s')) >> putStrLn ((show.tokenOf) client ++ " disconnected")
             

relay :: WS.Connection -> MVar ServerState -> Client -> IO ()
relay conn state (tkn, inout, _) = forever $ do
    msg <- WS.receiveData conn
    cls <- readMVar state
    let (to, t1) = T.breakOn (T.pack ": ") msg
    let tot = T.unpack to
    let text = T.drop 2 t1
    putStrLn (tkn ++ " -> " ++ tot ++ " : " ++ (T.unpack text))
    if any (\(tkn, io, conn) -> (inout /= io && tkn == tot)) cls then
        sendToReceivers tot tkn inout text cls
    else
        WS.sendTextData conn (T.pack ((take tokenLength $ repeat '0') ++ ": NOPE: No Endpoint"))





main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state