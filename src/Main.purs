module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.ST (ST)
import Data.Array.ST as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Node.HTTP (HTTP, listen)
import Node.HTTP as HTTP
import Node.Websocket (ConnectionClose, ConnectionMessage, EventProxy(EventProxy), Request, on)
import Node.Websocket.Connection (remoteAddress, sendMessage, sendUTF)
import Node.Websocket.Request (accept, origin)
import Node.Websocket.Server (newWebsocketServer, onConnect)
import Node.Websocket.Types (TextFrame(..), WSSERVER, defaultServerConfig)

data AppState

-- | Routes incoming messages to all clients except the one that sent it, and sends
-- | message history to new connections.
main :: forall e. Eff (st :: ST AppState, wss :: WSSERVER, console :: CONSOLE, http :: HTTP | e) Unit
main = do

  httpServer <- HTTP.createServer \ _ _ -> log "Server created"
  listen
    httpServer
    {hostname: "localhost", port: 8010, backlog: Nothing} do
      log "Server now listening"

  wsServer <- newWebsocketServer (defaultServerConfig httpServer)
  onConnect wsServer (\wscon -> do
                              httpServer <- HTTP.createServer \ req res -> sendUTF wscon "sending"
                              listen 
                                httpServer 
                                {hostname : "localhost", port: 8083, backlog: Nothing} do 
                                  log "socket server listening")

  on request wsServer \ req -> do
    log do
      "New connection from: " <> show (origin req)
    conn <- accept req (toNullable Nothing) (origin req)
    httpServer <- HTTP.createServer \ req res ->   
    log "New connection accepted"
    sendUTF conn "done"

  {--   -- sending a batched history requires client-side decoding support --}

  {--   on message conn \ msg -> do --}

  {--     case msg of --}
  {--       Left (TextFrame {utf8Data}) -> do --}
  {--         _ <- Array.pushSTArray historyRef utf8Data --}
  {--         log ("Received message: " <> utf8Data) --}
  {--         pure unit --}
  {--       Right _ -> pure unit --}

  {--     clients <- Array.freeze clientsRef --}
  {--     forWithIndex_ clients \ i client -> do --}

  {--       when (i /= idx) do --}
  {--         sendMessage client msg --}

  {--   on close conn \ _ _ -> do --}
  {--     _ <- Array.spliceSTArray clientsRef idx 1 [] --}
  {--     log ("Peer disconnected " <> remoteAddress conn) --}
  where
    close = EventProxy :: EventProxy ConnectionClose
    message = EventProxy :: EventProxy ConnectionMessage
    request = EventProxy :: EventProxy Request
