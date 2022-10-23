{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.Text.Encoding as Enc

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Network.WebSockets as WS
import Network.WebSockets.Connection (PendingConnection(pendingOnAccept))

import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)

type ServerState = Map RoomId RoomState

type YoutubeDlhId = Text

type ArtistName = Text

type SongName = Text

type UserId = Text

type RoomId = Text

data User =
  User UserId WS.Connection UserSongQueue

type ConnectedUsers = [User]

data RoomState
  = ActiveRoom ConnectedUsers RoomSongQueue
  | EmptyRoom

data RoomEvent
  = UserJoinedEvent User
  | UserLeftEvent User

data Song
  = UnprocessedSong YoutubeDlhId
  | ProcessedSong YoutubeDlhId ArtistName SongName

data UserSongQueue
  = UserSongQueue SongQueue
  | EmptyUserSongQueue

data RoomSongQueue
  = RoomSongQueue SongQueue
  | EmptyRoomSongQueue

type SongQueue = [Song]

getUserId :: User -> UserId
getUserId (User userId _ _) = userId

getUserSongQueue :: User -> UserSongQueue
getUserSongQueue (User _ _ userSongQueue) = userSongQueue

addSongToUsersQueue :: Song -> User -> User
addSongToUsersQueue song (User userId wsConnection EmptyUserSongQueue) =
  User userId wsConnection (UserSongQueue [song])
addSongToUsersQueue song (User userId wsConnection (UserSongQueue xs)) =
  User userId wsConnection (UserSongQueue (song : xs))

createNewUser :: WS.Connection -> IO User
createNewUser conn = do
  uuid <- nextRandom
  return (User (T.pack $ toString uuid) conn (UserSongQueue []))

newServerState :: ServerState
newServerState = Map.empty

doesRoomExist :: RoomId -> ServerState -> Bool
doesRoomExist roomId s =
  case Map.lookup roomId s of
    Just room -> True
    Nothing -> False

numUsersInRoom :: RoomState -> Int
numUsersInRoom EmptyRoom = 0
numUsersInRoom (ActiveRoom connectedUsers _) = length connectedUsers

userAlreadyInRoom :: User -> RoomState -> Bool
userAlreadyInRoom (User userId _ _) (ActiveRoom connectedUsers _) =
  userId `elem` userIds
  where
    userIds = map getUserId connectedUsers

addUserToRoom :: User -> RoomState -> RoomState
addUserToRoom user EmptyRoom = ActiveRoom [user] EmptyRoomSongQueue
addUserToRoom user activeRoom@(ActiveRoom connectedUsers roomSongQueue) =
  if userAlreadyInRoom user activeRoom
    then activeRoom
    else ActiveRoom (user : connectedUsers) roomSongQueue

removeUserFromRoom :: User -> RoomState -> RoomState
removeUserFromRoom _ EmptyRoom = EmptyRoom
removeUserFromRoom (User userId _ _) (ActiveRoom connectedUsers roomSongQueue) =
  ActiveRoom filteredUsers roomSongQueue
  where
    filteredUsers = filter (\u -> getUserId u /= userId) connectedUsers

broadcast :: RoomEvent -> RoomState -> IO ()
broadcast _ EmptyRoom = undefined
broadcast (UserJoinedEvent user) activeRoom@(ActiveRoom connectedUsers _) = do
  let message = T.pack "User Joined the Room"
  T.putStrLn message
  forM_ connectedUsers (\(User _ con _) -> WS.sendTextData con message)
broadcast (UserLeftEvent user) activeRoom@(ActiveRoom connectedUsers _) = do
  let message = T.pack "User left the Room"
  T.putStrLn message
  forM_ connectedUsers (\(User _ con _) -> WS.sendTextData con message)

isGET :: WS.RequestHead -> Bool
isGET = undefined

isPOST :: WS.RequestHead -> Bool
isPOST = undefined

getRoomIdFromReq :: WS.RequestHead -> Maybe RoomId
getRoomIdFromReq = undefined

application :: MVar ServerState -> WS.ServerApp
application state pendingReq = do
  let WS.RequestHead path headers secure = WS.pendingRequest pendingReq
  let roomId = Enc.decodeUtf8 path
  T.putStrLn roomId
  rooms <- liftIO (readMVar state)
  if doesRoomExist roomId rooms
    then do
      conn <- WS.acceptRequest pendingReq
      newUser <- createNewUser conn
      WS.forkPingThread conn 30
      liftIO $
        modifyMVar_
          state
          (\s -> do
             let getRoom = Map.lookup roomId s
             forM_ getRoom (broadcast (UserJoinedEvent newUser))
             let s' =
                   case getRoom of
                     Just r -> Map.insert roomId (addUserToRoom newUser r) s
                     Nothing -> s
             return s')
    else WS.rejectRequest pendingReq "Room does not exist"

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" 9160 (application state)
