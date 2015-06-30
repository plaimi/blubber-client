{- |
Module      :  $Header$
Description :  The blubber client entry point.
Copyright   :  (c) plaimi 2015
License     :  GPL-3

Maintainer  :  blubber@plaimi.net
-} module Main where

import Control.Arrow
  (
  (***),
  )
import Control.Concurrent
  (
  forkIO,
  killThread,
  ThreadId,
  )
import Control.Concurrent.Chan
  (
  Chan,
  newChan,
  readChan,
  writeChan,
  )
import Control.Monad
  (
  forever,
  join,
  void,
  when,
  )
import Data.Functor
  (
  (<$>),
  )
import Data.IORef
  (
  IORef,
  newIORef,
  readIORef,
  writeIORef,
  )
import Data.Map
  (
  toList,
  )
import Data.Serialize
  (
  decode,
  encode,
  )
import Graphics.Gloss.Data.Display
  (
  Display (InWindow),
  )
import Graphics.Gloss.Data.Color
  (
  black,
  red,
  yellow,
  )
import Graphics.Gloss.Data.Picture
  (
  Picture (Color, Pictures, Scale, Text, Translate),
  circleSolid,
  )
import Graphics.Gloss.Interface.IO.Game
  (
  Event (EventKey, EventMotion),
  Key (Char, SpecialKey),
  SpecialKey (KeySpace, KeyEsc),
  KeyState (Down),
  playIO,
  )
import Network.Socket
  (
  Family (AF_INET),
  HostAddress,
  PortNumber,
  Socket,
  SockAddr (SockAddrInet),
  SocketType (Datagram),
  inet_addr,
  socket,
  withSocketsDo,
  )
import Network.Socket.ByteString
  (
  recvFrom,
  sendTo,
  )
import System.Exit
  (
  exitSuccess,
  )
import System.Environment
  (
  getArgs,
  )

import Blubber.Server.Message
  (
  Message (AddPlayer, Connect, Disconnect, SrcPls, UpdateVelocity),
  Reply (SrcLink, View),
  )
import Blubber.Server.Entity
  (
  position,
  radius,
  )
import Blubber.Server.ViewPort
  (
  visibleNeutrals,
  visiblePlayers,
  ViewPort,
  viewHeight,
  viewPos,
  viewWidth,
  )
import Blubber.Server.Vector
  (
  vx,
  vy,
  )

-- | The 'Game' needs to keep track of its 'ViewPort' -- which is the part of
-- the blubber 'World' it is privy to, and the name of the player.
data Game = MkGame
          {vp      :: ViewPort
          -- ^ The 'ViewPort'.
          ,pn      :: String
          -- ^ The player's name.
          ,listenT :: ThreadId
          -- ^ The listen thread.
          ,talkT   :: ThreadId
          -- ^ The talk thread.
          ,ip      :: String
          -- ^ The IP connected to.
          } deriving (Show)

windowWidth :: Int
-- | The width of the initial game window.
windowWidth = 1600

windowHeight :: Int
-- | The height of the initial game window.
windowHeight = 900

main :: IO ()
-- | Our 'main' entry to the client. It sets up a socket to send stuff from,
--   and connects to the server using the passed in IP, port number, & player
--   name(or not, and crashes via error...). Then it sets up a 'Chan' and an
--   'IORef', before forking off 'talk' and 'listen', and launching
--   the game with 'playIO'.
--
-- 'playIO' sets up an 'InWindow' that's redrawn using 'draw'. 'handle' takes
-- care of the input, and 'step' updates the game.
main = withSocketsDo $ do
  as <- getArgs
  let (a, p, n) = case as of
             (a':p':n') -> (a', fromInteger . read $ p', unwords n')
             _          -> error "start with ./blubber [IP] [PORT] [NAME]"
  i   <- inet_addr a
  s   <- socket AF_INET Datagram 0
  ivp <- getInitialViewPort s i p
  c   <- newChan
  v   <- newIORef ivp
  q   <- newIORef False
  l   <- forkIO $ forever $ listen s v
  t   <- forkIO $ forever $ talk s q c i p
  playIO
    (InWindow "blubber" (windowWidth, windowHeight) (0, 0))
    black
    60
    MkGame {vp      = ivp
           ,pn      = n
           ,listenT = l
           ,talkT   = t
           ,ip      = a
           }
    draw
    (handle c)
    (step q v)

draw :: Game -> IO Picture
-- | Re'draw' the 'Game' based on the handed in 'ViewPort'. It 'Scale's things
--   according to the 'worldScale', and 'Translate's things according to their
--   'position's. Then it adds some 'Color' and shapes, and uses 'Text' to
--   print the name of the 'blub's -- and the 'mass' of the client player's
--   own 'blub'.
draw g = return
       $ Scale worldScale worldScale
       $ Pictures
       [Pictures ((\e ->
         Translate (viewPortTrans vx) (viewPortTrans vy)
       $ Translate (positionTrans e vx) (positionTrans e vy)
       $ Pictures [Color yellow $ circleSolid (realToFrac . radius $ e)])
     <$> (visibleNeutrals . vp $ g))
       ,Pictures ((\(s, e) ->
         Translate (viewPortTrans vx) (viewPortTrans vy)
       $ Translate (positionTrans e vx) (positionTrans e vy)
       $ Pictures [Color yellow $ circleSolid (realToFrac . radius $ e)
                  ,Scale 0.01 0.01 $ Color red $ Text s])
     <$> toList (visiblePlayers . vp $ g))]
  where worldScale        = min (fromIntegral windowHeight
                                / realToFrac (viewHeight . vp $ g))
                                (fromIntegral windowWidth
                                / realToFrac (viewWidth . vp $ g))
        viewPortTrans d   = -(realToFrac . d . viewPos . vp $ g)
        positionTrans e d = realToFrac . d . position $ e

handle :: Chan Message -> Event -> Game -> IO Game
-- | 'handle' input. If there's an 'EventMotion', write an 'UpdateVelocity' to
--   the passed in channel. If the client hits the space key, try to
--   'AddPlayer'.
handle c (EventMotion (x, y)) g = do
  writeChan c $
    UpdateVelocity . join (***) realToFrac $
      (realToFrac x / fromIntegral windowWidth  * viewWidth  (vp g)
      ,realToFrac y / fromIntegral windowHeight * viewHeight (vp g))
  return g
handle c (EventKey (SpecialKey KeySpace) Down _ _) g = do
  writeChan c $ AddPlayer (pn g)
  return g
handle c (EventKey (Char 'c') Down _ _) g = do
  writeChan c $ SrcPls (ip g)
  return g
handle c (EventKey (SpecialKey KeyEsc) Down _ _) g = do
  writeChan c Disconnect
  return g
handle _ _ g = return g

talk :: Socket -> IORef Bool -> Chan Message -> HostAddress -> PortNumber
          -> IO ()
-- | Every time there's an update to the channel, it means that there's
--   a 'Message' to send -- so send it.
talk s q c i p = do
  m <- readChan c
  void $ sendTo s (encode m) (SockAddrInet p i)
  when (m == Disconnect) $ writeIORef q True

getInitialViewPort :: Socket -> HostAddress -> PortNumber -> IO ViewPort
-- | Try to 'Connect' to the server and get the initial 'ViewPort'. Basically,
--   this should work or you're fucked.
getInitialViewPort s i p = do
  _ <- sendTo s (encode Connect) (SockAddrInet p i)
  (mesg, _) <- recvFrom s 1024
  let m = decode mesg :: Either String Reply
  case m of
    Right (View wp) -> return wp
    _               -> getInitialViewPort s i p

listen :: Socket -> IORef ViewPort -> IO ()
-- | Every time the client receives a message from the server
--
-- If it is a 'ViewPort', then update the 'Game' 'ViewPort'.
--
-- If it is a 'SrcLink', then print the source code link.
listen s v = do
  (mesg, _) <- recvFrom s 1024
  let m = decode mesg :: Either String Reply
  case m of
    Right (View wp)   -> writeIORef v wp
    Right (SrcLink l) -> putStrLn $ "source code link: " ++ l
    _                 -> return ()

step :: IORef Bool -> IORef ViewPort -> Float -> Game -> IO Game
-- | 'step' through the 'Game' one 'step' at the time. This simply reads the
--   newest 'ViewPort' as written by 'listen', and makes a new 'Game'
--   with it.
step q v _ g = do
  quit <- readIORef q
  when quit $ do
    killThread (listenT g)
    killThread (talkT g)
    exitSuccess
  wp <- readIORef v
  return g {vp = wp}
