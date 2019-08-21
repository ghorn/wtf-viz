module Main ( main ) where

import WtfViz ( VizRunner(..), runViz )

data UserState = UserState -- dummy

main :: IO ()
main = runViz vizRunner

vizRunner :: VizRunner UserState
vizRunner =
  VizRunner
  { vizInitialize = pure UserState
  , vizUpdate = \_ userState -> pure userState
  , vizKeyPressed = \_ userState -> pure userState
  , vizKeyReleased = \_ userState -> pure userState
  , vizMousePressed = \_ userState -> pure userState
  , vizMouseReleased = \_ userState -> pure userState
  , vizMouseMoved = \_ _ _ _ userState -> pure userState
  }
