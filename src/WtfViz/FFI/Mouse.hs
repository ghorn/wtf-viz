{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module WtfViz.FFI.Mouse
       ( Axis(..), MouseButton(..)
       , MouseState(..), MouseState'
       , isButtonDown
       ) where

import GHC.Generics ( Generic, Generic1 )

import Foreign.C.Types ( CInt(..) )
import Foreign.Ptr ( Ptr )

data MouseButton
  = MB_Left
  | MB_Right
  | MB_Middle
  | MB_Button3
  | MB_Button4
  | MB_Button5
  | MB_Button6
  | MB_Button7
  deriving (Eq, Show, Ord, Generic)

instance Enum MouseButton where
  fromEnum MB_Left = 0
  fromEnum MB_Right = 1
  fromEnum MB_Middle = 2
  fromEnum MB_Button3 = 3
  fromEnum MB_Button4 = 4
  fromEnum MB_Button5 = 5
  fromEnum MB_Button6 = 6
  fromEnum MB_Button7 = 7

  toEnum 0 = MB_Left
  toEnum 1 = MB_Right
  toEnum 2 = MB_Middle
  toEnum 3 = MB_Button3
  toEnum 4 = MB_Button4
  toEnum 5 = MB_Button5
  toEnum 6 = MB_Button6
  toEnum 7 = MB_Button7
  toEnum k = error $ "MouseButton toEnum got out of bounds integer ( " ++ show k ++ ")"

data Axis a =
  Axis
  { axisAbs :: a
  , axisRel :: a
  } deriving (Functor, Foldable, Traversable, Eq, Ord, Show, Generic, Generic1)

data MouseState'
newtype MouseState = MouseState (Ptr MouseState')

-- Mouse
foreign import ccall unsafe "wv2_mouse_state_is_button_down"
  c_mouseStateIsButtonDown :: Ptr MouseState' -> CInt -> IO Bool

isButtonDown :: MouseState -> MouseButton -> IO Bool
isButtonDown (MouseState obj) button = c_mouseStateIsButtonDown obj (fromIntegral (fromEnum button))

--foreign import ccall unsafe "wv2_mouse_get_state"
--  c_mouseGetState :: IO (Ptr Keyboard')
--
--getKeyboard :: IO Keyboard
--getKeyboard = Keyboard <$> c_keyboardGet
--
--foreign import ccall unsafe "wv2_keyboard_is_key_down"
--  c_isKeyDown :: Ptr Keyboard' -> CInt -> IO CInt
--
--isKeyDown :: Keyboard -> KeyCode -> IO Bool
--isKeyDown (Keyboard obj) key = do
--  isDown <- c_isKeyDown obj (fromIntegral (fromEnum key))
--  return $ case isDown of
--    0 -> False
--    _ -> True
--
