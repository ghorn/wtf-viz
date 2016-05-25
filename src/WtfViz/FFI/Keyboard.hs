{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module WtfViz.FFI.Keyboard
       ( KeyCode(..), Keyboard, getKeyboard, isKeyDown
       ) where

import GHC.Generics ( Generic )

import Foreign.C.Types ( CInt(..) )
import Foreign.Ptr ( Ptr )

data KeyCode
  = KC_UNASSIGNED
  | KC_ESCAPE
  | KC_1
  | KC_2
  | KC_3
  | KC_4
  | KC_5
  | KC_6
  | KC_7
  | KC_8
  | KC_9
  | KC_0
  | KC_MINUS           -- ^ - on main keyboard
  | KC_EQUALS
  | KC_BACK            -- ^ backspace
  | KC_TAB
  | KC_Q
  | KC_W
  | KC_E
  | KC_R
  | KC_T
  | KC_Y
  | KC_U
  | KC_I
  | KC_O
  | KC_P
  | KC_LBRACKET
  | KC_RBRACKET
  | KC_RETURN          -- ^ Enter on main keyboard
  | KC_LCONTROL
  | KC_A
  | KC_S
  | KC_D
  | KC_F
  | KC_G
  | KC_H
  | KC_J
  | KC_K
  | KC_L
  | KC_SEMICOLON
  | KC_APOSTROPHE
  | KC_GRAVE           -- ^ accent
  | KC_LSHIFT
  | KC_BACKSLASH
  | KC_Z
  | KC_X
  | KC_C
  | KC_V
  | KC_B
  | KC_N
  | KC_M
  | KC_COMMA
  | KC_PERIOD          -- ^ . on main keyboard
  | KC_SLASH           -- ^ / on main keyboard
  | KC_RSHIFT
  | KC_MULTIPLY        -- ^ * on numeric keypad
  | KC_LMENU           -- ^ left Alt
  | KC_SPACE
  | KC_CAPITAL
  | KC_F1
  | KC_F2
  | KC_F3
  | KC_F4
  | KC_F5
  | KC_F6
  | KC_F7
  | KC_F8
  | KC_F9
  | KC_F10
  | KC_NUMLOCK
  | KC_SCROLL          -- ^ Scroll Lock
  | KC_NUMPAD7
  | KC_NUMPAD8
  | KC_NUMPAD9
  | KC_SUBTRACT        -- ^ - on numeric keypad
  | KC_NUMPAD4
  | KC_NUMPAD5
  | KC_NUMPAD6
  | KC_ADD             -- ^ + on numeric keypad
  | KC_NUMPAD1
  | KC_NUMPAD2
  | KC_NUMPAD3
  | KC_NUMPAD0
  | KC_DECIMAL         -- ^ . on numeric keypad
  | KC_OEM_102         -- ^ < > | on UK/Germany keyboards
  | KC_F11
  | KC_F12
  | KC_F13             -- ^                     (NEC PC98)
  | KC_F14             -- ^                     (NEC PC98)
  | KC_F15             -- ^                     (NEC PC98)
  | KC_KANA            -- ^ (Japanese keyboard)
  | KC_ABNT_C1         -- ^ / ? on Portugese (Brazilian) keyboards
  | KC_CONVERT         -- ^ (Japanese keyboard)
  | KC_NOCONVERT       -- ^ (Japanese keyboard)
  | KC_YEN             -- ^ (Japanese keyboard)
  | KC_ABNT_C2         -- ^ Numpad . on Portugese (Brazilian) keyboards
  | KC_NUMPADEQUALS    -- ^ = on numeric keypad (NEC PC98)
  | KC_PREVTRACK       -- ^ Previous Track (KC_CIRCUMFLEX on Japanese keyboard)
  | KC_AT              -- ^                     (NEC PC98)
  | KC_COLON           -- ^                     (NEC PC98)
  | KC_UNDERLINE       -- ^                     (NEC PC98)
  | KC_KANJI           -- ^ (Japanese keyboard)
  | KC_STOP            -- ^                     (NEC PC98)
  | KC_AX              -- ^                     (Japan AX)
  | KC_UNLABELED       -- ^                        (J3100)
  | KC_NEXTTRACK       -- ^ Next Track
  | KC_NUMPADENTER     -- ^ Enter on numeric keypad
  | KC_RCONTROL
  | KC_MUTE            -- ^ Mute
  | KC_CALCULATOR      -- ^ Calculator
  | KC_PLAYPAUSE       -- ^ Play / Pause
  | KC_MEDIASTOP       -- ^ Media Stop
  | KC_VOLUMEDOWN      -- ^ Volume -
  | KC_VOLUMEUP        -- ^ Volume +
  | KC_WEBHOME         -- ^ Web home
  | KC_NUMPADCOMMA     -- ^ , on numeric keypad (NEC PC98)
  | KC_DIVIDE          -- ^ / on numeric keypad
  | KC_SYSRQ
  | KC_RMENU           -- ^ right Alt
  | KC_PAUSE           -- ^ Pause
  | KC_HOME            -- ^ Home on arrow keypad
  | KC_UP              -- ^ UpArrow on arrow keypad
  | KC_PGUP            -- ^ PgUp on arrow keypad
  | KC_LEFT            -- ^ LeftArrow on arrow keypad
  | KC_RIGHT           -- ^ RightArrow on arrow keypad
  | KC_END             -- ^ End on arrow keypad
  | KC_DOWN            -- ^ DownArrow on arrow keypad
  | KC_PGDOWN          -- ^ PgDn on arrow keypad
  | KC_INSERT          -- ^ Insert on arrow keypad
  | KC_DELETE          -- ^ Delete on arrow keypad
  | KC_LWIN            -- ^ Left Windows key
  | KC_RWIN            -- ^ Right Windows key
  | KC_APPS            -- ^ AppMenu key
  | KC_POWER           -- ^ System Power
  | KC_SLEEP           -- ^ System Sleep
  | KC_WAKE            -- ^ System Wake
  | KC_WEBSEARCH       -- ^ Web Search
  | KC_WEBFAVORITES    -- ^ Web Favorites
  | KC_WEBREFRESH      -- ^ Web Refresh
  | KC_WEBSTOP         -- ^ Web Stop
  | KC_WEBFORWARD      -- ^ Web Forward
  | KC_WEBBACK         -- ^ Web Back
  | KC_MYCOMPUTER      -- ^ My Computer
  | KC_MAIL            -- ^ Mail
  | KC_MEDIASELECT     -- ^ Media Select
  deriving (Eq, Show, Ord, Generic)

instance Enum KeyCode where
  fromEnum KC_UNASSIGNED  = 0x00
  fromEnum KC_ESCAPE      = 0x01
  fromEnum KC_1           = 0x02
  fromEnum KC_2           = 0x03
  fromEnum KC_3           = 0x04
  fromEnum KC_4           = 0x05
  fromEnum KC_5           = 0x06
  fromEnum KC_6           = 0x07
  fromEnum KC_7           = 0x08
  fromEnum KC_8           = 0x09
  fromEnum KC_9           = 0x0A
  fromEnum KC_0           = 0x0B
  fromEnum KC_MINUS       = 0x0C
  fromEnum KC_EQUALS      = 0x0D
  fromEnum KC_BACK        = 0x0E
  fromEnum KC_TAB         = 0x0F
  fromEnum KC_Q           = 0x10
  fromEnum KC_W           = 0x11
  fromEnum KC_E           = 0x12
  fromEnum KC_R           = 0x13
  fromEnum KC_T           = 0x14
  fromEnum KC_Y           = 0x15
  fromEnum KC_U           = 0x16
  fromEnum KC_I           = 0x17
  fromEnum KC_O           = 0x18
  fromEnum KC_P           = 0x19
  fromEnum KC_LBRACKET    = 0x1A
  fromEnum KC_RBRACKET    = 0x1B
  fromEnum KC_RETURN      = 0x1C
  fromEnum KC_LCONTROL    = 0x1D
  fromEnum KC_A           = 0x1E
  fromEnum KC_S           = 0x1F
  fromEnum KC_D           = 0x20
  fromEnum KC_F           = 0x21
  fromEnum KC_G           = 0x22
  fromEnum KC_H           = 0x23
  fromEnum KC_J           = 0x24
  fromEnum KC_K           = 0x25
  fromEnum KC_L           = 0x26
  fromEnum KC_SEMICOLON   = 0x27
  fromEnum KC_APOSTROPHE  = 0x28
  fromEnum KC_GRAVE       = 0x29
  fromEnum KC_LSHIFT      = 0x2A
  fromEnum KC_BACKSLASH   = 0x2B
  fromEnum KC_Z           = 0x2C
  fromEnum KC_X           = 0x2D
  fromEnum KC_C           = 0x2E
  fromEnum KC_V           = 0x2F
  fromEnum KC_B           = 0x30
  fromEnum KC_N           = 0x31
  fromEnum KC_M           = 0x32
  fromEnum KC_COMMA       = 0x33
  fromEnum KC_PERIOD      = 0x34
  fromEnum KC_SLASH       = 0x35
  fromEnum KC_RSHIFT      = 0x36
  fromEnum KC_MULTIPLY    = 0x37
  fromEnum KC_LMENU       = 0x38
  fromEnum KC_SPACE       = 0x39
  fromEnum KC_CAPITAL     = 0x3A
  fromEnum KC_F1          = 0x3B
  fromEnum KC_F2          = 0x3C
  fromEnum KC_F3          = 0x3D
  fromEnum KC_F4          = 0x3E
  fromEnum KC_F5          = 0x3F
  fromEnum KC_F6          = 0x40
  fromEnum KC_F7          = 0x41
  fromEnum KC_F8          = 0x42
  fromEnum KC_F9          = 0x43
  fromEnum KC_F10         = 0x44
  fromEnum KC_NUMLOCK     = 0x45
  fromEnum KC_SCROLL      = 0x46
  fromEnum KC_NUMPAD7     = 0x47
  fromEnum KC_NUMPAD8     = 0x48
  fromEnum KC_NUMPAD9     = 0x49
  fromEnum KC_SUBTRACT    = 0x4A
  fromEnum KC_NUMPAD4     = 0x4B
  fromEnum KC_NUMPAD5     = 0x4C
  fromEnum KC_NUMPAD6     = 0x4D
  fromEnum KC_ADD         = 0x4E
  fromEnum KC_NUMPAD1     = 0x4F
  fromEnum KC_NUMPAD2     = 0x50
  fromEnum KC_NUMPAD3     = 0x51
  fromEnum KC_NUMPAD0     = 0x52
  fromEnum KC_DECIMAL     = 0x53
  fromEnum KC_OEM_102     = 0x56
  fromEnum KC_F11         = 0x57
  fromEnum KC_F12         = 0x58
  fromEnum KC_F13         = 0x64
  fromEnum KC_F14         = 0x65
  fromEnum KC_F15         = 0x66
  fromEnum KC_KANA        = 0x70
  fromEnum KC_ABNT_C1     = 0x73
  fromEnum KC_CONVERT     = 0x79
  fromEnum KC_NOCONVERT   = 0x7B
  fromEnum KC_YEN         = 0x7D
  fromEnum KC_ABNT_C2     = 0x7E
  fromEnum KC_NUMPADEQUALS= 0x8D
  fromEnum KC_PREVTRACK   = 0x90
  fromEnum KC_AT          = 0x91
  fromEnum KC_COLON       = 0x92
  fromEnum KC_UNDERLINE   = 0x93
  fromEnum KC_KANJI       = 0x94
  fromEnum KC_STOP        = 0x95
  fromEnum KC_AX          = 0x96
  fromEnum KC_UNLABELED   = 0x97
  fromEnum KC_NEXTTRACK   = 0x99
  fromEnum KC_NUMPADENTER = 0x9C
  fromEnum KC_RCONTROL    = 0x9D
  fromEnum KC_MUTE        = 0xA0
  fromEnum KC_CALCULATOR  = 0xA1
  fromEnum KC_PLAYPAUSE   = 0xA2
  fromEnum KC_MEDIASTOP   = 0xA4
  fromEnum KC_VOLUMEDOWN  = 0xAE
  fromEnum KC_VOLUMEUP    = 0xB0
  fromEnum KC_WEBHOME     = 0xB2
  fromEnum KC_NUMPADCOMMA = 0xB3
  fromEnum KC_DIVIDE      = 0xB5
  fromEnum KC_SYSRQ       = 0xB7
  fromEnum KC_RMENU       = 0xB8
  fromEnum KC_PAUSE       = 0xC5
  fromEnum KC_HOME        = 0xC7
  fromEnum KC_UP          = 0xC8
  fromEnum KC_PGUP        = 0xC9
  fromEnum KC_LEFT        = 0xCB
  fromEnum KC_RIGHT       = 0xCD
  fromEnum KC_END         = 0xCF
  fromEnum KC_DOWN        = 0xD0
  fromEnum KC_PGDOWN      = 0xD1
  fromEnum KC_INSERT      = 0xD2
  fromEnum KC_DELETE      = 0xD3
  fromEnum KC_LWIN        = 0xDB
  fromEnum KC_RWIN        = 0xDC
  fromEnum KC_APPS        = 0xDD
  fromEnum KC_POWER       = 0xDE
  fromEnum KC_SLEEP       = 0xDF
  fromEnum KC_WAKE        = 0xE3
  fromEnum KC_WEBSEARCH   = 0xE5
  fromEnum KC_WEBFAVORITES= 0xE6
  fromEnum KC_WEBREFRESH  = 0xE7
  fromEnum KC_WEBSTOP     = 0xE8
  fromEnum KC_WEBFORWARD  = 0xE9
  fromEnum KC_WEBBACK     = 0xEA
  fromEnum KC_MYCOMPUTER  = 0xEB
  fromEnum KC_MAIL        = 0xEC
  fromEnum KC_MEDIASELECT = 0xED


  toEnum 0x00 = KC_UNASSIGNED
  toEnum 0x01 = KC_ESCAPE
  toEnum 0x02 = KC_1
  toEnum 0x03 = KC_2
  toEnum 0x04 = KC_3
  toEnum 0x05 = KC_4
  toEnum 0x06 = KC_5
  toEnum 0x07 = KC_6
  toEnum 0x08 = KC_7
  toEnum 0x09 = KC_8
  toEnum 0x0A = KC_9
  toEnum 0x0B = KC_0
  toEnum 0x0C = KC_MINUS
  toEnum 0x0D = KC_EQUALS
  toEnum 0x0E = KC_BACK
  toEnum 0x0F = KC_TAB
  toEnum 0x10 = KC_Q
  toEnum 0x11 = KC_W
  toEnum 0x12 = KC_E
  toEnum 0x13 = KC_R
  toEnum 0x14 = KC_T
  toEnum 0x15 = KC_Y
  toEnum 0x16 = KC_U
  toEnum 0x17 = KC_I
  toEnum 0x18 = KC_O
  toEnum 0x19 = KC_P
  toEnum 0x1A = KC_LBRACKET
  toEnum 0x1B = KC_RBRACKET
  toEnum 0x1C = KC_RETURN
  toEnum 0x1D = KC_LCONTROL
  toEnum 0x1E = KC_A
  toEnum 0x1F = KC_S
  toEnum 0x20 = KC_D
  toEnum 0x21 = KC_F
  toEnum 0x22 = KC_G
  toEnum 0x23 = KC_H
  toEnum 0x24 = KC_J
  toEnum 0x25 = KC_K
  toEnum 0x26 = KC_L
  toEnum 0x27 = KC_SEMICOLON
  toEnum 0x28 = KC_APOSTROPHE
  toEnum 0x29 = KC_GRAVE
  toEnum 0x2A = KC_LSHIFT
  toEnum 0x2B = KC_BACKSLASH
  toEnum 0x2C = KC_Z
  toEnum 0x2D = KC_X
  toEnum 0x2E = KC_C
  toEnum 0x2F = KC_V
  toEnum 0x30 = KC_B
  toEnum 0x31 = KC_N
  toEnum 0x32 = KC_M
  toEnum 0x33 = KC_COMMA
  toEnum 0x34 = KC_PERIOD
  toEnum 0x35 = KC_SLASH
  toEnum 0x36 = KC_RSHIFT
  toEnum 0x37 = KC_MULTIPLY
  toEnum 0x38 = KC_LMENU
  toEnum 0x39 = KC_SPACE
  toEnum 0x3A = KC_CAPITAL
  toEnum 0x3B = KC_F1
  toEnum 0x3C = KC_F2
  toEnum 0x3D = KC_F3
  toEnum 0x3E = KC_F4
  toEnum 0x3F = KC_F5
  toEnum 0x40 = KC_F6
  toEnum 0x41 = KC_F7
  toEnum 0x42 = KC_F8
  toEnum 0x43 = KC_F9
  toEnum 0x44 = KC_F10
  toEnum 0x45 = KC_NUMLOCK
  toEnum 0x46 = KC_SCROLL
  toEnum 0x47 = KC_NUMPAD7
  toEnum 0x48 = KC_NUMPAD8
  toEnum 0x49 = KC_NUMPAD9
  toEnum 0x4A = KC_SUBTRACT
  toEnum 0x4B = KC_NUMPAD4
  toEnum 0x4C = KC_NUMPAD5
  toEnum 0x4D = KC_NUMPAD6
  toEnum 0x4E = KC_ADD
  toEnum 0x4F = KC_NUMPAD1
  toEnum 0x50 = KC_NUMPAD2
  toEnum 0x51 = KC_NUMPAD3
  toEnum 0x52 = KC_NUMPAD0
  toEnum 0x53 = KC_DECIMAL
  toEnum 0x56 = KC_OEM_102
  toEnum 0x57 = KC_F11
  toEnum 0x58 = KC_F12
  toEnum 0x64 = KC_F13
  toEnum 0x65 = KC_F14
  toEnum 0x66 = KC_F15
  toEnum 0x70 = KC_KANA
  toEnum 0x73 = KC_ABNT_C1
  toEnum 0x79 = KC_CONVERT
  toEnum 0x7B = KC_NOCONVERT
  toEnum 0x7D = KC_YEN
  toEnum 0x7E = KC_ABNT_C2
  toEnum 0x8D = KC_NUMPADEQUALS
  toEnum 0x90 = KC_PREVTRACK
  toEnum 0x91 = KC_AT
  toEnum 0x92 = KC_COLON
  toEnum 0x93 = KC_UNDERLINE
  toEnum 0x94 = KC_KANJI
  toEnum 0x95 = KC_STOP
  toEnum 0x96 = KC_AX
  toEnum 0x97 = KC_UNLABELED
  toEnum 0x99 = KC_NEXTTRACK
  toEnum 0x9C = KC_NUMPADENTER
  toEnum 0x9D = KC_RCONTROL
  toEnum 0xA0 = KC_MUTE
  toEnum 0xA1 = KC_CALCULATOR
  toEnum 0xA2 = KC_PLAYPAUSE
  toEnum 0xA4 = KC_MEDIASTOP
  toEnum 0xAE = KC_VOLUMEDOWN
  toEnum 0xB0 = KC_VOLUMEUP
  toEnum 0xB2 = KC_WEBHOME
  toEnum 0xB3 = KC_NUMPADCOMMA
  toEnum 0xB5 = KC_DIVIDE
  toEnum 0xB7 = KC_SYSRQ
  toEnum 0xB8 = KC_RMENU
  toEnum 0xC5 = KC_PAUSE
  toEnum 0xC7 = KC_HOME
  toEnum 0xC8 = KC_UP
  toEnum 0xC9 = KC_PGUP
  toEnum 0xCB = KC_LEFT
  toEnum 0xCD = KC_RIGHT
  toEnum 0xCF = KC_END
  toEnum 0xD0 = KC_DOWN
  toEnum 0xD1 = KC_PGDOWN
  toEnum 0xD2 = KC_INSERT
  toEnum 0xD3 = KC_DELETE
  toEnum 0xDB = KC_LWIN
  toEnum 0xDC = KC_RWIN
  toEnum 0xDD = KC_APPS
  toEnum 0xDE = KC_POWER
  toEnum 0xDF = KC_SLEEP
  toEnum 0xE3 = KC_WAKE
  toEnum 0xE5 = KC_WEBSEARCH
  toEnum 0xE6 = KC_WEBFAVORITES
  toEnum 0xE7 = KC_WEBREFRESH
  toEnum 0xE8 = KC_WEBSTOP
  toEnum 0xE9 = KC_WEBFORWARD
  toEnum 0xEA = KC_WEBBACK
  toEnum 0xEB = KC_MYCOMPUTER
  toEnum 0xEC = KC_MAIL
  toEnum 0xED = KC_MEDIASELECT
  toEnum k = error $ "toEnum KeyCode got unrecognized integer (" ++ show k ++ ")"


data Keyboard'
newtype Keyboard = Keyboard (Ptr Keyboard')

-- Keyboard
foreign import ccall unsafe "wv2_keyboard_is_key_down"
  c_isKeyDown :: Ptr Keyboard' -> CInt -> IO CInt

isKeyDown :: Keyboard -> KeyCode -> IO Bool
isKeyDown (Keyboard obj) key = do
  isDown <- c_isKeyDown obj (fromIntegral (fromEnum key))
  return $ case isDown of
    0 -> False
    _ -> True

foreign import ccall unsafe "wv2_keyboard_get"
  c_keyboardGet :: IO (Ptr Keyboard')

getKeyboard :: IO Keyboard
getKeyboard = Keyboard <$> c_keyboardGet
