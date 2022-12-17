{-# LANGUAGE DeriveGeneric #-}

module Msg where

import GHC.Generics (Generic)
import Foreign.CStorable
import Foreign.Storable

import Graphics.Win32 ( HWND
                      , UINT
                      , WPARAM
                      , LPARAM
                      )

data MSG =
    MSG { hwnd :: HWND
        , message :: UINT
        , wParam :: WPARAM
        , lParam :: LPARAM
        } deriving (Generic)

instance CStorable MSG

instance Storable MSG where
    sizeOf = cSizeOf
    alignment = cAlignment
    poke = cPoke
    peek = cPeek