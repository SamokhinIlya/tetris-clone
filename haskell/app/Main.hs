{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Exception (SomeException, try)

import GHC.Generics (Generic)
import Foreign.CStorable
import Foreign.Storable
import Foreign.Ptr

import System.Win32.DLL (getModuleHandle)
import System.Win32.Types (HINSTANCE)

import Graphics.Win32 ( mkClassName
                      , registerClass
                      , createWindowEx
                      , showWindow
                      , getDC
                      , peekMessage
                      , allocaMessage
                      , translateMessage
                      , dispatchMessage
                      )
import Graphics.Win32 ( wS_OVERLAPPEDWINDOW
                      , cS_HREDRAW, cS_VREDRAW
                      , LPPAINTSTRUCT
                      , RECT
                      , HDC
                      , WindowMessage
                      , LRESULT
                      , defWindowProcSafe
                      , sW_SHOWNORMAL
                      , HWND
                      , UINT
                      , WPARAM
                      , LPARAM
                      , DWORD
                      , POINT
                      )
import Graphics.Win32.Window (ClassName)

import Graphics.Win32.Message (wM_QUIT)

main :: IO ()
main = do
    currentInstance <- getModuleHandle Nothing
    className <- createClass currentInstance "className"
    let width = 1280
    let height = 720
    window <- createWindowEx 0 className "Window title" wS_OVERLAPPEDWINDOW Nothing Nothing (Just width) (Just height) Nothing Nothing currentInstance wndProc
    showWindow window sW_SHOWNORMAL
    deviceContext <- getDC (Just window)
    allocaMessage $ \ptrMsg ->
        let mainLoop = do
                       peekMessage ptrMsg Nothing 0 0 1
                       msg <- peek (castPtr ptrMsg :: Ptr MSG)
                       let m = message msg
                       if m == wM_QUIT
                           then return ()
                           else do
                                () <$ translateMessage ptrMsg
                                () <$ dispatchMessage ptrMsg
                                mainLoop
        in mainLoop
    return ()

data MSG = MSG { hwnd :: HWND
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

createClass :: HINSTANCE -> String -> IO ClassName
createClass currentInstance name = do
    let style = cS_HREDRAW + cS_VREDRAW
    let icon = Nothing
    let cursor = Nothing
    let brush = Nothing
    let menu = Nothing
    let className = mkClassName name
    registerClass (style, currentInstance, icon, cursor, brush, menu, className)
    return className

wndProc :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc hwnd wmsg wParam lParam = defWindowProcSafe (Just hwnd) wmsg wParam lParam