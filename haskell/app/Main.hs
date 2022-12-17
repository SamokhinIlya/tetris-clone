module Main where

import Msg

import Control.Exception (SomeException, try)

import Foreign.Storable
import Foreign.Ptr

import System.Win32.DLL (getModuleHandle)
import System.Win32.Types (HINSTANCE)

import qualified Graphics.Win32 (createWindowEx)
import Graphics.Win32 ( mkClassName
                      , registerClass
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
    window <- createWindow currentInstance className "Window title" 1280 720
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

createWindow :: HINSTANCE -> ClassName -> String -> Int -> Int -> IO HWND
createWindow inst className title width height =
    let exStyle = 0
        style = wS_OVERLAPPEDWINDOW
        x = Nothing
        y = Nothing
        w = Just width
        h = Just height
        parent = Nothing
        menu = Nothing
    in Graphics.Win32.createWindowEx exStyle className title style x y w h parent menu inst wndProc

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