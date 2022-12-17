module Main where

import Msg

import Foreign.Storable
import Foreign.Ptr

import System.Win32.DLL (getModuleHandle)
import System.Win32.Types (HINSTANCE)

import qualified Graphics.Win32 (createWindowEx)
import Graphics.Win32 ( mkClassName
                      , registerClass
                      , showWindow
                      , getDC
                      , c_PeekMessage
                      , allocaMessage
                      , translateMessage
                      , dispatchMessage
                      , postQuitMessage
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

import Graphics.Win32.Message ( wM_QUIT
                              , wM_DESTROY
                              --, wM_SETCURSOR - no SetCursor function
                              --, wM_EXITSIZEMOVE - no message
                              )

main :: IO ()
main = do
    currentInstance <- getModuleHandle Nothing
    className <- createClass currentInstance "className"
    window <- createWindow currentInstance className "Window title" 1280 720
    showWindow window sW_SHOWNORMAL
    deviceContext <- getDC (Just window)
    let mainLoop = do
            continue <- messagePump
            if continue
                then mainLoop
                else return ()
        in mainLoop

messagePump :: IO Bool
messagePump = allocaMessage $ \ptrMsg ->
    let handleMsg = do
            res <- c_PeekMessage ptrMsg nullPtr 0 0 1
            if res > 0
                then do
                    msg <- peek (castPtr ptrMsg :: Ptr MSG)
                    let m = message msg
                    if m == wM_QUIT
                        then return False
                        else do
                            () <$ translateMessage ptrMsg
                            () <$ dispatchMessage ptrMsg
                            handleMsg
                else return True
    in handleMsg

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
        icon = Nothing
        cursor = Nothing
        brush = Nothing
        menu = Nothing
        className = mkClassName name
    registerClass (style, currentInstance, icon, cursor, brush, menu, className)
    return className

wndProc :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
wndProc hwnd wmsg wParam lParam
    | wmsg == wM_DESTROY   = do postQuitMessage 0
                                return 0
    | otherwise            = defWindowProcSafe (Just hwnd) wmsg wParam lParam