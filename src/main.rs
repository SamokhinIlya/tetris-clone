#![warn(clippy::pedantic)]
#![allow(clippy::semicolon_if_nothing_returned)]

mod game;
mod num_cast;

use num_cast::NumCast;
use game::RawCanvas;
use std::time::Instant;
use anyhow::{bail, Context};
use windows::{
    core::PCSTR,
    Win32::{
        Foundation::{HWND, WPARAM, LPARAM, LRESULT, RECT, GetLastError},
        System::LibraryLoader::GetModuleHandleA,
        UI::WindowsAndMessaging::{
            WNDCLASSA,
            WINDOW_EX_STYLE,
            RegisterClassA,
            CreateWindowExA,
            ShowWindow,
            DefWindowProcA,
            PeekMessageA,
            TranslateMessage,
            DispatchMessageA,
            GetClientRect,
            PostQuitMessage,
            PostMessageA,
            WS_OVERLAPPEDWINDOW,
            CW_USEDEFAULT,
            CS_HREDRAW,
            CS_VREDRAW,
            PM_REMOVE,
            WM_QUIT,
            WM_MOUSEMOVE,
            WM_EXITSIZEMOVE,
            MK_LBUTTON,
            MK_RBUTTON,
            WM_DESTROY,
            WM_SETCURSOR,
            HMENU,
            SHOW_WINDOW_CMD,
            MSG,
            SetCursor,
            HCURSOR, SetWindowTextA,
        },
        Graphics::Gdi::{GetDC, StretchDIBits, DIB_RGB_COLORS, SRCCOPY, BITMAPINFO, BITMAPINFOHEADER, BI_RGB},
    },
};

fn main() -> anyhow::Result<()> {
    let instance = unsafe { GetModuleHandleA(PCSTR::default()) }.context("GetModuleHandleA failed")?;
    if instance.is_invalid() {
        bail!("hinstance is invalid: {:?}", instance)
    }

    let class_name = PCSTR(&b"illuminator\0"[0]);

    let window_class = WNDCLASSA {
        style: CS_HREDRAW | CS_VREDRAW,
        lpfnWndProc: Some(win_proc),
        lpszClassName: class_name,
        ..Default::default()
    };
    if unsafe { RegisterClassA(&window_class) } == 0 {
        bail!("RegisterClassA failed: GetLastError() -> {:?}", unsafe { GetLastError() })
    }

    let window = unsafe {
        CreateWindowExA(
            WINDOW_EX_STYLE::default(),
            class_name,
            PCSTR(&b"hello\0"[0]),
            WS_OVERLAPPEDWINDOW,
            CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
            HWND(0),
            HMENU(0),
            instance,
            std::ptr::null()
        )
    };
    if window == HWND(0) {
        bail!("CreateWindowExA failed: GetLastError() -> {:?}", unsafe { GetLastError() });
    }
    unsafe { ShowWindow(window, SHOW_WINDOW_CMD(10)) };

    let device_context = unsafe { GetDC(window) };
    if device_context.is_invalid() {
        bail!("GetDC failed");
    }

    let mut game_state = game::State::new();
    let mut input = game::Input::default();
    let mut bitmap = Bitmap::with_size(1280, 720).context("Bitmap::with_size failed")?;
    let mut resize_bitmap = true;
    let mut time = Instant::now();
    'main: loop {
        let mut msg = MSG::default();
        while unsafe { PeekMessageA(&mut msg, HWND(0), 0, 0, PM_REMOVE) }.as_bool() {
            match msg.message {
                WM_QUIT => break 'main,
                WM_EXITSIZEMOVE => {
                    resize_bitmap = true;
                },
                WM_MOUSEMOVE => {
                    input.mouse.left = msg.wParam.0 & MK_LBUTTON as usize != 0;
                    input.mouse.right = msg.wParam.0 & MK_RBUTTON as usize != 0;

                    let [x, y, _, _] = unsafe { std::mem::transmute::<_, [u16; 4]>(msg.lParam) };
                    [input.mouse.y, input.mouse.x] = [y.into(), x.into()];
                },
                _ => unsafe {
                    TranslateMessage(&msg);
                    DispatchMessageA(&msg);
                },
            }
        }

        let title = std::ffi::CString::new(format!("cursor: {:?}", (input.mouse.x, input.mouse.y))).expect("input has nul byte");
        unsafe {
            // TODO: check result
            SetWindowTextA(window, PCSTR(title.as_ptr().cast()));
        }

        let mut client_rect = RECT::default();
        if !unsafe { GetClientRect(window, &mut client_rect) }.as_bool() {
            bail!("GetClientRect failed: GetLastError() -> {:?}", unsafe { GetLastError() })
        }
        let window_width = client_rect.right - client_rect.left;
        let window_height = client_rect.bottom - client_rect.top;

        if resize_bitmap {
            bitmap.resize(window_width.num_cast(), window_height.num_cast()).context("bitmap.resize failed")?;
            resize_bitmap = false;
        }

        let elapsed = time.elapsed().as_secs_f64();
        time = Instant::now();
        game::update(&mut game_state, &mut bitmap, &input, elapsed);

        let result = unsafe {
            StretchDIBits(
                device_context,
                0, 0, window_width, window_height,
                0, 0, bitmap.width.num_cast(), bitmap.height.num_cast(),
                bitmap.ptr as *const _,
                &bitmap.info,
                DIB_RGB_COLORS,
                SRCCOPY,
            )
        };
        if result == 0 {
            bail!("StretchDIBits failed");
        }
    }

    Ok(())
}

unsafe extern "system" fn win_proc(hwnd: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    match msg {
        WM_DESTROY => {
            PostQuitMessage(0);
            LRESULT(0)
        },
        WM_SETCURSOR => {
            SetCursor(HCURSOR(0));
            LRESULT(0)
        },
        WM_EXITSIZEMOVE => {
            // TODO: check result
            PostMessageA(hwnd, msg, wparam, lparam);
            LRESULT(0)
        },
        _ => DefWindowProcA(hwnd, msg, wparam, lparam)
    }
}

type BitmapData = u32;

struct Bitmap {
    ptr: *mut BitmapData,
    width: usize,
    height: usize,
    info: BITMAPINFO,
}

impl RawCanvas for Bitmap {
    fn width(&self) -> usize {
        self.width
    }

    fn height(&self) -> usize {
        self.height
    }
}

impl std::ops::Index<usize> for Bitmap {
    type Output = BitmapData;

    fn index(&self, index: usize) -> &Self::Output {
        let size = self.size();
        assert!(index <= size, "index = {}, size = {}", index, size);

        unsafe { &*self.ptr.add(index) }
    }
}

impl std::ops::IndexMut<usize> for Bitmap {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let size = self.size();
        assert!(index <= size, "index = {}, size = {}", index, size);

        unsafe { &mut *self.ptr.add(index) }
    }
}

impl Bitmap {
    fn with_size(width: usize, height: usize) -> Result<Self, std::alloc::LayoutError> {
        use std::alloc::{alloc, Layout, handle_alloc_error};
        use std::mem::{size_of, align_of};

        let layout = Layout::from_size_align(width * height * size_of::<BitmapData>(), align_of::<BitmapData>())?;
        let ptr: *mut BitmapData = unsafe { alloc(layout) }.cast();
        if ptr.is_null() {
            handle_alloc_error(layout);
        }

        Ok(Self {
            ptr,
            width,
            height,
            info: BITMAPINFO {
                bmiHeader: BITMAPINFOHEADER {
                    biSize: std::mem::size_of::<BITMAPINFOHEADER>().num_cast(),
                    biWidth: width.num_cast::<i32>(),
                    biHeight: -height.num_cast::<i32>(),
                    biPlanes: 1,
                    biBitCount: 32,
                    biCompression: BI_RGB as _,
                    ..Default::default()
                },
                ..Default::default()
            },
        })
    }

    fn size(&self) -> usize {
        (self.width * self.height) as _
    }

    fn resize(&mut self, width: usize, height: usize) -> Result<(), std::alloc::LayoutError> {
        use std::alloc::{realloc, Layout, handle_alloc_error};
        use std::mem::{size_of, align_of};

        let layout = Layout::from_size_align(self.width * self.height * size_of::<BitmapData>(), align_of::<BitmapData>())?;
        let ptr: *mut BitmapData = unsafe { realloc(self.ptr.cast(), layout, width * height * size_of::<BitmapData>()) }.cast();
        if ptr.is_null() {
            handle_alloc_error(layout);
        }

        self.ptr = ptr;
        self.width = width;
        self.height = height;
        self.info.bmiHeader.biWidth = width.num_cast::<i32>();
        self.info.bmiHeader.biHeight = -height.num_cast::<i32>();
        Ok(())
    }
}