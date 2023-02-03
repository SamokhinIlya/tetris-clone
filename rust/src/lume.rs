pub mod num_cast;

use std::any::Any;
use std::ops::IndexMut;
use std::time::Instant;

pub use num_cast::NumCast;

use anyhow::{bail, Context};
use windows::{
    core::PCSTR,
    Win32::{
        Foundation::{HWND, WPARAM, LPARAM, LRESULT, RECT, GetLastError},
        System::LibraryLoader::GetModuleHandleA,
        UI::{
            Input::KeyboardAndMouse::{
                GetAsyncKeyState,
                VIRTUAL_KEY,
                VK_LBUTTON,
                VK_RBUTTON,
                VK_LEFT,
                VK_RIGHT,
                VK_DOWN,
            },
            WindowsAndMessaging::{
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
                WM_DESTROY,
                WM_SETCURSOR,
                HMENU,
                SHOW_WINDOW_CMD,
                MSG,
                SetCursor,
                HCURSOR, SetWindowTextA,
            },
        },
        Graphics::Gdi::{GetDC, StretchDIBits, DIB_RGB_COLORS, SRCCOPY, BITMAPINFO, BITMAPINFOHEADER, BI_RGB},
    },
};

// TODO: graphics behaving funny when windows scale is 125%
//       check ShowWindow options

pub trait RawCanvas: IndexMut<usize, Output=u32> {
    fn width(&self) -> usize;
    fn height(&self) -> usize;
}

#[derive(Default, Debug)]
pub struct Input {
    pub mouse: Mouse,
    pub keyboard: Keyboard,
}

#[derive(Default, Debug)]
pub struct Mouse {
    pub x: usize,
    pub y: usize,
    pub left: Button,
    pub right: Button,
}

#[derive(Default, Debug)]
pub struct Keyboard {
    pub left: Button,
    pub right: Button,
    pub down: Button,
}

#[derive(Default, Debug, Clone, Copy)]
pub struct Button {
    prev: bool,
    curr: bool,
}

impl Button {
    pub fn is_pressed(self) -> bool {
        self.curr
    }

    pub fn just_pressed(self) -> bool {
        !self.prev && self.curr
    }

    fn update(&mut self, curr: bool) {
        self.prev = self.curr;
        self.curr = curr;
    }
}

pub type Update = fn(data: &mut dyn Any, raw_canvas: &mut dyn RawCanvas, input: &Input, dt: f64);

pub fn run(data: &mut dyn Any, update: Update) -> anyhow::Result<()> {
    let instance = unsafe { GetModuleHandleA(PCSTR::null()) }.context("GetModuleHandleA failed")?;
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
            None,
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

    let mut input = Input::default();
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
                    let [x, y, _, _] = unsafe { std::mem::transmute::<_, [u16; 4]>(msg.lParam) };
                    [input.mouse.y, input.mouse.x] = [y.into(), x.into()];
                },
                _ => unsafe {
                    TranslateMessage(&msg);
                    DispatchMessageA(&msg);
                },
            }
        }
        gather_input(&mut input);

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

        let now = Instant::now();
        let elapsed = now.duration_since(time).as_secs_f64();
        time = now;
        update(data, &mut bitmap, &input, elapsed);

        let result = unsafe {
            StretchDIBits(
                device_context,
                0, 0, window_width, window_height,
                0, 0, bitmap.width.num_cast(), bitmap.height.num_cast(),
                Some(bitmap.ptr as *const _),
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

fn gather_input(input: &mut Input) {
    fn is_pressed(vk: VIRTUAL_KEY) -> bool {
        let result = unsafe { GetAsyncKeyState(vk.0.into()) };
        result != 0 && result < 0
    }

    input.mouse.left.update(is_pressed(VK_LBUTTON));
    input.mouse.right.update(is_pressed(VK_RBUTTON));

    input.keyboard.left.update(is_pressed(VK_LEFT));
    input.keyboard.right.update(is_pressed(VK_RIGHT));
    input.keyboard.down.update(is_pressed(VK_DOWN));
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
        assert!(index <= size, "index = {index}, size = {size}");

        unsafe { &*self.ptr.add(index) }
    }
}

impl std::ops::IndexMut<usize> for Bitmap {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let size = self.size();
        assert!(index <= size, "index = {index}, size = {size}");

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
        self.width * self.height
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