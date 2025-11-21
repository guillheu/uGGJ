// src/sdl.gleam

/// Opaque type representing an SDL window
pub type Window

/// Opaque type representing an SDL renderer
pub type Renderer

/// Opaque type representing a rectangle
pub type Rect

/// Opaque type representing an SDL event
pub type Event

/// Initialize SDL with video subsystem
@external(erlang, "sdl_ffi", "init")
pub fn do_init() -> Nil

/// Create a new window
@external(erlang, "sdl_ffi", "create_window")
pub fn do_create_window(
  title: String,
  x: Int,
  y: Int,
  width: Int,
  height: Int,
) -> Window

/// Create a renderer for a window
@external(erlang, "sdl_ffi", "create_renderer")
pub fn do_create_renderer(window: Window) -> Renderer

/// Set the drawing color (RGBA)
@external(erlang, "sdl_ffi", "set_draw_color")
pub fn do_set_draw_color(
  renderer: Renderer,
  r: Int,
  g: Int,
  b: Int,
  a: Int,
) -> Nil

/// Clear the renderer with the current draw color
@external(erlang, "sdl_ffi", "clear")
pub fn do_clear(renderer: Renderer) -> Nil

/// Fill a rectangle with the current draw color
@external(erlang, "sdl_ffi", "fill_rect")
pub fn do_fill_rect(renderer: Renderer, rect: Rect) -> Nil

/// Present the rendered content to the screen
@external(erlang, "sdl_ffi", "present")
pub fn do_present(renderer: Renderer) -> Nil

/// Poll for events (returns False if no events)
@external(erlang, "sdl_ffi", "poll_event")
pub fn do_poll_event() -> Event

/// Delay for a number of milliseconds
@external(erlang, "sdl_ffi", "delay")
pub fn do_delay(ms: Int) -> Nil

/// Cleanup renderer and window, quit SDL
@external(erlang, "sdl_ffi", "cleanup")
pub fn do_cleanup(renderer: Renderer, window: Window) -> Nil
