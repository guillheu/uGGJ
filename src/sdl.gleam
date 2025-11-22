import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist

// src/sdl.gleam

pub type Texture

/// Opaque type representing an SDL window
pub type Window

/// Opaque type representing an SDL renderer
pub type Renderer

/// Opaque type representing an SDL event
pub type Event {
  Quit(timestamp: Int)
  Undefined(timestamp: Int)
  MouseMotion(
    timestamp: Int,
    state: List(Nil),
    which: Int,
    y: Int,
    window_id: Int,
    x: Int,
    xrel: Int,
    yrel: Int,
  )
  NoEvent
}

pub type EventDetails(any) =
  Dict(Atom, any)

/// Initialize SDL with video subsystem
@external(erlang, "sdl_ffi", "init")
pub fn do_init(title: String, width: Int, height: Int) -> #(Window, Renderer)

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
pub fn do_fill_rect(renderer: Renderer, x: Int, y: Int, w: Int, h: Int) -> Nil

@external(erlang, "sdl_ffi", "create_texture_from_file")
pub fn do_create_texture_from_file(
  renderer: Renderer,
  filename: charlist.Charlist,
) -> Texture

@external(erlang, "sdl_ffi", "draw_texture")
pub fn do_draw_texture(
  renderer: Renderer,
  texture: Texture,
  x: Int,
  y: Int,
  w: Int,
  h: Int,
) -> Nil

/// Present the rendered content to the screen
@external(erlang, "sdl_ffi", "present")
pub fn do_present(renderer: Renderer) -> Nil

/// Poll for events (returns False if no events)
@external(erlang, "sdl_ffi", "poll_event")
pub fn do_poll_event() -> Event

@external(erlang, "sdl_ffi", "flush_events")
pub fn do_flush_events() -> Nil

/// Cleanup renderer and window, quit SDL
@external(erlang, "sdl_ffi", "terminate")
pub fn do_terminate() -> Nil
