import game/map
import gleam/erlang/charlist
import gleam/int
import gleam/result
import gleam/time/duration
import gleam/time/timestamp
import sdl

const window_title = "Hello Lucy!"

const window_width = 640

const window_height = 480

const map_tile_pixel_size = 32

const map_tiles_width = 20

const map_tiles_height = 15

const background_texture_filename = "assets/gleam/lucy.png"

type State {
  State(map: map.Map)
}

pub fn main() {
  let #(_window, renderer) =
    sdl.do_init(window_title, window_width, window_height)
  sdl.do_clear(renderer)

  let background_texture =
    sdl.do_create_texture_from_file(
      renderer,
      background_texture_filename |> charlist.from_string,
    )
  let map =
    map.Map(
      map_tile_pixel_size,
      map_tiles_width,
      map_tiles_height,
      background_texture,
      [],
    )

  let state = State(map)

  // map.draw(renderer, map)

  sdl.do_poll_event()
  loop(state, renderer)
}

fn loop(state: State, renderer: sdl.Renderer) {
  let new_state = handle_events(state)
  sdl.do_clear(renderer)
  sdl.do_set_draw_color(renderer, 255, 255, 255, 255)
  map.draw(renderer, state.map)
  sdl.do_present(renderer)
  loop(new_state, renderer)
}

fn handle_events(state: State) -> State {
  case sdl.do_poll_event() {
    sdl.Quit(_timestamp) -> {
      sdl.do_terminate()
      state
    }
    _ -> state
  }
}
