import gleam/int
import gleam/result
import sdl

const window_title = "Hello Lucy!"

const window_width = 500

const window_height = 500

// const forced_delay_ms = 16

type State =
  #(Int, Int, Int)

pub fn main() {
  let #(_window, renderer) =
    sdl.do_init(window_title, window_width, window_height)
  sdl.do_clear(renderer)

  let state = #(30, 30, 30)
  sdl.do_poll_event()
  loop(state, renderer)
}

fn loop(state: State, renderer: sdl.Renderer) {
  let new_state = handle_events(state) |> echo
  let #(r, g, b) = new_state
  sdl.do_clear(renderer)
  sdl.do_set_draw_color(renderer, r, g, b, 255)
  sdl.do_present(renderer)
  // process.sleep(forced_delay_ms)
  loop(new_state, renderer)
}

fn handle_events(state: State) -> State {
  case sdl.do_poll_event() {
    sdl.Quit(_timestamp) -> {
      sdl.do_terminate()
      state
    }
    sdl.MouseMotion(_, _, _, y_pos, _, x_pos, _, _) -> {
      let x_dist =
        x_pos
        |> int.absolute_value
        |> int.modulo(256)
        |> result.lazy_unwrap(fn() { panic })
      let y_dist =
        y_pos
        |> int.absolute_value
        |> int.modulo(256)
        |> result.lazy_unwrap(fn() { panic })

      sdl.do_flush_events()
      #(x_dist, y_dist, 30)
    }
    _ -> state
  }
}
