import game/config
import game/input
import game/map
import game/player
import gleam/dict
import gleam/erlang/charlist
import sdl

type State {
  State(map: map.Map, player: player.PlayerCharacter)
}

pub fn main() {
  let #(_window, renderer) =
    sdl.do_init(config.window_title, config.window_width, config.window_height)
  sdl.do_clear(renderer)
  let map = create_map(renderer)
  let player = create_player(renderer)

  let state = State(map, player)

  // map.draw(renderer, map)

  sdl.do_poll_event()
  loop(state, renderer)
}

fn loop(state: State, renderer: sdl.Renderer) {
  let new_state = handle_events(state)
  sdl.do_clear(renderer)
  sdl.do_set_draw_color(renderer, 255, 255, 255, 255)
  map.draw(renderer, state.map)
  player.draw(renderer, state.player)
  sdl.do_present(renderer)
  loop(new_state, renderer)
}

fn handle_events(state: State) -> State {
  case sdl.do_poll_event() {
    sdl.Quit(_timestamp) -> {
      sdl.do_terminate()
      state
    }
    sdl.KeyDown(_, sdl.Pressed, _, _, scancode, _, _) -> {
      let new_player_position =
        input.scancode_to_player_input(scancode)
        |> player.compute_new_position(state.player)
      case
        new_player_position
        |> map.pixel_position_to_tile_position
        |> map.is_tile_walkable(state.map)
      {
        True ->
          player.move_to(new_player_position, state.player)
          |> update_player(state)
        False -> state
      }
    }
    _ -> state
  }
}

fn create_map(renderer: sdl.Renderer) -> map.Map {
  let background_texture =
    sdl.do_create_texture_from_file(
      renderer,
      config.background_texture_filename |> charlist.from_string,
    )
  let obstacle_sprite =
    map.Texture(sdl.do_create_texture_from_file(
      renderer,
      config.map_obstacle_texture_filename |> charlist.from_string,
    ))
  let obstacles =
    [
      #(#(10, 10), map.Collider(obstacle_sprite)),
      #(#(5, 5), map.Collider(obstacle_sprite)),
    ]
    |> dict.from_list
  map.Map(
    config.map_tiles_width,
    config.map_tiles_height,
    background_texture,
    obstacles,
  )
}

fn create_player(renderer: sdl.Renderer) -> player.PlayerCharacter {
  let player_texture =
    sdl.do_create_texture_from_file(
      renderer,
      config.player_texture_filename |> charlist.from_string,
    )
  player.FromTexture(
    player_texture,
    0,
    0,
    config.map_tile_pixel_size,
    config.map_tile_pixel_size,
  )
}

fn update_player(player: player.PlayerCharacter, state: State) -> State {
  State(..state, player:)
}
