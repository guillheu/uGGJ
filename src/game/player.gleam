import game/config
import game/input
import game/tilemap
import gleam/result

import sdl

pub type PlayerCharacter {
  FromTileMap(
    tilemap_item: tilemap.TileMapItem,
    x: Int,
    y: Int,
    w: Int,
    h: Int,
    draw_shift_x: Int,
    draw_shift_y: Int,
  )
  FromTexture(texture: sdl.Texture, x: Int, y: Int, w: Int, h: Int)
  // Remove this eventually
}

pub fn draw(renderer: sdl.Renderer, player: PlayerCharacter) -> Nil {
  case player {
    FromTexture(texture:, x:, y:, w:, h:) ->
      sdl.do_draw_texture(renderer, texture, x, y, w, h)
    FromTileMap(tilemap_item:, x:, y:, w:, h:, draw_shift_x:, draw_shift_y:) ->
      tilemap.draw_tilemap_item(
        renderer,
        tilemap_item,
        x + draw_shift_x,
        y + draw_shift_y,
        w,
        h,
      )
  }
}

pub fn compute_new_position(
  input: input.Input(input.PlayerInput),
  player: PlayerCharacter,
) -> #(Int, Int) {
  case input {
    input.Down -> #(player.x, player.y + config.map_tile_pixel_size)
    input.Left -> #(player.x - config.map_tile_pixel_size, player.y)
    input.Right -> #(player.x + config.map_tile_pixel_size, player.y)
    input.Up -> #(player.x, player.y - config.map_tile_pixel_size)
    _ -> #(player.x, player.y)
  }
}

pub fn turn_player(
  player: PlayerCharacter,
  new_direction: input.Input(input.PlayerInput),
) -> PlayerCharacter {
  // We assume the tileset will always be [right, up, left, down]
  {
    let tileset_index_result = case new_direction {
      input.Right -> Ok(0)
      input.Up -> Ok(1)
      input.Left -> Ok(2)
      input.Down -> Ok(3)
      _ -> Error(Nil)
    }
    use tileset_index <- result.map(tileset_index_result)

    case player {
      FromTexture(_, _, _, _, _) -> player
      FromTileMap(tilemap_item, _x, _y, _w, _h, _draw_shift_x, _draw_shift_y) -> {
        let new_tilemap_item =
          tilemap.set_tilemap_item_index(tilemap_item, tileset_index)
        FromTileMap(..player, tilemap_item: new_tilemap_item)
      }
    }
  }
  |> result.unwrap(player)
}

pub fn move_to(
  new_position: #(Int, Int),
  player: PlayerCharacter,
) -> PlayerCharacter {
  let #(new_x, new_y) = new_position
  case player {
    FromTexture(_, _, _, _, _) -> FromTexture(..player, x: new_x, y: new_y)
    FromTileMap(_, _, _, _, _, _, _) ->
      FromTileMap(..player, x: new_x, y: new_y)
  }
}
