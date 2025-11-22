import game/config
import game/input
import game/tilemap

import sdl

pub type PlayerCharacter {
  FromTileMap(tilemap: tilemap.TileMapItem, x: Int, y: Int, w: Int, h: Int)
  FromTexture(texture: sdl.Texture, x: Int, y: Int, w: Int, h: Int)
  // Remove this eventually
}

pub fn draw(renderer: sdl.Renderer, player: PlayerCharacter) -> Nil {
  case player {
    FromTexture(texture:, x:, y:, w:, h:) ->
      sdl.do_draw_texture(renderer, texture, x, y, w, h)
    FromTileMap(tilemap:, x:, y:, w:, h:) -> todo
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

pub fn move_to(
  new_position: #(Int, Int),
  player: PlayerCharacter,
) -> PlayerCharacter {
  let #(new_x, new_y) = new_position
  case player {
    FromTexture(_, _, _, _, _) -> FromTexture(..player, x: new_x, y: new_y)
    FromTileMap(_, _, _, _, _) -> FromTileMap(..player, x: new_x, y: new_y)
  }
}
