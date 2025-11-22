import game/config
import game/input
import game/tilemap.{type TileMapItem}
import gleam/dict.{type Dict}
import gleam/list
import sdl

pub type Entity {
  Collider(sprite: MapSprite)
  NonCollider(sprite: MapSprite)
}

pub type MapSprite {
  TileMap(TileMapItem)
  Texture(sdl.Texture)
}

pub type Map {
  Map(
    tiles_x: Int,
    tiles_y: Int,
    background: sdl.Texture,
    entities: Dict(#(Int, Int), Entity),
  )
}

pub fn draw(renderer: sdl.Renderer, map: Map) -> Nil {
  let x_pixels = config.map_tile_pixel_size * map.tiles_x
  let y_pixels = config.map_tile_pixel_size * map.tiles_y
  sdl.do_draw_texture(renderer, map.background, 0, 0, x_pixels, y_pixels)
  dict.each(map.entities, fn(position, entity) {
    let #(x, y) = position
    case entity.sprite {
      Texture(entity_texture) ->
        sdl.do_draw_texture(
          renderer,
          entity_texture,
          x * config.map_tile_pixel_size,
          y * config.map_tile_pixel_size,
          config.map_tile_pixel_size,
          config.map_tile_pixel_size,
        )
      TileMap(_) -> todo
    }
  })
}

pub fn is_tile_walkable(position: #(Int, Int), map: Map) -> Bool {
  case dict.get(map.entities, position) {
    Ok(Collider(_)) -> False
    _ -> True
  }
}

pub fn pixel_position_to_tile_position(
  pixel_position: #(Int, Int),
) -> #(Int, Int) {
  let #(px, py) = pixel_position
  let tx = px / config.map_tile_pixel_size
  let ty = py / config.map_tile_pixel_size
  #(tx, ty)
}
