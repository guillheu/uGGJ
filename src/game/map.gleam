import game/tilemap.{type TileMapItem}
import sdl

pub type Entity {
  Collider(x_pos: Int, y_pos: Int, sprite: TileMapItem)
  NonCollider(x_pos: Int, y_pos: Int, sprite: TileMapItem)
}

pub type Map {
  Map(
    tile_pixel_size: Int,
    tiles_x: Int,
    tiles_y: Int,
    background: sdl.Texture,
    entities: List(Entity),
  )
}

pub fn draw(renderer: sdl.Renderer, map: Map) -> Nil {
  let x_pixels = map.tile_pixel_size * map.tiles_x
  let y_pixels = map.tile_pixel_size * map.tiles_y
  sdl.do_draw_texture(renderer, map.background, 0, 0, x_pixels, y_pixels)
}
