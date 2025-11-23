import game/config
import sdl

pub type TileMap {
  TileMap(from: sdl.Texture, tile_width: Int, tile_height: Int)
}

pub type TileMapItem {
  Sprite(from: TileMap, x: Int, y: Int)
  // AnimatedSprite(
  //   from: TileMap,
  //   x_start: Int,
  //   y_start: Int,
  //   animation_length: Int,
  //   direction: Direction,
  // )
}

pub type Direction {
  Horizontal
  Vertical
}

pub fn draw_tilemap_item(
  renderer: sdl.Renderer,
  item: TileMapItem,
  x: Int,
  y: Int,
  w: Int,
  h: Int,
) -> Nil {
  case item {
    // AnimatedSprite(from:, x_start:, y_start:, animation_length:, direction:) ->
    //   todo
    Sprite(from, xpos, ypos) ->
      draw_sprite(
        renderer,
        from,
        xpos,
        ypos,
        x,
        y,
        w * config.entity_pixel_size_scaling,
        h * config.entity_pixel_size_scaling,
      )
  }
}

pub fn set_tilemap_item_index(for: TileMapItem, to_x: Int) -> TileMapItem {
  Sprite(for.from, to_x, for.y)
}

fn draw_sprite(
  renderer: sdl.Renderer,
  tilemap: TileMap,
  xpos: Int,
  ypos: Int,
  x: Int,
  y: Int,
  w: Int,
  h: Int,
) -> Nil {
  let texture = tilemap.from
  let tx = tilemap.tile_width * xpos
  let ty = tilemap.tile_height * ypos
  sdl.do_draw_sub_texture(
    renderer,
    texture,
    tx,
    ty,
    tilemap.tile_width,
    tilemap.tile_height,
    x,
    y,
    w,
    h,
  )
}
