import sdl

pub type TileMap {
  TileMap(from: sdl.Texture, tile_width: Int, tile_height: Int)
}

pub type TileMapItem {
  Sprite(from: TileMap, x: Int, y: Int)
  AnimatedSprite(
    from: TileMap,
    x_start: Int,
    y_start: Int,
    animation_length: Int,
    direction: Direction,
  )
}

pub type Direction {
  Horizontal
  Vertical
}

pub fn draw_tilemap_item(item: TileMapItem) {
  // These need to implement sdl_renderer:copy/7
  // https://github.com/ninenines/esdl2/blob/c1eeb85e7256e3fc9ee95a4093ae85bd8ca62710/src/sdl_renderer.erl#L89
  case item {
    AnimatedSprite(from:, x_start:, y_start:, animation_length:, direction:) ->
      todo
    Sprite(from:, x:, y:) -> todo
  }
}
