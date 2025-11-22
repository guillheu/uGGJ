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
