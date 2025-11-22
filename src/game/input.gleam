pub type Input(kind) {
  Up
  Down
  Left
  Right
  Interact
  None
}

pub type PlayerInput

pub type WorldInput

pub fn scancode_to_player_input(scancode: Int) -> Input(PlayerInput) {
  case scancode {
    s if s == 79 -> Right
    s if s == 80 -> Left
    s if s == 81 -> Down
    s if s == 82 -> Up
    _ -> None
  }
}

pub fn scancode_to_world_input(scancode: Int) -> Input(WorldInput) {
  None
}
