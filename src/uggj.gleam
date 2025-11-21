import sdl

pub fn main() {
  do_run()
}

@external(erlang, "hello_gsdl", "init")
fn do_run() -> Nil
