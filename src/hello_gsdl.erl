%% This is an example. Feel free to copy and reuse as you wish.

-module(hello_gsdl).
-export([run/0, init/0]).


% -on_load(setup_paths/0).


% setup_paths() ->
%     code:add_pathz("/git/uGGJ/esdl2/_build/default/lib/esdl2/ebin"),
%     ok.


run() ->
	spawn(fun init/0).

init() ->
    % code:add_pathz("../esdl2/_build/default/lib/esdl2/ebin"),
	% application:load(sdl),
	% application:set_env(sdl, priv_dir, "esdl2/_build/default/lib/esdl2/priv"),
	ok = sdl:start([video]),
	ok = sdl:stop_on_exit(),
	{ok, Window} = sdl_window:create(<<"Hello SDL">>, 10, 10, 500, 500, []),
	{ok, Renderer} = sdl_renderer:create(Window, -1, [accelerated, present_vsync]),
	ok = sdl_renderer:set_draw_color(Renderer, 255, 255, 255, 255),
	{ok, Texture} = sdl_texture:create_from_file(Renderer, "src/lucy.png"),
	loop(#{window=>Window, renderer=>Renderer, texture=>Texture}).

loop(State) ->
	events_loop(),
	render(State),
	loop(State).

events_loop() ->
	case sdl_events:poll() of
		false -> ok;
		#{type:=quit} -> terminate();
		_ -> events_loop()
	end.

render(#{renderer:=Renderer, texture:=Texture}) ->
	ok = sdl_renderer:clear(Renderer),
	ok = sdl_renderer:copy(Renderer, Texture, undefined, #{x=>100, y=>100, w=>300, h=>300}),
	ok = sdl_renderer:present(Renderer).

terminate() ->
	init:stop(),
	exit(normal).
