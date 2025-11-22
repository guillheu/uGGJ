-module(sdl_ffi).
-export([init/3, set_draw_color/5, create_texture_from_file/2, draw_texture/6, clear/1, fill_rect/5, present/1,
         poll_event/0, terminate/0, flush_events/0]).

init(Title, Width, Height) ->
	ok = sdl:start([video]),
	ok = sdl:stop_on_exit(),
	{ok, Window} = sdl_window:create(Title, 10, 10, Width, Height, []),
	{ok, Renderer} = sdl_renderer:create(Window, -1, [accelerated, present_vsync]),
    {Window, Renderer}.

set_draw_color(Renderer, R, G, B, A) ->
    sdl_renderer:set_draw_color(Renderer, R, G, B, A).

clear(Renderer) ->
    sdl_renderer:clear(Renderer).

fill_rect(Renderer, X, Y, W, H) ->
    sdl_renderer:fill_rect(Renderer, {X, Y, W, H}).

create_texture_from_file(Renderer, Filename) ->
    % NewFilename = unicode:characters_to_binary(Filename),
    % io:format("Filename: ~p~n", [Filename]),
    % io:format("NewFilename: ~p~n", [NewFilename]),
    % io:format("Works: ~p~n", ["src/lucy.png"]),
    {ok, Texture} = sdl_texture:create_from_file(Renderer, Filename),
    Texture.

draw_texture(Renderer, Texture, X, Y, W, H) ->
    sdl_renderer:copy(Renderer, Texture, undefined, #{x=>X, y=>Y, w=>W, h=>H}).

present(Renderer) ->
    sdl_renderer:present(Renderer).

poll_event() ->
    Event = sdl_events:poll(),
    % io:format("Event: ~p~n", [Event]),
    TransformedEvent = transform_event(Event),
    % io:format("Transformed: ~p~n", [TransformedEvent]),
    TransformedEvent.

flush_events() ->
    sdl_events:flush(mouse_motion).

transform_event(false) ->
    {no_event};
transform_event(#{type := Type} = Event) ->
    Values = maps:values(Event),
    list_to_tuple([Type | lists:delete(Type, Values)]).

terminate() ->
    init:stop(),
    exit(normal).