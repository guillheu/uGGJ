-module(sdl_ffi).
-export([init/0, create_window/5, create_renderer/1, 
         set_draw_color/5, clear/1, fill_rect/2, present/1,
         poll_event/0, delay/1, cleanup/2]).

init() ->
    code:add_pathz("esdl2/_build/default/lib/esdl2/ebin").
    % esdl2:init([video]).

create_window(Title, X, Y, Width, Height) ->
    {ok, Window} = sdl_window:create(Title, X, Y, Width, Height, []),
    Window.

create_renderer(Window) ->
    {ok, Renderer} = sdl_renderer:create(Window, -1, [accelerated]),
    Renderer.

set_draw_color(Renderer, R, G, B, A) ->
    sdl_renderer:set_draw_color(Renderer, R, G, B, A).

clear(Renderer) ->
    sdl_renderer:clear(Renderer).

fill_rect(Renderer, Rect) ->
    sdl_renderer:fill_rect(Renderer, Rect).

present(Renderer) ->
    sdl_renderer:present(Renderer).

poll_event() ->
    sdl_events:poll().

delay(Ms) ->
    esdl2:delay(Ms).

cleanup(Renderer, Window) ->
    sdl_renderer:destroy(Renderer),
    sdl_window:destroy(Window),
    esdl2:quit().