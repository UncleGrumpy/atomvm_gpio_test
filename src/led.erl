%%
%% Copyright (c) 2022 Winford grumpyuncle@protonmail.com
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(led).

-export([start/2]).

-define(TRACE_ENABLED, true).
-include_lib("include/trace.hrl").

start(GPIO, Pin) ->
    gpio:set_direction(GPIO, Pin, output),
    ?TRACE("Pid ~p set led pin ~p on gpio bus ~p to 'output'.", [self(), Pin, GPIO]),
    ok = on(Pin),
    LEDpid = spawn(fun() -> control(Pin) end),
    timer:sleep(250),
    ok = off(Pin),
{ok, LEDpid}.

control(Pin) ->
    ?TRACE("Pid ~p controlling pin ~p.~n", [self(), Pin]),
    receive
        {blink, Count} ->
            ?TRACE("Pid ~p received request for ~p blinks in pin ~p.~n", [self(), Count, Pin]),
            ok = flash(Pin, Count),
            control(Pin);
        {on, Pin} ->
            ?TRACE("Pid ~p received 'on' request for pin ~p.~n", [self(), Pin]),
            ok = on(Pin),
            control(Pin);
        {off, Pin} ->
            ?TRACE("Pid ~p received 'off' request for pin ~p.~n", [self(), Pin]),
            ok = off(Pin),
            control(Pin);
        Error ->
            ?TRACE("Pid ~p received unknown request...~n~p~n", [self(), Error]),
            control(Pin)
    end.

%%flash(_Pin, -1) ->    %% This can be used later to add a blink process, but there should be state tracking first...
%%    ok;
flash(_Pin, 0) ->
    ok;
flash(Pin, I) ->
    ?TRACE("Pid ~p got request to flash led on pin ~p [ x~p ].", [self(), Pin, I]),
    gpio:digital_write(Pin, high),
    timer:sleep(500),
    gpio:digital_write(Pin, low),
    timer:sleep(500),
    flash(Pin, I-1).

on(Pin) ->
    gpio:digital_write(Pin, high),
    ?TRACE("Pid ~p turned pin ~p on.~n", [self(), Pin]),
ok.

off(Pin) ->
    gpio:digital_write(Pin, low),
    ?TRACE("Pid ~p turned pin ~p off.~n", [self(), Pin]),
ok.
