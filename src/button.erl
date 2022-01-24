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
-module(button).

-export([start/2]).

-define(TRACE_ENABLED, true).
-include_lib("include/trace.hrl").

start(GPIO, Pin) ->
    gpio:set_direction(GPIO, Pin, input),
    ?TRACE("...Pid ~p starting state and interupt manager on GPIO ~p Pin ~p.~n", [self(), GPIO, Pin]),
    ButtonState = spawn(fun() -> state(GPIO, Pin, high) end),
{ok, ButtonState}.

state(GPIO, Pin, State) ->    
    ?TRACE("Pid ~p setting gpio [~p] interupt on pin ~p for state 'low'...~n", [self(), GPIO, Pin]),
    gpio:set_int(GPIO, Pin, rising),
    receive
        {get, Sender} ->
            ?TRACE("Pid ~p received 'get' request for pin ~p.  Sending: ~p~n", [self(), Pin, State]),
            Sender ! {buttonstate, State, self()},
            state(GPIO, Pin, State);
        {low, Pin} ->
            ?TRACE("Pid ~p set pin ~p low.~n", [self(), Pin]),
            state(GPIO, Pin, low);
        {high, Pin} ->
            ?TRACE("Pid ~p set pin ~p high.~n", [self(), Pin]),
            state(GPIO, Pin, high);
        {gpio_interrupt, Pin} ->
            ?TRACE("Pid ~p got signal: {gpio_interrupt, ~p}~n", [self(), Pin]),
            ?TRACE("Updating Pid ~p :: {low, ~p}~n", [self(), Pin]),
            state(GPIO, Pin, low);
        Any ->
            ?TRACE("Pid ~p got UNKNOWN signal: ~p", [self(), Any]),
            state(GPIO, Pin, State)
    end.
