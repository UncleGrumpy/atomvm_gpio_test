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


start(GPIO, Pin) ->
    gpio:set_direction(GPIO, Pin, input),
    io:format("... starting button:state/3 state and interupt manager on GPIO ~p Pin ~p.~n", [GPIO, Pin]),
    ButtonState = spawn(fun() -> state(GPIO, Pin, high) end),
{ok, ButtonState}.

state(GPIO, Pin, State) ->    
    io:format("button:state/3 setting gpio [~p] interupt on pin ~p for state 'low'...~n", [GPIO, Pin]),
    gpio:set_int(GPIO, Pin, rising),
    receive
        {get, Sender} ->
            io:format("button:state/3 received 'get' request for pin ~p.  Sending: ~p~n", [Pin, State]),
            Sender ! {buttonstate, State, self()},
            state(GPIO, Pin, State);
        {low, Pin} ->
            io:format("button:state/3 set pin ~p low.~n", [Pin]),
            state(GPIO, Pin, low);
        {high, Pin} ->
            io:format("button:state/3 set pin ~p high.~n", [Pin]),
            state(GPIO, Pin, high);
        {gpio_interrupt, Pin} ->
            io:format("button:state/3 got signal: {gpio_interrupt, ~p}~n", [Pin]),
            io:format("Updating button:state/3 :: {low, ~p}~n", [Pin]),
            state(GPIO, Pin, low);
        Any ->
            io:format("button:state/3 got UNKNOWN signal: ~p", [Any]),
            state(GPIO, Pin, State)
    end.
