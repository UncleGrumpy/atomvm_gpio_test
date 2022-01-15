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

-export([start/2, state/2]).


start(GPIO, Pin) ->
	gpio:set_direction(GPIO, Pin, input),
	Button_State = spawn(fun() -> state(Pin, high) end),
	io:format("... starting button interupt.~n"),
	spawn(fun() -> watch(GPIO, Pin, Button_State) end),
{ok, Button_State}.

watch(GPIO, Pin, Button_State) ->
	io:format("button:watch/3 setting gpio [~p] interupt on pin ~p for state 'low'...~n", [GPIO, Pin]),
	gpio:set_int(GPIO, Pin, rising),
	receive
		{gpio_interrupt, Pin} ->
			io:format("button:watch/3 got signal: {gpio_interrupt, ~p}~n", [Pin]),
			io:format("Updating button:state/2 :: {low, ~p}~n", [Pin]),
			Button_State ! {low, Pin},
			watch(GPIO, Pin, Button_State);
		Any ->
			io:format("button:watch/3 got signal: ~p~n", [Any]),
			watch(GPIO, Pin, Button_State)
	end.

state(Pin, State) ->	
	%%io:format("Button on pin ~p state is: ~p.~n", [Pin, State]),
	receive
		{get, Sender} ->
			io:format("button:state/2 received 'get' request for pin ~p.  Sending: ~p~n", [Pin, State]),
			Sender ! {buttonstate, State},
			state(Pin, State);
		{low, Pin} ->
			io:format("button:state/2 set pin ~p low.~n", [Pin]),
			state(Pin, low);
		{high, Pin} ->
			io:format("button:state/2 set pin ~p high.~n", [Pin]),
			state(Pin, high)
		after 5000 ->
			state(Pin, State)
	end.
