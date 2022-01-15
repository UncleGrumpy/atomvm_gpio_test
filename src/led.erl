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

-export([start/2, control/1]).


start(GPIO, Pin) ->
	gpio:set_direction(GPIO, Pin, output),
	io:format("led on gpio bus ~p, pin ~p set to 'output'.~n", [GPIO, Pin]),
	on(Pin),
	LEDpid = spawn(fun() -> control(Pin) end),
	timer:sleep(250),
	off(Pin),
{ok, LEDpid}.

control(Pin) ->
	io:format("led:control/1 started on pin ~p.~n", [Pin]),
	receive
		{blink, Count} ->
			io:format("led:control/1 received request for ~p blinks in pin ~p.~n", [Count, Pin]),
			ok = flash(Pin, Count),
			control(Pin);
		{on, Pin} ->
			io:format("led:control/1 received 'on' request for pin ~p.~n", [Pin]),
			ok = on(Pin),
			control(Pin);
		{off, Pin} ->
			io:format("led:control/1 received 'off' request for pin ~p.~n", [Pin]),
			ok = off(Pin),
			control(Pin);
		_ ->
			io:format("led:control/1 received unknown request.~n")
		after 5000 ->
			control(Pin)
	end.

flash(Pin, I) ->
	case I of
		-1 ->
			blink_maybe;
		0 ->
			ok;
		_ ->
			io:format("led:flash/2 got request to flash led on pin ~p [ x~p ].~n", [Pin, I]),
    		gpio:digital_write(Pin, high),
    		timer:sleep(500),
    		gpio:digital_write(Pin, low),
			timer:sleep(500),
			flash(Pin, I-1)
	end.

on(Pin) ->
    gpio:digital_write(Pin, high),
    io:format("led:on/1 pin ~p... on.~n", [Pin]),
ok.

off(Pin) ->
    gpio:digital_write(Pin, low),
    io:format("led:off/1 pin ~p... off.~n", [Pin]),
ok.
