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
-module(gpio_test).

-export([start/0]).

start() ->
	LED = 2,
	BTN = 0,
	io:format("Initalizing GPIO bus... "),
	GPIO = gpio:open(),
	io:format("success!  bus:~p~n", [GPIO]),
	io:format("... calling led:start/2 on gpio: ~p, pin: ~p.~n", [GPIO, LED]),
	{ok, LedCtl} = led:start(GPIO, LED),
	io:format("... calling button:start/2 on gpio: ~p, pin: ~p.~n", [GPIO, BTN]),
	{ok, Button_State} = button:start(GPIO, BTN),
	io:format("... starting button_loop/3.~n"),
	button_loop(LedCtl, BTN, Button_State),
    ok.

button_loop(LedCtl, Pin, Button_State) ->
	FreeHeap = erlang:system_info(esp32_free_heap_size),
	io:format("Free Heap: ~p~n", [FreeHeap]),
	io:format("Process Count: ~p~n", [erlang:system_info(process_count)]),
	Pids = erlang:processes(),
	[io:format("Pid ~p: ~p ~p~n", [Pid, erlang:process_info(Pid, message_queue_len), erlang:process_info(Pid, memory)]) || Pid <- Pids],
	Button_State ! {get, self()},
	receive
		{buttonstate, State} ->
			case State of
				high ->
					dont_care,
					ok = timer:sleep(5000),
					button_loop(LedCtl, Pin, Button_State);
				low ->
					Button_State ! {high, Pin},
					LedCtl ! {blink, 6},
					ok = timer:sleep(6000),
					button_loop(LedCtl, Pin, Button_State);
				Any ->
					io:format("button:state/2 returned INVALID state for button on pin ~p:~n~p.~n", [Pin, Any]),
					button_loop(LedCtl, Pin, Button_State)
			end;
		Any ->
			io:format("button_loop/3 recieved INVALID signal: ~p.~n", [Any]),
			button_loop(LedCtl, Pin, Button_State)
		after 5000 ->
			button_loop(LedCtl, Pin, Button_State)
	end.
