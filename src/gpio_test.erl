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
    {ok, ButtonState} = button:start(GPIO, BTN),
    io:format("... starting button_loop/3.~n"),
    button_loop(LedCtl, BTN, ButtonState),
    ok.

button_loop(LedCtl, Pin, ButtonState) ->
    FreeHeap = erlang:system_info(esp32_free_heap_size),
    LargestBlock = erlang:system_info(esp32_largest_free_block),
    LowestFree = erlang:system_info(esp32_minimum_free_size),
    io:format("Free Heap: ~p.~nLargest free block size: ~p~nLowest free block size since boot: ~p~n", [FreeHeap, LargestBlock, LowestFree]),
    io:format("Process Count: ~p~n", [erlang:system_info(process_count)]),
    Pids = erlang:processes(),
    [io:format("Pid ~p: ~p ~p~n", [Pid, erlang:process_info(Pid, message_queue_len), erlang:process_info(Pid, memory)]) || Pid <- Pids],
    ButtonState ! {get, self()},
    receive
        {buttonstate, State, ButtonState} ->
            case State of
                high ->
                    dont_care,
                    ok = timer:sleep(5000),
                    button_loop(LedCtl, Pin, ButtonState);
                low ->
                    ButtonState ! {high, Pin},
                    LedCtl ! {blink, 6},
                    ok = timer:sleep(6000),
                    button_loop(LedCtl, Pin, ButtonState);
                Any ->
                    io:format("button:state/3 sent INVALID state for button on pin ~p:~n~p.~n", [Pin, Any]),
                    button_loop(LedCtl, Pin, ButtonState)
            end;
        Any ->
            io:format("gpio_test:button_loop/3 recieved INVALID signal: ~p.~n", [Any]),
            button_loop(LedCtl, Pin, ButtonState)
        after 5000 ->
            button_loop(LedCtl, Pin, ButtonState)
    end.
