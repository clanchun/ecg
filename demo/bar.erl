-module(bar).

-export([run/0,
         rest/0
        ]).

run() ->
    spawn(fun play_music/0),
    {10, miles}.

rest() ->
    {have, a, stretch}.

play_music() ->
    {run, like, hell}.