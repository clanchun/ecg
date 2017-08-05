-module(foo).

-export([start/0]).

start() ->
    walk(),
    bar:run(),
    bar:rest(),
    {good, day}.

walk() ->
    spawn(fun sing/0),
    spawn(fun answer_call/0).

sing() ->
    {the, show, must, go, on}.

answer_call() ->
    spawn(fun sit_down/0),
    {"hello Bob", "ok, bye"}.

sit_down() ->
    {on, a, bench}.
