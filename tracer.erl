%%%-------------------------------------------------------------------
%%% @author clanchun <clanchun@gmail.com>
%%% @copyright (C) 2016, clanchun
%%% @doc
%%%
%%% @end
%%% Created :  18 Oct 2016 by clanchun <clanchun@gmail.com>
%%%-------------------------------------------------------------------
-module(tracer).

%% API
-export([trace/2,
         trace/3,
         analyze/2,
         stop/0
        ]).

-export([parse/2]).

%%%===================================================================
%%% API
%%%===================================================================

trace(File, Modules) ->
    trace(File, Modules, global).

trace(File, Modules, Mode) ->
    {ok, Tracer} = dbg:tracer(port, dbg:trace_port(file, File)),
    dbg:p(all, [c, p]),
    case Mode of
        global ->
            [dbg:tp(Mod, cx) || Mod <- Modules];
        local ->
            [dbg:tpl(Mod, cx) || Mod <- Modules]
    end,
    {ok, Tracer}.

analyze(InFile, OutFile) ->
    dbg:flush_trace_port(),
    {ok, Fd} = file:open(OutFile, [write]),
    dbg:trace_client(file, InFile, {fun parse/2, Fd}),
    ok.

stop() ->
    dbg:stop().

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse(end_of_trace, Out) ->
    Out;
parse(Trace, Out) when element(1, Trace) == trace, tuple_size(Trace) >= 3 ->
    parse2(Trace, tuple_size(Trace), Out);
parse(_Trace, Out) ->
    Out.

parse2(Trace, Size, Out) ->
    From = element(2, Trace),
    case element(3, Trace) of
        call ->
            case element(4, Trace) of
		MFA when Size == 5 ->
		    Message = element(5, Trace),
                    io:format(Out, "~p;call;~s;~s~n",
                              [From, mfa(MFA), message(Message)]);
		MFA ->
                    io:format(Out, "~p;call;~s;~s~n",
                              [From, mfa(MFA), message(nil)])
	    end;
        return_from ->
	    MFA = element(4, Trace),
            io:format(Out, "~p;returned from;~s;~s~n",
                      [From, mfa(MFA), message(nil)]);
        spawn when Size == 5 ->
            Pid = element(4, Trace),
	    MFA = element(5, Trace),
	    io:format(Out, "~p;spawn;~p;~s;~s~n",
                      [From, Pid, mfa(MFA), message(nil)]);
        _ ->
            ignore
    end,
    Out.

mfa({M,F,Argl}) when is_list(Argl) ->
    io_lib:format("~p;~p;~p", [M, F, length(Argl)]);
mfa({M,F,Arity}) ->
    io_lib:format("~p;~p;~p", [M,F,Arity]);
mfa(X) -> io_lib:format("~p", [X]).

message(undefined) ->
    io_lib:format("spawn", []);
message({proc_lib, init_p_do_apply, 3}) ->
    io_lib:format("proc_lib", []);
message(_) ->
    io_lib:format("ignore", []).
