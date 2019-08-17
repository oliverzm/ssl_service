-module(ssl_service_erl).

-export([get_env/1, get_env/2]).

%%------------------------------------------------------------------------------
%% @doc get an environment variable's value (or undefined if it doesn't exist)
%%------------------------------------------------------------------------------
-spec get_env(atom()) -> term() | 'undefined'.
get_env(Key) ->
    get_env(Key, 'undefined').

%%------------------------------------------------------------------------------
%% @doc get an environment variable's value (or Default if it doesn't exist)
%%------------------------------------------------------------------------------
-spec get_env(atom(), term()) -> term().
get_env(Key, Default) ->
    case application:get_env(ssl_service_erl, Key) of
        {ok, X} -> X;
        'undefined' -> Default
    end.