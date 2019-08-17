%%%-------------------------------------------------------------------
%% @doc ssl_service_erl public API
%% @end
%%%-------------------------------------------------------------------

-module(ssl_service_erl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_',
        [
            {"/sslService/getHistory", ssl_service_erl_handler, []},
            {"/sslService/searchSite", ssl_service_erl_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(ssl_service_erl_http_listener,
                                 [{port, 8002}, {num_acceptors, 10}, {max_connections, 10}],
                                 #{env => #{dispatch => Dispatch}}
    ),
    ssl_service_erl_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================