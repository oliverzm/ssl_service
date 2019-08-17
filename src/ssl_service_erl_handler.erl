-module(ssl_service_erl_handler).

-export([init/2]).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    UrlPath = cowboy_req:path(Req),
    PathInfo = binary:split(UrlPath, <<"/">>, [trim_all, global]),

    handle_request(Method, PathInfo, Req, State).


handle_request(<<"POST">>, [<<"sslService">>, <<"searchSite">>], Req, State) ->
    case cowboy_req:has_body(Req) of
        false ->
            normalize_response(400, Req, <<"">>, State, #{});
        _ ->
            {ok, Body, _} = cowboy_req:read_urlencoded_body(Req),
            case lists:keyfind(<<"site">>, 1, Body) of
                {_, Site} when Site =/= <<"">> ->
                    Data = ssllabs:ssllabs_search_site(Site),
                    Res = jiffy:encode(Data),
                    normalize_response(200, Req, Res, State, #{<<"content-type">> => <<"application/json">>});
                _ ->
                    normalize_response(400, Req, <<"">>, State, #{})
            end
    end;

handle_request(<<"GET">>, [<<"sslService">>, <<"getHistory">>], Req0, State) ->
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello World! getHistory">>, Req0),
    {ok, Req, State};

handle_request(_, _, Req, State) ->
    normalize_response(404, Req, <<"">>, State, #{}),
    {ok, Req, State}.


normalize_response(Code, Req, Response, State, Headers) ->
    Res = cowboy_req:reply(Code, Headers, Response, Req),
    {ok, Res, State}.
