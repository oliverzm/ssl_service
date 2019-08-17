-module(ssllabs).

-define(GRADESMAP, #{<<"A+">> => 0,
                     <<"A-">> => 1,
                     <<"A">> => 2,
                     <<"B">> => 3,
                     <<"C">> => 4,
                     <<"D">> => 5,
                     <<"E">> => 6,
                     <<"F">> => 7,
                     <<"T">> => 8,
                     <<"M">> => 9,
                     <<"no_grade">> => 10}).

-export([ssllabs_search_site/1]).

ssllabs_search_site(Site) ->
    SSLLabs = ssl_service_erl:get_env(ssl_labs),
    Url = proplists:get_value(url, SSLLabs),
    Port = proplists:get_value(port, SSLLabs),
    Resource = "/api/v3/analyze?host=" ++ Site,

    {ok, ConnPid} = gun:open(Url, Port),
    StreamRef = gun:get(ConnPid, Resource),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, _Status, _Headers} ->
            maps:new();
        {response, nofin, _Status, _Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            process_site(jiffy:decode(Body, [return_maps]))
    end.

process_site(SiteInfo) ->
    Endpoints = maps:get(<<"endpoints">>, SiteInfo, []),
    {SSLGrade, Servers} = process_servers(Endpoints, <<"no_grade">>, []),
    Status = maps:get(<<"status">>, SiteInfo),
    {StatusVal, StatusMsg} = process_status_value(Status),
    #{
        <<"status">> => StatusVal,
        <<"status_message">> => StatusMsg,
        <<"is_down">> => Status =:= "ERROR",
        <<"ssl_grade">> => SSLGrade,
        <<"servers">> => Servers
    }.

-spec process_servers(ServerList :: list(), Grade :: binary(), SP :: list()) -> tuple().
process_servers([], Grade, SP) ->
    {Grade, SP};
process_servers([Server | NxtServer], Grade, SP) ->
    % taking ip address to search in whois
    AddressServer = maps:get(<<"ipAddress">>, Server, <<"">>),
    {ok, WhoisData} = ewhois:query(AddressServer),
    Country = proplists:get_value(<<"Country">>, WhoisData, <<"">>),
    Owner = proplists:get_value(<<"Organization">>, WhoisData, <<"">>),
    % calculating the general grade
    GradeServer = maps:get(<<"grade">>, Server, <<"no_grade">>),
    GradeServerValue = maps:get(GradeServer, ?GRADESMAP),
    GradeCurrentValue = maps:get(Grade, ?GRADESMAP),
    NewGrade = case GradeServerValue < GradeCurrentValue of
                true ->
                    GradeServer;
                _ ->
                    Grade
                end,
    % defyning fields needed
    NewServer = [
        #{
            <<"address">> => AddressServer,
            <<"ssl_grade">> => GradeServer,
            <<"country">> => Country,
            <<"owner">> => Owner
        }
    ],
    process_servers(NxtServer, NewGrade, SP ++ NewServer).

-spec process_status_value(Status :: binary()) -> tuple().
%%----------------------------------------------------------------------
%% @doc
%%
%% According with the status value from SSL Labs service, return a status and a message
%%
%% @param Status - status of the process of get the ssl information binary()
%%
%% @returns tuple()
%%----------------------------------------------------------------------
process_status_value(<<"Error">>) ->
    {<<"error">>, <<"Error getting the data">>};
process_status_value(<<"READY">>) ->
    {<<"ok">>, <<"The process has finished">>};
process_status_value(_) ->
    {<<"in_progress">>, <<"The request is in progress">>}.
