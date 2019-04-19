-module(party_client_config).

-export([create/1]).
-export([get_party_service/1]).
-export([get_cache_mode/1]).
-export([get_aggressive_caching_timeout/1]).
-export([get_woody_transport_opts/1]).
-export([get_woody_options/1]).

-opaque client() :: options().
-type options() :: #{
    party_service => woody_service(),
    aggressive_caching_timeout => timeout(),
    woody_options => map()
}.
-type cache_mode() :: disabled | safe | aggressive.

-export_type([client/0]).
-export_type([options/0]).
-export_type([cache_mode/0]).

-define(APPLICATION, party_client).
-define(DEFAULT_CACHE_NAME, party_client_default_cache).
-define(DEFAULT_WORKERS_NAME, party_client_default_workers).
-define(DEFAULT_AGGERSSIVE_CACHING_TIMEOUT, 30000).
-define(DEFAULT_CACHE_MODE, safe).

%% Internal types

-type config_path() :: atom() | [atom() | [any()]].
-type woody_service() :: woody:service().
-type woody_options() :: woody_caching_client:options().
-type woody_transport_opts() :: woody_client_thrift_http_transport:transport_options().

%% API

-spec create(options()) -> client().
create(Options) ->
    Options.

-spec get_party_service(client()) -> woody:service().
get_party_service(#{party_service := Service}) ->
    Service;
get_party_service(_Client) ->
    get_default([woody, party_service], {dmsl_payment_processing_thrift, 'PartyManagement'}).

-spec get_cache_mode(client()) -> cache_mode().
get_cache_mode(#{cache_mode := CacheMode}) ->
    CacheMode;
get_cache_mode(_Client) ->
    get_default([woody, cache_mode], ?DEFAULT_CACHE_MODE).

-spec get_aggressive_caching_timeout(client()) -> timeout().
get_aggressive_caching_timeout(#{aggressive_caching_timeout := Timeout}) ->
    Timeout;
get_aggressive_caching_timeout(_Client) ->
    get_default([woody, aggressive_caching_time], ?DEFAULT_AGGERSSIVE_CACHING_TIMEOUT).

-spec get_woody_transport_opts(client()) -> woody_transport_opts().
get_woody_transport_opts(#{woody_transport_opts := Opts}) ->
    Opts;
get_woody_transport_opts(_Client) ->
    get_default([woody, transport_opts], #{}).

-spec get_woody_options(client()) -> woody_options().
get_woody_options(Client) ->
    DefaultOptions = #{
        cache        => #{local_name => ?DEFAULT_CACHE_NAME},
        workers_name => ?DEFAULT_WORKERS_NAME,
        woody_client => #{
            url            => get_default([services, party_management]),
            event_handler  => woody_event_handler_default,
            transport_opts => #{}
        }
    },
    EnvOptions = merge_nested_maps(DefaultOptions, get_default([woody, options], #{})),
    merge_nested_maps(EnvOptions, maps:get(woody_options, Client, #{})).

%% Internal functions

-spec get_default(config_path()) -> any().
get_default(Path) ->
    Ref = erlang:make_ref(),
    case get_default(Path, Ref) of
        Ref ->
            erlang:error({badpath, Path});
        Other ->
            Other
    end.

-spec get_default(config_path(), any()) -> any().
get_default(Key, Default) when is_atom(Key) ->
    genlib_app:env(?APPLICATION, Key, Default);
get_default([Key | Rest] = Path, Default) when is_list(Path) ->
    ConfigItem = get_default(Key, #{}),
    get_nested_map(Rest, ConfigItem, Default).

-spec get_nested_map(Path :: [any()], map(), any()) -> any().
get_nested_map([Key], Map, Default) ->
    maps:get(Key, Map, Default);
get_nested_map([Key | Path], Map, Default) ->
    NextMap = maps:get(Key, Map, Default),
    get_nested_map(Path, NextMap, Default).

merge_nested_maps(Map1, Map2) when map_size(Map2) =:= 0 ->
    Map1;
merge_nested_maps(Map1, Map2) ->
    maps:fold(fun merge_map_item/3, Map1, Map2).

merge_map_item(K, V, Acc) when is_map(V) ->
    NewV = case maps:is_key(K, Acc) of
        true ->
            merge_nested_maps(maps:get(K, Acc), V);
        false ->
            V
    end,
    Acc#{K => NewV};
merge_map_item(K, V, Acc) ->
    Acc#{K => V}.
