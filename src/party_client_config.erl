-module(party_client_config).

-export([create/1]).
-export([get_party_service/1]).
-export([get_party_url/1]).
-export([get_cache_options/1]).
-export([get_cache_mode/1]).
-export([get_aggressive_caching_timeout/1]).
-export([get_woody_event_handler/1]).
-export([get_woody_options/1]).
-export([get_workers_name/1]).

-opaque client() :: options().
-type options() :: #{
    party_root => binary(),
    party_service => woody_service(),
    cache_options => cache_options(),
    cache_mode => cache_mode(),
    aggressive_caching_timeout => timeout(),
    workers_name => workers_name(),
    woody_event_handler => atom()
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
-type cache_options() :: woody_caching_client:cache_options().
-type workers_name() :: atom().
-type woody_service() :: woody:service().
-type woody_options() :: woody_caching_client:options().

%% API

-spec create(options()) -> client().
create(Options) ->
    Options.

-spec get_party_url(client()) -> binary().
get_party_url(#{party_root := Root}) ->
    Root;
get_party_url(_Client) ->
    get_default([services, party_management]).

-spec get_party_service(client()) -> woody:service().
get_party_service(#{party_service := Service}) ->
    Service;
get_party_service(_Client) ->
    get_default([woody, party_service], {dmsl_payment_processing_thrift, 'PartyManagement'}).

-spec get_cache_options(client()) -> cache_options().
get_cache_options(#{cache_options := CacheOptions}) ->
    CacheOptions;
get_cache_options(_Client) ->
    DefaultOptions = #{local_name => ?DEFAULT_CACHE_NAME},
    Options = get_default([woody, cache_options], #{}),
    maps:merge(DefaultOptions, Options).

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

-spec get_workers_name(client()) -> workers_name().
get_workers_name(#{workers_name := WorkersName}) ->
    WorkersName;
get_workers_name(_Client) ->
    get_default([woody, workers_name], ?DEFAULT_WORKERS_NAME).

-spec get_woody_event_handler(client()) -> atom().
get_woody_event_handler(#{woody_event_handler := WorkersName}) ->
    WorkersName;
get_woody_event_handler(_Client) ->
    get_default([woody, event_handler], woody_event_handler_default).

-spec get_woody_options(client()) -> woody_options().
get_woody_options(Client) ->
    #{
        cache        => get_cache_options(Client),
        workers_name => get_workers_name(Client),
        woody_client => #{
            url           => get_party_url(Client),
            event_handler => get_woody_event_handler(Client)
        }
    }.

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
