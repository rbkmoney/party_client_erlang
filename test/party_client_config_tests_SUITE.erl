-module(party_client_config_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([config_merge_test/1]).

%% Internal types

-type test_entry() :: atom() | {group, atom()}.
-type config() :: [{atom(), any()}].

%% CT description

-spec all() -> [test_entry()].
all() ->
    [
        config_merge_test
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    AppConfig = [
        {party_client, [{woody, #{options => #{a => b, woody_client => #{c => d}}}}]}
    ],
    Apps = lists:flatten([genlib_app:start_application_with(A, C) || {A, C} <- AppConfig]),
    [{apps, Apps} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(C) ->
    genlib_app:stop_unload_applications(proplists:get_value(apps, C)).

%% Tests

-spec config_merge_test(config()) -> any().
config_merge_test(_C) ->
    Client = party_client:create_client(#{woody_options => #{a => c, woody_client => #{e => f}}}),
    WoodyOptions = party_client_config:get_woody_options(Client),
    #{
        a := c,
        cache := #{
            local_name := party_client_default_cache
        },
        woody_client := #{
            e := f,
            c := d,
            event_handler := woody_event_handler_default,
            transport_opts := #{},
            url := _Urls
        },
        workers_name := party_client_default_workers
    } = WoodyOptions.
