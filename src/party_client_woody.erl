-module(party_client_woody).

-export([child_spec/2]).
-export([start_link/1]).

-export([call/4]).

%% Internal types

-type client() :: party_client_config:client().
-type context() :: party_client_context:context().
-type business_error() :: any().

%% API

-spec child_spec(atom(), client()) -> supervisor:child_spec().
child_spec(ChildID, Client) ->
    WoodyOptions = party_client_config:get_woody_options(Client),
    woody_caching_client:child_spec(ChildID, WoodyOptions).

-spec start_link(client()) -> genlib_gen:start_ret().
start_link(Client) ->
    WoodyOptions = party_client_config:get_woody_options(Client),
    woody_caching_client:start_link(WoodyOptions).

-spec call(atom(), [any()], client(), context()) ->
    ok | {ok, any()} | {error, business_error()} | no_return().
call(Function, Args, Client, Context) ->
    Service = party_client_config:get_party_service(Client),
    Request = {Service, Function, Args},
    CacheControl = get_cache_control(Function, Client),
    WoodyContext = party_client_context:get_woody_context(Context),
    WoodyOptions = party_client_config:get_woody_options(Client),
    case woody_caching_client:call(Request, CacheControl, WoodyOptions, WoodyContext) of
        {exception, Exception} ->
            {error, Exception};
        {ok, ok} ->
            ok;
        {ok, _Other} = Result ->
            Result
    end.

%% Internal functions

-spec get_cache_control(atom(), client()) -> woody_caching_client:cache_control().
get_cache_control(Function, Client) ->
    case party_client_config:get_cache_mode(Client) of
        safe ->
            get_safe_cache_control(Function);
        aggressive ->
            Timeout = party_client_config:get_aggressive_caching_timeout(Client),
            get_aggressive_cache_control(Function, Timeout);
        disabled ->
            no_cache
    end.

-spec get_safe_cache_control(atom()) -> woody_caching_client:cache_control().
get_safe_cache_control('Checkout') ->
    cache;
get_safe_cache_control(_Other) ->
    no_cache.

-spec get_aggressive_cache_control(atom(), timeout()) -> woody_caching_client:cache_control().
get_aggressive_cache_control(Function, Timeout) ->
    CachingFunctions = #{
        'Checkout' => cache,
        'Get' => temporary,
        'GetContract' => temporary,
        'ComputeContractTerms' => temporary,
        'GetShop' => temporary,
        'GetClaim' => temporary,
        'GetClaims' => temporary,
        'GetEvents' => temporary,
        'GetShopAccount' => temporary,
        'ComputePaymentInstitutionTerms' => temporary,
        'ComputePayoutCashFlow' => temporary
    },
    case maps:get(Function, CachingFunctions, no_cache) of
        cache ->
            cache;
        temporary ->
            {cache_for, Timeout};
        no_cache ->
            no_cache
    end.
