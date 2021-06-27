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

-spec call(atom(), tuple(), client(), context()) -> ok | {ok, any()} | {error, business_error()} | no_return().
call(Function, Args, Client, Context) ->
    Service = party_client_config:get_party_service(Client),
    CacheControl = get_cache_control(Function, Client),
    WoodyOptions = party_client_config:get_woody_options(Client),
    WoodyContext0 = party_client_context:get_woody_context(Context),
    WoodyContext = ensure_deadline(WoodyContext0, Client),
    Retry = get_function_retry(Function, Client),
    Request = {Service, Function, Args},
    call(Request, CacheControl, WoodyOptions, WoodyContext, Retry).

call(Request, CacheControl, WoodyOptions, WoodyContext, Retry) ->
    try
        case
            woody_caching_client:call(
                Request,
                CacheControl,
                WoodyOptions,
                WoodyContext
            )
        of
            {exception, Exception} ->
                {error, Exception};
            {ok, ok} ->
                ok;
            {ok, _Other} = Result ->
                Result
        end
    catch
        error:{woody_error, {_Source, Class, _Details}} = Error when
            Class =:= resource_unavailable orelse Class =:= result_unknown
        ->
            NextRetry = apply_retry_strategy(Retry, Error, WoodyContext),
            call(Request, CacheControl, WoodyOptions, WoodyContext, NextRetry)
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
    case get_aggressive_function_cache_mode(Function) of
        cache ->
            cache;
        temporary ->
            {cache_for, Timeout};
        no_cache ->
            no_cache
    end.

get_aggressive_function_cache_mode('Checkout') -> cache;
get_aggressive_function_cache_mode('Get') -> temporary;
get_aggressive_function_cache_mode('GetRevision') -> temporary;
get_aggressive_function_cache_mode('GetContract') -> temporary;
get_aggressive_function_cache_mode('ComputeContractTerms') -> temporary;
get_aggressive_function_cache_mode('GetShop') -> temporary;
get_aggressive_function_cache_mode('GetClaim') -> temporary;
get_aggressive_function_cache_mode('GetClaims') -> temporary;
get_aggressive_function_cache_mode('GetEvents') -> temporary;
get_aggressive_function_cache_mode('GetShopAccount') -> temporary;
get_aggressive_function_cache_mode('ComputePaymentInstitutionTerms') -> temporary;
get_aggressive_function_cache_mode('ComputePayoutCashFlow') -> temporary;
get_aggressive_function_cache_mode(_Other) -> no_cache.

% Retry

ensure_deadline(WoodyContext, Client) ->
    case woody_context:get_deadline(WoodyContext) of
        undefined ->
            Deadline = get_deadline(Client),
            woody_context:set_deadline(Deadline, WoodyContext);
        _AlreadySet ->
            WoodyContext
    end.

get_deadline(Client) ->
    woody_deadline:from_timeout(party_client_config:get_deadline_timeout(Client)).

get_function_retry(Function, Client) ->
    FunctionReties = party_client_config:get_retries(Client),
    DefaultRetry = maps:get('_', FunctionReties, finish),
    maps:get(Function, FunctionReties, DefaultRetry).

apply_retry_strategy(Retry, Error, Context) ->
    apply_retry_step(genlib_retry:next_step(Retry), woody_context:get_deadline(Context), Error).

apply_retry_step(finish, _, Error) ->
    erlang:error(Error);
apply_retry_step({wait, Timeout, Retry}, undefined, _) ->
    ok = timer:sleep(Timeout),
    Retry;
apply_retry_step({wait, Timeout, Retry}, Deadline0, Error) ->
    Deadline1 = woody_deadline:from_unixtime_ms(
        woody_deadline:to_unixtime_ms(Deadline0) - Timeout
    ),
    case woody_deadline:is_reached(Deadline1) of
        true ->
            % no more time for retries
            erlang:error(Error);
        false ->
            ok = timer:sleep(Timeout),
            Retry
    end.
