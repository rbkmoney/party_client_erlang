-module(party_client_context).

-export([create/1]).
-export([get_woody_context/1]).
-export([get_user_info/1]).
-export([get_user_info/2]).

-opaque context() :: #{
    woody_context := woody_context(),
    user_info => user_info()
}.
-type options() :: #{
    woody_context => woody_context(),
    user_info => user_info()
}.
-type user_info() :: woody_user_identity:user_identity().

-export_type([context/0]).
-export_type([options/0]).
-export_type([user_info/0]).

%% Internal types

-type woody_context() :: woody_context:ctx().

%% API

-spec create(options()) -> context().
create(Options) ->
    ensure_woody_context_exists(Options).

-spec get_woody_context(context()) -> woody_context().
get_woody_context(Context) ->
    #{woody_context := WoodyContext} = ensure_user_info_set(Context),
    WoodyContext.

-spec get_user_info(context()) -> user_info() | undefined.
get_user_info(Context) ->
    get_user_info(Context, undefined).

-spec get_user_info(context(), Default) -> user_info() | Default.
get_user_info(#{user_info := UserInfo}, _Default) ->
    UserInfo;
get_user_info(#{woody_context := WoodyContext}, Default) ->
    get_woody_user_info(WoodyContext, Default).

%% Internal functions

-spec ensure_woody_context_exists(options()) -> options().
ensure_woody_context_exists(#{woody_context := _WoodyContext} = Options) ->
    Options;
ensure_woody_context_exists(Options) ->
    Options#{woody_context => woody_context:new()}.

-spec ensure_user_info_set(context()) -> context().
ensure_user_info_set(#{user_info := UserInfo, woody_context := WoodyContext} = Context) ->
    NewWoodyContext = woody_user_identity:put(UserInfo, WoodyContext),
    Context#{woody_context := NewWoodyContext};
ensure_user_info_set(Context) ->
    Context.

-spec get_woody_user_info(woody_context(), Default) -> user_info() | Default.
get_woody_user_info(WoodyContext, Default) ->
    try woody_user_identity:get(WoodyContext) of
        WoodyIdentity ->
            WoodyIdentity
    catch
        throw:{missing_required, _Key} ->
            Default
    end.
