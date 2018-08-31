-module(party_client).

-export([create_client/0]).
-export([create_client/1]).
-export([create_context/0]).
-export([create_context/1]).
-export([child_spec/2]).
-export([start_link/1]).

%% Client types

-type context() :: party_client_thrift:context().
-type context_options() :: party_client_context:options().

-type client() :: party_client_thrift:client().
-type client_options() :: party_client_config:options().

-export_type([client/0]).
-export_type([client_options/0]).
-export_type([context/0]).
-export_type([context_options/0]).

%% Client API

-spec create_client() -> client().
create_client() ->
    create_client(#{}).

-spec create_client(client_options()) -> client().
create_client(Options) ->
    party_client_config:create(Options).

-spec create_context() -> context().
create_context() ->
    create_context(#{}).

-spec create_context(context_options()) -> context().
create_context(Options) ->
    party_client_context:create(Options).

-spec child_spec(atom(), client()) -> supervisor:child_spec().
child_spec(ChildID, Client) ->
    party_client_woody:child_spec(ChildID, Client).

-spec start_link(client()) -> genlib_gen:start_ret().
start_link(Client) ->
    party_client_woody:start_link(Client).
