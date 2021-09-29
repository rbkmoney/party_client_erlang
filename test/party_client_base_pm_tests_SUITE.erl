-module(party_client_base_pm_tests_SUITE).

-include("party_domain_fixtures.hrl").

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([create_and_get_test/1]).
-export([user_info_using_test/1]).
-export([party_errors_test/1]).
-export([party_operations_test/1]).

-export([compute_provider_ok/1]).
-export([compute_provider_not_found/1]).
-export([compute_provider_terminal_terms_ok/1]).
-export([compute_provider_terminal_terms_not_found/1]).
-export([compute_globals_ok/1]).
-export([compute_routing_ruleset_ok/1]).
-export([compute_routing_ruleset_unreducable/1]).
-export([compute_routing_ruleset_not_found/1]).

%% Internal types

-type test_entry() :: atom() | {group, atom()}.
-type group() :: {atom(), [Opts :: atom()], [test_entry()]}.
-type config() :: [{atom(), any()}].

-define(WRONG_DMT_OBJ_ID, 99999).

%% CT description

-spec all() -> [test_entry()].
all() ->
    [
        {group, party_management_api},
        {group, party_management_compute_api}
    ].

-spec groups() -> [group()].
groups() ->
    [
        {party_management_api, [parallel], [
            create_and_get_test,
            user_info_using_test,
            party_errors_test,
            party_operations_test
        ]},
        {party_management_compute_api, [parallel], [
            compute_provider_ok,
            compute_provider_not_found,
            compute_provider_terminal_terms_ok,
            compute_provider_terminal_terms_not_found,
            compute_globals_ok,
            compute_routing_ruleset_ok,
            compute_routing_ruleset_unreducable,
            compute_routing_ruleset_not_found
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    % _ = dbg:tracer(),
    % _ = dbg:p(all, c),
    % _ = dbg:tpl({'scoper_woody_event_handler', 'handle_event', '_'}, x),
    AppConfig = [
        {dmt_client, [
            % milliseconds
            {cache_update_interval, 5000},
            {max_cache_size, #{
                elements => 1,
                % 2Kb
                memory => 2048
            }},
            {service_urls, #{
                'Repository' => <<"http://dominant:8022/v1/domain/repository">>,
                'RepositoryClient' => <<"http://dominant:8022/v1/domain/repository_client">>
            }}
        ]},
        {party_client, []}
    ],
    Apps = lists:flatten([genlib_app:start_application_with(A, C) || {A, C} <- AppConfig]),
    {ok, Revision} = init_domain(),
    Client = party_client:create_client(),
    {ok, ClientPid} = party_client:start_link(Client),
    true = erlang:unlink(ClientPid),
    [{apps, Apps}, {client, Client}, {client_pid, ClientPid}, {test_id, genlib:to_binary(Revision)} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(C) ->
    true = erlang:exit(conf(client_pid, C), shutdown),
    genlib_app:stop_unload_applications(proplists:get_value(apps, C)).

-spec init_per_group(atom(), config()) -> config().
init_per_group(Group, Config) ->
    [{test_id, genlib:to_binary(Group)} | Config].

-spec end_per_group(atom(), config()) -> config().
end_per_group(_Group, _Config) ->
    ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(Name, Config) ->
    [{test_id, genlib:to_binary(Name)} | Config].

-spec end_per_testcase(atom(), config()) -> config().
end_per_testcase(_Name, _Config) ->
    ok.

%% Tests

-spec create_and_get_test(config()) -> any().
create_and_get_test(C) ->
    {ok, PartyId, Client, Context} = test_init_info(C),
    ContactInfo = #domain_PartyContactInfo{email = PartyId},
    ok = party_client_thrift:create(PartyId, make_party_params(ContactInfo), Client, Context),
    {ok, Party} = party_client_thrift:get(PartyId, Client, Context),
    #domain_Party{id = PartyId, contact_info = ContactInfo} = Party.

-spec user_info_using_test(config()) -> any().
user_info_using_test(C) ->
    {ok, PartyId, Client, _Context} = test_init_info(C),
    UserInfo = user_info(test, service),
    ContextWithoutUser = party_client:create_context(),
    ContextWithUser = party_client:create_context(#{user_info => UserInfo}),
    WoodyContext = woody_user_identity:put(UserInfo, woody_context:new()),
    ContextWithWoody = party_client:create_context(#{woody_context => WoodyContext}),
    ContactInfo = #domain_PartyContactInfo{email = PartyId},
    ok = party_client_thrift:create(PartyId, make_party_params(ContactInfo), Client, ContextWithUser),
    {ok, _} = (catch party_client_thrift:get(PartyId, Client, ContextWithUser)),
    {ok, _} = (catch party_client_thrift:get(PartyId, Client, ContextWithWoody)),
    {'EXIT', {invalid_user_info, _}} = (catch party_client_thrift:get(PartyId, Client, ContextWithoutUser)),
    ok.

-spec party_errors_test(config()) -> any().
party_errors_test(C) ->
    {ok, PartyId, Client, Context} = test_init_info(C),
    ContactInfo = #domain_PartyContactInfo{email = PartyId},
    PartyParams = make_party_params(ContactInfo),
    ok = party_client_thrift:create(PartyId, PartyParams, Client, Context),
    {error, #payproc_PartyExists{}} = party_client_thrift:create(PartyId, PartyParams, Client, Context),
    {error, #payproc_PartyNotFound{}} = party_client_thrift:get(<<"not_exists">>, Client, Context),
    {error, #payproc_InvalidPartyRevision{}} =
        party_client_thrift:checkout(PartyId, {revision, 100500}, Client, Context),
    {error, #payproc_InvalidPartyStatus{}} = party_client_thrift:activate(PartyId, Client, Context),
    OtherContext = party_client:create_context(#{user_info => user_info(test2, external)}),
    {error, #payproc_InvalidUser{}} = party_client_thrift:get(PartyId, Client, OtherContext),
    ok.

-spec party_operations_test(config()) -> any().
party_operations_test(C) ->
    {ok, _TestId, Client, Context} = test_init_info(C),
    {ok, PartyId} = create_party(C),
    ok = party_client_thrift:suspend(PartyId, Client, Context),
    ok = party_client_thrift:activate(PartyId, Client, Context),
    ok = party_client_thrift:block(PartyId, <<"block_test">>, Client, Context),
    ok = party_client_thrift:unblock(PartyId, <<"unblock_test">>, Client, Context),
    OtherContext = party_client:create_context(#{user_info => user_info(test2, external)}),
    {error, #payproc_InvalidUser{}} = party_client_thrift:get(PartyId, Client, OtherContext),
    ok.

-spec compute_provider_ok(config()) -> any().
compute_provider_ok(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    Varset = #payproc_Varset{
        currency = ?cur(<<"RUB">>)
    },
    CashFlow = make_test_cashflow(),
    {ok, #domain_Provider{
        terms = #domain_ProvisionTermSet{
            payments = #domain_PaymentsProvisionTerms{
                cash_flow = {value, [CashFlow]}
            },
            recurrent_paytools = #domain_RecurrentPaytoolsProvisionTerms{
                cash_value = {value, ?cash(1000, <<"RUB">>)}
            }
        }
    }} = party_client_thrift:compute_provider(?prv(1), DomainRevision, Varset, Client, Context).

-spec compute_provider_not_found(config()) -> any().
compute_provider_not_found(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    {error, #payproc_ProviderNotFound{}} =
        party_client_thrift:compute_provider(
            ?prv(2),
            DomainRevision,
            #payproc_Varset{},
            Client,
            Context
        ).

-spec compute_provider_terminal_terms_ok(config()) -> any().
compute_provider_terminal_terms_ok(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    Varset = #payproc_Varset{
        currency = ?cur(<<"RUB">>)
    },
    CashFlow = make_test_cashflow(),
    PaymentMethods = ?ordset([?pmt(bank_card_deprecated, visa)]),
    {ok, #domain_ProvisionTermSet{
        payments = #domain_PaymentsProvisionTerms{
            cash_flow = {value, [CashFlow]},
            payment_methods = {value, PaymentMethods}
        }
    }} = party_client_thrift:compute_provider_terminal_terms(
        ?prv(1),
        ?trm(1),
        DomainRevision,
        Varset,
        Client,
        Context
    ).

-spec compute_provider_terminal_terms_not_found(config()) -> any().
compute_provider_terminal_terms_not_found(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    {error, #payproc_TerminalNotFound{}} =
        party_client_thrift:compute_provider_terminal_terms(
            ?prv(1),
            ?trm(?WRONG_DMT_OBJ_ID),
            DomainRevision,
            #payproc_Varset{},
            Client,
            Context
        ),
    {error, #payproc_ProviderNotFound{}} =
        party_client_thrift:compute_provider_terminal_terms(
            ?prv(2),
            ?trm(1),
            DomainRevision,
            #payproc_Varset{},
            Client,
            Context
        ),
    {error, #payproc_ProviderNotFound{}} =
        party_client_thrift:compute_provider_terminal_terms(
            ?prv(2),
            ?trm(?WRONG_DMT_OBJ_ID),
            DomainRevision,
            #payproc_Varset{},
            Client,
            Context
        ).

-spec compute_globals_ok(config()) -> any().
compute_globals_ok(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    Varset = #payproc_Varset{},
    {ok, #domain_Globals{
        external_account_set = {value, ?eas(1)}
    }} = party_client_thrift:compute_globals(DomainRevision, Varset, Client, Context).

-spec compute_routing_ruleset_ok(config()) -> any().
compute_routing_ruleset_ok(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    Varset = #payproc_Varset{
        party_id = <<"67890">>
    },
    {ok, #domain_RoutingRuleset{
        name = <<"Rule#1">>,
        decisions =
            {candidates, [
                #domain_RoutingCandidate{
                    terminal = ?trm(2),
                    allowed = {constant, true}
                },
                #domain_RoutingCandidate{
                    terminal = ?trm(3),
                    allowed = {constant, true}
                },
                #domain_RoutingCandidate{
                    terminal = ?trm(1),
                    allowed = {constant, true}
                }
            ]}
    }} = party_client_thrift:compute_routing_ruleset(?ruleset(1), DomainRevision, Varset, Client, Context).

-spec compute_routing_ruleset_unreducable(config()) -> any().
compute_routing_ruleset_unreducable(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    Varset = #payproc_Varset{},
    {ok, #domain_RoutingRuleset{
        name = <<"Rule#1">>,
        decisions =
            {delegates, [
                #domain_RoutingDelegate{
                    allowed = {condition, {party, #domain_PartyCondition{id = <<"12345">>}}},
                    ruleset = ?ruleset(2)
                },
                #domain_RoutingDelegate{
                    allowed = {condition, {party, #domain_PartyCondition{id = <<"67890">>}}},
                    ruleset = ?ruleset(3)
                },
                #domain_RoutingDelegate{
                    allowed = {constant, true},
                    ruleset = ?ruleset(4)
                }
            ]}
    }} = party_client_thrift:compute_routing_ruleset(?ruleset(1), DomainRevision, Varset, Client, Context).

-spec compute_routing_ruleset_not_found(config()) -> any().
compute_routing_ruleset_not_found(C) ->
    {ok, _PartyId, Client, Context} = test_init_info(C),
    {ok, DomainRevision} = dmt_client_cache:update(),
    {error, #payproc_RuleSetNotFound{}} =
        (catch party_client_thrift:compute_routing_ruleset(
            ?ruleset(5),
            DomainRevision,
            #payproc_Varset{},
            Client,
            Context
        )).

%% Internal functions

%% Environment confirators

-spec init_domain() -> {ok, integer()}.
init_domain() ->
    {ok, _} = dmt_client_cache:update(),
    ok = party_domain_fixtures:cleanup(),
    {ok, _} = dmt_client_cache:update(),
    ok = party_domain_fixtures:apply_domain_fixture(),
    % Wait until hellgate dmt_client cache updating
    timer:sleep(5000),
    {ok, _Revision} = dmt_client_cache:update().

create_party(C) ->
    {ok, TestId, Client, Context} = test_init_info(C),
    PartyId = <<TestId/binary, ".party">>,
    ContactInfo = #domain_PartyContactInfo{email = <<TestId/binary, "@example.com">>},
    ok = party_client_thrift:create(PartyId, make_party_params(ContactInfo), Client, Context),
    {ok, PartyId}.

%% Config helpers

-spec get_test_id(config()) -> binary().
get_test_id(Config) ->
    AllId = lists:reverse(proplists:append_values(test_id, Config)),
    erlang:iolist_to_binary([[<<".">> | I] || I <- AllId]).

conf(Key, Config) ->
    proplists:get_value(Key, Config).

%% Domain objects constructors

make_party_params(ContactInfo) ->
    #payproc_PartyParams{contact_info = ContactInfo}.

-spec user_info(any(), any()) -> party_client_context:user_info().
user_info(User, Realm) ->
    #{id => genlib:to_binary(User), realm => genlib:to_binary(Realm)}.

create_context() ->
    party_client:create_context(#{user_info => user_info(test, service)}).

test_init_info(C) ->
    PartyId = get_test_id(C),
    Client = conf(client, C),
    Context = create_context(),
    {ok, PartyId, Client, Context}.

-spec make_test_cashflow() -> dmsl_domain_thrift:'CashFlowPosting'().
make_test_cashflow() ->
    ?cfpost(
        {system, settlement},
        {provider, settlement},
        {product,
            {min_of,
                ?ordset([
                    ?fixed(10, <<"RUB">>),
                    ?share(5, 100, operation_amount, round_half_towards_zero)
                ])}}
    ).
