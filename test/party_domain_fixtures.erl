-module(party_domain_fixtures).

-include("party_domain_fixtures.hrl").
-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-export([construct_domain_fixture/0]).
-export([apply_domain_fixture/0]).
-export([apply_domain_fixture/1]).
-export([cleanup/0]).

%% Internal types

-type name()        :: binary().
-type category()    :: dmsl_domain_thrift:'CategoryRef'().
-type currency()    :: dmsl_domain_thrift:'CurrencyRef'().
-type proxy()       :: dmsl_domain_thrift:'ProxyRef'().
-type inspector()   :: dmsl_domain_thrift:'InspectorRef'().
-type template()    :: dmsl_domain_thrift:'ContractTemplateRef'().
-type terms()       :: dmsl_domain_thrift:'TermSetHierarchyRef'().
-type lifetime()    :: dmsl_domain_thrift:'Lifetime'() | undefined.

-type system_account_set() :: dmsl_domain_thrift:'SystemAccountSetRef'().
-type external_account_set() :: dmsl_domain_thrift:'ExternalAccountSetRef'().

-type business_schedule() :: dmsl_domain_thrift:'BusinessScheduleRef'().

%% API

-spec apply_domain_fixture() -> ok.
apply_domain_fixture() ->
    apply_domain_fixture(construct_domain_fixture()).

-spec apply_domain_fixture([dmsl_domain_thrift:'DomainObject'()]) -> ok.
apply_domain_fixture(Fixture) ->
    #'Snapshot'{version = Head} = dmt_client:checkout({head, #'Head'{}}),
    Commit = #'Commit'{ops = [{insert, #'InsertOp'{object = F}} || F <- Fixture]},
%%    logger:error("Fixture: ~p~nCommit: ~p", [Fixture, Commit]),
    _NextRevision = dmt_client:commit(Head, Commit),
    ok.

-spec cleanup() -> ok.
cleanup() ->
    #'Snapshot'{domain = Domain, version = Head} = dmt_client:checkout({head, #'Head'{}}),
    Objects = maps:values(Domain),
    Commit = #'Commit'{ops = [{remove, #'RemoveOp'{object = O}} || O <- Objects]},
    _NextRevision = dmt_client:commit(Head, Commit),
    ok.

-spec construct_domain_fixture() -> [dmsl_domain_thrift:'DomainObject'()].
construct_domain_fixture() ->
    TestTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ordsets:from_list([?cur(<<"RUB">>)])},
            categories = {value, ordsets:from_list([?cat(1)])}
        }
    },
    DefaultTermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            currencies = {value, ordsets:from_list([
                ?cur(<<"RUB">>),
                ?cur(<<"USD">>)
            ])},
            categories = {value, ordsets:from_list([
                ?cat(2),
                ?cat(3)
            ])},
            payment_methods = {value, ordsets:from_list([
                ?pmt(bank_card, visa)
            ])}
        }
    },
    TermSet = #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            cash_limit = {value, #domain_CashRange{
                lower = {inclusive, #domain_Cash{amount = 1000, currency = ?cur(<<"RUB">>)}},
                upper = {exclusive, #domain_Cash{amount = 4200000, currency = ?cur(<<"RUB">>)}}
            }},
            fees = {value, [
                ?cfpost(
                    {merchant, settlement},
                    {system, settlement},
                    ?share(45, 1000, operation_amount)
                )
            ]}
        },
        payouts = #domain_PayoutsServiceTerms{
            payout_methods = {decisions, [
                #domain_PayoutMethodDecision{
                    if_   = {constant, true},
                    then_ = {value, ordsets:from_list([?pomt(russian_bank_account)])}
                }
            ]},
            fees = {value, [
                ?cfpost(
                    {merchant, settlement},
                    {merchant, payout},
                    ?share(750, 1000, operation_amount)
                ),
                ?cfpost(
                    {merchant, settlement},
                    {system, settlement},
                    ?share(250, 1000, operation_amount)
                )
            ]}
        },
        wallets = #domain_WalletServiceTerms{
            currencies = {value, ordsets:from_list([?cur(<<"RUB">>)])}
        }
    },
    [
        construct_currency(?cur(<<"RUB">>)),
        construct_currency(?cur(<<"USD">>)),

        construct_category(?cat(1), <<"Test category">>, test),
        construct_category(?cat(2), <<"Generic Store">>, live),
        construct_category(?cat(3), <<"Guns & Booze">>, live),

        construct_payment_method(?pmt(bank_card, visa)),
        construct_payment_method(?pmt(bank_card, mastercard)),
        construct_payment_method(?pmt(bank_card, maestro)),
        construct_payment_method(?pmt(payment_terminal, euroset)),

        construct_payout_method(?pomt(russian_bank_account)),
        construct_payout_method(?pomt(international_bank_account)),

        construct_proxy(?prx(1), <<"Dummy proxy">>),
        construct_inspector(?insp(1), <<"Dummy Inspector">>, ?prx(1)),
        construct_system_account_set(?sas(1)),
        construct_system_account_set(?sas(2)),
        construct_external_account_set(?eas(1)),

        construct_business_schedule(?bussched(1)),

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(1),
            data = #domain_PaymentInstitution{
                name = <<"Test Inc.">>,
                system_account_set = {value, ?sas(1)},
                default_contract_template = {value, ?tmpl(1)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = [],
                realm = test
            }
        }},

        {payment_institution, #domain_PaymentInstitutionObject{
            ref = ?pinst(2),
            data = #domain_PaymentInstitution{
                name = <<"Chetky Payments Inc.">>,
                system_account_set = {value, ?sas(2)},
                default_contract_template = {value, ?tmpl(2)},
                providers = {value, ?ordset([])},
                inspector = {value, ?insp(1)},
                residences = [],
                realm = live
            }
        }},

        {globals, #domain_GlobalsObject{
            ref = #domain_GlobalsRef{},
            data = #domain_Globals{
                external_account_set = {value, ?eas(1)},
                payment_institutions = ?ordset([?pinst(1), ?pinst(2)])
            }
        }},
        construct_contract_template(
            ?tmpl(1),
            ?trms(1)
        ),
        construct_contract_template(
            ?tmpl(2),
            ?trms(3)
        ),
        construct_contract_template(
            ?tmpl(3),
            ?trms(2),
            {interval, #domain_LifetimeInterval{years = -1}},
            {interval, #domain_LifetimeInterval{days = -1}}
        ),
        construct_contract_template(
            ?tmpl(4),
            ?trms(1),
            undefined,
            {interval, #domain_LifetimeInterval{months = 1}}
        ),
        construct_contract_template(
            ?tmpl(5),
            ?trms(4)
        ),
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(1),
            data = #domain_TermSetHierarchy{
                parent_terms = undefined,
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TestTermSet
                }]
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(2),
            data = #domain_TermSetHierarchy{
                parent_terms = undefined,
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = DefaultTermSet
                }]
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(3),
            data = #domain_TermSetHierarchy{
                parent_terms = ?trms(2),
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = TermSet
                }]
            }
        }},
        {term_set_hierarchy, #domain_TermSetHierarchyObject{
            ref = ?trms(4),
            data = #domain_TermSetHierarchy{
                parent_terms = ?trms(3),
                term_sets = [#domain_TimedTermSet{
                    action_time = #'TimestampInterval'{},
                    terms = #domain_TermSet{
                        payments = #domain_PaymentsServiceTerms{
                            currencies = {value, ordsets:from_list([
                                ?cur(<<"RUB">>)
                            ])},
                            categories = {value, ordsets:from_list([
                                ?cat(2)
                            ])},
                            payment_methods = {value, ordsets:from_list([
                                ?pmt(bank_card, visa)
                            ])}
                        }
                    }
                }]
            }
        }},
        {provider, #domain_ProviderObject{
            ref = ?prv(1),
            data = #domain_Provider{
                name = <<"Brovider">>,
                description = <<"A provider but bro">>,
                terminal = {value, [?prvtrm(1)]},
                proxy = #domain_Proxy{ref = ?prx(1), additional = #{}},
                abs_account = <<"1234567890">>,
                terms = #domain_ProvisionTermSet{
                    payments = #domain_PaymentsProvisionTerms{
                        currencies = {value, ?ordset([?cur(<<"RUB">>)])},
                        categories = {value, ?ordset([?cat(1)])},
                        payment_methods = {value, ?ordset([
                            ?pmt(bank_card, visa),
                            ?pmt(bank_card, mastercard)
                        ])},
                        cash_limit = {value, ?cashrng(
                            {inclusive, ?cash(      1000, <<"RUB">>)},
                            {exclusive, ?cash(1000000000, <<"RUB">>)}
                        )},
                        cash_flow = {decisions, [
                            #domain_CashFlowDecision{
                                if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                                then_ = {value, [
                                    ?cfpost(
                                        {system, settlement},
                                        {provider, settlement},
                                        {product, {min_of, ?ordset([
                                            ?fixed(10, <<"RUB">>),
                                            ?share(
                                                5, 100, operation_amount, round_half_towards_zero
                                            )
                                        ])}}
                                    )
                                ]}
                            },
                            #domain_CashFlowDecision{
                                if_   = {condition, {currency_is, ?cur(<<"USD">>)}},
                                then_ = {value, [
                                    ?cfpost(
                                        {system, settlement},
                                        {provider, settlement},
                                        {product, {min_of, ?ordset([
                                            ?fixed(10, <<"USD">>),
                                            ?share(
                                                5, 100, operation_amount, round_half_towards_zero
                                            )
                                        ])}}
                                    )
                                ]}
                            }
                        ]}
                    },
                    recurrent_paytools = #domain_RecurrentPaytoolsProvisionTerms{
                        categories = {value, ?ordset([?cat(1)])},
                        payment_methods = {value, ?ordset([
                            ?pmt(bank_card, visa),
                            ?pmt(bank_card, mastercard)
                        ])},
                        cash_value = {decisions, [
                            #domain_CashValueDecision{
                                if_   = {condition, {currency_is, ?cur(<<"RUB">>)}},
                                then_ = {value, ?cash(1000, <<"RUB">>)}
                            },
                            #domain_CashValueDecision{
                                if_   = {condition, {currency_is, ?cur(<<"USD">>)}},
                                then_ = {value, ?cash(1000, <<"USD">>)}
                            }
                        ]}
                    }
                }
            }
        }},

        {terminal, #domain_TerminalObject{
            ref = ?trm(1),
            data = #domain_Terminal{
                name = <<"Brominal 1">>,
                description = <<"Brominal 1">>,
                terms = #domain_ProvisionTermSet{
                    payments = #domain_PaymentsProvisionTerms{
                        payment_methods = {value, ?ordset([
                            ?pmt(bank_card, visa)
                        ])}
                    }
                }
            }
        }}
    ].

%% Internal functions

-spec construct_currency(currency()) ->
    {currency, dmsl_domain_thrift:'CurrencyObject'()}.
construct_currency(Ref) ->
    construct_currency(Ref, 2).

-spec construct_currency(currency(), Exponent :: pos_integer()) ->
    {currency, dmsl_domain_thrift:'CurrencyObject'()}.
construct_currency(?cur(SymbolicCode) = Ref, Exponent) ->
    {currency, #domain_CurrencyObject{
        ref = Ref,
        data = #domain_Currency{
            name = SymbolicCode,
            numeric_code = 666,
            symbolic_code = SymbolicCode,
            exponent = Exponent
        }
    }}.

-spec construct_category(category(), name(), test | live) ->
    {category, dmsl_domain_thrift:'CategoryObject'()}.
construct_category(Ref, Name, Type) ->
    {category, #domain_CategoryObject{
        ref = Ref,
        data = #domain_Category{
            name = Name,
            description = Name,
            type = Type
        }
    }}.

-spec construct_payment_method(dmsl_domain_thrift:'PaymentMethodRef'()) ->
    {payment_method, dmsl_domain_thrift:'PaymentMethodObject'()}.
construct_payment_method(?pmt(_Type, ?tkz_bank_card(Name, _)) = Ref) when is_atom(Name) ->
    construct_payment_method(Name, Ref);
construct_payment_method(?pmt(_Type, Name) = Ref) when is_atom(Name) ->
    construct_payment_method(Name, Ref).

construct_payment_method(Name, Ref) ->
    Def = erlang:atom_to_binary(Name, unicode),
    {payment_method, #domain_PaymentMethodObject{
        ref = Ref,
        data = #domain_PaymentMethodDefinition{
            name = Def,
            description = Def
        }
    }}.

-spec construct_payout_method(dmsl_domain_thrift:'PayoutMethodRef'()) ->
    {payout_method, dmsl_domain_thrift:'PayoutMethodObject'()}.
construct_payout_method(?pomt(M) = Ref) ->
    Def = erlang:atom_to_binary(M, unicode),
    {payout_method, #domain_PayoutMethodObject{
        ref = Ref,
        data = #domain_PayoutMethodDefinition{
            name = Def,
            description = Def
        }
    }}.

-spec construct_proxy(proxy(), name()) ->
    {proxy, dmsl_domain_thrift:'ProxyObject'()}.
construct_proxy(Ref, Name) ->
    construct_proxy(Ref, Name, #{}).

-spec construct_proxy(proxy(), name(), Opts :: map()) ->
    {proxy, dmsl_domain_thrift:'ProxyObject'()}.
construct_proxy(Ref, Name, Opts) ->
    {proxy, #domain_ProxyObject{
        ref = Ref,
        data = #domain_ProxyDefinition{
            name        = Name,
            description = Name,
            url         = <<>>,
            options     = Opts
        }
    }}.

-spec construct_inspector(inspector(), name(), proxy()) ->
    {inspector, dmsl_domain_thrift:'InspectorObject'()}.
construct_inspector(Ref, Name, ProxyRef) ->
    construct_inspector(Ref, Name, ProxyRef, #{}).

-spec construct_inspector(inspector(), name(), proxy(), Additional :: map()) ->
    {inspector, dmsl_domain_thrift:'InspectorObject'()}.
construct_inspector(Ref, Name, ProxyRef, Additional) ->
    {inspector, #domain_InspectorObject{
        ref = Ref,
        data = #domain_Inspector{
            name = Name,
            description = Name,
            proxy = #domain_Proxy{
                ref = ProxyRef,
                additional = Additional
            }
        }
    }}.

-spec construct_contract_template(template(), terms()) ->
    {contract_template, dmsl_domain_thrift:'ContractTemplateObject'()}.
construct_contract_template(Ref, TermsRef) ->
    construct_contract_template(Ref, TermsRef, undefined, undefined).

-spec construct_contract_template(template(), terms(), ValidSince :: lifetime(), ValidUntil :: lifetime()) ->
    {contract_template, dmsl_domain_thrift:'ContractTemplateObject'()}.
construct_contract_template(Ref, TermsRef, ValidSince, ValidUntil) ->
    {contract_template, #domain_ContractTemplateObject{
        ref = Ref,
        data = #domain_ContractTemplate{
            valid_since = ValidSince,
            valid_until = ValidUntil,
            terms = TermsRef
        }
    }}.

-spec construct_system_account_set(system_account_set()) ->
    {system_account_set, dmsl_domain_thrift:'SystemAccountSetObject'()}.
construct_system_account_set(Ref) ->
    construct_system_account_set(Ref, <<"Primaries">>, ?cur(<<"RUB">>)).

-spec construct_system_account_set(system_account_set(), name(), currency()) ->
    {system_account_set, dmsl_domain_thrift:'SystemAccountSetObject'()}.
construct_system_account_set(Ref, Name, ?cur(CurrencyCode)) ->
    AccountID = 3,
    {system_account_set, #domain_SystemAccountSetObject{
        ref = Ref,
        data = #domain_SystemAccountSet{
            name = Name,
            description = Name,
            accounts = #{?cur(CurrencyCode) => #domain_SystemAccount{
                settlement = AccountID
            }}
        }
    }}.

-spec construct_external_account_set(external_account_set()) ->
    {system_account_set, dmsl_domain_thrift:'ExternalAccountSetObject'()}.
construct_external_account_set(Ref) ->
    construct_external_account_set(Ref, <<"Primaries">>, ?cur(<<"RUB">>)).

-spec construct_external_account_set(external_account_set(), name(), currency()) ->
    {system_account_set, dmsl_domain_thrift:'ExternalAccountSetObject'()}.
construct_external_account_set(Ref, Name, ?cur(CurrencyCode)) ->
    AccountID1 = 1,
    AccountID2 = 2,
    {external_account_set, #domain_ExternalAccountSetObject{
        ref = Ref,
        data = #domain_ExternalAccountSet{
            name = Name,
            description = Name,
            accounts = #{?cur(CurrencyCode) => #domain_ExternalAccount{
                income  = AccountID1,
                outcome = AccountID2
            }}
        }
    }}.

-spec construct_business_schedule(business_schedule()) ->
    {business_schedule, dmsl_domain_thrift:'BusinessScheduleObject'()}.
construct_business_schedule(Ref) ->
    {business_schedule, #domain_BusinessScheduleObject{
        ref = Ref,
        data = #domain_BusinessSchedule{
            name = <<"Every day at 7:40">>,
            schedule = #'Schedule'{
                year = ?every,
                month = ?every,
                day_of_month = ?every,
                day_of_week = ?every,
                hour = {on, [7]},
                minute = {on, [40]},
                second = {on, [0]}
            }
        }
    }}.
