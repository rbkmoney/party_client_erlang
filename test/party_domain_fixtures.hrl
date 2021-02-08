-ifndef(__party_domain_fixtures__).
-define(__party_domain_fixtures__, true).

-include_lib("damsel/include/dmsl_domain_config_thrift.hrl").

-define(ordset(Es), ordsets:from_list(Es)).

-define(cur(ID), #domain_CurrencyRef{symbolic_code = ID}).
-define(pmt(C, T), #domain_PaymentMethodRef{id = {C, T}}).
-define(pomt(M), #domain_PayoutMethodRef{id = M}).
-define(cat(ID), #domain_CategoryRef{id = ID}).
-define(prx(ID), #domain_ProxyRef{id = ID}).
-define(tmpl(ID), #domain_ContractTemplateRef{id = ID}).
-define(trms(ID), #domain_TermSetHierarchyRef{id = ID}).
-define(sas(ID), #domain_SystemAccountSetRef{id = ID}).
-define(eas(ID), #domain_ExternalAccountSetRef{id = ID}).
-define(insp(ID), #domain_InspectorRef{id = ID}).
-define(pinst(ID), #domain_PaymentInstitutionRef{id = ID}).
-define(bussched(ID), #domain_BusinessScheduleRef{id = ID}).
-define(trm(ID), #domain_TerminalRef{id = ID}).
-define(prv(ID), #domain_ProviderRef{id = ID}).
-define(p2pprov(ID), #domain_P2PProviderRef{id = ID}).
-define(wtdrlprov(ID), #domain_WithdrawalProviderRef{id = ID}).
-define(prvtrm(ID), #domain_ProviderTerminalRef{id = ID}).
-define(ruleset(ID), #domain_RoutingRulesetRef{id = ID}).

-define(cashrng(Lower, Upper), #domain_CashRange{lower = Lower, upper = Upper}).

-define(currency(SymCode), #domain_CurrencyRef{symbolic_code = SymCode}).

-define(cash(Amount, SymCode), #domain_Cash{amount = Amount, currency = ?currency(SymCode)}).

-define(fixed(Amount, Currency),
    {fixed, #domain_CashVolumeFixed{
        cash = #domain_Cash{
            amount = Amount,
            currency = ?currency(Currency)
        }
    }}
).

-define(prvacc(Stl), #domain_ProviderAccount{settlement = Stl}).

-define(cfpost(A1, A2, V), #domain_CashFlowPosting{
    source = A1,
    destination = A2,
    volume = V
}).

-define(share(P, Q, C),
    {share, #domain_CashVolumeShare{
        parts = #'Rational'{p = P, q = Q},
        'of' = C
    }}
).

-define(share(P, Q, C, RM),
    {share, #domain_CashVolumeShare{
        parts = #'Rational'{p = P, q = Q},
        'of' = C,
        'rounding_method' = RM
    }}
).

-define(tkz_bank_card(PaymentSystem, TokenProvider), #domain_TokenizedBankCard{
    payment_system = PaymentSystem,
    token_provider = TokenProvider
}).

-define(every, {every, #'ScheduleEvery'{}}).

-endif.
