%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System claim transaction
%%% @end
%%%=============================================================================

-module(aens_claim_tx).

-include("ns_txs.hrl").
-include_lib("apps/aecore/include/trees.hrl").

-behavior(aetx).

%% Behavior API
-export([new/1,
         fee/1,
         nonce/1,
         origin/1,
         check/3,
         process/3,
         signers/1,
         serialize/1,
         deserialize/1,
         type/0,
         for_client/1
        ]).

%% Getters
-export([ttl/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_CLAIM_TX_TYPE, <<"name_claim">>).
-define(NAME_CLAIM_TX_VSN, 1).

-opaque claim_tx() :: #ns_claim_tx{}.

-export_type([claim_tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, claim_tx()}.
new(#{account  := AccountPubKey,
      nonce    := Nonce,
      name     := Name,
      pointers := Pointers,
      ttl      := TTL,
      fee      := Fee}) ->
    {ok, #ns_claim_tx{account  = AccountPubKey,
                      nonce    = Nonce,
                      name     = Name,
                      pointers = Pointers,
                      ttl      = TTL,
                      fee      = Fee}}.

-spec fee(claim_tx()) -> integer().
fee(#ns_claim_tx{fee = Fee}) ->
    Fee.

-spec nonce(claim_tx()) -> non_neg_integer().
nonce(#ns_claim_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(claim_tx()) -> pubkey().
origin(#ns_claim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(claim_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#ns_claim_tx{account = AccountPubKey, nonce = Nonce,
                   fee = Fee, name = Name, ttl = TTL}, Trees, Height) ->
    Checks =
        [fun() -> check_ttl(TTL) end,
         fun() -> check_fee(Fee) end,
         fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> check_preclaimed(Name, AccountPubKey, Trees, Height) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(claim_tx(), trees(), height()) -> {ok, trees()}.
process(#ns_claim_tx{account = AccountPubKey, fee = Fee,
                     name = PlainName, nonce = Nonce} = ClaimTx, Trees0, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    NamesTree0 = aec_trees:names(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce, Height),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    Name0 = aens_state_tree:get(aens_names:hash_name(PlainName), NamesTree0),
    Name1 = aens_names:claim(ClaimTx, Name0, Height),
    NamesTree1 = aens_state_tree:enter(Name1, NamesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_names(Trees1, NamesTree1),

    {ok, Trees2}.

-spec signers(claim_tx()) -> [pubkey()].
signers(#ns_claim_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec serialize(claim_tx()) -> list(map()).
serialize(#ns_claim_tx{account = AccountPubKey,
                       nonce   = None,
                       name    = Name,
                       ttl     = TTL,
                       fee     = Fee}) ->
    [#{<<"type">>    => type()},
     #{<<"vsn">>     => version()},
     #{<<"account">> => AccountPubKey},
     #{<<"nonce">>   => None},
     #{<<"name">>    => Name},
     #{<<"ttl">>     => TTL},
     #{<<"fee">>     => Fee}].

-spec deserialize(list(map())) -> claim_tx().
deserialize([#{<<"type">>    := ?NAME_CLAIM_TX_TYPE},
             #{<<"vsn">>     := ?NAME_CLAIM_TX_VSN},
             #{<<"account">> := AccountPubKey},
             #{<<"nonce">>   := Nonce},
             #{<<"name">>    := Name},
             #{<<"ttl">>     := TTL},
             #{<<"fee">>     := Fee}]) ->
    #ns_claim_tx{account = AccountPubKey,
                 nonce   = Nonce,
                 name    = Name,
                 ttl     = TTL,
                 fee     = Fee}.

-spec type() -> binary().
type() ->
    ?NAME_CLAIM_TX_TYPE.

-spec for_client(claim_tx()) -> map().
for_client(#ns_claim_tx{}) ->
    %% TODO: Implement it, this is something for HTTP API
    #{}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec ttl(claim_tx()) -> non_neg_integer().
ttl(#ns_claim_tx{ttl = TTL}) ->
    TTL.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_ttl(TTL) ->
    case TTL =:= aec_governance:name_claim_tx_ttl() of
        true ->
            ok;
        false ->
            {error, wrong_ttl}
    end.

check_fee(Fee) ->
    case Fee >= aec_governance:name_claim_tx_minimal_fee() of
        true ->
            ok;
        false ->
            {error, too_low_fee}
    end.

check_preclaimed(Name, AccountPubKey, Trees, Height) ->
    NamesTree = aec_trees:names(Trees),
    case aens_state_tree:lookup(aens_names:hash_name(Name), NamesTree) of
        {value, Name} ->
            Checks =
                [fun() -> aens_utils:check_name_not_expired(Name, Height) end,
                 fun() -> aens_utils:check_name_owned_by_account(Name, AccountPubKey) end],
            aeu_validation:run(Checks);
        none ->
            {error, name_not_preclaimed}
    end.

version() ->
    ?NAME_CLAIM_TX_VSN.
