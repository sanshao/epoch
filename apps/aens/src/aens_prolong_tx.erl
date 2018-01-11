%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System prolong transaction
%%% @end
%%%=============================================================================

-module(aens_prolong_tx).

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

-define(NAME_PROLONG_TX_TYPE, <<"name_prolong">>).
-define(NAME_PROLONG_TX_VSN, 1).

-opaque prolong_tx() :: #ns_prolong_tx{}.

-export_type([prolong_tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, prolong_tx()}.
new(#{account   := AccountPubKey,
      nonce     := Nonce,
      name_hash := NameHash,
      ttl       := TTL,
      fee       := Fee}) ->
    {ok, #ns_prolong_tx{account   = AccountPubKey,
                        nonce     = Nonce,
                        name_hash = NameHash,
                        ttl       = TTL,
                        fee       = Fee}}.

-spec fee(prolong_tx()) -> integer().
fee(#ns_prolong_tx{fee = Fee}) ->
    Fee.

-spec nonce(prolong_tx()) -> non_neg_integer().
nonce(#ns_prolong_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(prolong_tx()) -> pubkey().
origin(#ns_prolong_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(prolong_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#ns_prolong_tx{account = AccountPubKey, nonce = Nonce,
                     fee = Fee, name_hash = NameHash, ttl = TTL}, Trees, Height) ->
    Checks =
        [fun() -> assert_ttl(TTL) end,
         fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> assert_ttl_fee(TTL, Fee) end,
         fun() -> aens_utils:ensure_claimed_and_owned(NameHash, AccountPubKey, Trees, Height) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(prolong_tx(), trees(), height()) -> {ok, trees()}.
process(#ns_prolong_tx{account = AccountPubKey, fee = Fee,
                       name_hash = NameHash, nonce = Nonce} = ProlongTx, Trees0, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    NamesTree0 = aec_trees:names(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce, Height),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    Name0 = aens_state_tree:get(NameHash, NamesTree0),
    Name1 = aens_names:prolong(ProlongTx, Name0, Height),
    NamesTree1 = aens_state_tree:enter(Name1, NamesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_names(Trees1, NamesTree1),

    {ok, Trees2}.

-spec signers(prolong_tx()) -> [pubkey()].
signers(#ns_prolong_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec serialize(prolong_tx()) -> list(map()).
serialize(#ns_prolong_tx{account = AccountPubKey,
                         nonce = Nonce,
                         name_hash = NameHash,
                         ttl = TTL,
                         fee = Fee}) ->
    [#{<<"type">>    => type()},
     #{<<"vsn">>     => version()},
     #{<<"account">> => AccountPubKey},
     #{<<"nonce">>   => Nonce},
     #{<<"hash">>    => NameHash},
     #{<<"ttl">>     => TTL},
     #{<<"fee">>     => Fee}].

-spec deserialize(list(map())) -> prolong_tx().
deserialize([#{<<"type">>    := ?NAME_PROLONG_TX_TYPE},
             #{<<"vsn">>     := ?NAME_PROLONG_TX_VSN},
             #{<<"account">> := AccountPubKey},
             #{<<"nonce">>   := Nonce},
             #{<<"hash">>    := NameHash},
             #{<<"ttl">>     := TTL},
             #{<<"fee">>     := Fee}]) ->
    #ns_prolong_tx{account   = AccountPubKey,
                   nonce     = Nonce,
                   name_hash = NameHash,
                   ttl       = TTL,
                   fee       = Fee}.

-spec type() -> binary().
type() ->
    ?NAME_PROLONG_TX_TYPE.

-spec for_client(prolong_tx()) -> map().
for_client(#ns_prolong_tx{}) ->
    %% TODO: Implement it, this is something for HTTP API
    #{}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec ttl(prolong_tx()) -> non_neg_integer().
ttl(#ns_prolong_tx{ttl = TTL}) ->
    TTL.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assert_ttl(TTL) ->
    case TTL =< aec_governance:name_prolong_tx_max_ttl() of
        true ->
            ok;
        false ->
            {error, wrong_ttl}
    end.

assert_ttl_fee(TTL, Fee) ->
    case ttl_fee(TTL) =< Fee of
        true ->
            ok;
        false ->
            {error, too_low_fee}
    end.

ttl_fee(NBlocks) ->
    ceil(NBlocks * aec_governance:name_prolong_tx_single_block_fee()).

version() ->
    ?NAME_PROLONG_TX_VSN.
