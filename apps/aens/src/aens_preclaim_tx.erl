%%%=============================================================================
%%% @copyright 2018, Aeternity Anstalt
%%% @doc
%%%    Module defining the Naming System pre-claim transaction
%%% @end
%%%=============================================================================

-module(aens_preclaim_tx).

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
-export([account/1,
         name_hash/1,
         ttl/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-define(NAME_PRECLAIM_TX_TYPE, <<"name_preclaim">>).
-define(NAME_PRECLAIM_TX_VSN, 1).

-opaque preclaim_tx() :: #ns_preclaim_tx{}.

-export_type([preclaim_tx/0]).

%%%===================================================================
%%% Behaviour API
%%%===================================================================

-spec new(map()) -> {ok, preclaim_tx()}.
new(#{account := AccountPubKey,
      nonce := Nonce,
      name_hash := NameHash,
      ttl := TTL,
      fee := Fee}) ->
    {ok, #ns_preclaim_tx{account = AccountPubKey,
                         nonce = Nonce,
                         name_hash = NameHash,
                         ttl = TTL,
                         fee = Fee}}.

-spec fee(preclaim_tx()) -> integer().
fee(#ns_preclaim_tx{fee = Fee}) ->
    Fee.

-spec nonce(preclaim_tx()) -> non_neg_integer().
nonce(#ns_preclaim_tx{nonce = Nonce}) ->
    Nonce.

-spec origin(preclaim_tx()) -> pubkey().
origin(#ns_preclaim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec check(preclaim_tx(), trees(), height()) -> {ok, trees()} | {error, term()}.
check(#ns_preclaim_tx{account = AccountPubKey, nonce = Nonce,
                      fee = Fee, name_hash = NameHash}, Trees, Height) ->
    Checks =
        [fun() -> aetx_utils:check_account(AccountPubKey, Trees, Height, Nonce, Fee) end,
         fun() -> ensure_not_occupied(NameHash, Trees, Height) end],

    case aeu_validation:run(Checks) of
        ok              -> {ok, Trees};
        {error, Reason} -> {error, Reason}
    end.

-spec process(preclaim_tx(), trees(), height()) -> {ok, trees()}.
process(#ns_preclaim_tx{account = AccountPubKey, fee = Fee,
                        nonce = Nonce} = PreclaimTx, Trees0, Height) ->
    AccountsTree0 = aec_trees:accounts(Trees0),
    NamesTree0 = aec_trees:names(Trees0),

    Account0 = aec_accounts_trees:get(AccountPubKey, AccountsTree0),
    {ok, Account1} = aec_accounts:spend(Account0, Fee, Nonce, Height),
    AccountsTree1 = aec_accounts_trees:enter(Account1, AccountsTree0),

    Name = aens_names:new(PreclaimTx, Height),
    NamesTree1 = aens_state_tree:enter(Name, NamesTree0),

    Trees1 = aec_trees:set_accounts(Trees0, AccountsTree1),
    Trees2 = aec_trees:set_names(Trees1, NamesTree1),

    {ok, Trees2}.

-spec signers(preclaim_tx()) -> [pubkey()].
signers(#ns_preclaim_tx{account = AccountPubKey}) ->
    [AccountPubKey].

-spec serialize(preclaim_tx()) -> list(map()).
serialize(#ns_preclaim_tx{account = AccountPubKey,
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

-spec deserialize(list(map())) -> preclaim_tx().
deserialize([#{<<"type">>    := ?NAME_PRECLAIM_TX_TYPE},
             #{<<"vsn">>     := ?NAME_PRECLAIM_TX_VSN},
             #{<<"account">> := AccountPubKey},
             #{<<"nonce">>   := Nonce},
             #{<<"hash">>    := NameHash},
             #{<<"ttl">>     := TTL},
             #{<<"fee">>     := Fee}]) ->
    #ns_preclaim_tx{account   = AccountPubKey,
                    nonce     = Nonce,
                    name_hash = NameHash,
                    ttl       = TTL,
                    fee       = Fee}.

-spec type() -> binary().
type() ->
    ?NAME_PRECLAIM_TX_TYPE.

-spec for_client(preclaim_tx()) -> map().
for_client(#ns_preclaim_tx{}) ->
    %% TODO: Implement it, this is something for HTTP API
    #{}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec account(preclaim_tx()) -> pubkey().
account(#ns_preclaim_tx{account = AccountPubKey}) ->
    AccountPubKey.

-spec name_hash(preclaim_tx()) -> binary().
name_hash(#ns_preclaim_tx{name_hash = NameHash}) ->
    NameHash.

-spec ttl(preclaim_tx()) -> non_neg_integer().
ttl(#ns_preclaim_tx{ttl = TTL}) ->
    TTL.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ensure_not_occupied(NameHash, Trees, Height) ->
    NamesTree = aec_trees:names(Trees),
    case aens_state_tree:lookup(NameHash, NamesTree) of
        {value, Name} ->
            case is_expired(Name, Height) of
                true ->
                    ok;
                false ->
                    {error, name_is_already_taken}
            end;
        none ->
            ok
    end.

is_expired(Name, Height) ->
    aens_names:expires(Name) < Height.

version() ->
    ?NAME_PRECLAIM_TX_VSN.
