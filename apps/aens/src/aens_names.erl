%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% ADT for name objects
%%% @end
%%%-------------------------------------------------------------------

-module(aens_names).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([claim/3,
         hash_name/1,
         id/1,
         new/2,
         revoke/3,
         transfer/2,
         serialize/1,
         deserialize/1]).

%% Getters
-export([expires/1,
         owner/1,
         status/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type name_status() :: preclaimed | claimed | revoked.

-record(name, {hash    :: binary(),
               owner   :: pubkey(),
               expires :: height(),
               status  :: name_status()}).

-opaque name() :: #name{}.

-type id() :: binary().
-type serialized() :: binary().

-export_type([id/0,
              name/0,
              serialized/0]).

-define(PUB_SIZE, 65).
-define(NAME_TYPE, <<"name">>).
-define(NAME_VSN, 1).

%%%===================================================================
%%% API
%%%===================================================================

-spec claim(aens_claim_tx:claim_tx(), name(), height()) -> name().
claim(ClaimTx, Name, BlockHeight) ->
    Expires = BlockHeight + aens_claim_tx:ttl(ClaimTx),
    Name#name{status  = claimed,
              expires = Expires}.

-spec hash_name(binary()) -> binary().
hash_name(Name) ->
    %% TODO: Implement NameHash as described in https://github.com/aeternity/protocol/blob/aens/drafts/AENS.md#hashing
    Name.

-spec id(name()) -> binary().
id(N) ->
    hash(N).

-spec new(aens_preclaim_tx:preclaim_tx(), height()) -> name().
new(PreclaimTx, BlockHeight) ->
    Expires = BlockHeight + aens_preclaim_tx:ttl(PreclaimTx),
    %% TODO: add assertions on fields, similarily to what is done in aeo_oracles:new/2
    #name{hash    = aens_preclaim_tx:name_hash(PreclaimTx),
          owner   = aens_preclaim_tx:account(PreclaimTx),
          expires = Expires,
          status  = preclaimed}.

-spec revoke(aens_revoke_tx:revoke_tx(), name(), height()) -> name().
revoke(RevokeTx, Name, BlockHeight) ->
    Expires = BlockHeight + aens_revoke_tx:ttl(RevokeTx),
    Name#name{status = revoked,
              expires = Expires}.

-spec transfer(aens_transfer_tx:transfer_tx(), name()) -> name().
transfer(TransferTx, Name) ->
    Name#name{owner = aens_transfer_tx:recipient_account(TransferTx)}.

-spec serialize(name()) -> binary().
serialize(#name{} = N) ->
    msgpack:pack([#{<<"type">>    => ?NAME_TYPE},
                  #{<<"vsn">>     => ?NAME_VSN},
                  #{<<"hash">>    => hash(N)},
                  #{<<"owner">>   => owner(N)},
                  #{<<"expires">> => expires(N)},
                  #{<<"status">>  => status(N)}]).

-spec deserialize(binary()) -> name().
deserialize(Bin) ->
    {ok, List} = msgpack:unpack(Bin),
    [#{<<"type">>    := ?NAME_TYPE},
     #{<<"vsn">>     := ?NAME_VSN},
     #{<<"hash">>    := Hash},
     #{<<"owner">>   := Owner},
     #{<<"expires">> := Expires},
     #{<<"status">>  := Status}] = List,
    #name{hash    = Hash,
          owner   = Owner,
          expires = Expires,
          status  = Status}.

%%%===================================================================
%%% Getters
%%%===================================================================

-spec expires(name()) -> height().
expires(N) -> N#name.expires.

-spec owner(name()) -> pubkey().
owner(N) -> N#name.owner.

-spec status(name()) -> name_status().
status(N) -> N#name.status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec hash(name()) -> binary().
hash(N) -> N#name.hash.
