%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% ADT for name objects
%%% @end
%%%-------------------------------------------------------------------

-module(aens_names).

-include_lib("apps/aecore/include/common.hrl").

%% API
-export([new/2,
         id/1,
         serialize/1,
         deserialize/1]).

%% Getters
-export([expires/1]).

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

-spec new(aens_preclaim_tx:preclaim_tx(), height()) -> name().
new(PreclaimTx, BlockHeight) ->
    Expires = BlockHeight + aens_preclaim_tx:ttl(PreclaimTx),
    %% TODO: add assertions on fields, similarily to what is done in aeo_oracles:new/2
    #name{hash    = aens_preclaim_tx:name_hash(PreclaimTx),
          owner   = aens_preclaim_tx:account(PreclaimTx),
          expires = Expires,
          status  = preclaimed}.

-spec id(name()) -> binary().
id(N) ->
    hash(N).

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec hash(name()) -> binary().
hash(N) -> N#name.hash.

-spec owner(name()) -> pubkey().
owner(N) -> N#name.owner.

-spec status(name()) -> name_status().
status(N) -> N#name.status.
