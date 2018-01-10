%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%% ADT for keeping the state of Naming System
%%% @end
%%%-------------------------------------------------------------------
-module(aens_state_tree).

%% API
-export([empty/0,
         enter/2,
         get/2,
         lookup/2,
         root_hash/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type mtree() :: aeu_mtrees:tree(aens_names:id(), aens_names:serialized()).
-type name() :: aens_names:name().

-record(names_tree, {mtree = gb_merkle_trees:empty() :: mtree()}).

-opaque tree() :: #names_tree{}.

-export_type([tree/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec empty() -> tree().
empty() ->
    MTree = aeu_mtrees:empty(),
    #names_tree{mtree = MTree}.

-spec enter(name(), tree()) -> tree().
enter(N, T) ->
    Id = aens_names:id(N),
    Serialized = aens_names:serialize(N),
    MTree1 = aeu_mtrees:enter(Id, Serialized, T#names_tree.mtree),
    T#names_tree{mtree = MTree1}.

-spec get(binary(), tree()) -> name().
get(Id, Tree) ->
    aens_names:deserialize(aeu_mtrees:get(Id, Tree#names_tree.mtree)).

-spec lookup(binary(), tree()) -> {value, name()} | none.
lookup(Id, Tree) ->
    case aeu_mtrees:get(Id, Tree#names_tree.mtree) of
        {value, Val} -> {value, aens_names:deserialize(Val)};
        none -> none
    end.

-spec root_hash(tree()) -> {ok, aeu_mtrees:root_hash()} | {error, empty}.
root_hash(#names_tree{mtree = MTree}) ->
    aeu_mtrees:root_hash(MTree).
