%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Naming System utility functions
%%% @end
%%%-------------------------------------------------------------------

-module(aens_utils).

-include_lib("apps/aecore/include/common.hrl").
-include_lib("apps/aecore/include/trees.hrl").

%% API
-export([check_claimed_and_owned/4,
         check_name_not_expired/2,
         check_name_owned_by_account/2,
         name_is_claimed/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec check_claimed_and_owned(binary(), pubkey(), trees(), height()) -> ok | {error, term()}.
check_claimed_and_owned(NameHash, AccountPubKey, Trees, Height) ->
    NamesTree = aec_trees:names(Trees),
    case aens_state_tree:lookup(NameHash, NamesTree) of
        {value, Name} ->
            Checks =
                [fun() -> check_name_not_expired(Name, Height) end,
                 fun() -> check_name_owned_by_account(Name, AccountPubKey) end,
                 fun() -> name_is_claimed(Name) end],
            aeu_validation:run(Checks);
        none ->
            {error, name_not_claimed}
    end.

-spec check_name_not_expired(aens_names:name(), height()) -> ok | {error, name_expired}.
check_name_not_expired(Name, Height) ->
    case name_is_expired(Name, Height) of
        false ->
            ok;
        true ->
            {error, name_expired}
    end.

-spec check_name_owned_by_account(aens_names:name(), pubkey()) -> ok | {error, name_not_owned}.
check_name_owned_by_account(Name, AccountPubKey) ->
    case aens_names:owner(Name) =:= AccountPubKey of
        true ->
            ok;
        false ->
            {error, name_not_owned}
    end.

name_is_claimed(Name) ->
    %% TODO Check if should compare to binary for deserialized tx
    aens_names:status(Name) =:= claimed.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec name_is_expired(aens_names:name(), height()) -> boolean().
name_is_expired(Name, Height) ->
    aens_names:expires(Name) < Height.
