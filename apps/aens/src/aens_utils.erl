%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%    Naming System utility functions
%%% @end
%%%-------------------------------------------------------------------

-module(aens_utils).

%% API
-export([ensure_name_not_expired/2,
         name_is_expired/2,
         ensure_name_owned_by_account/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec ensure_name_not_expired(aens_names:name(), height()) -> ok | {error, name_expired}.
ensure_name_not_expired(Name, Height) ->
    case name_is_expired(Name, Height) of
        false ->
            ok;
        true ->
            {error, name_expired}
    end.

-spec name_is_expired(aens_names:name(), height()) -> boolean().
name_is_expired(Name, Height) ->
    aens_names:expires(Name) < Height.

-spec ensure_name_owned_by_account(aens_names:name(), pubkey()) -> ok | {error, name_not_owned}.
ensure_name_owned_by_account(Name, AccountPubKey) ->
    case aens_names:owner(Name) =:= AccountPubKey of
        true ->
            ok;
        false ->
            {error, name_not_owned}
    end.
