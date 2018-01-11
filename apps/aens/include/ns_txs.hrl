%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%%-------------------------------------------------------------------
-include_lib("apps/aecore/include/common.hrl").

-record(ns_preclaim_tx, {
          account   :: pubkey(),
          nonce     :: integer(),
          name_hash :: binary(),
          ttl       :: non_neg_integer(),
          fee       :: integer()
         }).

-record(ns_claim_tx, {
          account :: pubkey(),
          nonce   :: integer(),
          name    :: binary(),
          ttl     :: non_neg_integer(),
          fee     :: integer()
         }).

-record(ns_prolong_tx, {
          account   :: pubkey(),
          nonce     :: integer(),
          name_hash :: binary(),
          ttl       :: non_neg_integer(),
          fee       :: pubkey()
         }).

-record(ns_transfer_tx, {
          account           :: pubkey(),
          nonce             :: integer(),
          name_hash         :: binary(),
          recipient_account :: pubkey(),
          fee               :: pubkey()
         }).

-record(ns_revoke_tx, {
          account   :: pubkey(),
          nonce     :: integer(),
          name_hash :: binary(),
          ttl       :: non_neg_integer(),
          fee       :: pubkey()
         }).
