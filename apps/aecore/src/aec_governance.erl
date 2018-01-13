-module(aec_governance).

%% API
-export([blocks_to_check_difficulty_count/0,
         expected_block_mine_rate/0,
         block_mine_reward/0,
         max_txs_in_block/0,
         minimum_tx_fee/0,
         name_preclaim_tx_minimal_fee/0,
         name_preclaim_tx_ttl/0,
         name_claim_tx_minimal_fee/0,
         name_claim_tx_ttl/0,
         name_prolong_fixed_fee/0,
         name_prolong_tx_single_block_fee/0,
         name_prolong_tx_max_ttl/0,
         name_revoke_tx_ttl/0]).

-define(BLOCKS_TO_CHECK_DIFFICULTY_COUNT, 10).
-define(EXPECTED_BLOCK_MINE_RATE, 300000). %% 60secs * 1000ms * 5 = 300000msecs
-define(BLOCK_MINE_REWARD, 10).


blocks_to_check_difficulty_count() ->
    ?BLOCKS_TO_CHECK_DIFFICULTY_COUNT.

expected_block_mine_rate() ->
    application:get_env(aecore, expected_mine_rate,
                        ?EXPECTED_BLOCK_MINE_RATE).

block_mine_reward() ->
    ?BLOCK_MINE_REWARD.

max_txs_in_block() ->
    %% TODO: Consider trade-offs sync latency vs pow time
    %%       Relate to transaction size expressed with gas (when we have channels)
    10946.

minimum_tx_fee() ->
    1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Naming system variables

name_preclaim_tx_minimal_fee() ->
    2.

name_preclaim_tx_ttl() ->
    %% One block is mined every 5 mins, so 20 blocks are mined per hour.
    %% Preclaim should expire in 48h: 20blocks * 48h = 960blocks
    960.

name_claim_tx_minimal_fee() ->
    4.

name_claim_tx_ttl() ->
    %% One block is mined every 5 mins, so 20 blocks are mined per hour.
    %% 480blocks blocks are mined dialy.
    %% Initial name claim is for 30 days, 30 * 480 = 14 400.
    14400.

name_prolong_fixed_fee() ->
    1.

name_prolong_tx_single_block_fee() ->
    0.0001.

name_prolong_tx_max_ttl() ->
    %% One block is mined every 5 mins, so 20 blocks are mined per hour.
    %% 480blocks blocks are mined dialy.
    %% Max prolong time is 365 days, 365 * 480 = 175 200
    175200.

name_revoke_tx_ttl() ->
    %% One block is mined every 5 mins, so 20 blocks are mined per hour.
    %% Revoke state should remain in 48h: 20blocks * 24h * 14d = 6 720
    6720.
