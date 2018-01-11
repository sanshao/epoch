-module(aec_governance).

%% API
-export([blocks_to_check_difficulty_count/0,
         expected_block_mine_rate/0,
         block_mine_reward/0,
         max_txs_in_block/0,
         minimum_tx_fee/0,
         name_preclaim_tx_ttl/0,
         name_claim_tx_ttl/0]).

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

name_preclaim_tx_ttl() ->
    %% One block is mined every 5 mins, so 20 blocks are mined per hour.
    %% Preclaim should expire in 48h: 20blocks * 48h = 960blocks
    960.

name_claim_tx_ttl() ->
    %% One block is mined every 5 mins, so 20 blocks are mined per hour.
    %% 480blocks blocks are mined dialy.
    %% Initial name claim is for 30 days, 30 * 480 = 14 400.
    14400.
