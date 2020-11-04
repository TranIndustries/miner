-module(miner_transfer_hotspot_txn_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("blockchain/include/blockchain_vars.hrl").
-include("miner_ct_macros.hrl").

-export([
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0
        ]).

-export([
         basic_test/1,
         simple_replay_test/1,
         refund_replay_test/1
        ]).

%% common test callbacks

all() -> [
          basic_test,
          simple_replay_test,
          refund_replay_test
         ].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config0) ->
    Config = miner_ct_utils:init_per_testcase(?MODULE, _TestCase, Config0),
    [Seller | _ ] = Miners = ?config(miners, Config),
    SellerAddr = ct_rpc:call(Seller, blockchain_swarm, pubkey_bin, []),
    Addresses = ?config(addresses, Config),
    InitialPaymentTransactions = [ blockchain_txn_coinbase_v1:new(Addr, 5000) || Addr <- Addresses],
    AddGwTxns = [blockchain_txn_gen_gateway_v1:new(Addr, SellerAddr,
                                                   h3:from_geo({37.780586, -122.469470}, 13), 0)
                 || Addr <- Addresses],

    NumConsensusMembers = ?config(num_consensus_members, Config),
    BlockTime = ?config(block_time, Config),
    BatchSize = ?config(batch_size, Config),
    Curve = ?config(dkg_curve, Config),
    %% VarCommitInterval = ?config(var_commit_interval, Config),

    Keys = libp2p_crypto:generate_keys(ecc_compact),

    InitialVars = miner_ct_utils:make_vars(Keys, #{?block_time => BlockTime,
                                                   %% rule out rewards
                                                   ?election_interval => infinity,
                                                   ?num_consensus_members => NumConsensusMembers,
                                                   ?batch_size => BatchSize,
                                                   ?dkg_curve => Curve,
                                                   ?max_payments => 10,
                                                   ?transfer_hotspot_stale_poc_blocks => 100,
                                                   ?allow_zero_amount => false}),

    DKGResults = miner_ct_utils:initial_dkg(Miners, InitialVars ++ InitialPaymentTransactions ++ AddGwTxns,
                                           Addresses, NumConsensusMembers, Curve),
    true = lists:all(fun(Res) -> Res == ok end, DKGResults),

    %% Get both consensus and non consensus miners
    {ConsensusMiners, NonConsensusMiners} = miner_ct_utils:miners_by_consensus_state(Miners),
    %% integrate genesis block
    _GenesisLoadResults = miner_ct_utils:integrate_genesis_block(hd(ConsensusMiners), NonConsensusMiners),

    ok = miner_ct_utils:wait_for_gte(height, Miners, 2),

    [   {consensus_miners, ConsensusMiners},
        {non_consensus_miners, NonConsensusMiners}
        | Config].


end_per_testcase(_TestCase, Config) ->
    miner_ct_utils:end_per_testcase(_TestCase, Config).

basic_test(Config) ->
    Miners = ?config(miners, Config),
    [GwAddr | _] = ?config(addresses, Config),
    [Seller, Buyer | _ ] = Miners,
    SellerAddr = ct_rpc:call(Seller, blockchain_swarm, pubkey_bin, []),
    BuyerAddr = ct_rpc:call(Buyer, blockchain_swarm, pubkey_bin, []),

    ct:pal("SellerAddr: ~p~nBuyerAddr: ~p", [SellerAddr, BuyerAddr]),

    %% check initial balances
    5000 = miner_ct_utils:get_balance(Seller, SellerAddr),
    5000 = miner_ct_utils:get_balance(Buyer, BuyerAddr),

    %% ensure seller owns the gateway
    SellerAddr = miner_ct_utils:get_gw_owner(Seller, GwAddr),

    %% make transfer hotspot transaction
    Txn = ct_rpc:call(Seller, blockchain_txn_transfer_hotspot_v1, new,
                      [GwAddr, SellerAddr, BuyerAddr, 1, 1000]),

    {ok, _, SellerSigFun, _} = ct_rpc:call(Seller, blockchain_swarm, keys, []),
    {ok, _, BuyerSigFun , _} = ct_rpc:call(Buyer, blockchain_swarm, keys, []),

    SellerSignedTxn = ct_rpc:call(Seller, blockchain_txn_transfer_hotspot_v1, sign_seller,
                                  [Txn, SellerSigFun]),
    ct:pal("SellerSignedTxn: ~p", [SellerSignedTxn]),

    SignedTxn = ct_rpc:call(Buyer, blockchain_txn_transfer_hotspot_v1, sign_buyer,
                                  [SellerSignedTxn, BuyerSigFun]),
    ct:pal("SignedTxn: ~p", [SignedTxn]),

    ok = ct_rpc:call(Seller, blockchain_worker, submit_txn, [SignedTxn]),

    %% wait until all the nodes agree the payment has happened
    %% NOTE: Fee is zero
    ok = miner_ct_utils:confirm_balance(Miners, SellerAddr, 6000),

    %% ensure the buyer is the owner of the gateway
    BuyerAddr = miner_ct_utils:get_gw_owner(Seller, GwAddr),
    ok.

simple_replay_test(Config) ->
    Miners = ?config(miners, Config),
    [GwAddr | _] = ?config(addresses, Config),
    [Seller, Buyer | _ ] = Miners,
    SellerAddr = ct_rpc:call(Seller, blockchain_swarm, pubkey_bin, []),
    BuyerAddr = ct_rpc:call(Buyer, blockchain_swarm, pubkey_bin, []),

    ct:pal("SellerAddr: ~p~nBuyerAddr: ~p", [SellerAddr, BuyerAddr]),

    %% check initial balances
    5000 = miner_ct_utils:get_balance(Seller, SellerAddr),
    5000 = miner_ct_utils:get_balance(Buyer, BuyerAddr),

    %% ensure seller owns the gateway
    SellerAddr = miner_ct_utils:get_gw_owner(Seller, GwAddr),

    %% make transfer hotspot transaction
    Txn = ct_rpc:call(Seller, blockchain_txn_transfer_hotspot_v1, new,
                      [GwAddr, SellerAddr, BuyerAddr, 1, 1000]),

    {ok, _, SellerSigFun, _} = ct_rpc:call(Seller, blockchain_swarm, keys, []),
    {ok, _, BuyerSigFun , _} = ct_rpc:call(Buyer, blockchain_swarm, keys, []),

    SellerSignedTxn = ct_rpc:call(Seller, blockchain_txn_transfer_hotspot_v1, sign_seller,
                                  [Txn, SellerSigFun]),
    ct:pal("SellerSignedTxn: ~p", [SellerSignedTxn]),

    SignedTxn = ct_rpc:call(Buyer, blockchain_txn_transfer_hotspot_v1, sign_buyer,
                                  [SellerSignedTxn, BuyerSigFun]),
    ct:pal("SignedTxn: ~p", [SignedTxn]),

    ok = ct_rpc:call(Seller, blockchain_worker, submit_txn, [SignedTxn]),

    %% wait until all the nodes agree the payment has happened
    %% NOTE: Fee is zero
    ok = miner_ct_utils:confirm_balance(Miners, SellerAddr, 6000),

    %% ensure the buyer is the owner of the gateway
    BuyerAddr = miner_ct_utils:get_gw_owner(Seller, GwAddr),

    %% resubmit original transaction and it should fail
    ok = ct_rpc:call(Seller, blockchain_worker, submit_txn, [SignedTxn]),

    %% ensure this is unchanged
    ok = miner_ct_utils:confirm_balance(Miners, SellerAddr, 6000),
    BuyerAddr = miner_ct_utils:get_gw_owner(Seller, GwAddr),

    ok.

refund_replay_test(Config) ->
    Miners = ?config(miners, Config),
    [GwAddr | _] = ?config(addresses, Config),
    [Seller, Buyer | _ ] = Miners,
    SellerAddr = ct_rpc:call(Seller, blockchain_swarm, pubkey_bin, []),
    BuyerAddr = ct_rpc:call(Buyer, blockchain_swarm, pubkey_bin, []),

    ct:pal("SellerAddr: ~p~nBuyerAddr: ~p", [SellerAddr, BuyerAddr]),

    %% check initial balances
    5000 = miner_ct_utils:get_balance(Seller, SellerAddr),
    5000 = miner_ct_utils:get_balance(Buyer, BuyerAddr),

    %% ensure seller owns the gateway
    SellerAddr = miner_ct_utils:get_gw_owner(Seller, GwAddr),

    %% make transfer hotspot transaction
    Txn = ct_rpc:call(Seller, blockchain_txn_transfer_hotspot_v1, new,
                      [GwAddr, SellerAddr, BuyerAddr, 1, 1000]),

    {ok, _, SellerSigFun, _} = ct_rpc:call(Seller, blockchain_swarm, keys, []),
    {ok, _, BuyerSigFun , _} = ct_rpc:call(Buyer, blockchain_swarm, keys, []),

    SellerSignedTxn = ct_rpc:call(Seller, blockchain_txn_transfer_hotspot_v1, sign_seller,
                                  [Txn, SellerSigFun]),

    SignedTxn = ct_rpc:call(Buyer, blockchain_txn_transfer_hotspot_v1, sign_buyer,
                                  [SellerSignedTxn, BuyerSigFun]),

    ok = ct_rpc:call(Seller, blockchain_worker, submit_txn, [SignedTxn]),

    %% wait until all the nodes agree the payment has happened
    %% NOTE: Fee is zero
    ok = miner_ct_utils:confirm_balance(Miners, SellerAddr, 6000),

    %% ensure the buyer is the owner of the gateway
    BuyerAddr = miner_ct_utils:get_gw_owner(Seller, GwAddr),

    %% send gw back to seller (maybe because RMA/refund)
    RefundTxn = ct_rpc:call(Seller, blockchain_txn_transfer_hotspot_v1, new,
                            [GwAddr, BuyerAddr, SellerAddr, 1, 1000]),

    SignedRefundTxn0 = ct_rpc:call(Buyer, blockchain_txn_transfer_hotspot_v1, sign_seller,
                                   [RefundTxn, BuyerSigFun]),


    SignedRefundTxn = ct_rpc:call(Seller, blockchain_txn_transfer_hotspot_v1, sign_buyer,
                                  [SignedRefundTxn0, SellerSigFun]),

    ok = ct_rpc:call(Buyer, blockchain_worker, submit_txn, [SignedRefundTxn]),

    %% wait until all nodes agree to refund
    ok = miner_ct_utils:confirm_balance(Miners, BuyerAddr, 5000),
    SellerAddr = miner_ct_utils:get_gw_owner(Seller, GwAddr),

    %% now replay original transaction where the seller transfers to the buyer
    ok = ct_rpc:call(Buyer, blockchain_worker, submit_txn, [SignedTxn]),

    %% validate that state remains unchanged after replay
    ok = miner_ct_utils:confirm_balance(Miners, BuyerAddr, 5000),
    SellerAddr = miner_ct_utils:get_gw_owner(Seller, GwAddr),

    ok.
