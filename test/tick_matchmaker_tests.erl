-module(tick_matchmaker_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../../fixtures/fixtures.hrl").
-include_lib("../../fixtures/manager_fixtures.hrl").
-include_lib("../../fixtures/user_lib_fixtures.hrl").
-include_lib("../../fixtures/notifier_fixtures.hrl").
-include_lib("../../fixtures/referee_fixtures.hrl").
-include_lib("../../fixtures/interop_fixtures.hrl").

setup() ->
    mock_manager(),
    mock_user_lib(),
    mock_observer(),
    mock_notifier_dummy(),
    mock_referee(),
    mock_redis(90000),
    mock_interop().
cleanup(_) ->
    unload_mock_user_lib(),
    unload_mock_manager(),
    unload_mock_observer(),
    unload_mock_notifier(),
    unload_mock_referee(),
    unload_mock_redis(),
    unload_mock_interop().

tick_matchmaker_test_() ->
    tick_matchmaker_serv:start_link(),
    {setup, fun setup/0, fun cleanup/1,
     [
      fun test_request_match/0,
      fun test_request_another_match/0,
      fun test_cancel_match/0,
      fun test_match_pair/0
     ]
    }.

test_request_match() ->
    tick_matchmaker:request_match(rich_data(),
                                  [
                                   [1, 2, 3, 4, 5, 6]
                                  ]),
    Pool = gen_server:call(tick_matchmaker_serv, get_pool),
    ?assertEqual(1, length(Pool)).

test_request_another_match() ->
    tick_matchmaker:request_match(rich_data(),
                                  [
                                   [1, 2, 3, 4, 5, 6]
                                  ]),
    Pool = gen_server:call(tick_matchmaker_serv, get_pool),
    [
     ?assertEqual(1, length(Pool))
    ].

test_cancel_match() ->
    tick_matchmaker:cancel_match(rich_data()),
    Pool = gen_server:call(tick_matchmaker_serv, get_pool),
    ?assertEqual(0, length(Pool)).

test_match_pair() ->
    tick_matchmaker:request_match(rich_data(),
                                  [
                                   [1, 2, 3, 4, 5, 6]
                                  ]),
    tick_matchmaker:request_match(poor_data(),
                                  [
                                   [1, 2, 3, 4, 5, 6]
                                  ]),
    Pool = gen_server:call(tick_matchmaker_serv, get_pool),
    [
     ?assertEqual(0, length(Pool))
    ].
