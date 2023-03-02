-module(client).

-behaviour(gen_server).

-export([start_link/4, start_link_simple/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([performSimulation/2]).

-define(SERVER, ?MODULE).

-record(client_state, {}).


start_link_simple(ServerSocket) ->
  {ok, UserName} = io:read("Enter username: "),
  spawn(fun() -> listenToServer(ServerSocket) end),
  gen_server:start_link(client, [], []),
  registerUserName(UserName, ServerSocket, atomInput),
  startInteractiveClient(UserName, ServerSocket).

start_link(ServerSocket, UserName, SubscribersBasedOnReach, TweetFrequency) ->
  gen_server:start_link(client, [], []),
  registerUserName(UserName, ServerSocket, listInput),
  timer:sleep(65000),
  bulkSubscribeToUsers(SubscribersBasedOnReach, ServerSocket, UserName),
  timer:sleep(15000),
  startSimulateClient(ServerSocket, UserName, TweetFrequency).

init([]) ->
  {ok, #client_state{}}.

handle_call(_Request, _From, State = #client_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #client_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #client_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #client_state{}) ->
  ok.

code_change(_OldVsn, State = #client_state{}, _Extra) ->
  {ok, State}.

performSimulation(ServerSocket, NumberOfUsers) ->
  DummyUserNames = lists:foldl(
    fun(Index, Accumulator) ->
      NameAsList = string:concat("user", integer_to_list(Index)),
      Accumulator ++ [NameAsList]
    end,
    [],
    lists:seq(1, NumberOfUsers)
  ),
  io:fwrite("DummyUserNames ~p ~n", [DummyUserNames]),
  ZipFConstant = getZipFConstant(NumberOfUsers),
  io:fwrite("ZipFConstant ~p ~n", [ZipFConstant]),
  TopPercent = round(math:ceil(NumberOfUsers * 0.1)),
  BottomPercent = NumberOfUsers - TopPercent,
  spawn(fun() -> listenToServer(ServerSocket) end),
  lists:foreach(
    fun(UserName) ->
      IndexOfUser = string:str(DummyUserNames, [UserName]),
      AvailableSubscribers = getAvailableSubscribers(DummyUserNames, IndexOfUser),
      PossibleReach = getZipFProbability(ZipFConstant, IndexOfUser, NumberOfUsers),
      SubscribersBasedOnReach = getSubscribersBasedOnReach(AvailableSubscribers, PossibleReach),
      TweetFrequency =
        if
          IndexOfUser =< TopPercent ->
            high;
          true ->
            if
              IndexOfUser > BottomPercent ->
                low;
              true ->
                medium
            end
        end,
      io:fwrite("UserName ~p SubscribersBasedOnReach ~p ~n", [UserName, SubscribersBasedOnReach]),
      spawn(fun() -> start_link(ServerSocket, UserName, SubscribersBasedOnReach, TweetFrequency) end),
      timer:sleep(1000)
    end,
    DummyUserNames
  ).

listenToServer(ServerSocket) ->
  {StatusCode, Response} = gen_tcp:recv(ServerSocket, 0),
  case StatusCode of
    ok ->
      [RequiredActionCode|_] = Response,
      case RequiredActionCode of
        1 ->
          ok;
%%          [_|TweetData] = Response,
%%          [TweetUnderUserName,Tweet] = re:split(TweetData,";"),
%%          io:fwrite("~n User: ~p -> Tweet: ~p ~n", [binary_to_list(TweetUnderUserName), binary_to_list(Tweet)]);
        2 ->
          [_|ListOfTweetsWithHashTagJoined] = Response,
          ListOfTweetsWithHashTagAsBinaries = re:split(ListOfTweetsWithHashTagJoined, ";"),
          ListOfTweetsWithHashTag = lists:map(
            fun(Elem) ->
              binary_to_list(Elem)
            end,
            ListOfTweetsWithHashTagAsBinaries
          ),
          io:fwrite("~n ListOfTweetsWithHashTag: -> ~p ~n", [ListOfTweetsWithHashTag]);
        3 ->
          [_|ListOfTweetsWithMentionJoined] = Response,
          ListOfTweetsWithMentionAsBinaries = re:split(ListOfTweetsWithMentionJoined, ";"),
          ListOfTweetsWithMention = lists:map(
            fun(Elem) ->
              binary_to_list(Elem)
            end,
            ListOfTweetsWithMentionAsBinaries
          ),
          io:fwrite("~n ListOfTweetsWithMention: -> ~p ~n", [ListOfTweetsWithMention]);
        _ ->
          ok
      end;
    _ ->
      ok
  end,
  listenToServer(ServerSocket).

registerUserName(UserName, ServerSocket, AreInputsAtoms) ->
  case AreInputsAtoms of
    atomInput ->
      gen_tcp:send(ServerSocket, [0, atom_to_list(UserName)]);
    listInput ->
      gen_tcp:send(ServerSocket, [0, UserName])
  end.

sendTweet(UserName, ServerSocket, Tweet) ->
  CombinedMessage = string:concat(atom_to_list(UserName), string:concat(";", Tweet)),
  gen_tcp:send(ServerSocket, [1, CombinedMessage]).

sendTweetBySimulation(UserName, ServerSocket, Tweet) ->
  CombinedMessage = string:concat(UserName, string:concat(";", Tweet)),
  gen_tcp:send(ServerSocket, [1, CombinedMessage]).

performRandomLogoutAndLogin(ServerSocket, TweetFrequency) ->
  RandomNumber = rand:uniform() * 100,
  case TweetFrequency of
    high ->
      if
        RandomNumber =< 10 ->
          gen_tcp:send(ServerSocket, [8]),
          timer:sleep(30000),
          gen_tcp:send(ServerSocket, [9]);
        true ->
          ok
      end;
    medium ->
      if
        RandomNumber =< 30 ->
          gen_tcp:send(ServerSocket, [8]),
          timer:sleep(20000),
          gen_tcp:send(ServerSocket, [9]);
        true ->
          ok
      end;
    low ->
      if
        RandomNumber =< 50 ->
          gen_tcp:send(ServerSocket, [8]),
          timer:sleep(10000),
          gen_tcp:send(ServerSocket, [9]);
        true ->
          ok
      end
  end.

queryHashTag(ServerSocket, RequesterUserName, HashTagToQuery) ->
  CombinedMessage = string:concat(atom_to_list(RequesterUserName), string:concat(";", HashTagToQuery)),
  gen_tcp:send(ServerSocket, [2, CombinedMessage]).

queryMention(ServerSocket, RequesterUserName, MentionToQuery) ->
  CombinedMessage = string:concat(atom_to_list(RequesterUserName), string:concat(";", MentionToQuery)),
  gen_tcp:send(ServerSocket, [3, CombinedMessage]).

subscribeTo(Subscriber, ServerSocket, SubscribeTo) ->
  CombinedMessage = string:concat(atom_to_list(SubscribeTo), string:concat(";", atom_to_list(Subscriber))),
  io:fwrite("CombinedMessage ~p ~n", [CombinedMessage]),
  gen_tcp:send(ServerSocket, [4, CombinedMessage]).

bulkSubscribe(Subscribers, ServerSocket, SubscribeTo) ->
  CombinedMessage = lists:foldl(
    fun(Subscriber, Accumulator) ->
      string:concat(Accumulator, string:concat(Subscriber, ";"))
    end,
    string:concat(SubscribeTo, "-"),
    Subscribers
  ),
  case length(Subscribers) of
    0 ->
      gen_tcp:send(ServerSocket, [7, CombinedMessage]);
    _ ->
      {Left, _} = lists:split(length(CombinedMessage) - 1, CombinedMessage),
      gen_tcp:send(ServerSocket, [7, Left])
  end.

unSubscribeFrom(Subscriber, ServerSocket, UnSubscribeFrom) ->
  CombinedMessage = string:concat(atom_to_list(UnSubscribeFrom), string:concat(";", atom_to_list(Subscriber))),
  gen_tcp:send(ServerSocket, [5, CombinedMessage]).

startInteractiveClient(UserName, ServerSocket) ->
  io:fwrite("~p Choose an option from the following ~n", [UserName]),
  io:fwrite("1. Tweet 2. HashTag Query 3. Mentions Query 4. Subscribe To 5. Unsubscribe 6. Logout~n"),
  {ok, SelectedActionCode} = io:read("Enter key"),
  io:fwrite("~n"),
  case SelectedActionCode of
    1 ->
      {ok, Tweet} = io:read("Enter Tweet: "),
      io:fwrite("Tweet ~p", [Tweet]),
      sendTweet(UserName, ServerSocket, Tweet);
    2 ->
      {ok, HashTagToQuery} = io:read("Enter HashTagToQuery: "),
      queryHashTag(ServerSocket, UserName, HashTagToQuery);
    3 ->
      {ok, MentionToQuery} = io:read("Enter MentionToQuery: "),
      queryMention(ServerSocket, UserName, MentionToQuery);
    4 ->
      {ok, SubscribeTo} = io:read("Enter UserName: "),
      subscribeTo(UserName, ServerSocket, SubscribeTo);
    5 ->
      {ok, UnSubscribeFrom} = io:read("Enter UserName: "),
      unSubscribeFrom(UserName, ServerSocket, UnSubscribeFrom);
    _ ->
      io:fwrite("Default case")
  end,
  startInteractiveClient(UserName, ServerSocket).


getZipFConstant(NumberOfUsers) ->
  UserRatios = lists:foldl(
    fun(Index, Accumulator) ->
      Accumulator ++ [1/Index]
    end,
    [],
    lists:seq(1, NumberOfUsers)
  ),
  UserRatiosSum = lists:foldl(
    fun(Elem, Accumulator) ->
      Elem + Accumulator
    end,
    0,
    UserRatios
  ),
  math:pow(UserRatiosSum, -1).

getAvailableSubscribers(DummyUserNames, CurrentIndex) ->
  lists:filter(fun(UserName) -> string:str(DummyUserNames, [UserName]) =/= CurrentIndex end, DummyUserNames).

getZipFProbability(ZipFConstant, CurrentIndex, NumberOfUsers) ->
  round((ZipFConstant/CurrentIndex) * NumberOfUsers).

getSubscribersBasedOnReach(DummyUserNames, PossibleReach) ->
  lists:foldl(
    fun(_, Accumulator) ->
      RandomUser = lists:nth(rand:uniform(length(DummyUserNames)), DummyUserNames),
      Accumulator ++ [RandomUser]
    end,
    [],
    lists:seq(1, PossibleReach)
  ).

bulkSubscribeToUsers(SubscribersBasedOnReach, ServerSocket, SubscribeTo) ->
  bulkSubscribe(SubscribersBasedOnReach, ServerSocket, SubscribeTo).

startSimulateClient(ServerSocket, UserName, TweetFrequency) ->
  RandomTweet = getRandomStringAsTweet(),
  sendTweetBySimulation(UserName, ServerSocket, RandomTweet),
  case TweetFrequency of
    high ->
      timer:sleep(7000),
      performRandomLogoutAndLogin(ServerSocket, TweetFrequency);
    medium ->
      timer:sleep(14000),
      performRandomLogoutAndLogin(ServerSocket, TweetFrequency);
    low ->
      timer:sleep(21000),
      performRandomLogoutAndLogin(ServerSocket, TweetFrequency)
  end,
  startSimulateClient(ServerSocket, UserName, TweetFrequency).

getRandomStringAsTweet() ->
  RandomString = getRandomStringFromCrypto(),
  HashAsInteger = binary:decode_unsigned(crypto:hash(sha256,RandomString)),
  io_lib:format("~64.16.0b", [HashAsInteger]).

getRandomStringFromCrypto() -> base64:encode_to_string(crypto:strong_rand_bytes(6)).