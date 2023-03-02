-module(server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([setup_tables/0, setup_counters/0, get_activity_details/0]).

-define(SERVER, ?MODULE).

-record(server_state, {}).

start_link(PortForListening) ->
  io:fwrite("About to create ListenSocket on Server ~n"),
  {ok, ListenSocket} = gen_tcp:listen(PortForListening, [{mode, list}, {ip, {0,0,0,0}}, {packet, 0}, {active, false}, {reuseaddr, true}]),
  io:fwrite("ListenSocket created on Server ~n"),
  setup_tables(),
  setup_counters(),
  io:fwrite("About to trigger Server start link ~n"),
  {ok, ServerPid} = gen_server:start_link(server, [], []),
  spawn(fun() -> get_activity_details() end),
  accept_connections_recursively(ListenSocket, ServerPid).

init([]) ->
  {ok, #server_state{}}.

handle_call(_Request, _From, State = #server_state{}) ->
  {reply, ok, State}.

handle_cast({registerUserName, AcceptanceSocket, UserName}, State) ->
  setUser(UserName, AcceptanceSocket),
  gen_tcp:send(AcceptanceSocket, "UserName Received Successfully"),
  {noreply, State};

handle_cast({queryHashTag, RequesterUserName, HashTagToQuery}, State) ->
  queryHashTag(RequesterUserName, HashTagToQuery),
  {noreply, State};

handle_cast({queryMention, RequesterUserName, MentionToQuery}, State) ->
  queryMention(RequesterUserName, MentionToQuery),
  {noreply, State};

handle_cast({subscribeToUserName, SubscribeTo, Subscriber}, State) ->
  subscribeToUserName(SubscribeTo, Subscriber),
  {noreply, State};

handle_cast({unsubscribeFromUserName, UnsubscribeFrom, Subscriber}, State) ->
  unsubscribeFromUserName(UnsubscribeFrom, Subscriber),
  {noreply, State};

handle_cast({tweetUnderUserName, TweetUnderUserName, Tweet}, State) ->
  sendTweetToSubscribers(TweetUnderUserName, Tweet),
  {noreply, State}.

handle_info(_Info, State = #server_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #server_state{}) ->
  ok.

code_change(_OldVsn, State = #server_state{}, _Extra) ->
  {ok, State}.

setup_tables() ->
  ets:new(hashtags, [named_table, public, set, {read_concurrency, true}]),
  ets:new(mentions, [named_table, public, set, {read_concurrency, true}]),
  ets:new(users, [named_table, public, set, {read_concurrency, true}]),
  ets:new(subscribers, [named_table, public, set, {read_concurrency, true}]),
  ets:new(counter, [named_table, public, set, {read_concurrency, true}]).

setup_counters() ->
  ets:new(counters_table, [named_table, public, set, {keypos, 1}]),
  ets:insert(counters_table, {tweets, 0}),
  ets:insert(counters_table, {total_users, 0}),
  ets:insert(counters_table, {online_users, 0}),
  ets:insert(counters_table, {offline_users, 0}).

get_activity_details() ->
  timer:sleep(10000),
  [{_, Tweets}] = ets:lookup(counters_table,tweets),
  [{_, TotalUsers}] = ets:lookup(counters_table,total_users),
  [{_, OnlineUsers}] = ets:lookup(counters_table,online_users),
  [{_, OfflineUsers}] = ets:lookup(counters_table,offline_users),
  io:fwrite("Twitter Activity ~n Tweets ~p TotalUsers ~p OnlineUsers ~p OfflineUsers ~p ~n", [
    Tweets, TotalUsers, OnlineUsers, OfflineUsers
  ]),
  get_activity_details().

accept_connections_recursively(SocketData, ServerPid) ->
  {ok, AcceptanceSocket} = gen_tcp:accept(SocketData),
  spawn(fun() -> handleAcceptanceSocketMessages(AcceptanceSocket, ServerPid) end),
  accept_connections_recursively(SocketData, ServerPid).

handleAcceptanceSocketMessages(AcceptanceSocket, ServerPid) ->
  {StatusCode, Response} = gen_tcp:recv(AcceptanceSocket, 0),
  case StatusCode of
    ok ->
      [RequiredActionCode|_] = Response,
      case RequiredActionCode of
        0 ->
          [_|UserName] = Response,
          gen_server:cast(ServerPid, {registerUserName, AcceptanceSocket, UserName});
        1 ->
          [_|TweetData] = Response,
          %io:fwrite("TweetData ~p ~n", [TweetData]),
          [TweetUnderUserName,Tweet] = re:split(TweetData,";"),
          gen_server:cast(ServerPid, {tweetUnderUserName, binary_to_list(TweetUnderUserName), binary_to_list(Tweet)});
        2 ->
          [_|HashTagToQueryData] = Response,
          [RequesterUserName, HashTagToQuery] = re:split(HashTagToQueryData,";"),
          gen_server:cast(ServerPid, {
            queryHashTag, binary_to_list(RequesterUserName), binary_to_list(HashTagToQuery)
          });
        3 ->
          [_|MentionToQueryData] = Response,
          [RequesterUserName, MentionToQuery] = re:split(MentionToQueryData,";"),
          gen_server:cast(ServerPid, {
            queryMention, binary_to_list(RequesterUserName), binary_to_list(MentionToQuery)
          });
        4 ->
          [_|SubscriptionData] = Response,
          [SubscribeTo,Subscriber] = re:split(SubscriptionData,";"),
          gen_server:cast(ServerPid, {
            subscribeToUserName, binary_to_list(SubscribeTo), binary_to_list(Subscriber)
          });
        5 ->
          [_|SubscriptionData] = Response,
          [UnSubscribeFrom,Subscriber] = re:split(SubscriptionData,";"),
          gen_server:cast(ServerPid, {
            unsubscribeFromUserName, binary_to_list(UnSubscribeFrom), binary_to_list(Subscriber)
          });
        7 ->
          [_|BulkSubscriptionData] = Response,
          [SubscribeTo,SubscribersDataAsBinary] = re:split(BulkSubscriptionData,"-"),
          SubscribersData = binary_to_list(SubscribersDataAsBinary),
          case (string:str(SubscribersData, ";") == 0) of
            true ->
              ok;
            false ->
              SubscribersAsBinaries = re:split(SubscribersData,";"),
              Subscribers = lists:map(fun(Elem) -> binary_to_list(Elem) end, SubscribersAsBinaries),
              bulkSubscribeToUserName(binary_to_list(SubscribeTo), Subscribers)
          end;
        8 ->
          ets:update_counter(counters_table, offline_users, {2,1}),
          ets:update_counter(counters_table, online_users, {2,-1});
        9 ->
          ets:update_counter(counters_table, offline_users, {2,-1}),
          ets:update_counter(counters_table, online_users, {2,1});
        _ ->
          gen_tcp:send(AcceptanceSocket, "You asked invalid command")
      end;
    _ ->
      ok
  end,
  handleAcceptanceSocketMessages(AcceptanceSocket, ServerPid).

setUser(Username, AcceptanceSocket) ->
  Record = ets:lookup(users, Username),
  if
    Record == [] ->
      ets:insert(users, {Username, AcceptanceSocket}),
      ets:update_counter(counters_table, total_users, {2,1}),
      ets:update_counter(counters_table, online_users, {2,1});
    true ->
      io:fwrite("User already exists")
  end.

subscribeToUserName(SubscribeTo, Subscriber) ->
  Record = ets:lookup(subscribers, SubscribeTo),
  if
    Record == [] ->
      ets:insert(subscribers, {SubscribeTo, [Subscriber]});
    true ->
      [{_,ListOfSubscribers}] = Record,
      NewList = lists:append(ListOfSubscribers, [Subscriber]),
      io:fwrite("NewList of subscribers ~p ~n", [NewList]),
      ets:insert(subscribers, {SubscribeTo, NewList})
  end.

bulkSubscribeToUserName(SubscribeTo, Subscribers) ->
  Record = ets:lookup(subscribers, SubscribeTo),
  if
    Record == [] ->
      ets:insert(subscribers, {SubscribeTo, Subscribers});
    true ->
      [{_,ListOfSubscribers}] = Record,
      NewList = lists:append(ListOfSubscribers, Subscribers),
      io:fwrite("NewList of subscribers ~p ~n", [NewList]),
      ets:insert(subscribers, {SubscribeTo, NewList})
  end.

unsubscribeFromUserName(UnsubscribeFrom, Subscriber) ->
  Record = ets:lookup(subscribers, UnsubscribeFrom),
  if
    Record == [] ->
      ok;
    true ->
      [{_,ListOfSubscribers}] = Record,
      NewList = lists:delete(Subscriber, ListOfSubscribers),
      io:fwrite("NewList of subscribers after unsubscription ~p ~n", [NewList]),
      ets:insert(subscribers, {UnsubscribeFrom, NewList})
  end.

sendTweetToSubscribers(TweetUnderUserName, Tweet) ->
  ListOfWords = re:split(Tweet, " "),
  MentionsAsBinaries = lists:filter(
    fun (Elem) ->
      ElemAsList = binary_to_list(Elem),
      string:str(ElemAsList, "@") == 1
    end,
    ListOfWords),
  HashTagsAsBinaries = lists:filter(
    fun (Elem) ->
      ElemAsList = binary_to_list(Elem),
      string:str(ElemAsList, "#") == 1
    end,
    ListOfWords),
  Record = ets:lookup(subscribers, TweetUnderUserName),
  if
    Record == [] ->
      ets:update_counter(counters_table, tweets, {2,1});
    true ->
      ets:update_counter(counters_table, tweets, {2,1}),
      handleTweetParsingAndSending(Record, TweetUnderUserName, Tweet, MentionsAsBinaries, HashTagsAsBinaries)
  end.

handleTweetParsingAndSending(Record, TweetUnderUserName, Tweet, MentionsAsBinaries, HashTagsAsBinaries) ->
  case length(MentionsAsBinaries) of
    0 ->
      ok;
    _ ->
      lists:foreach(
        fun(MentionAsBinary) ->
          MentionAsList = binary_to_list(MentionAsBinary),
          MentionRecord = ets:lookup(mentions, MentionAsList),
          if
            MentionRecord == [] ->
              ets:insert(mentions, {MentionAsList, [Tweet]});
            true ->
              [{_,ListOfTweetsWithMentions}] = MentionRecord,
              NewList = lists:append(ListOfTweetsWithMentions, [Tweet]),
              ets:insert(mentions, {MentionAsList, NewList})
          end
        end,
        MentionsAsBinaries
      )
  end,
  case length(HashTagsAsBinaries) of
    0 ->
      ok;
    _ ->
      lists:foreach(
        fun(HashTagsAsBinary) ->
          HashTagAsList = binary_to_list(HashTagsAsBinary),
          HashTagRecord = ets:lookup(hashtags, HashTagAsList),
          if
            HashTagRecord == [] ->
              ets:insert(hashtags, {HashTagAsList, [Tweet]});
            true ->
              [{_,ListOfTweetsWithHashTag}] = HashTagRecord,
              NewList = lists:append(ListOfTweetsWithHashTag, [Tweet]),
              ets:insert(hashtags, {HashTagAsList, NewList})
          end
        end,
        HashTagsAsBinaries
      )
  end,
  [{_, SubscriberUserNamesList}] = Record,
  lists:foreach(
    fun(SubscriberUserName) ->
      SubscriberRecord = ets:lookup(users, SubscriberUserName),
      [{_, SubscriberAcceptanceSocket}] = SubscriberRecord,
      CombinedMessage = string:concat(TweetUnderUserName, string:concat(";", Tweet)),
      gen_tcp:send(SubscriberAcceptanceSocket, [1, CombinedMessage])
    end,
    SubscriberUserNamesList
  ).

queryHashTag(RequesterUserName, HashTagToQuery) ->
  HashTagRecord = ets:lookup(hashtags, HashTagToQuery),
  [{_,ListOfTweetsWithHashTag}] = HashTagRecord,
  SubscriberRecord = ets:lookup(users, RequesterUserName),
  [{_, SubscriberAcceptanceSocket}] = SubscriberRecord,
  ListOfTweetsWithHashTagJoined = lists:join(";", ListOfTweetsWithHashTag),
  io:fwrite("ListOfTweetsWithHashTagJoined ~p ~n", [ListOfTweetsWithHashTagJoined]),
  gen_tcp:send(SubscriberAcceptanceSocket, [2, ListOfTweetsWithHashTagJoined]).

queryMention(RequesterUserName, MentionToQuery) ->
  MentionRecord = ets:lookup(mentions, MentionToQuery),
  [{_,ListOfTweetsWithMention}] = MentionRecord,
  SubscriberRecord = ets:lookup(users, RequesterUserName),
  [{_, SubscriberAcceptanceSocket}] = SubscriberRecord,
  ListOfTweetsWithMentionJoined = lists:join(";", ListOfTweetsWithMention),
  io:fwrite("ListOfTweetsWithMentionJoined ~p ~n", [ListOfTweetsWithMentionJoined]),
  gen_tcp:send(SubscriberAcceptanceSocket, [3, ListOfTweetsWithMentionJoined]).