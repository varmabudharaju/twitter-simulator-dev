-module(middleware).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([initializeEngineAsClient/2, initializeEngineAsServer/1, initializeEngineAsSimulator/4]).

-define(SERVER, ?MODULE).

start_link() ->
  {ok, _} = gen_server:start_link(middleware, [], []).

init([]) ->
  {ok, {}}.

handle_call(_Request, _From, State = {}) ->
  {reply, ok, State}.

handle_cast(_Request, State = {}) ->
  {noreply, State}.

handle_info(_Info, State = {}) ->
  {noreply, State}.

terminate(_Reason, _State = {}) ->
  ok.

code_change(_OldVsn, State = {}, _Extra) ->
  {ok, State}.

initializeEngineAsServer(PortNumber) ->
  server:start_link(PortNumber).

initializeEngineAsClient(PortNumber, ServerIP) ->
  {_, ServerSocket} = establishConnectionToServer(PortNumber, ServerIP),
  client:start_link_simple(ServerSocket).

establishConnectionToServer(PortNumber, ServerIP) ->
  gen_tcp:connect(ServerIP, PortNumber, [{mode, list}, {active, false}, {packet, 0}]).


initializeEngineAsSimulator(PortNumber, ServerIP, NumberOfUsers, NumberOfIterations) ->
  lists:foldl(
    fun(Index, Accumulator) ->
      {_, ServerSocket} = establishConnectionToServer(PortNumber, ServerIP),
      spawn(fun() -> client:performSimulation(ServerSocket, NumberOfUsers) end),
      Accumulator ++ [Index]
    end,
    [],
    lists:seq(1, NumberOfIterations)
  ).
