-module(ws_handler).
-behavior(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2, terminate/3]).
-export([send_message/3, broadcast_message/1, subscribe/1, mongodb/0]).
-define(WSKEY, processKey).

subscribe(Name) ->
	gproc:reg_or_locate({n, l, {?MODULE, [Name]}}, [self()]).

send_message(From, Name, Msg) ->
 	try gproc:send({n, l, {?MODULE, [Name]}}, {{private_msg, From}, {?MODULE, [Name]}, Msg}) of
 		{_From, _To, _Msg}	->
 		 	valid
  	catch 
 		_:_ ->
 			invalid
 	end.

broadcast_message({broadcast_msg, From, Msg}) -> 
	gproc:send({p, l, {?MODULE, ?WSKEY}}, {{broadcast_msg, From}, {?MODULE, ?WSKEY}, Msg});

broadcast_message({notify_presence, Presence}) ->
	gproc:send({p, l, {?MODULE, ?WSKEY}}, {notify_presence, {?MODULE, ?WSKEY}, Presence});

broadcast_message({both, From, Msg, Presence}) ->
	gproc:send({p, l, {?MODULE, ?WSKEY}}, {notify_presence, {?MODULE, ?WSKEY}, Presence}),
	gproc:send({p, l, {?MODULE, ?WSKEY}}, {{broadcast_msg, From}, {?MODULE, ?WSKEY}, Msg}).

mongodb() ->
	Database = <<"test">>,
	Connection = mc_worker_api:connect([{database, Database}]),
	Collection = <<"users">>,
	mc_worker_api:insert(Connection, Collection, #{<<"name">> => <<"Yankees">>, <<"home">> =>
    #{<<"city">> => <<"New York">>, <<"state">> => <<"NY">>}, <<"league">> => <<"American">>}).
	
init(Req, State) ->
	{cowboy_websocket, Req, State}.

websocket_init(State) ->
	io:format("State : ~p ~n", [State]),
	gproc:reg({p, l, {?MODULE, ?WSKEY}}),
	JSON = jsx:encode([{<<"presence">>, true}, {<<"chat_presence">>, chat_room:chat_presence()}]),
	{reply, {text, JSON}, State, hibernate}.

websocket_handle({text, _Data}, _State) ->	
	Payload = jsx:decode(_Data),
	{From, To, Message} = {proplists:get_value(<<"From">>, Payload), proplists:get_value(<<"To">>, Payload),
	proplists:get_value(<<"Message">>, Payload)},
	 case proplists:get_value(<<"Broadcast_msg">>, Payload) of
	 		true ->
	 			if 
	 				length(_State) /= 0 ->
	 					io:format("State : ~p ~n", [_State]),
	 					broadcast_message({broadcast_msg, _State, Message}), %% _State is the variable for your nickname
	 					{ok, _State, hibernate};
	 				true ->
	 					%% user_state = name of the state for ws_handler, new_presence - updated user lists
	 					[{user_state, NewState}, {new_presence, Presence}] = chat_room:chat_enter(From), 
	 					%% Create new channel room for private messaging
	 				 	subscribe(From),
	 				 	%% Broadcast the updated online users and Message as well
	 					broadcast_message({both, NewState, Message, Presence}),
	 					io:format("NewState : ~p ~n", [NewState]),
	 					{ok, NewState, hibernate}
	 			end;
	 		undefined ->
	 			case proplists:get_value(<<"Private_msg">>, Payload) of
				 		true ->
				 			Data = send_message(_State, To, Message),
				 			if 
				 				Data ==	valid ->
				 					{ok, _State};
				 				true ->
				 					JSON = jsx:encode([{<<"Info">>, true},{<<"Msg">>,<<"Nickname doesn't exist!">>}]),
				 					{reply, {text, JSON}, _State}
				 			end;
				 			
				 		undefined ->
				 			{ok, _State, hibernate}
				 end
	 end;
	 

websocket_handle({binary, Data}, State) ->
	{reply, {binary, Data}, State};
websocket_handle(_Frame, State) ->
	{ok, State}.

websocket_info({notify_presence, _All, Presence}, State) ->
	io:format("Notify Presence ~n"),
	JSON = jsx:encode([{<<"presence">>, true}, {<<"chat_presence">>, Presence}]),
 	{reply, {text, JSON}, State};
websocket_info({{broadcast_msg, Sender}, _All, Msg}, State) when length(State) /= 0 ->
 	io:format("broadcast ~n"),
 	[From] = Sender,
	JSON = jsx:encode([{<<"Message">>, true}, {<<"From">>, From}, {<<"Msg">>,Msg}]),
 	{reply, {text, JSON}, State};
websocket_info({{private_msg, Sender}, _All, Msg}, State) when length(State) /= 0 ->
 	[From] = Sender,
	JSON = jsx:encode([{<<"Priv">>, true}, {<<"From">>, From}, {<<"Msg">>,Msg}]),
 	{reply, {text, JSON}, State};
websocket_info(_Info, State) ->    %% Info with no state
 	io:format("No state"),
 	{ok, State}.

terminate(_Reason, _Req, State) when length(State) /= 0 ->
    io:format("EXIT ~n"),
    NewState = chat_room:chat_leave(State),
    broadcast_message({notify_presence, NewState}),
	{ok, State};
terminate(_Reason, _Req, State) ->
	{ok, State}.