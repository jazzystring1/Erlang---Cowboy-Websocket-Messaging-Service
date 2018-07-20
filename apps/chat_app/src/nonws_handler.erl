-module(nonws_handler).
-behavior(cowboy_handler).
%%-record(users, {name=""}).
-export([init/2, info/3, send_message/2, subscribe/1, terminate/3]).

subscribe(Name) ->
	gproc:reg_or_locate({n, l, {?MODULE, [Name]}}, self()),
	io:format("PID : ~p ~n", [self()]).


init(Req0, State) ->
	io:format("NON-WS STARTED : ~p ~n", [self()]),
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"POST, GET, OPTIONS">>, Req0),
	Req2 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"content-type">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2),
	case cowboy_req:method(Req3) of 
		<<"GET">> ->			
			io:format("I AM GET"),
			QsVals = cowboy_req:parse_qs(Req3),
			{_ , Name} = lists:keyfind(<<"name">>, 1, QsVals),
			NewState = [Name],
			subscribe(Name),
			{cowboy_loop, Req3, NewState, hibernate};
		<<"POST">> ->
			io:format("I AM POST"),
			send_message(Req3, State),
			{ok, Req3, State};
		<<"OPTIONS">> ->
			io:format("I AM OPTIONS"),
			cowboy_req:reply(200, #{}, <<"true">>, Req3),
			{ok, Req3, State};
		_  ->
		    {ok, Req3, State}
	end.

send_message(Req0, State) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
        	Payload = jsx:decode(Data),
        	{From, To, Msg} = {proplists:get_value(<<"From">>, Payload), proplists:get_value(<<"To">>, Payload),
			proplists:get_value(<<"Message">>, Payload)},
			try gproc:send({n, l, {?MODULE, [To]}}, {{private_message, From}, {?MODULE, [To]}, Msg}) of
		 		{_From, _To, _Msg} ->
		 		 	io:format("VALID ~n"),
		 		 	JSON = jsx:encode([{<<"status">>,true}]),
		 		 	cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, JSON, Req)
		  	catch 
		 		_:_ ->
		 		 	io:format("INVALID ~n"),
		 		    JSON = jsx:encode([{<<"status">>,false}]),
		 		 	cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, JSON, Req)
		 	end,
		 	{ok, Req, State};
        {more, Data, Req} ->
            Payload = jsx:decode(Data),
        	{_From, _To, _Message} = {proplists:get_value(<<"From">>, Payload), proplists:get_value(<<"Message">>, Payload),
			proplists:get_value(<<"To">>, Payload)},
    		cowboy_req:reply(200, #{}, <<"true">>, Req),
			send_message(_To, _Message),
			{ok, Req, State}
    end.

info({{private_message, From}, _To, Msg}, Req, State) ->
	io:format("BLOOP ~n"),
	JSON = jsx:encode([{<<"from">>, From},{<<"msg">>,Msg}]),
    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, JSON, Req),
    {stop, Req, State};
info(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.

terminate(_Reason, _Req, _State) ->
	io:format("terminate ~n"),
	ok.
