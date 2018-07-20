-module(chat_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
		case os:getenv("PORT") of
            false ->
             {_Status, Port } =  application:get_env(chat, port);
            Other ->
                Port = Other
        end,        
	Dispatch = cowboy_router:compile([
            {'_', [
            {"/", cowboy_static, {priv_file, chat, "index.html"}},
            {"/non-ws", nonws_handler, []},
            {"/wsocket", ws_handler,  []}
            ]}
        ]),
        {ok, _} = cowboy:start_clear(my_http_listener,
            [{port, list_to_integer(Port)}],
            #{env => #{dispatch => Dispatch}}
        ),
	chat_sup:start_link().

stop(_State) ->
	ok.
