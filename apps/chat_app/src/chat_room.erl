-module(chat_room).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([chat_enter_nws/1, send_message_nws/2, chat_enter/1, chat_leave/1, chat_presence/0, do_send/2]).

-define(SERVER, ?MODULE). 

-record(state, {user = []}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

chat_enter_nws(Name) ->
    gen_server:cast(?SERVER, {chat_enter_nws, Name}).

send_message_nws(To, Message) ->
    gen_server:call(?SERVER, {send_message_nws, To, Message}).

chat_enter(Name) ->
    gen_server:call(?SERVER, {chat_enter, Name}).

chat_leave(Name) ->
    gen_server:call(?SERVER, {chat_leave, Name}).

chat_presence() ->
    gen_server:call(?SERVER, {chat_presence}).

init([]) ->
    application:start(gproc),
    application:start(jsx),
    application:ensure_all_started (mongodb),
    application:start(cowboy_fcgi),
    {ok, #state{}}.


handle_call({send_message_nws, To, Message}, _From, State = #state{user=_Users}) ->
   %% Send the value of Name for the state in ws_handler :)
    case do_send(To, Message) of
        valid ->
            {reply, <<"VALID">>, State, hibernate};
        invalid ->
            {reply, <<"INVALID">>, State, hibernate};
        _ ->
            {reply, <<"ERROR">>, State, hibernate}
    end;
handle_call({chat_enter, Name}, _From, State = #state{user=Users}) ->
   %% Send the value of Name for the state in ws_handler :)
    io:format("~p ~n", [self()]),
    NewState = State#state{user=[Name|Users]},
    {reply, [{user_state, [Name]}, {new_presence, [Name|Users]}], NewState, hibernate};
handle_call({chat_presence}, _From, State = #state{user=Users}) ->
   %% Send the list of users of the state record to the ws_handler :)
    {reply, Users, State, hibernate};
handle_call({chat_leave, Name}, _From, State =  #state{user=Users}) ->
    if 
        length(Name) /= 0 ->
            [From] = Name,
            NewState = lists:delete(From, Users),
            io:format("~p ~n", [NewState]),
            {reply, [NewState], State#state{user = NewState}, hibernate};
        true ->
            {noreply, State}
    end.
handle_cast({chat_enter_nws, Name}, State = #state{user=_Users}) ->
   %% Send the value of Name for the state in ws_handler :)
    io:format("~p ~n", [self()]),
    gproc:reg_or_locate({n, l, {?MODULE, [Name]}}, [self()]),
  %%  NewState = State#state{user=[Name|Users]},
    {noreply, State, hibernate};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(my_http_listener),
    ok.

code_change(_OldVsn, State, _Extra) ->
{ok, State}.

do_send(To, Message) ->
    try gproc:send({n, l, {?MODULE, [To]}}, {self(), {?MODULE, [To]}, Message}) of
        {_From, _To, _Msg}  ->
         valid
    catch 
        _:_ ->
         invalid

    end.
