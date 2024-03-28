-module(chat_server).

-behaviour(gen_server).

-import(utils, [list_contains/2]). % Importa una función list_contains/2 del módulo utils.

-export([start_link/0, start/0, stop/0]). % Exporta funciones start_link/0, start/0 y stop/0.

-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE). % Define el nombre del servidor como ?MODULE, que se sustituirá con chat_server.

-include("chat_types.hrl"). % Incluye el archivo de cabecera "chat_types.hrl".

-record(state, {clients=[] :: [client()], messages=[] :: [message()]}). % Define un registro llamado state con campos clients y messages.

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []). % Inicia el servidor de gen_server con el nombre local ?SERVER.

start() -> start_link(). % Función de conveniencia para iniciar el servidor.

stop() -> gen_server:stop(?MODULE). % Detiene el servidor.

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}. % Inicializa el estado del servidor.

-spec(handle_call(Request :: command(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: chat_server_response(), NewState :: #state{}} |
  {reply, Reply :: chat_server_response(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, From, State) ->
  % Maneja las llamadas al servidor.
  io:format("server: received call: ~p~n", [Request]),
  {Pid, Tag} = From,
  #state{clients=Clients, messages=Messages} = State,
  case Request of
    {join, Username} -> 
      % Maneja la solicitud de unirse al chat.
      case client_with_pid_exists(Pid, Clients) of
        true -> {reply, already_joined, State};
        false ->
          erlang:monitor(process, Pid),
          NewClients = [{Pid, Tag, Username}|Clients],
          {reply, {history, Messages}, State#state{clients=NewClients}};
        _ -> this_shouldnt_happen 
      end;
    {say, Text} ->
      % Maneja el envío de un mensaje al chat.
      case get_username(Pid, Clients) of
        false -> {reply, you_are_not_a_user_yet, State}; 
        Username -> 
          send_message_to_clients(Username, Text, Clients),
          {reply, you_said_something, State#state{messages=[{Username,Text} | Messages]}}
      end;
    get_clients -> {reply, State#state.clients, State};
    get_history -> {reply, State#state.messages, State};
    stop -> {stop, normal, shutdown_ok, State};
    _ -> {reply, wat, State}
  end.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Request, State) ->
  % Maneja las operaciones de cast al servidor.
  case Request of
    {part, Pid} -> 
      io:format("~p wants to part, removing it from clients~n", [Pid]),
      no_reply_with_state_without_pid(State, Pid);
    _ ->
      io:format("server: received cast: ~p~n", [Request]),
      {noreply, State}
  end.

no_reply_with_state_without_pid(State, Pid) ->
  % Elimina un cliente del estado.
  OldClients = State#state.clients,
  NewClients = get_clients_without_pid(Pid, OldClients),
  {noreply, State#state{clients = NewClients}}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(Info, State) ->
  % Maneja la información recibida por el servidor.
  case Info of
    {'DOWN', _Ref, process, Pid, Why} ->
      io:format("~p has died because of ~p, removing it from clients~n", [Pid, Why]),
      no_reply_with_state_without_pid(State, Pid);
    _ ->
      io:format("server: received info: ~p~n", [Info]),
      {noreply, State}
  end.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  % Maneja la terminación del servidor.
  io:format("server: terminating~n"),
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  % Maneja el cambio de código.
  {ok, State}.

client_has_pid(Client, Pid) ->
  % Comprueba si un cliente tiene un Pid específico.
  {Client_pid, _Tag, _Name} = Client,
  Client_pid == Pid.

client_with_pid_exists(Pid, Clients) -> 
  % Comprueba si un Pid de cliente ya existe en la lista de clientes.
  lists:any(fun(C) -> client_has_pid(C, Pid) end, Clients).

get_clients_without_pid(Pid, Clients) ->
  % Obtiene la lista de clientes sin un Pid específico.
  lists:filter(fun(C) -> not client_has_pid(C, Pid) end, Clients). 

-spec get_username(Pid :: pid(), Clients :: [client()]) -> false | string().
get_username(Pid, Clients) ->
  % Obtiene el nombre de usuario asociado a un Pid de cliente.
  case lists:keyfind(Pid, 1, Clients) of
    {_Pid, _Tag, Username} -> Username;
    false -> false
  end.

get_client_pid(Client) -> 
  % Obtiene el Pid de un cliente.
  case Client of
    {Pid, _Tag, _Username} -> Pid
  end.

send_message_to_clients(Username, Text, Clients) ->
  % Envía un mensaje a todos los clientes.
  Pids = lists:map(fun get_client_pid/1, Clients),
  SendMessageToPid= fun(Pid) ->
    io:format("server: sending ~p to ~p~n", [{said, Username, Text}, Pid]),
    Pid ! {said, Username, Text}
  end,
  lists:foreach(SendMessageToPid, Pids).
