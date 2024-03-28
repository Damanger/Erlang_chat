-module(chat_client).

-include("chat_types.hrl"). % Incluye el archivo de cabecera "chat_types.hrl" que define los tipos de datos necesarios.

-export([ start/2, messaging_loop/1, print_message/1]). % Exporta las funciones start/2, messaging_loop/1 y print_message/1.

% Función para iniciar el cliente de chat.
start(Username, Server) ->
  MessagingClientPid = spawn_link(?MODULE, messaging_loop, [Server]), % Crea un nuevo proceso para el bucle de mensajería y lo vincula.
  MessagingClientPid ! {join, Username}, % Envía un mensaje al proceso de bucle de mensajería para unirse al servidor.
  input_loop(MessagingClientPid). % Inicia el bucle de entrada.

% Bucle de entrada del cliente.
input_loop(MessagingClientPid) ->
  Input = string:strip(io:get_line(""), both, $\n), % Lee una línea de entrada del usuario.
  case Input of
    "part" ->
      io:format("disconnecting from the server~n"),
      MessagingClientPid ! part; % Envía un mensaje al proceso de bucle de mensajería para desconectarse.
    _ ->
      MessagingClientPid ! {say, Input}, % Envía el mensaje de entrada al servidor.
      input_loop(MessagingClientPid)
  end.

% Bucle de mensajería del cliente.
messaging_loop(Server) -> 
  receive
    {join, Username} ->
      {history, History} = gen_server:call(Server, {join, Username}), % Realiza una llamada al servidor para unirse al chat.
      HistoryInOrder = lists:reverse(History), % Invierte el historial para imprimirlo en orden cronológico.
      lists:foreach(fun print_message/1, HistoryInOrder), % Imprime cada mensaje en el historial.
      messaging_loop(Server);
    {say, Text} -> 
      _Reply = gen_server:call(Server, {say, Text}), % Realiza una llamada al servidor para enviar un mensaje.
      messaging_loop(Server);
    {said, Username, Text} ->
      print_message({Username, Text}), % Imprime un mensaje recibido.
      messaging_loop(Server);
    part -> 
      gen_server:cast(Server, {part, self()}), % Envia un mensaje de despedida al servidor.
      io:format("messaging loop finishing~n"),
      init:stop() % Detiene el cliente.
  end.

% Función para imprimir un mensaje en la consola.
print_message(Message) ->
  case Message of
    {Username, Text} -> io:format("<~ts> ~ts~n",[Username, Text]) % Imprime el mensaje con formato.
  end.
