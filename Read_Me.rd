make chat_server

erl -sname client1 -setcookie pass

chat_client:start("Nombre", {chat_server, serv@Damanger}).