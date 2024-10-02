-module(server).
-export([start/1]).
-export([call/2, cast/2]).
-export([init/1]).

%% Função start/1:
%% Inicia o servidor genérico, criando um novo processo que executa a função init/1
%% com o módulo especificado como argumento.
start(Mod) ->
    spawn(server, init, [Mod]).

%% Função call/2:
%% Envia uma mensagem síncrona ao servidor e aguarda uma resposta.
%% - Name: o nome do servidor (processo registrado).
%% - Req: a requisição enviada ao servidor.
call(Name, Req) ->
    %% Envia uma mensagem {call, self(), Req} ao servidor.
    Name ! {call, self(), Req},
    %% Espera por uma resposta na forma {Name, Res}.
    receive
        {Name, Res} ->
            Res
    end.

%% Função cast/2:
%% Envia uma mensagem assíncrona ao servidor e retorna imediatamente.
%% - Name: o nome do servidor (processo registrado).
%% - Req: a requisição enviada ao servidor.
cast(Name, Req) ->
    %% Envia uma mensagem {cast, Req} ao servidor.
    Name ! {cast, Req},
    ok.

%% Função init/1:
%% Inicializa o servidor registrando o processo atual e iniciando o estado.
%% - Mod: o módulo que implementa as funções de callback.
init(Mod) ->
    %% Registra o processo atual com o nome Mod.
    register(Mod, self()),
    %% Chama a função init/0 do módulo para inicializar o estado.
    State = Mod:init(),
    %% Entra no loop principal do servidor.
    loop(Mod, State).

%% Função loop/2:
%% Loop principal do servidor, processa mensagens recebidas e atualiza o estado.
%% - Mod: o módulo que implementa as funções de callback.
%% - State: o estado atual do servidor.
loop(Mod, State) ->
    receive
        %% Processa mensagens do tipo {call, From, Req}.
        {call, From, Req} ->
            %% Chama a função handle_call/2 do módulo para processar a requisição.
            {Res, State2} = Mod:handle_call(Req, State),
            %% Envia a resposta de volta ao remetente.
            From ! {Mod, Res},
            %% Continua o loop com o novo estado.
            loop(Mod, State2);
        %% Processa mensagens do tipo {cast, Req}.
        {cast, Req} ->
            %% Chama a função handle_cast/2 do módulo para processar a requisição.
            State2 = Mod:handle_cast(Req, State),
            %% Continua o loop com o novo estado.
            loop(Mod, State2)
    end.
