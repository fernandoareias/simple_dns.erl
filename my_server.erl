-module(my_server).
-export([start/0, init/0, handle_call/2, handle_cast/2]).

%% Função start/0:
%% Inicia o servidor específico utilizando o servidor genérico.
start() ->
    server:start(my_server).

%% Função init/0:
%% Inicializa o estado do servidor.
init() ->
    %% Estado inicial é 0.
    0.

%% Função handle_call/2:
%% Processa requisições síncronas.
%% - Req: a requisição recebida.
%% - State: o estado atual.
handle_call({add, N}, State) ->
    %% Adiciona N ao estado e retorna o novo estado.
    {ok, State + N};
handle_call(get, State) ->
    %% Retorna o estado atual.
    {ok, State}.

%% Função handle_cast/2:
%% Processa requisições assíncronas.
%% - Req: a requisição recebida.
%% - State: o estado atual.
handle_cast(reset, _State) ->
    %% Reseta o estado para 0.
    0;
handle_cast(_, State) ->
    %% Para qualquer outra requisição, mantém o estado atual.
    State.
