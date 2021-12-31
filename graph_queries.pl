:- use_module(graph_helpers).
:- use_module(knowledgeBase).
:- use_module(helpers).

origem(neverland).

% PROCURAS NÃO INFORMADAS

% DFS

dfsQ(Origem,Destino,Caminho) :- profundidade(Origem,Destino,Caminho).

% BFS

bfsQ(Origem,Destino,Caminho) :- largura(Origem,Destino,Caminho).

% PROCURAS INFORMADAS

% Gulosa

gulosaQ(Origem,Destino,CaminhoDist,CaminhoTran) :- gulosa(Origem,Destino,CaminhoDist,CaminhoTran).

% AEstrela

aestrelaQ(Origem,Destino,CaminhoDist,CaminhoTran) :- aestrela(Origem,Destino,CaminhoDist,CaminhoTran).

% Gerar Circuitos

gerarCircuitos(Circuitos) :- 
	gerarGulosa(Circuitos).
	%gerarDfs(Dfs),
	%gerarBfs(Bfs),
	%gerarAestrela(Aestrela).

% Calcular tempo de um percurso dado peso e transporte

tempo([],_,_,0).
tempo([_],_,_,0).
tempo([Nodo1,Nodo2|OutrosNodos], Transporte, Peso, Tempo) :- 
    edge(Nodo1,Nodo2,_,Distancia,_),
    transporte(Transporte,_,VelocidadeMedia),
    velocidadeDiminui(Transporte, Diminuicao),
    Velocidade is VelocidadeMedia - Diminuicao*Peso,
    tempo([Nodo2|OutrosNodos], Transporte, Peso, Tempo2), 
    Tempo is Distancia/Velocidade + Tempo2.

% Identificar circuitos com maior número de entregas (por volume e peso). Recebem LISTA de caminhos.

%% Por peso  

sumPesos([],0).
sumPesos([CodEnc|T], PesoTotal) :- 
    encomenda(CodEnc,_,_,_,Peso,_,_,_,_,_),
    sumPesos(T, PesoR), 
    PesoTotal is Peso + PesoR.

ligaCircuitosAoPesoEncomendas([],_).
ligaCircuitosAoPesoEncomendas([Circuito|OutrosCircuitos], [Circuito/PesoTotal|Res]) :-
    caminho(CodCaminho, Circuito),
    findall(CodEncomenda, encomendaCaminho(CodCaminho, CodEncomenda), ListaEncomendas),
    sumPesos(ListaEncomendas, PesoTotal),
    ligaCircuitosAoPesoEncomendas(OutrosCircuitos, Res).

ordenaCircuitosPeso([],_).
ordenaCircuitosPeso(Circuitos, Ordenados) :-
    ligaCircuitosAoPesoEncomendas(Circuitos, CircuitosMaisValor),
    ordDecrescente(CircuitosMaisValor, Ordenados).

%% Por volume

sumVolumes([],0).
sumVolumes([CodEnc|T], VolumeTotal) :-
    encomenda(CodEnc,_,_,_,_,Volume,_,_,_,_),
    sumVolumes(T, VolumeR),
    VolumeTotal is Volume + VolumeR.

ligaCircuitosAoVolumeEncomendas([],_).
ligaCircuitosAoVolumeEncomendas([Circuito|OutrosCircuitos], [Circuito/VolumeTotal|Res]) :-
    caminho(CodCaminho, Circuito),
    findall(CodEncomenda, encomendaCaminho(CodCaminho, CodEncomenda), ListaEncomendas),
    sumVolumes(ListaEncomendas, VolumeTotal),
    ligaCircuitosAoVolumeEncomendas(OutrosCircuitos, Res).

ordenaCircuitosVolume([],_).
ordenaCircuitosVolume(Circuitos, Ordenados) :-
    ligaCircuitosAoVolumeEncomendas(Circuitos, CircuitosMaisValor),
    ordDecrescente(CircuitosMaisValor, Ordenados).
